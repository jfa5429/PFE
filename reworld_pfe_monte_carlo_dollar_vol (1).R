suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(ggplot2)
  library(scales)
  library(MASS)  # mvrnorm
  library(gridExtra)  # grid.arrange
})

options(
  pillar.sigfig = 6,
  digits = 6
)

# -----------------------------------------------------------
# 1. CONFIG
# -----------------------------------------------------------

cfg <- list(
  file_path       = "C:/Users/AllenFr/OneDrive - SmartestEnergy/Documents/",
  price_file      = "mass_hub_26_31.csv",
  volume_file     = "reworld_pfe_volumes.csv",
  col_asof        = "As of Date",
  col_maturity    = "Maturity Date",
  col_curve_name  = "Peak Offpeak",
  col_curve_value = "Curve Value",
  start_maturity  = as.Date("2026-04-01"),
  end_maturity    = as.Date("2032-12-31"),
  n_sims          = 10000L,
  drift_annual    = 0.00,  # No drift for dollar vol model (or set small $/month)
  vol_lookback_n  = 30L,
  price_floor     = 5.0,   # Minimum price floor to prevent negative prices
  # Set valuation_date to use historical data (NULL = use latest available)
  # For pre-storm analysis: as.Date("2026-01-16")
  valuation_date  = NULL,
  # Output file suffix (auto-set based on valuation_date if NULL)
  output_suffix   = NULL
)

# Set output suffix based on valuation date
if (!is.null(cfg$valuation_date)) {
  cfg$output_suffix <- paste0("_prestorm_", format(cfg$valuation_date, "%Y%m%d"))
} else {
  cfg$output_suffix <- ""
}

cat("========================================\n")
cat("ReWorld Holdings PFE Model (DOLLAR VOL)\n")
cat("========================================\n\n")

# -----------------------------------------------------------
# 2. LOAD RAW CURVE DATA
# -----------------------------------------------------------

curve_raw <- read_csv(paste0(cfg$file_path, cfg$price_file), show_col_types = FALSE) %>%
  rename(
    as_of_raw      = !!cfg$col_asof,
    maturity_raw   = !!cfg$col_maturity,
    curve_name_raw = !!cfg$col_curve_name,
    curve_value    = !!cfg$col_curve_value
  ) %>%
  mutate(
    as_of_date    = as.Date(as_of_raw, format = "%m/%d/%Y"),
    maturity_date = as.Date(maturity_raw, format = "%m/%d/%Y")
  ) %>%
  filter(!is.na(as_of_date), !is.na(maturity_date))

cat("Loaded", nrow(curve_raw), "price records\n")
cat("Date range:", as.character(min(curve_raw$as_of_date)), "to", 
    as.character(max(curve_raw$as_of_date)), "\n")

# -----------------------------------------------------------
# 2b. EXTRAPOLATE 2032 PRICES FROM 2031
# -----------------------------------------------------------

# Check if 2032 prices are missing
max_maturity <- max(curve_raw$maturity_date)
if (year(max_maturity) < 2032) {
  cat("\n--- Extrapolating 2032 prices from 2031 ---\n")
  
  # Get 2031 prices
  prices_2031 <- curve_raw %>%
    filter(year(maturity_date) == 2031)
  
  # Create 2032 prices by copying 2031 and shifting year
  prices_2032 <- prices_2031 %>%
    mutate(
      maturity_date = maturity_date %m+% years(1),
      maturity_raw = format(maturity_date, "%m/%d/%Y")
    )
  
  # Append to curve_raw
  curve_raw <- bind_rows(curve_raw, prices_2032)
  
  cat("Added", nrow(prices_2032), "extrapolated 2032 price records\n")
  cat("New maturity range:", as.character(min(curve_raw$maturity_date)), "to",
      as.character(max(curve_raw$maturity_date)), "\n")
}

# -----------------------------------------------------------
# 3. BUILD HISTORICAL PEAK AND OFF-PEAK DATA
# -----------------------------------------------------------

peak_data <- curve_raw %>%
  filter(curve_name_raw == "Peak") %>%
  transmute(as_of_date, maturity_date, price = curve_value, type = "Peak")

offpeak_data <- curve_raw %>%
  filter(curve_name_raw == "Off-Peak") %>%
  transmute(as_of_date, maturity_date, price = curve_value, type = "Off-Peak")

# Combine and filter to maturity range
historical_prices <- bind_rows(peak_data, offpeak_data) %>%
  filter(maturity_date >= cfg$start_maturity, maturity_date <= cfg$end_maturity) %>%
  arrange(type, maturity_date, as_of_date)

cat("\nPrice data after filtering:\n")
cat("Maturity range:", as.character(min(historical_prices$maturity_date)), "to",
    as.character(max(historical_prices$maturity_date)), "\n")
cat("Number of unique maturities:", n_distinct(historical_prices$maturity_date), "\n")

# -----------------------------------------------------------
# 4. CURRENT FORWARD CURVE + DOLLAR VOLATILITY STATS
# -----------------------------------------------------------

# Set valuation date (use config or latest available)
if (!is.null(cfg$valuation_date)) {
  # Filter historical data to only include dates up to valuation_date
  historical_prices <- historical_prices %>%
    filter(as_of_date <= cfg$valuation_date)
  # Use latest available date up to valuation_date (handles weekends/holidays)
  valuation_date <- max(historical_prices$as_of_date, na.rm = TRUE)
  cat("\n*** HISTORICAL ANALYSIS MODE ***\n")
  cat("Requested valuation date:", as.character(cfg$valuation_date), "\n")
  cat("Actual valuation date used:", as.character(valuation_date), "\n")
} else {
  valuation_date <- max(historical_prices$as_of_date, na.rm = TRUE)
}
cat("\nValuation date:", as.character(valuation_date), "\n")

# Current curve - get prices as of valuation date
current_curve <- historical_prices %>%
  filter(as_of_date == valuation_date) %>%
  distinct(maturity_date, type, price) %>%
  arrange(maturity_date, type)

cat("\nCurrent forward curve (first 12 rows):\n")
print(head(current_curve, 12))

# Build DOLLAR changes (not log returns)
historical_changes <- historical_prices %>%
  arrange(type, maturity_date, as_of_date) %>%
  group_by(type, maturity_date) %>%
  mutate(dollar_change = price - lag(price)) %>%
  ungroup()

# Trailing volatility (30-day) in DOLLAR terms
vol_summary <- historical_changes %>%
  filter(!is.na(dollar_change)) %>%
  group_by(type, maturity_date) %>%
  arrange(as_of_date, .by_group = TRUE) %>%
  slice_tail(n = cfg$vol_lookback_n) %>%
  summarise(
    sd_dollar_daily = sd(dollar_change, na.rm = TRUE),
    n_obs_used      = n(),
    .groups         = "drop"
  ) %>%
  mutate(
    ann_factor        = sqrt(252),
    sd_dollar_ann     = sd_dollar_daily * ann_factor,
    sd_dollar_monthly = sd_dollar_ann / sqrt(12)
  )

# Forward vol table
forward_vol_table <- current_curve %>%
  left_join(vol_summary, by = c("maturity_date", "type")) %>%
  arrange(maturity_date, type)

cat("\n========================================\n")
cat("VOLATILITY SUMMARY (DOLLAR TERMS)\n")
cat("========================================\n")

cat("\nPeak volatilities (first 12):\n")
forward_vol_table %>%
  filter(type == "Peak") %>%
  dplyr::select(maturity_date, price, sd_dollar_daily, sd_dollar_ann, sd_dollar_monthly, n_obs_used) %>%
  head(12) %>%
  print()

cat("\nOff-Peak volatilities (first 12):\n")
forward_vol_table %>%
  filter(type == "Off-Peak") %>%
  dplyr::select(maturity_date, price, sd_dollar_daily, sd_dollar_ann, sd_dollar_monthly, n_obs_used) %>%
  head(12) %>%
  print()

# Average volatility stats
cat("\n--- Average Volatility (Annualized $/MWh) ---\n")
cat("Peak: $", round(mean(vol_summary$sd_dollar_ann[vol_summary$type == "Peak"], na.rm = TRUE), 2), "/MWh\n", sep = "")
cat("Off-Peak: $", round(mean(vol_summary$sd_dollar_ann[vol_summary$type == "Off-Peak"], na.rm = TRUE), 2), "/MWh\n", sep = "")

# -----------------------------------------------------------
# 5. BUILD CORRELATION MATRIX FROM DOLLAR CHANGES
# -----------------------------------------------------------

# Create contract IDs: PEAK_YYYY-MM-DD or OFFPK_YYYY-MM-DD
historical_changes <- historical_changes %>%
  mutate(
    contract_id = paste0(
      ifelse(type == "Peak", "PEAK_", "OFFPK_"),
      as.character(maturity_date)
    )
  )

# Pivot to wide format
changes_wide <- historical_changes %>%
  filter(!is.na(dollar_change)) %>%
  dplyr::select(as_of_date, contract_id, dollar_change) %>%
  pivot_wider(names_from = contract_id, values_from = dollar_change) %>%
  arrange(as_of_date)

changes_matrix <- changes_wide %>%
  dplyr::select(-as_of_date) %>%
  as.matrix()

cor_matrix <- cor(changes_matrix, use = "pairwise.complete.obs")

cat("\n========================================\n")
cat("CORRELATION MATRIX\n")
cat("========================================\n")
cat("Correlation matrix dimensions:", dim(cor_matrix), "\n")

# Sample correlations
cat("\nSample correlations:\n")
if ("PEAK_2026-04-01" %in% colnames(cor_matrix) && "PEAK_2026-05-01" %in% colnames(cor_matrix)) {
  cat("  Peak Apr-26 vs Peak May-26:", round(cor_matrix["PEAK_2026-04-01", "PEAK_2026-05-01"], 3), "\n")
}
if ("PEAK_2026-04-01" %in% colnames(cor_matrix) && "OFFPK_2026-04-01" %in% colnames(cor_matrix)) {
  cat("  Peak Apr-26 vs OffPk Apr-26:", round(cor_matrix["PEAK_2026-04-01", "OFFPK_2026-04-01"], 3), "\n")
}

# -----------------------------------------------------------
# 6. LOAD DEAL VOLUMES
# -----------------------------------------------------------

volumes_raw <- read_csv(paste0(cfg$file_path, cfg$volume_file), show_col_types = FALSE, skip = 1)

# Clean and parse
volumes <- volumes_raw %>%
  dplyr::select(1:6) %>%
  rename(
    Month = 1,
    peak_hrs = 2,
    offpeak_hrs = 3,
    peak_mwh = 4,
    offpeak_mwh = 5,
    peak_mw = 6
  ) %>%
  filter(!is.na(Month), Month != "") %>%
  mutate(
    maturity_date = as.Date(Month, format = "%m/%d/%Y"),
    peak_mwh = as.numeric(gsub("[, ]", "", peak_mwh)),
    offpeak_mwh = as.numeric(gsub("[, ]", "", offpeak_mwh)),
    peak_hrs = as.numeric(peak_hrs),
    offpeak_hrs = as.numeric(offpeak_hrs),
    peak_mw = as.numeric(gsub("[, ]", "", peak_mw))
  ) %>%
  filter(!is.na(maturity_date)) %>%
  dplyr::select(maturity_date, peak_mwh, offpeak_mwh, peak_hrs, offpeak_hrs, peak_mw)

cat("\n========================================\n")
cat("DEAL VOLUMES\n")
cat("========================================\n")
cat("Number of delivery months:", nrow(volumes), "\n")
cat("Delivery period:", as.character(min(volumes$maturity_date)), "to",
    as.character(max(volumes$maturity_date)), "\n")
cat("Total Peak MWh:", format(sum(volumes$peak_mwh, na.rm = TRUE), big.mark = ","), "\n")
cat("Total Off-Peak MWh:", format(sum(volumes$offpeak_mwh, na.rm = TRUE), big.mark = ","), "\n")
cat("Total MWh:", format(sum(volumes$peak_mwh, na.rm = TRUE) + sum(volumes$offpeak_mwh, na.rm = TRUE), big.mark = ","), "\n")

cat("\nVolumes (first 12 months):\n")
print(head(volumes, 12))

# -----------------------------------------------------------
# 7. BUILD CONTRACT TABLE
# -----------------------------------------------------------

# Create Peak contracts
peak_contracts <- volumes %>%
  inner_join(
    forward_vol_table %>% filter(type == "Peak") %>% dplyr::select(maturity_date, price, sd_dollar_monthly),
    by = "maturity_date"
  ) %>%
  transmute(
    contract_id = paste0("PEAK_", as.character(maturity_date)),
    maturity_date,
    type = "Peak",
    mwhrs = peak_mwh,
    strike = price,
    monthly_vol = sd_dollar_monthly,  # Now in $/MWh
    delivery_end = ceiling_date(maturity_date, "month") - 1
  )

# Create Off-Peak contracts
offpeak_contracts <- volumes %>%
  inner_join(
    forward_vol_table %>% filter(type == "Off-Peak") %>% dplyr::select(maturity_date, price, sd_dollar_monthly),
    by = "maturity_date"
  ) %>%
  transmute(
    contract_id = paste0("OFFPK_", as.character(maturity_date)),
    maturity_date,
    type = "Off-Peak",
    mwhrs = offpeak_mwh,
    strike = price,
    monthly_vol = sd_dollar_monthly,  # Now in $/MWh
    delivery_end = ceiling_date(maturity_date, "month") - 1
  )

# Combine all contracts
contracts <- bind_rows(peak_contracts, offpeak_contracts) %>%
  arrange(maturity_date, type) %>%
  filter(!is.na(mwhrs), !is.na(strike), !is.na(monthly_vol))

n_contracts <- nrow(contracts)

cat("\n========================================\n")
cat("CONTRACT TABLE\n")
cat("========================================\n")
cat("Total contracts:", n_contracts, "\n")
cat("Peak contracts:", sum(contracts$type == "Peak"), "\n")
cat("Off-Peak contracts:", sum(contracts$type == "Off-Peak"), "\n")
cat("Total MWh:", format(sum(contracts$mwhrs), big.mark = ","), "\n")

cat("\nContracts (first 20):\n")
print(head(contracts, 20))

# -----------------------------------------------------------
# 8. PATH-BASED MONTE CARLO SIMULATION (ARITHMETIC/DOLLAR VOL)
# -----------------------------------------------------------

set.seed(42)
n_sims <- cfg$n_sims

# Margin evaluation dates: end of each month
first_margin <- floor_date(min(contracts$maturity_date) - months(1), "month")
first_of_months <- seq(first_margin, max(contracts$delivery_end), by = "month")
margin_dates <- ceiling_date(first_of_months, "month") - 1
n_periods <- length(margin_dates)

cat("\n========================================\n")
cat("SIMULATION SETUP (ARITHMETIC MODEL)\n")
cat("========================================\n")
cat("Number of simulations:", format(n_sims, big.mark = ","), "\n")
cat("Number of margin periods:", n_periods, "\n")
cat("Margin dates:", as.character(margin_dates[1]), "to", as.character(margin_dates[n_periods]), "\n")
cat("Price floor: $", cfg$price_floor, "/MWh\n", sep = "")

# Monthly drift in $/MWh (typically 0 for arithmetic model)
drift_monthly <- cfg$drift_annual / 12

# Initialize price matrix: rows = sims, cols = contracts
sim_prices <- matrix(
  rep(contracts$strike, each = n_sims),
  nrow = n_sims,
  ncol = n_contracts
)

# Matrix to store MTM at each margin date
mtm_matrix <- matrix(0, nrow = n_sims, ncol = n_periods)

# Store simulated prices at each margin date
price_history <- vector("list", n_periods)

# Get contract IDs and volatilities (now in $/MWh)
contract_ids <- contracts$contract_id
monthly_vols <- contracts$monthly_vol

cat("\nSimulating", n_sims, "paths across", n_periods, "months (arithmetic model)...\n")

for (t in 1:n_periods) {
  margin_date <- margin_dates[t]
  
  # Identify active contracts
  active_idx <- which(contracts$delivery_end > margin_date)
  n_active <- length(active_idx)
  
  if (n_active == 0) {
    mtm_matrix[, t] <- 0
    price_history[[t]] <- sim_prices
    next
  }
  
  # Get correlation subset for active contracts
  active_ids <- contract_ids[active_idx]
  
  # Check which contracts have correlation data
  valid_cor_ids <- active_ids[active_ids %in% colnames(cor_matrix)]
  
  if (length(valid_cor_ids) == 0) {
    cor_subset <- diag(n_active)
    rownames(cor_subset) <- colnames(cor_subset) <- active_ids
  } else if (length(valid_cor_ids) < n_active) {
    # Some missing - build composite matrix
    cor_subset <- diag(n_active)
    rownames(cor_subset) <- colnames(cor_subset) <- active_ids
    
    for (i in 1:n_active) {
      for (j in 1:n_active) {
        id_i <- active_ids[i]
        id_j <- active_ids[j]
        if (id_i %in% valid_cor_ids && id_j %in% valid_cor_ids) {
          cor_subset[i, j] <- cor_matrix[id_i, id_j]
        } else if (i != j) {
          cor_subset[i, j] <- 0.85  # Conservative estimate
        }
      }
    }
  } else {
    cor_subset <- cor_matrix[active_ids, active_ids, drop = FALSE]
  }
  
  # Ensure positive definiteness
  eigen_vals <- eigen(cor_subset, only.values = TRUE)$values
  if (any(eigen_vals <= 0)) {
    cor_subset <- cor_subset + diag(n_active) * 0.01
    cor_subset <- cor_subset / 1.01
  }
  
  # Generate correlated shocks
  if (n_active == 1) {
    Z <- matrix(rnorm(n_sims), ncol = 1)
  } else {
    Z <- mvrnorm(n = n_sims, mu = rep(0, n_active), Sigma = cor_subset)
  }
  
  # Update prices for active contracts - ARITHMETIC (not lognormal)
  for (i in 1:n_active) {
    idx <- active_idx[i]
    sigma_m <- monthly_vols[idx]  # Now in $/MWh
    
    # Handle NA volatility
    if (is.na(sigma_m)) {
      sigma_m <- mean(monthly_vols, na.rm = TRUE)
    }
    
    # ARITHMETIC evolution: P_new = P_old + drift + sigma * Z
    sim_prices[, idx] <- sim_prices[, idx] + drift_monthly + sigma_m * Z[, i]
    
    # Apply price floor to prevent negative prices
    sim_prices[, idx] <- pmax(sim_prices[, idx], cfg$price_floor)
  }
  
  # Store prices
  price_history[[t]] <- sim_prices
  
  # Calculate MTM for active contracts
  mtm <- rep(0, n_sims)
  for (idx in active_idx) {
    mtm <- mtm + contracts$mwhrs[idx] * (sim_prices[, idx] - contracts$strike[idx])
  }
  
  mtm_matrix[, t] <- mtm
  
  # Progress
  if (t %% 20 == 0 || t == n_periods) {
    cat("  Period", t, "of", n_periods, "complete\n")
  }
}

cat("Simulation complete.\n")

# -----------------------------------------------------------
# 9. CALCULATE PFE PROFILE
# -----------------------------------------------------------

# Month-over-month changes
change_matrix <- matrix(0, nrow = n_sims, ncol = n_periods)
change_matrix[, 1] <- mtm_matrix[, 1]
for (t in 2:n_periods) {
  change_matrix[, t] <- mtm_matrix[, t] - mtm_matrix[, t - 1]
}

# Calculate no-correlation P95
vm_p95_no_corr <- numeric(n_periods)

for (t in 1:n_periods) {
  margin_date <- margin_dates[t]
  active_idx <- which(contracts$delivery_end > margin_date)
  
  if (length(active_idx) == 0) {
    vm_p95_no_corr[t] <- 0
    next
  }
  
  sum_p95 <- 0
  for (idx in active_idx) {
    price_p95 <- quantile(price_history[[t]][, idx], probs = 0.95)
    sum_p95 <- sum_p95 + contracts$mwhrs[idx] * (price_p95 - contracts$strike[idx])
  }
  vm_p95_no_corr[t] <- sum_p95
}

# Build PFE profile
pfe_profile <- tibble(
  margin_date = margin_dates,
  n_active_contracts = sapply(margin_dates, function(d) sum(contracts$delivery_end > d)),
  
  # Variation Margin
  vm_p05 = apply(mtm_matrix, 2, quantile, probs = 0.05),
  vm_p50 = apply(mtm_matrix, 2, quantile, probs = 0.50),
  vm_p95 = apply(mtm_matrix, 2, quantile, probs = 0.95),
  
  vm_p95_no_corr = vm_p95_no_corr,
  diversification_benefit = vm_p95_no_corr - apply(mtm_matrix, 2, quantile, probs = 0.95),
  
  # 1-Month PFE
  one_month_pfe_p95 = c(
    apply(change_matrix[, 2:n_periods, drop = FALSE], 2, quantile, probs = 0.95),
    0
  ),
  
  # Total Margin
  total_margin_p50 = vm_p50 + one_month_pfe_p95,
  total_margin_p95 = vm_p95 + one_month_pfe_p95,
  
  vm_mean = apply(mtm_matrix, 2, mean),
  one_month_change_sd = apply(change_matrix, 2, sd)
)

cat("\n========================================\n")
cat("PFE MARGIN PROFILE\n")
cat("========================================\n")

# Display profile
pfe_profile %>%
  dplyr::select(margin_date, n_active_contracts, vm_p95, vm_p95_no_corr, 
                diversification_benefit, one_month_pfe_p95, total_margin_p95) %>%
  print(n = 100)

# -----------------------------------------------------------
# 10. SUMMARY STATISTICS
# -----------------------------------------------------------

max_vm_p95 <- max(pfe_profile$vm_p95)
max_vm_date <- pfe_profile$margin_date[which.max(pfe_profile$vm_p95)]
max_total <- max(pfe_profile$total_margin_p95)
max_total_date <- pfe_profile$margin_date[which.max(pfe_profile$total_margin_p95)]

cat("\n========================================\n")
cat("SUMMARY\n")
cat("========================================\n")
cat("Maximum VM P95: $", format(round(max_vm_p95, 0), big.mark = ","), " at ", as.character(max_vm_date), "\n", sep = "")
cat("Maximum Total Margin P95: $", format(round(max_total, 0), big.mark = ","), " at ", as.character(max_total_date), "\n", sep = "")

max_active <- pfe_profile$n_active_contracts[which.max(pfe_profile$total_margin_p95)]
cat("Contracts active at max total:", max_active, "\n")

# Diversification analysis
max_div_benefit <- max(pfe_profile$diversification_benefit)
avg_div_pct <- mean(pfe_profile$diversification_benefit / pfe_profile$vm_p95_no_corr, na.rm = TRUE) * 100

cat("\nDiversification benefit:\n")
cat("Maximum: $", format(round(max_div_benefit, 0), big.mark = ","), "\n", sep = "")
cat("Average as % of no-corr P95:", round(avg_div_pct, 1), "%\n")

# -----------------------------------------------------------
# 11. DETAILED BREAKDOWN AT PEAK MARGIN DATE
# -----------------------------------------------------------

peak_period <- which(margin_dates == max_vm_date)

cat("\n========================================\n")
cat("DETAILED BREAKDOWN AT PEAK MARGIN DATE:", as.character(max_vm_date), "\n")
cat("========================================\n\n")

peak_active_idx <- which(contracts$delivery_end > max_vm_date)
n_peak_active <- length(peak_active_idx)

peak_prices <- price_history[[peak_period]][, peak_active_idx, drop = FALSE]

# Portfolio P&L
peak_pnl <- rep(0, n_sims)
for (i in 1:n_peak_active) {
  idx <- peak_active_idx[i]
  peak_pnl <- peak_pnl + contracts$mwhrs[idx] * (peak_prices[, i] - contracts$strike[idx])
}

p95_value <- quantile(peak_pnl, 0.95)
p95_sim_idx <- which.min(abs(peak_pnl - p95_value))

# Build detail table
peak_detail <- tibble(
  contract_id = contracts$contract_id[peak_active_idx],
  maturity_date = contracts$maturity_date[peak_active_idx],
  type = contracts$type[peak_active_idx],
  mwhrs = contracts$mwhrs[peak_active_idx],
  strike = contracts$strike[peak_active_idx],
  price_p05 = apply(peak_prices, 2, quantile, probs = 0.05),
  price_p50 = apply(peak_prices, 2, quantile, probs = 0.50),
  price_p95 = apply(peak_prices, 2, quantile, probs = 0.95),
  pnl_p95 = mwhrs * (price_p95 - strike)
)

cat("Top 20 contracts by P95 P&L:\n")
peak_detail %>%
  arrange(desc(pnl_p95)) %>%
  dplyr::select(maturity_date, type, mwhrs, strike, price_p95, pnl_p95) %>%
  head(20) %>%
  print()

cat("\n--- Summary ---\n")
cat("Total MWh:", format(round(sum(peak_detail$mwhrs), 0), big.mark = ","), "\n")
cat("Sum of individual P95 P&L: $", format(round(sum(peak_detail$pnl_p95), 0), big.mark = ","), "\n", sep = "")
cat("Portfolio P95 (with correlation): $", format(round(p95_value, 0), big.mark = ","), "\n", sep = "")
cat("Diversification benefit: $", format(round(sum(peak_detail$pnl_p95) - p95_value, 0), big.mark = ","), "\n", sep = "")

# -----------------------------------------------------------
# 12. PFE BY YEAR
# -----------------------------------------------------------

cat("\n========================================\n")
cat("PFE BY YEAR (Max P95 by Calendar Year)\n")
cat("========================================\n")

pfe_by_year <- pfe_profile %>%
  mutate(year = year(margin_date)) %>%
  group_by(year) %>%
  summarise(
    max_vm_p95 = max(vm_p95),
    max_total_p95 = max(total_margin_p95),
    avg_vm_p95 = mean(vm_p95),
    .groups = "drop"
  )

print(pfe_by_year)

# -----------------------------------------------------------
# 13. PEAK VS OFF-PEAK PFE ANALYSIS
# -----------------------------------------------------------

cat("\n========================================\n")
cat("PEAK VS OFF-PEAK PFE BREAKDOWN\n")
cat("========================================\n")

# Calculate separate MTM for Peak and Off-Peak at each margin date
peak_mtm_matrix <- matrix(0, nrow = n_sims, ncol = n_periods)
offpeak_mtm_matrix <- matrix(0, nrow = n_sims, ncol = n_periods)

for (t in 1:n_periods) {
  margin_date <- margin_dates[t]
  
  # Peak contracts
  peak_active_idx <- which(contracts$delivery_end > margin_date & contracts$type == "Peak")
  if (length(peak_active_idx) > 0) {
    peak_mtm <- rep(0, n_sims)
    for (idx in peak_active_idx) {
      peak_mtm <- peak_mtm + contracts$mwhrs[idx] * (price_history[[t]][, idx] - contracts$strike[idx])
    }
    peak_mtm_matrix[, t] <- peak_mtm
  }
  
  # Off-Peak contracts
  offpeak_active_idx <- which(contracts$delivery_end > margin_date & contracts$type == "Off-Peak")
  if (length(offpeak_active_idx) > 0) {
    offpeak_mtm <- rep(0, n_sims)
    for (idx in offpeak_active_idx) {
      offpeak_mtm <- offpeak_mtm + contracts$mwhrs[idx] * (price_history[[t]][, idx] - contracts$strike[idx])
    }
    offpeak_mtm_matrix[, t] <- offpeak_mtm
  }
}

# Build Peak/OffPeak PFE profiles
peak_pfe <- tibble(
  margin_date = margin_dates,
  vm_p05 = apply(peak_mtm_matrix, 2, quantile, probs = 0.05),
  vm_p50 = apply(peak_mtm_matrix, 2, quantile, probs = 0.50),
  vm_p95 = apply(peak_mtm_matrix, 2, quantile, probs = 0.95)
)

offpeak_pfe <- tibble(
  margin_date = margin_dates,
  vm_p05 = apply(offpeak_mtm_matrix, 2, quantile, probs = 0.05),
  vm_p50 = apply(offpeak_mtm_matrix, 2, quantile, probs = 0.50),
  vm_p95 = apply(offpeak_mtm_matrix, 2, quantile, probs = 0.95)
)

cat("\nPeak Max P95: $", format(round(max(peak_pfe$vm_p95), 0), big.mark = ","), 
    " at ", as.character(peak_pfe$margin_date[which.max(peak_pfe$vm_p95)]), "\n", sep = "")
cat("Off-Peak Max P95: $", format(round(max(offpeak_pfe$vm_p95), 0), big.mark = ","), 
    " at ", as.character(offpeak_pfe$margin_date[which.max(offpeak_pfe$vm_p95)]), "\n", sep = "")

# -----------------------------------------------------------
# 14. PLOTS
# -----------------------------------------------------------

# PFE Profile Plot (with Total Margin)
p_pfe <- ggplot(pfe_profile, aes(x = margin_date)) +
  geom_ribbon(aes(ymin = vm_p05, ymax = vm_p95), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = vm_p50), color = "darkblue", linewidth = 1) +
  geom_line(aes(y = vm_p95), color = "red", linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = total_margin_p95), color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
  labs(
    title = "ReWorld Holdings - PFE Margin Profile (Dollar Vol)",
    subtitle = "Counterparty margin requirement (MTM gain to us)",
    x = "Margin Date",
    y = "Mark-to-Market",
    caption = "Shaded: VM P05-P95. Blue = VM Median. Red dashed = VM P95. Dark red = Total Margin P95."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_pfe)

# VM-Only Plot (no Total Margin line)
p_vm <- ggplot(pfe_profile, aes(x = margin_date)) +
  geom_ribbon(aes(ymin = vm_p05, ymax = vm_p95), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = vm_p50), color = "darkblue", linewidth = 1) +
  geom_line(aes(y = vm_p95), color = "red", linewidth = 0.8, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
  labs(
    title = "ReWorld Holdings - PFE Profile (Dollar Vol)",
    subtitle = "MTM gain to SEUS",
    x = "Margin Date",
    y = "Mark-to-Market",
    caption = "Shaded: P05-P95. Blue = Median. Red dashed = P95."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_vm)

# -----------------------------------------------------------
# OPTION 1: Separate Peak and Off-Peak Charts
# -----------------------------------------------------------

# Peak-only PFE chart
p_peak <- ggplot(peak_pfe, aes(x = margin_date)) +
  geom_ribbon(aes(ymin = vm_p05, ymax = vm_p95), fill = "red", alpha = 0.3) +
  geom_line(aes(y = vm_p50), color = "darkred", linewidth = 1) +
  geom_line(aes(y = vm_p95), color = "red", linewidth = 0.8, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
  labs(
    title = "ReWorld Holdings - Peak PFE Profile (Dollar Vol)",
    subtitle = "MTM gain to SEUS - Peak Hours Only",
    x = "Margin Date",
    y = "Mark-to-Market",
    caption = "Shaded: P05-P95. Solid = Median. Dashed = P95."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_peak)

# Off-Peak-only PFE chart
p_offpeak <- ggplot(offpeak_pfe, aes(x = margin_date)) +
  geom_ribbon(aes(ymin = vm_p05, ymax = vm_p95), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = vm_p50), color = "darkblue", linewidth = 1) +
  geom_line(aes(y = vm_p95), color = "steelblue", linewidth = 0.8, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
  labs(
    title = "ReWorld Holdings - Off-Peak PFE Profile (Dollar Vol)",
    subtitle = "MTM gain to SEUS - Off-Peak Hours Only",
    x = "Margin Date",
    y = "Mark-to-Market",
    caption = "Shaded: P05-P95. Solid = Median. Dashed = P95."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_offpeak)

# -----------------------------------------------------------
# OPTION 2: Combined Peak vs Off-Peak P95 Comparison
# -----------------------------------------------------------

combined_p95 <- tibble(
  margin_date = margin_dates,
  Peak = peak_pfe$vm_p95,
  `Off-Peak` = offpeak_pfe$vm_p95,
  Total = pfe_profile$vm_p95
) %>%
  pivot_longer(cols = c(Peak, `Off-Peak`, Total), names_to = "Type", values_to = "P95")

p_combined <- ggplot(combined_p95, aes(x = margin_date, y = P95, color = Type, linetype = Type)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
  scale_color_manual(values = c("Peak" = "red", "Off-Peak" = "steelblue", "Total" = "black")) +
  scale_linetype_manual(values = c("Peak" = "solid", "Off-Peak" = "solid", "Total" = "dashed")) +
  labs(
    title = "ReWorld Holdings - Peak vs Off-Peak P95 Comparison (Dollar Vol)",
    subtitle = "MTM gain to SEUS",
    x = "Margin Date",
    y = "P95 Mark-to-Market",
    color = "Contract Type",
    linetype = "Contract Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_combined)

# -----------------------------------------------------------
# OPTION 3: Stacked Area Chart
# -----------------------------------------------------------

stacked_data <- tibble(
  margin_date = margin_dates,
  Peak = peak_pfe$vm_p95,
  `Off-Peak` = offpeak_pfe$vm_p95
) %>%
  pivot_longer(cols = c(Peak, `Off-Peak`), names_to = "Type", values_to = "P95") %>%
  mutate(Type = factor(Type, levels = c("Off-Peak", "Peak")))  # Order for stacking

p_stacked <- ggplot(stacked_data, aes(x = margin_date, y = P95, fill = Type)) +
  geom_area(alpha = 0.7, position = "stack") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
  scale_fill_manual(values = c("Peak" = "red", "Off-Peak" = "steelblue")) +
  labs(
    title = "ReWorld Holdings - Stacked P95 Exposure by Type (Dollar Vol)",
    subtitle = "MTM gain to SEUS - Peak + Off-Peak contribution",
    x = "Margin Date",
    y = "P95 Mark-to-Market",
    fill = "Contract Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_stacked)

# Active MWh over time
mwh_by_date <- pfe_profile %>%
  mutate(
    active_peak_mwh = sapply(margin_date, function(d) {
      idx <- which(contracts$delivery_end > d & contracts$type == "Peak")
      sum(contracts$mwhrs[idx])
    }),
    active_offpeak_mwh = sapply(margin_date, function(d) {
      idx <- which(contracts$delivery_end > d & contracts$type == "Off-Peak")
      sum(contracts$mwhrs[idx])
    }),
    total_mwh = active_peak_mwh + active_offpeak_mwh
  )

p_mwh <- ggplot(mwh_by_date, aes(x = margin_date)) +
  geom_area(aes(y = total_mwh), fill = "steelblue", alpha = 0.5) +
  geom_area(aes(y = active_peak_mwh), fill = "red", alpha = 0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma_format(scale = 1/1e6, suffix = "M")) +
  labs(
    title = "ReWorld Holdings - Active MWh Over Time",
    subtitle = "Red = Peak, Blue overlay = Off-Peak",
    x = "Margin Date",
    y = "Active MWh (Millions)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_mwh)

# Forward Curve at Peak Date
p_curve <- ggplot(peak_detail, aes(x = maturity_date)) +
  geom_ribbon(aes(ymin = price_p05, ymax = price_p95, fill = type), alpha = 0.3) +
  geom_line(aes(y = price_p50, color = type), linewidth = 1) +
  geom_line(aes(y = strike, color = type), linewidth = 1, linetype = "dashed") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_color_manual(values = c("Peak" = "red", "Off-Peak" = "steelblue")) +
  scale_fill_manual(values = c("Peak" = "red", "Off-Peak" = "steelblue")) +
  labs(
    title = paste("Forward Curves at Peak Margin Date:", max_vm_date, "(Dollar Vol)"),
    subtitle = "Dashed = Strike price. Solid = Simulated median. Shaded = P05-P95",
    x = "Delivery Month",
    y = "Price ($/MWh)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_curve)

# -----------------------------------------------------------
# 15. EXPORT RESULTS
# -----------------------------------------------------------

# Export PFE profile
write_csv(pfe_profile, paste0(cfg$file_path, "reworld_pfe_profile", cfg$output_suffix, "_dollar_vol.csv"))
cat("\nSaved:", paste0(cfg$file_path, "reworld_pfe_profile", cfg$output_suffix, "_dollar_vol.csv"), "\n")

# Export peak detail
write_csv(peak_detail, paste0(cfg$file_path, "reworld_peak_margin_detail", cfg$output_suffix, "_dollar_vol.csv"))
cat("Saved:", paste0(cfg$file_path, "reworld_peak_margin_detail", cfg$output_suffix, "_dollar_vol.csv"), "\n")

# Export contracts
write_csv(contracts, paste0(cfg$file_path, "reworld_contracts", cfg$output_suffix, "_dollar_vol.csv"))
cat("Saved:", paste0(cfg$file_path, "reworld_contracts", cfg$output_suffix, "_dollar_vol.csv"), "\n")

# -----------------------------------------------------------
# CONTRACT SUMMARY WITH SIMULATED PRICES
# -----------------------------------------------------------

# For each contract, get P05/P95 prices at the last margin date before delivery
contract_summary <- contracts %>%
  rowwise() %>%
  mutate(
    # Find the last margin date before delivery_end
    last_margin_idx = max(which(margin_dates < delivery_end)),
    last_margin_date = margin_dates[last_margin_idx]
  ) %>%
  ungroup()

# Get simulated prices at each contract's last margin date
price_p05 <- numeric(n_contracts)
price_p95 <- numeric(n_contracts)
price_p50 <- numeric(n_contracts)

for (i in 1:n_contracts) {
  t <- contract_summary$last_margin_idx[i]
  price_p05[i] <- quantile(price_history[[t]][, i], probs = 0.05)
  price_p50[i] <- quantile(price_history[[t]][, i], probs = 0.50)
  price_p95[i] <- quantile(price_history[[t]][, i], probs = 0.95)
}

contract_summary <- contract_summary %>%
  mutate(
    price_p05 = price_p05,
    price_p50 = price_p50,
    price_p95 = price_p95,
    ann_vol = monthly_vol * sqrt(12),  # Annualized $/MWh
    # Calculate months from start to last margin date
    months_simulated = last_margin_idx,
    # P&L at P05 and P95
    pnl_p05 = mwhrs * (price_p05 - strike),
    pnl_p95 = mwhrs * (price_p95 - strike)
  ) %>%
  dplyr::select(
    contract_id, maturity_date, type, mwhrs, strike, 
    monthly_vol, ann_vol, months_simulated, last_margin_date,
    price_p05, price_p50, price_p95, pnl_p05, pnl_p95
  )

write_csv(contract_summary, paste0(cfg$file_path, "reworld_contract_summary", cfg$output_suffix, "_dollar_vol.csv"))
cat("Saved:", paste0(cfg$file_path, "reworld_contract_summary", cfg$output_suffix, "_dollar_vol.csv"), "\n")

cat("\nContract Summary (first 20):\n")
contract_summary %>%
  dplyr::select(contract_id, strike, monthly_vol, price_p05, price_p95) %>%
  head(20) %>%
  print()

# Save plots
ggsave(paste0(cfg$file_path, "reworld_pfe_profile", cfg$output_suffix, "_dollar_vol.png"), p_pfe, width = 12, height = 7, dpi = 150)
cat("Saved:", paste0(cfg$file_path, "reworld_pfe_profile", cfg$output_suffix, "_dollar_vol.png"), "\n")

ggsave(paste0(cfg$file_path, "reworld_pfe_vm_only", cfg$output_suffix, "_dollar_vol.png"), p_vm, width = 12, height = 7, dpi = 150)
cat("Saved:", paste0(cfg$file_path, "reworld_pfe_vm_only", cfg$output_suffix, "_dollar_vol.png"), "\n")

ggsave(paste0(cfg$file_path, "reworld_pfe_peak_only", cfg$output_suffix, "_dollar_vol.png"), p_peak, width = 12, height = 7, dpi = 150)
cat("Saved:", paste0(cfg$file_path, "reworld_pfe_peak_only", cfg$output_suffix, "_dollar_vol.png"), "\n")

ggsave(paste0(cfg$file_path, "reworld_pfe_offpeak_only", cfg$output_suffix, "_dollar_vol.png"), p_offpeak, width = 12, height = 7, dpi = 150)
cat("Saved:", paste0(cfg$file_path, "reworld_pfe_offpeak_only", cfg$output_suffix, "_dollar_vol.png"), "\n")

ggsave(paste0(cfg$file_path, "reworld_pfe_peak_vs_offpeak", cfg$output_suffix, "_dollar_vol.png"), p_combined, width = 12, height = 7, dpi = 150)
cat("Saved:", paste0(cfg$file_path, "reworld_pfe_peak_vs_offpeak", cfg$output_suffix, "_dollar_vol.png"), "\n")

ggsave(paste0(cfg$file_path, "reworld_pfe_stacked", cfg$output_suffix, "_dollar_vol.png"), p_stacked, width = 12, height = 7, dpi = 150)
cat("Saved:", paste0(cfg$file_path, "reworld_pfe_stacked", cfg$output_suffix, "_dollar_vol.png"), "\n")

ggsave(paste0(cfg$file_path, "reworld_mwh_profile", cfg$output_suffix, "_dollar_vol.png"), p_mwh, width = 12, height = 6, dpi = 150)
cat("Saved:", paste0(cfg$file_path, "reworld_mwh_profile", cfg$output_suffix, "_dollar_vol.png"), "\n")

ggsave(paste0(cfg$file_path, "reworld_forward_curve_at_peak", cfg$output_suffix, "_dollar_vol.png"), p_curve, width = 12, height = 6, dpi = 150)
cat("Saved:", paste0(cfg$file_path, "reworld_forward_curve_at_peak", cfg$output_suffix, "_dollar_vol.png"), "\n")

cat("\n========================================\n")
cat("MODEL COMPLETE (DOLLAR VOL)\n")
cat("========================================\n")
