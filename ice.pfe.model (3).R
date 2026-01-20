suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(ggplot2)
  library(scales)
  library(MASS)  # mvrnorm
})

options(
  pillar.sigfig = 6,
  digits = 6
)

# ===================================================================
# 1. CONFIGURATION
# ===================================================================

cfg <- list(
  # File paths - only 2 files needed
  base_path = "C:/Users/AllenFr/OneDrive - SmartestEnergy/Documents/",
  settlement_file = "ice.mtm.settlement.rpt.2026.01.16.csv",
  curves_file = "pfe.power.gas.prices.2026.01.26.csv",
  
  # Model parameters
  n_sims = 10000L,
  confidence_level = 0.95,
  vol_lookback_days = 30L,
  ir_annual = 0.02,
  
  # Valuation date
  valuation_date = as.Date("2026-01-16")
)

cat("=====================================================\n")
cat("ICE EXCHANGE PFE MODEL - ANCHORED TO ACTUAL PNL\n")
cat("=====================================================\n")
cat("Valuation Date:", as.character(cfg$valuation_date), "\n")
cat("Simulations:", cfg$n_sims, "\n")
cat("Confidence Level:", cfg$confidence_level * 100, "%\n")
cat("Volatility Lookback:", cfg$vol_lookback_days, "days\n")
cat("=====================================================\n\n")

# ===================================================================
# 2. PRODUCT MAPPING
# ===================================================================

location_to_curve <- tribble(
  ~location, ~curve_iso, ~curve_zone,
  # Power - ISONE
  "ISONE Connecticut Zone", "ISONE", "CT",
  "ISONE Internal Hub", "ISONE", "IH",
  # Power - NYISO
  "NYISO Zone A (West)", "NYISO", "Zone A",
  "NYISO Zone C (Central)", "NYISO", "Zone C",
  "NYISO Zone G (Hudson Val)", "NYISO", "Zone G",
  # Power - PJM
  "PJM Western Hub", "PJM", "WHUB",
  "PJM AEP Dayton Hub", "PJM", "AD Hub",
  "PJM N Illinois Hub", "PJM", "NI Hub",
  # Natural Gas
  "Eastern Gas South", "NG", "EastGas",
  "Henry Hub", "NG", "HH",
  "Iroquois Zone 2", "NG", "Iroquois Zone 2",
  "Tennessee Zone 4-200 Leg", "NG", "TG4",
  "Texas Eastern M-3", "NG", "TXM3",
  "Transco Zone 6 Non-N.Y.", "NG", "TZ6NNY"
)

# ===================================================================
# 3. LOAD SETTLEMENT DATA
# ===================================================================

cat("Loading settlement data...\n")

settlement_raw <- read_csv(
  paste0(cfg$base_path, cfg$settlement_file),
  show_col_types = FALSE
)

names(settlement_raw) <- gsub(" ", "_", names(settlement_raw))
names(settlement_raw) <- gsub("[()]", "", names(settlement_raw))
names(settlement_raw) <- gsub("/", "_", names(settlement_raw))

# Parse and filter to Forward positions only
settlement <- settlement_raw %>%
  filter(Actual_Forward == "Forward") %>%
  filter(Peak_Name != "Peak (Daily)") %>%
  mutate(
    location = Location,
    peakness = Peak_Name,
    term_start = as.Date(paste0(Year_Month, "-01")),
    total_volume = as.numeric(gsub(",", "", Total_Volume)),
    market_price = as.numeric(gsub(",", "", Market_Price_Report_UOM)),
    pnl_amount = as.numeric(gsub(",", "", PNL_Amount)),
    commodity = Commodity
  ) %>%
  filter(!is.na(total_volume) & !is.na(market_price))

cat("Loaded", nrow(settlement), "forward trade records\n")

# Actual total MTM - this is our anchor
actual_total_mtm <- sum(settlement$pnl_amount)
cat("Actual Total MTM:", scales::dollar(actual_total_mtm), "\n")

# ===================================================================
# 4. AGGREGATE BY PRODUCT-MONTH
# ===================================================================

cat("\nAggregating by product-month...\n")

# Aggregate: net volume and actual PNL by product-month
# Market price should be same for all trades in same product-month
positions_agg <- settlement %>%
  group_by(location, peakness, term_start, commodity) %>%
  summarise(
    net_volume = sum(total_volume),
    actual_pnl = sum(pnl_amount),
    current_price = first(market_price),  # Same for all trades in group
    n_trades = n(),
    .groups = "drop"
  ) %>%
  filter(abs(net_volume) > 0.01) %>%
  mutate(
    delivery_end = ceiling_date(term_start, "month") - 1
  )

cat("Aggregated to", nrow(positions_agg), "product-month positions\n")

# Verify PNL reconciles
agg_pnl <- sum(positions_agg$actual_pnl)
cat("Aggregated PNL:", scales::dollar(agg_pnl), "\n")
cat("Difference from trade-level:", scales::dollar(agg_pnl - actual_total_mtm), "\n")

# ===================================================================
# 5. MAP TO FORWARD CURVES
# ===================================================================

cat("\nMapping to forward curves...\n")

contracts <- positions_agg %>%
  left_join(location_to_curve, by = "location") %>%
  filter(!is.na(curve_zone)) %>%
  mutate(
    curve_peakness = case_when(
      peakness == "Off-Peak" ~ "Off-Peak",
      peakness == "Peak" ~ "Peak",
      peakness == "ATC" ~ "ATC",
      TRUE ~ peakness
    ),
    product_id = paste(curve_iso, curve_zone, curve_peakness, sep = "|"),
    contract_id = paste(product_id, term_start, sep = "||")
  )

n_contracts <- nrow(contracts)
cat("Mapped", n_contracts, "contracts\n")

# Summary by commodity
cat("\nPosition Summary:\n")
contracts %>%
  group_by(commodity) %>%
  summarise(
    n_contracts = n(),
    total_volume = sum(net_volume),
    total_pnl = sum(actual_pnl)
  ) %>%
  print()

# ===================================================================
# 6. LOAD FORWARD CURVES
# ===================================================================

cat("\nLoading forward curves...\n")

curves_raw <- read_csv(
  paste0(cfg$base_path, cfg$curves_file),
  show_col_types = FALSE
)

names(curves_raw) <- gsub(" ", "_", names(curves_raw))

curves <- curves_raw %>%
  mutate(
    as_of_date = as.Date(As_of_Date, format = "%m/%d/%Y"),
    maturity_date = as.Date(Maturity_Date, format = "%m/%d/%Y"),
    iso = ISO,
    zone = Zone,
    peakness = Peak_Offpeak,
    curve_value = as.numeric(Curve_Value)
  ) %>%
  filter(!is.na(curve_value)) %>%
  dplyr::select(as_of_date, maturity_date, iso, zone, peakness, curve_value)

latest_curve_date <- max(curves$as_of_date)
cat("Latest curve date:", as.character(latest_curve_date), "\n")

# ===================================================================
# 7. BUILD HISTORICAL PRICES AND CALCULATE VOLATILITIES
# ===================================================================

cat("\nCalculating volatilities and correlations...\n")

products_needed <- unique(contracts$product_id)
maturities_needed <- unique(contracts$term_start)

historical_prices <- curves %>%
  mutate(product_id = paste(iso, zone, peakness, sep = "|")) %>%
  filter(product_id %in% products_needed) %>%
  filter(maturity_date %in% maturities_needed) %>%
  group_by(as_of_date, product_id, maturity_date) %>%
  summarise(price = mean(curve_value, na.rm = TRUE), .groups = "drop") %>%
  arrange(product_id, maturity_date, as_of_date)

# Calculate log returns
log_returns <- historical_prices %>%
  arrange(product_id, maturity_date, as_of_date) %>%
  group_by(product_id, maturity_date) %>%
  mutate(log_return = log(price / lag(price))) %>%
  ungroup() %>%
  filter(!is.na(log_return)) %>%
  mutate(contract_id = paste(product_id, maturity_date, sep = "||"))

# ===================================================================
# 8. BUILD CORRELATION MATRIX
# ===================================================================

returns_wide <- log_returns %>%
  dplyr::select(as_of_date, contract_id, log_return) %>%
  group_by(as_of_date, contract_id) %>%
  summarise(log_return = mean(log_return, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = contract_id, values_from = log_return) %>%
  arrange(as_of_date)

returns_for_corr <- returns_wide %>%
  slice_tail(n = cfg$vol_lookback_days)

cat("Using", nrow(returns_for_corr), "days for correlation\n")

returns_matrix <- returns_for_corr %>%
  dplyr::select(-as_of_date) %>%
  as.matrix()

storage.mode(returns_matrix) <- "numeric"

cor_matrix <- cor(returns_matrix, use = "pairwise.complete.obs")
cor_matrix[is.na(cor_matrix)] <- 0.5

# Ensure positive semi-definite
eigen_vals <- eigen(cor_matrix)$values
if (any(eigen_vals < 0)) {
  cat("Adjusting correlation matrix for positive semi-definiteness...\n")
  cor_matrix <- cor_matrix + diag(ncol(cor_matrix)) * abs(min(eigen_vals)) * 1.01
  cor_matrix <- cov2cor(cor_matrix)
}

# ===================================================================
# 9. CALCULATE VOLATILITIES
# ===================================================================

vol_summary <- log_returns %>%
  group_by(contract_id) %>%
  arrange(as_of_date, .by_group = TRUE) %>%
  slice_tail(n = cfg$vol_lookback_days) %>%
  summarise(
    sd_log_return = sd(log_return, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  mutate(
    sd_log_return_monthly = sd_log_return * sqrt(252) / sqrt(12)
  )

# Join volatilities to contracts
contracts <- contracts %>%
  left_join(
    vol_summary %>% dplyr::select(contract_id, sd_log_return_monthly),
    by = "contract_id"
  )

# Handle missing volatilities
missing_vols <- sum(is.na(contracts$sd_log_return_monthly))
if (missing_vols > 0) {
  median_vol <- median(vol_summary$sd_log_return_monthly, na.rm = TRUE)
  contracts <- contracts %>%
    mutate(sd_log_return_monthly = ifelse(is.na(sd_log_return_monthly), 
                                           median_vol, sd_log_return_monthly))
  cat("Using median vol for", missing_vols, "contracts\n")
}

# ===================================================================
# 10. MONTE CARLO SIMULATION
# ===================================================================

cat("\n=====================================================\n")
cat("RUNNING MONTE CARLO SIMULATION\n")
cat("=====================================================\n")

set.seed(42)

n_sims <- cfg$n_sims
n_contracts <- nrow(contracts)

# Margin dates
max_delivery <- max(contracts$term_start)
margin_dates <- seq(
  from = floor_date(cfg$valuation_date, "month"),
  to = max_delivery,
  by = "month"
) %>%
  ceiling_date("month") - 1

margin_dates <- margin_dates[margin_dates >= cfg$valuation_date]
n_periods <- length(margin_dates)

cat("Margin periods:", n_periods, "\n")

# Build full correlation matrix for all contracts
contract_ids <- contracts$contract_id
common_contracts <- intersect(contract_ids, colnames(cor_matrix))

n_total <- length(contract_ids)
full_cor_matrix <- diag(n_total)
rownames(full_cor_matrix) <- contract_ids
colnames(full_cor_matrix) <- contract_ids

for (ci in common_contracts) {
  for (cj in common_contracts) {
    full_cor_matrix[ci, cj] <- cor_matrix[ci, cj]
  }
}

# Ensure positive semi-definite
eigen_vals <- eigen(full_cor_matrix)$values
if (any(eigen_vals <= 0)) {
  full_cor_matrix <- full_cor_matrix + diag(n_total) * (abs(min(eigen_vals)) + 0.001)
  full_cor_matrix <- cov2cor(full_cor_matrix)
}

# Initialize prices at current market levels
sim_prices <- matrix(
  rep(contracts$current_price, each = n_sims),
  nrow = n_sims,
  ncol = n_contracts
)
colnames(sim_prices) <- contract_ids

monthly_vols <- contracts$sd_log_return_monthly
drift_monthly <- cfg$ir_annual / 12

# Store starting prices for MTM calculation
starting_prices <- contracts$current_price
actual_pnl_by_contract <- contracts$actual_pnl

# MTM matrix
mtm_matrix <- matrix(0, nrow = n_sims, ncol = n_periods)
price_history <- vector("list", n_periods)

# Run simulation
cat("\nSimulating paths...\n")
pb <- txtProgressBar(min = 0, max = n_periods, style = 3)

for (t in 1:n_periods) {
  margin_date <- margin_dates[t]
  
  active_mask <- contracts$delivery_end > margin_date
  active_idx <- which(active_mask)
  expired_idx <- which(!active_mask)
  n_active <- length(active_idx)
  
  if (n_active > 0) {
    # Generate correlated shocks for active contracts
    active_ids <- contract_ids[active_idx]
    cor_subset <- full_cor_matrix[active_ids, active_ids, drop = FALSE]
    
    if (n_active == 1) {
      Z <- matrix(rnorm(n_sims), ncol = 1)
    } else {
      Z <- mvrnorm(n = n_sims, mu = rep(0, n_active), Sigma = cor_subset)
    }
    
    # Update active prices
    for (i in 1:n_active) {
      idx <- active_idx[i]
      sigma_m <- monthly_vols[idx]
      sim_prices[, idx] <- sim_prices[, idx] * exp(
        drift_monthly - 0.5 * sigma_m^2 + sigma_m * Z[, i]
      )
    }
  }
  
  price_history[[t]] <- sim_prices
  
  # =====================================================
  # KEY CHANGE: MTM = Actual PNL + Price Change Effect
  # =====================================================
  # MTM = Σ(actual_pnl) + Σ[net_volume × (sim_price - starting_price)]
  #
  # For active contracts: use simulated price
  # For expired contracts: price locked at starting price (no additional change)
  
  mtm <- rep(actual_total_mtm, n_sims)  # Start with actual PNL
  
  # Add price change effect for active contracts
  for (idx in active_idx) {
    price_change <- sim_prices[, idx] - starting_prices[idx]
    mtm <- mtm + contracts$net_volume[idx] * price_change
  }
  
  # Expired contracts: no additional price change (already in actual_pnl)
  
  mtm_matrix[, t] <- mtm
  
  setTxtProgressBar(pb, t)
}
close(pb)

cat("\nSimulation complete.\n")

# ===================================================================
# 11. CALCULATE PFE PROFILE
# ===================================================================

cat("\nCalculating PFE profile...\n")

change_matrix <- matrix(0, nrow = n_sims, ncol = n_periods)
change_matrix[, 1] <- mtm_matrix[, 1] - actual_total_mtm
for (t in 2:n_periods) {
  change_matrix[, t] <- mtm_matrix[, t] - mtm_matrix[, t - 1]
}

pfe_profile <- tibble(
  margin_date = margin_dates,
  n_active = sapply(margin_dates, function(d) sum(contracts$delivery_end > d)),
  mtm_mean = apply(mtm_matrix, 2, mean),
  mtm_p05 = apply(mtm_matrix, 2, quantile, probs = 0.05),
  mtm_p50 = apply(mtm_matrix, 2, quantile, probs = 0.50),
  mtm_p95 = apply(mtm_matrix, 2, quantile, probs = cfg$confidence_level),
  one_month_pfe = c(apply(change_matrix[, 2:n_periods, drop = FALSE], 2, 
                          quantile, probs = cfg$confidence_level), 0),
  total_margin_p95 = mtm_p95 + one_month_pfe
)

# ===================================================================
# 12. RESULTS SUMMARY
# ===================================================================

cat("\n=====================================================\n")
cat("PFE RESULTS SUMMARY\n")
cat("=====================================================\n")

cat("\n--- STARTING POSITION (t=0) ---\n")
cat("Actual MTM (exact):", scales::dollar(actual_total_mtm), "\n")

cat("\n--- FIRST PERIOD MTM DISTRIBUTION ---\n")
cat("P05:", scales::dollar(pfe_profile$mtm_p05[1]), "\n")
cat("P50:", scales::dollar(pfe_profile$mtm_p50[1]), "\n")
cat("P95:", scales::dollar(pfe_profile$mtm_p95[1]), "\n")

# Key metrics
max_p95 <- max(pfe_profile$mtm_p95)
max_p95_date <- pfe_profile$margin_date[which.max(pfe_profile$mtm_p95)]
min_p05 <- min(pfe_profile$mtm_p05)
min_p05_date <- pfe_profile$margin_date[which.min(pfe_profile$mtm_p05)]
max_total <- max(pfe_profile$total_margin_p95)

cat("\n--- PEAK EXPOSURE ---\n")
cat("Maximum MTM P95:", scales::dollar(max_p95), "at", as.character(max_p95_date), "\n")
cat("Minimum MTM P05:", scales::dollar(min_p05), "at", as.character(min_p05_date), "\n")
cat("Maximum Total Margin P95:", scales::dollar(max_total), "\n")

cat("\n--- MARGIN RISK FROM CURRENT POSITION ---\n")
cat("Current MTM:", scales::dollar(actual_total_mtm), "\n")
cat("Worst case (P05):", scales::dollar(min_p05), "\n")
cat("Additional downside:", scales::dollar(min_p05 - actual_total_mtm), "\n")
cat("Best case (P95):", scales::dollar(max_p95), "\n")
cat("Potential upside:", scales::dollar(max_p95 - actual_total_mtm), "\n")

cat("\n--- FULL PFE PROFILE (first 24 months) ---\n")
pfe_profile %>%
  dplyr::select(margin_date, n_active, mtm_p05, mtm_p50, mtm_p95, one_month_pfe, total_margin_p95) %>%
  mutate(across(where(is.numeric) & !matches("n_active"), ~round(., 0))) %>%
  print(n = 24)

# ===================================================================
# 13. DETAILED BREAKDOWN AT PEAK
# ===================================================================

cat("\n=====================================================\n")
cat("BREAKDOWN AT PEAK P95 DATE:", as.character(max_p95_date), "\n")
cat("=====================================================\n")

peak_period <- which(margin_dates == max_p95_date)
peak_active_mask <- contracts$delivery_end > max_p95_date
peak_active_idx <- which(peak_active_mask)

# Get price distributions at peak
peak_prices <- price_history[[peak_period]]

# Calculate individual contract contributions to P95
peak_detail <- contracts[peak_active_idx, ] %>%
  mutate(
    starting_price = starting_prices[peak_active_idx],
    price_p50 = apply(peak_prices[, peak_active_idx, drop = FALSE], 2, median),
    price_p95 = apply(peak_prices[, peak_active_idx, drop = FALSE], 2, 
                      quantile, probs = cfg$confidence_level),
    price_change_p95 = price_p95 - starting_price,
    mtm_contribution_p95 = net_volume * price_change_p95
  ) %>%
  arrange(desc(abs(mtm_contribution_p95)))

cat("\nTop 20 Contributors to P95 (by price change effect):\n")
peak_detail %>%
  dplyr::select(product_id, term_start, net_volume, current_price, sd_log_return_monthly,
                price_p95, price_change_p95, mtm_contribution_p95) %>%
  head(20) %>%
  print()

cat("\nContribution by Commodity:\n")
peak_detail %>%
  group_by(commodity) %>%
  summarise(
    n = n(),
    total_volume = sum(net_volume),
    sum_mtm_contribution = sum(mtm_contribution_p95)
  ) %>%
  print()

# ===================================================================
# 14. PLOT
# ===================================================================

cat("\nGenerating plot...\n")

p_pfe <- ggplot(pfe_profile, aes(x = margin_date)) +
  geom_ribbon(aes(ymin = mtm_p05, ymax = mtm_p95), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = mtm_p50), color = "darkblue", linewidth = 1) +
  geom_line(aes(y = mtm_p95), color = "red", linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = mtm_p05), color = "darkgreen", linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = total_margin_p95), color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = actual_total_mtm, color = "orange", linewidth = 1) +
  annotate("text", x = margin_dates[1], y = actual_total_mtm,
           label = paste("Current MTM:", scales::dollar(actual_total_mtm/1e6, suffix = "M")),
           hjust = 0, vjust = -0.5, color = "orange", fontface = "bold") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(
    title = "ICE Exchange PFE Profile - Anchored to Actual PNL",
    subtitle = paste0("Current MTM: ", scales::dollar(actual_total_mtm/1e6, suffix = "M"),
                      " | ", cfg$n_sims, " sims | ", cfg$confidence_level * 100, "% confidence"),
    x = "Margin Date",
    y = "Mark-to-Market ($)",
    caption = "Orange = Current MTM. Shaded = P05-P95. Blue = Median. Dark red = Total Margin P95."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_pfe)

# ===================================================================
# 15. EXPORT RESULTS
# ===================================================================

cat("\nExporting results...\n")

write_csv(pfe_profile, paste0(cfg$base_path, "ice.pfe.profile.csv"))
write_csv(contracts, paste0(cfg$base_path, "ice.pfe.contracts.csv"))

cat("\nResults exported to:\n")
cat("  - ice.pfe.profile.csv\n")
cat("  - ice.pfe.contracts.csv\n")

cat("\n=====================================================\n")
cat("PFE MODEL COMPLETE\n")
cat("=====================================================\n")





#View all contributors
peak_detail %>%
  dplyr::select(product_id, term_start, net_volume, current_price, sd_log_return_monthly,
                price_p95, price_change_p95, mtm_contribution_p95) %>%
  View()





# Row 1 inputs
current_price <- 81.25
monthly_vol <- 0.135252

# Months from valuation (2026-01-16) to peak date
# What was the peak date?
max_p95_date  # Check this

# Calculate
months_fwd <- as.numeric(difftime(max_p95_date, as.Date("2026-01-16"), units = "days")) / 30
drift_total <- (0.02 / 12) * months_fwd
cumulative_vol <- monthly_vol * sqrt(months_fwd)

price_p95_calc <- current_price * exp(drift_total - 0.5 * cumulative_vol^2 + 1.645 * cumulative_vol)

cat("Months forward:", round(months_fwd, 1), "\n")
cat("Cumulative vol:", round(cumulative_vol * 100, 1), "%\n")
cat("Calculated P95:", round(price_p95_calc, 2), "\n")
cat("Table P95:", 135.63, "\n")
