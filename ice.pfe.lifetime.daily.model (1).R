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
  # File paths
  base_path = "C:/Users/AllenFr/OneDrive - SmartestEnergy/Documents/",
  settlement_file = "ice.mtm.settlement.rpt.2026.01.16.csv",
  curves_file = "pfe.power.gas.prices.2026.01.26.csv",
  
  # Model parameters
  n_sims = 10000L,
  confidence_level = 0.95,
  vol_lookback_days = 30L,
  ir_annual = 0.02,
  trading_days_per_year = 252L,
  
  # Valuation date
  valuation_date = as.Date("2026-01-16")
)

cat("=====================================================\n")
cat("ICE EXCHANGE LIFETIME PFE MODEL - DAILY SIMULATION\n")
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

actual_total_mtm <- sum(settlement$pnl_amount)
cat("Actual Total MTM:", scales::dollar(actual_total_mtm), "\n")

# ===================================================================
# 4. AGGREGATE BY PRODUCT-MONTH
# ===================================================================

cat("\nAggregating by product-month...\n")

positions_agg <- settlement %>%
  group_by(location, peakness, term_start, commodity) %>%
  summarise(
    net_volume = sum(total_volume),
    actual_pnl = sum(pnl_amount),
    current_price = first(market_price),
    n_trades = n(),
    .groups = "drop"
  ) %>%
  filter(abs(net_volume) > 0.01) %>%
  mutate(
    delivery_end = ceiling_date(term_start, "month") - 1
  )

cat("Aggregated to", nrow(positions_agg), "product-month positions\n")

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
# 7. CALCULATE DAILY VOLATILITIES AND CORRELATIONS
# ===================================================================

cat("\nCalculating daily volatilities and correlations...\n")

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

# Build correlation matrix
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

# Calculate DAILY volatilities
vol_summary <- log_returns %>%
  group_by(contract_id) %>%
  arrange(as_of_date, .by_group = TRUE) %>%
  slice_tail(n = cfg$vol_lookback_days) %>%
  summarise(
    sd_daily = sd(log_return, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

# Join volatilities to contracts
contracts <- contracts %>%
  left_join(
    vol_summary %>% dplyr::select(contract_id, sd_daily),
    by = "contract_id"
  )

# Handle missing volatilities
missing_vols <- sum(is.na(contracts$sd_daily))
if (missing_vols > 0) {
  median_vol <- median(vol_summary$sd_daily, na.rm = TRUE)
  contracts <- contracts %>%
    mutate(sd_daily = ifelse(is.na(sd_daily), median_vol, sd_daily))
  cat("Using median vol for", missing_vols, "contracts\n")
}

cat("Daily vol range:", round(min(contracts$sd_daily) * 100, 2), "% to", 
    round(max(contracts$sd_daily) * 100, 2), "%\n")

# ===================================================================
# 8. SET UP SIMULATION TIMELINE
# ===================================================================

cat("\nSetting up simulation timeline...\n")

# Margin evaluation dates (end of each month)
max_delivery <- max(contracts$term_start)
margin_dates <- seq(
  from = floor_date(cfg$valuation_date, "month"),
  to = max_delivery,
  by = "month"
) %>%
  ceiling_date("month") - 1

margin_dates <- margin_dates[margin_dates >= cfg$valuation_date]
n_periods <- length(margin_dates)

# Generate daily simulation dates (approximate trading days)
# Use ~21 trading days per month
all_dates <- seq(from = cfg$valuation_date, to = max(margin_dates), by = "day")
# Remove weekends (approximate trading days)
trading_dates <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]
n_trading_days <- length(trading_dates)

cat("Margin evaluation dates:", n_periods, "\n")
cat("Trading days to simulate:", n_trading_days, "\n")

# Map each margin date to closest trading day index
margin_day_idx <- sapply(margin_dates, function(md) {
  which.min(abs(trading_dates - md))
})

# ===================================================================
# 9. BUILD CORRELATION MATRIX
# ===================================================================

cat("\nBuilding full correlation matrix...\n")

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

# ===================================================================
# 10. MONTE CARLO SIMULATION - DAILY STEPS
# ===================================================================

cat("\n=====================================================\n")
cat("RUNNING DAILY MONTE CARLO SIMULATION\n")
cat("=====================================================\n")

set.seed(42)

n_sims <- cfg$n_sims
n_contracts <- nrow(contracts)

# Daily drift
drift_daily <- cfg$ir_annual / cfg$trading_days_per_year

# Starting prices
starting_prices <- contracts$current_price
daily_vols <- contracts$sd_daily

# Initialize price matrix
sim_prices <- matrix(
  rep(starting_prices, each = n_sims),
  nrow = n_sims,
  ncol = n_contracts
)
colnames(sim_prices) <- contract_ids

# MTM matrix (only at margin dates)
mtm_matrix <- matrix(0, nrow = n_sims, ncol = n_periods)

# Track which margin period we're in
current_margin_period <- 1

cat("Simulating", n_trading_days, "daily steps...\n")
pb <- txtProgressBar(min = 0, max = n_trading_days, style = 3)

for (day in 1:n_trading_days) {
  current_date <- trading_dates[day]
  
  # Identify active contracts (delivery_end > current_date)
  active_mask <- contracts$delivery_end > current_date
  active_idx <- which(active_mask)
  n_active <- length(active_idx)
  
  if (n_active > 0) {
    # Generate correlated daily shocks
    active_ids <- contract_ids[active_idx]
    cor_subset <- full_cor_matrix[active_ids, active_ids, drop = FALSE]
    
    if (n_active == 1) {
      Z <- matrix(rnorm(n_sims), ncol = 1)
    } else {
      Z <- mvrnorm(n = n_sims, mu = rep(0, n_active), Sigma = cor_subset)
    }
    
    # Update prices for active contracts
    for (i in 1:n_active) {
      idx <- active_idx[i]
      sigma_d <- daily_vols[idx]
      sim_prices[, idx] <- sim_prices[, idx] * exp(
        drift_daily - 0.5 * sigma_d^2 + sigma_d * Z[, i]
      )
    }
  }
  
  # Record MTM at margin dates
  if (day %in% margin_day_idx) {
    margin_period <- which(margin_day_idx == day)
    margin_date <- margin_dates[margin_period]
    
    # MTM = Actual PNL + price change effect for ACTIVE contracts only
    # Expired contracts' PNL is already locked in actual_total_mtm
    mtm <- rep(actual_total_mtm, n_sims)
    
    active_at_margin <- contracts$delivery_end > margin_date
    active_idx_margin <- which(active_at_margin)
    
    for (idx in active_idx_margin) {
      price_change <- sim_prices[, idx] - starting_prices[idx]
      mtm <- mtm + contracts$net_volume[idx] * price_change
    }
    
    mtm_matrix[, margin_period] <- mtm
  }
  
  setTxtProgressBar(pb, day)
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
cat("PFE RESULTS SUMMARY (DAILY SIMULATION)\n")
cat("=====================================================\n")

cat("\n--- STARTING POSITION ---\n")
cat("Actual MTM:", scales::dollar(actual_total_mtm), "\n")

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

cat("\n--- 5-DAY COMPARISON ---\n")
# Find the ~5 day mark (first week)
day5_approx <- pfe_profile %>% slice(1)
cat("First month P05:", scales::dollar(day5_approx$mtm_p05), "\n")
cat("First month P05 change:", scales::dollar(day5_approx$mtm_p05 - actual_total_mtm), "\n")

cat("\n--- FULL PFE PROFILE (first 24 months) ---\n")
pfe_profile %>%
  dplyr::select(margin_date, n_active, mtm_p05, mtm_p50, mtm_p95, one_month_pfe, total_margin_p95) %>%
  mutate(across(where(is.numeric) & !matches("n_active"), ~round(., 0))) %>%
  print(n = 24)

# ===================================================================
# 13. PLOT
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
    title = "ICE Exchange PFE Profile - Daily Simulation",
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
# 14. EXPORT RESULTS
# ===================================================================

cat("\nExporting results...\n")

write_csv(pfe_profile, paste0(cfg$base_path, "ice.pfe.profile.daily.csv"))
write_csv(contracts, paste0(cfg$base_path, "ice.pfe.contracts.daily.csv"))

cat("\nResults exported to:\n")
cat("  - ice.pfe.profile.daily.csv\n")
cat("  - ice.pfe.contracts.daily.csv\n")

cat("\n=====================================================\n")
cat("DAILY PFE MODEL COMPLETE\n")
cat("=====================================================\n")
