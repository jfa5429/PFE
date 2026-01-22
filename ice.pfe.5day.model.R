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
  
  # 5-day PFE specific
  pfe_horizon_days = 5L,
  trading_days_per_month = 21L,
  
  # Valuation date
  valuation_date = as.Date("2026-01-16")
)

cat("=====================================================\n")
cat("ICE EXCHANGE 5-DAY PFE MODEL\n")
cat("=====================================================\n")
cat("Valuation Date:", as.character(cfg$valuation_date), "\n")
cat("PFE Horizon:", cfg$pfe_horizon_days, "trading days\n")
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
# 7. CALCULATE VOLATILITIES AND CORRELATIONS
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

# Calculate DAILY volatilities (for 5-day horizon)
vol_summary <- log_returns %>%
  group_by(contract_id) %>%
  arrange(as_of_date, .by_group = TRUE) %>%
  slice_tail(n = cfg$vol_lookback_days) %>%
  summarise(
    sd_daily = sd(log_return, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  mutate(
    # 5-day volatility = daily vol × sqrt(5)
    sd_5day = sd_daily * sqrt(cfg$pfe_horizon_days)
  )

# Join volatilities to contracts
contracts <- contracts %>%
  left_join(
    vol_summary %>% dplyr::select(contract_id, sd_daily, sd_5day),
    by = "contract_id"
  )

# Handle missing volatilities
missing_vols <- sum(is.na(contracts$sd_5day))
if (missing_vols > 0) {
  median_vol <- median(vol_summary$sd_5day, na.rm = TRUE)
  contracts <- contracts %>%
    mutate(
      sd_daily = ifelse(is.na(sd_daily), median_vol / sqrt(cfg$pfe_horizon_days), sd_daily),
      sd_5day = ifelse(is.na(sd_5day), median_vol, sd_5day)
    )
  cat("Using median vol for", missing_vols, "contracts\n")
}

# ===================================================================
# 8. MONTE CARLO SIMULATION - 5-DAY HORIZON
# ===================================================================

cat("\n=====================================================\n")
cat("RUNNING 5-DAY MONTE CARLO SIMULATION\n")
cat("=====================================================\n")

set.seed(42)

n_sims <- cfg$n_sims
n_contracts <- nrow(contracts)

# Build full correlation matrix
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

# 5-day drift (negligible but included for completeness)
drift_5day <- cfg$ir_annual * cfg$pfe_horizon_days / 252

# Generate correlated shocks
cat("Generating", n_sims, "correlated scenarios...\n")
Z <- mvrnorm(n = n_sims, mu = rep(0, n_contracts), Sigma = full_cor_matrix)

# Simulate 5-day price changes
starting_prices <- contracts$current_price
vol_5day <- contracts$sd_5day

# Simulated prices after 5 days
sim_prices <- matrix(0, nrow = n_sims, ncol = n_contracts)
for (j in 1:n_contracts) {
  sim_prices[, j] <- starting_prices[j] * exp(
    drift_5day - 0.5 * vol_5day[j]^2 + vol_5day[j] * Z[, j]
  )
}

# Calculate MTM change for each simulation
# MTM change = Σ[net_volume × (sim_price - current_price)]
mtm_change <- rep(0, n_sims)
for (j in 1:n_contracts) {
  price_change <- sim_prices[, j] - starting_prices[j]
  mtm_change <- mtm_change + contracts$net_volume[j] * price_change
}

cat("Simulation complete.\n")

# ===================================================================
# 9. CALCULATE 5-DAY PFE RESULTS
# ===================================================================

cat("\n=====================================================\n")
cat("5-DAY PFE RESULTS\n")
cat("=====================================================\n")

# MTM change statistics
pfe_stats <- tibble(
  metric = c("Mean", "Std Dev", "P01", "P05", "P10", "P50", "P90", "P95", "P99"),
  mtm_change = c(
    mean(mtm_change),
    sd(mtm_change),
    quantile(mtm_change, 0.01),
    quantile(mtm_change, 0.05),
    quantile(mtm_change, 0.10),
    quantile(mtm_change, 0.50),
    quantile(mtm_change, 0.90),
    quantile(mtm_change, 0.95),
    quantile(mtm_change, 0.99)
  )
)

cat("\n5-Day MTM Change Distribution:\n")
pfe_stats %>%
  mutate(mtm_change = scales::dollar(mtm_change)) %>%
  print()

# Key metrics
pfe_p95_up <- quantile(mtm_change, cfg$confidence_level)
pfe_p05_down <- quantile(mtm_change, 1 - cfg$confidence_level)

cat("\n--- KEY METRICS ---\n")
cat("Current MTM:", scales::dollar(actual_total_mtm), "\n")
cat("\n5-Day P95 (upside - prices rise, you gain):", scales::dollar(pfe_p95_up), "\n")
cat("5-Day P05 (downside - prices fall, you lose):", scales::dollar(pfe_p05_down), "\n")
cat("\nWorst case MTM after 5 days (P05):", scales::dollar(actual_total_mtm + pfe_p05_down), "\n")
cat("Best case MTM after 5 days (P95):", scales::dollar(actual_total_mtm + pfe_p95_up), "\n")

# ===================================================================
# 10. CONTRACT-LEVEL BREAKDOWN
# ===================================================================

cat("\n=====================================================\n")
cat("CONTRACT-LEVEL BREAKDOWN\n")
cat("=====================================================\n")

# Calculate individual contract P95 contributions
contract_detail <- contracts %>%
  mutate(
    price_p95 = current_price * exp(drift_5day - 0.5 * sd_5day^2 + 1.645 * sd_5day),
    price_p05 = current_price * exp(drift_5day - 0.5 * sd_5day^2 - 1.645 * sd_5day),
    price_change_p95 = price_p95 - current_price,
    price_change_p05 = price_p05 - current_price,
    mtm_contribution_p95 = net_volume * price_change_p95,
    mtm_contribution_p05 = net_volume * price_change_p05
  ) %>%
  arrange(desc(abs(mtm_contribution_p95)))

cat("\nTop 20 Contributors to 5-Day P95:\n")
contract_detail %>%
  dplyr::select(product_id, term_start, net_volume, current_price, sd_5day,
                price_p95, price_change_p95, mtm_contribution_p95) %>%
  head(20) %>%
  print()

cat("\nBy Commodity:\n")
contract_detail %>%
  group_by(commodity) %>%
  summarise(
    n = n(),
    total_volume = sum(net_volume),
    sum_p95_contribution = sum(mtm_contribution_p95),
    sum_p05_contribution = sum(mtm_contribution_p05)
  ) %>%
  print()

# ===================================================================
# 11. COMPARISON: SUM OF INDIVIDUAL VS PORTFOLIO
# ===================================================================

cat("\n=====================================================\n")
cat("DIVERSIFICATION ANALYSIS\n")
cat("=====================================================\n")

sum_individual_p95 <- sum(contract_detail$mtm_contribution_p95)
sum_individual_p05 <- sum(contract_detail$mtm_contribution_p05)

cat("\nSum of Individual P95s:", scales::dollar(sum_individual_p95), "\n")
cat("Portfolio P95:", scales::dollar(pfe_p95_up), "\n")
cat("Diversification Benefit:", scales::dollar(sum_individual_p95 - pfe_p95_up), 
    "(", round((1 - pfe_p95_up / sum_individual_p95) * 100, 1), "% reduction)\n")

cat("\nSum of Individual P05s:", scales::dollar(sum_individual_p05), "\n")
cat("Portfolio P05:", scales::dollar(pfe_p05_down), "\n")
cat("Diversification Benefit:", scales::dollar(abs(sum_individual_p05) - abs(pfe_p05_down)),
    "(", round((1 - abs(pfe_p05_down) / abs(sum_individual_p05)) * 100, 1), "% reduction)\n")

# ===================================================================
# 12. HISTOGRAM
# ===================================================================

cat("\nGenerating histogram...\n")

p_hist <- ggplot(data.frame(mtm_change = mtm_change), aes(x = mtm_change)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = pfe_p05_down, color = "darkgreen", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = pfe_p95_up, color = "red", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  annotate("text", x = pfe_p05_down, y = Inf, label = paste("P05:", scales::dollar(pfe_p05_down/1e6, suffix = "M")),
           hjust = 1.1, vjust = 2, color = "darkgreen", fontface = "bold") +
  annotate("text", x = pfe_p95_up, y = Inf, label = paste("P95:", scales::dollar(pfe_p95_up/1e6, suffix = "M")),
           hjust = -0.1, vjust = 2, color = "red", fontface = "bold") +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(
    title = "5-Day MTM Change Distribution",
    subtitle = paste0("ICE Exchange Portfolio | ", cfg$n_sims, " simulations | ",
                      cfg$confidence_level * 100, "% confidence"),
    x = "5-Day MTM Change ($)",
    y = "Frequency"
  ) +
  theme_minimal()

print(p_hist)

# ===================================================================
# 13. EXPORT RESULTS
# ===================================================================

cat("\nExporting results...\n")

# Summary output
pfe_summary <- tibble(
  valuation_date = cfg$valuation_date,
  horizon_days = cfg$pfe_horizon_days,
  n_contracts = n_contracts,
  current_mtm = actual_total_mtm,
  pfe_p95_up = pfe_p95_up,
  pfe_p05_down = pfe_p05_down,
  mtm_after_p95 = actual_total_mtm + pfe_p95_up,
  mtm_after_p05 = actual_total_mtm + pfe_p05_down,
  sum_individual_p95 = sum_individual_p95,
  diversification_pct = (1 - pfe_p95_up / sum_individual_p95) * 100
)

write_csv(pfe_summary, paste0(cfg$base_path, "ice.pfe.5day.summary.csv"))
write_csv(contract_detail, paste0(cfg$base_path, "ice.pfe.5day.contracts.csv"))

cat("\nResults exported to:\n")
cat("  - ice.pfe.5day.summary.csv\n")
cat("  - ice.pfe.5day.contracts.csv\n")

cat("\n=====================================================\n")
cat("5-DAY PFE MODEL COMPLETE\n")
cat("=====================================================\n")
