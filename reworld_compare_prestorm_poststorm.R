# =========================================================
# COMPARE PRE-STORM VS POST-STORM VOLATILITY AND CORRELATION
# Dollar Vol Model Comparison
# =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(lubridate)
})

cfg <- list(
  file_path       = "C:/Users/AllenFr/OneDrive - SmartestEnergy/Documents/",
  price_file      = "mass_hub_26_31.csv",
  vol_lookback_n  = 30L,
  prestorm_date   = as.Date("2026-01-16"),
  poststorm_date  = NULL  # NULL = use latest
)

cat("========================================\n")
cat("PRE-STORM vs POST-STORM COMPARISON\n")
cat("Dollar Vol Model\n")
cat("========================================\n\n")

# -----------------------------------------------------------
# 1. LOAD PRICE DATA
# -----------------------------------------------------------

curve_raw <- read_csv(paste0(cfg$file_path, cfg$price_file), show_col_types = FALSE)

curve_raw <- curve_raw %>%
  mutate(
    as_of_date    = as.Date(`As of Date`, format = "%m/%d/%Y"),
    maturity_date = as.Date(`Maturity Date`, format = "%m/%d/%Y"),
    type          = `Peak Offpeak`,
    price         = `Curve Value`
  ) %>%
  filter(!is.na(as_of_date), !is.na(maturity_date)) %>%
  select(as_of_date, maturity_date, type, price)

cat("Loaded", nrow(curve_raw), "price records\n")
cat("Date range:", as.character(min(curve_raw$as_of_date)), "to", 
    as.character(max(curve_raw$as_of_date)), "\n\n")

# -----------------------------------------------------------
# 2. CALCULATE VOLATILITIES FOR BOTH PERIODS
# -----------------------------------------------------------

calc_dollar_vol <- function(data, valuation_date, lookback_n) {
  # Filter to data up to valuation date
  filtered_data <- data %>%
    filter(as_of_date <= valuation_date)
  
  actual_val_date <- max(filtered_data$as_of_date)
  
  # Calculate dollar changes
  changes <- filtered_data %>%
    arrange(type, maturity_date, as_of_date) %>%
    group_by(type, maturity_date) %>%
    mutate(dollar_change = price - lag(price)) %>%
    ungroup()
  
  # Trailing volatility
  vol_summary <- changes %>%
    filter(!is.na(dollar_change)) %>%
    group_by(type, maturity_date) %>%
    arrange(as_of_date, .by_group = TRUE) %>%
    slice_tail(n = lookback_n) %>%
    summarise(
      sd_dollar = sd(dollar_change, na.rm = TRUE),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    mutate(
      ann_vol = sd_dollar * sqrt(252),
      monthly_vol = ann_vol / sqrt(12)
    )
  
  # Get current prices
  current_prices <- filtered_data %>%
    filter(as_of_date == actual_val_date) %>%
    select(maturity_date, type, price)
  
  vol_summary <- vol_summary %>%
    left_join(current_prices, by = c("maturity_date", "type"))
  
  return(list(
    vol = vol_summary,
    val_date = actual_val_date
  ))
}

# Pre-storm volatilities
prestorm <- calc_dollar_vol(curve_raw, cfg$prestorm_date, cfg$vol_lookback_n)
cat("Pre-storm valuation date:", as.character(prestorm$val_date), "\n")

# Post-storm volatilities
poststorm_date <- if(is.null(cfg$poststorm_date)) max(curve_raw$as_of_date) else cfg$poststorm_date
poststorm <- calc_dollar_vol(curve_raw, poststorm_date, cfg$vol_lookback_n)
cat("Post-storm valuation date:", as.character(poststorm$val_date), "\n\n")

# -----------------------------------------------------------
# 3. COMPARE VOLATILITIES
# -----------------------------------------------------------

vol_comparison <- prestorm$vol %>%
  select(maturity_date, type, 
         monthly_vol_pre = monthly_vol, 
         price_pre = price) %>%
  inner_join(
    poststorm$vol %>% 
      select(maturity_date, type, 
             monthly_vol_post = monthly_vol, 
             price_post = price),
    by = c("maturity_date", "type")
  ) %>%
  mutate(
    vol_change = monthly_vol_post - monthly_vol_pre,
    vol_change_pct = (monthly_vol_post / monthly_vol_pre - 1) * 100,
    price_change = price_post - price_pre,
    price_change_pct = (price_post / price_pre - 1) * 100
  ) %>%
  arrange(maturity_date, type)

cat("========================================\n")
cat("VOLATILITY COMPARISON ($/MWh monthly)\n")
cat("========================================\n")

cat("\n--- PEAK CONTRACTS ---\n")
vol_comparison %>%
  filter(type == "Peak") %>%
  select(maturity_date, price_pre, price_post, price_change, 
         monthly_vol_pre, monthly_vol_post, vol_change, vol_change_pct) %>%
  print(n = 30)

cat("\n--- OFF-PEAK CONTRACTS ---\n")
vol_comparison %>%
  filter(type == "Off-Peak") %>%
  select(maturity_date, price_pre, price_post, price_change, 
         monthly_vol_pre, monthly_vol_post, vol_change, vol_change_pct) %>%
  print(n = 30)

# Summary statistics
cat("\n========================================\n")
cat("VOLATILITY SUMMARY\n")
cat("========================================\n")

vol_summary_stats <- vol_comparison %>%
  group_by(type) %>%
  summarise(
    avg_vol_pre = mean(monthly_vol_pre, na.rm = TRUE),
    avg_vol_post = mean(monthly_vol_post, na.rm = TRUE),
    avg_vol_change = mean(vol_change, na.rm = TRUE),
    avg_vol_change_pct = mean(vol_change_pct, na.rm = TRUE),
    avg_price_pre = mean(price_pre, na.rm = TRUE),
    avg_price_post = mean(price_post, na.rm = TRUE),
    avg_price_change = mean(price_change, na.rm = TRUE),
    .groups = "drop"
  )

print(vol_summary_stats)

cat("\nOverall average monthly vol ($/MWh):\n")
cat("  Pre-storm: $", round(mean(vol_comparison$monthly_vol_pre, na.rm = TRUE), 2), "\n", sep = "")
cat("  Post-storm: $", round(mean(vol_comparison$monthly_vol_post, na.rm = TRUE), 2), "\n", sep = "")
cat("  Change: $", round(mean(vol_comparison$vol_change, na.rm = TRUE), 2), 
    " (+", round(mean(vol_comparison$vol_change_pct, na.rm = TRUE), 1), "%)\n", sep = "")

# -----------------------------------------------------------
# 4. CALCULATE CORRELATIONS FOR BOTH PERIODS
# -----------------------------------------------------------

calc_correlations <- function(data, valuation_date, lookback_n) {
  # Filter to data up to valuation date
  filtered_data <- data %>%
    filter(as_of_date <= valuation_date)
  
  # Calculate dollar changes
  changes <- filtered_data %>%
    arrange(type, maturity_date, as_of_date) %>%
    group_by(type, maturity_date) %>%
    mutate(dollar_change = price - lag(price)) %>%
    ungroup() %>%
    mutate(
      contract_id = paste0(
        ifelse(type == "Peak", "PEAK_", "OFFPK_"),
        as.character(maturity_date)
      )
    )
  
  # Pivot to wide format
  changes_wide <- changes %>%
    filter(!is.na(dollar_change)) %>%
    select(as_of_date, contract_id, dollar_change) %>%
    pivot_wider(names_from = contract_id, values_from = dollar_change) %>%
    arrange(as_of_date)
  
  # Use only last N observations for correlation
  changes_wide <- changes_wide %>%
    slice_tail(n = lookback_n)
  
  # Calculate correlation matrix
  changes_matrix <- changes_wide %>%
    select(-as_of_date) %>%
    as.matrix()
  
  cor_matrix <- cor(changes_matrix, use = "pairwise.complete.obs")
  
  return(cor_matrix)
}

cat("\n========================================\n")
cat("CORRELATION COMPARISON\n")
cat("========================================\n")

cor_pre <- calc_correlations(curve_raw, cfg$prestorm_date, cfg$vol_lookback_n)
cor_post <- calc_correlations(curve_raw, poststorm_date, cfg$vol_lookback_n)

cat("\nCorrelation matrix dimensions:\n")
cat("  Pre-storm:", dim(cor_pre), "\n")
cat("  Post-storm:", dim(cor_post), "\n")

# Find common contracts
common_contracts <- intersect(rownames(cor_pre), rownames(cor_post))
cat("  Common contracts:", length(common_contracts), "\n")

# Extract correlations for common contracts
cor_pre_common <- cor_pre[common_contracts, common_contracts]
cor_post_common <- cor_post[common_contracts, common_contracts]

# Compare average correlations
# Peak-Peak correlations
peak_contracts <- common_contracts[grepl("^PEAK_", common_contracts)]
offpeak_contracts <- common_contracts[grepl("^OFFPK_", common_contracts)]

if (length(peak_contracts) > 1) {
  cor_peak_pre <- cor_pre_common[peak_contracts, peak_contracts]
  cor_peak_post <- cor_post_common[peak_contracts, peak_contracts]
  
  # Get off-diagonal (exclude self-correlations)
  peak_pre_offdiag <- cor_peak_pre[lower.tri(cor_peak_pre)]
  peak_post_offdiag <- cor_peak_post[lower.tri(cor_peak_post)]
  
  cat("\n--- Peak-Peak Correlations ---\n")
  cat("  Pre-storm avg:", round(mean(peak_pre_offdiag, na.rm = TRUE), 3), "\n")
  cat("  Post-storm avg:", round(mean(peak_post_offdiag, na.rm = TRUE), 3), "\n")
  cat("  Change:", round(mean(peak_post_offdiag, na.rm = TRUE) - mean(peak_pre_offdiag, na.rm = TRUE), 3), "\n")
}

if (length(offpeak_contracts) > 1) {
  cor_offpeak_pre <- cor_pre_common[offpeak_contracts, offpeak_contracts]
  cor_offpeak_post <- cor_post_common[offpeak_contracts, offpeak_contracts]
  
  offpeak_pre_offdiag <- cor_offpeak_pre[lower.tri(cor_offpeak_pre)]
  offpeak_post_offdiag <- cor_offpeak_post[lower.tri(cor_offpeak_post)]
  
  cat("\n--- Off-Peak / Off-Peak Correlations ---\n")
  cat("  Pre-storm avg:", round(mean(offpeak_pre_offdiag, na.rm = TRUE), 3), "\n")
  cat("  Post-storm avg:", round(mean(offpeak_post_offdiag, na.rm = TRUE), 3), "\n")
  cat("  Change:", round(mean(offpeak_post_offdiag, na.rm = TRUE) - mean(offpeak_pre_offdiag, na.rm = TRUE), 3), "\n")
}

if (length(peak_contracts) > 0 && length(offpeak_contracts) > 0) {
  cor_cross_pre <- cor_pre_common[peak_contracts, offpeak_contracts]
  cor_cross_post <- cor_post_common[peak_contracts, offpeak_contracts]
  
  cat("\n--- Peak / Off-Peak Cross-Correlations ---\n")
  cat("  Pre-storm avg:", round(mean(cor_cross_pre, na.rm = TRUE), 3), "\n")
  cat("  Post-storm avg:", round(mean(cor_cross_post, na.rm = TRUE), 3), "\n")
  cat("  Change:", round(mean(cor_cross_post, na.rm = TRUE) - mean(cor_cross_pre, na.rm = TRUE), 3), "\n")
}

# -----------------------------------------------------------
# 5. SAMPLE SPECIFIC CORRELATIONS
# -----------------------------------------------------------

cat("\n========================================\n")
cat("SAMPLE CORRELATIONS (Specific Pairs)\n")
cat("========================================\n")

# Define some interesting pairs to compare
sample_pairs <- list(
  c("PEAK_2026-07-01", "PEAK_2026-08-01"),
  c("PEAK_2027-01-01", "PEAK_2027-02-01"),
  c("PEAK_2028-01-01", "PEAK_2028-02-01"),
  c("PEAK_2026-07-01", "OFFPK_2026-07-01"),
  c("PEAK_2027-01-01", "OFFPK_2027-01-01")
)

cat("\nPair                                    Pre-storm  Post-storm  Change\n")
cat("----------------------------------------------------------------------\n")

for (pair in sample_pairs) {
  if (pair[1] %in% common_contracts && pair[2] %in% common_contracts) {
    cor_pre_val <- cor_pre_common[pair[1], pair[2]]
    cor_post_val <- cor_post_common[pair[1], pair[2]]
    change <- cor_post_val - cor_pre_val
    
    pair_name <- paste(pair[1], "vs", pair[2])
    cat(sprintf("%-38s  %6.3f     %6.3f     %+6.3f\n", 
                pair_name, cor_pre_val, cor_post_val, change))
  }
}

# -----------------------------------------------------------
# 6. VISUALIZATIONS
# -----------------------------------------------------------

# Volatility comparison plot
p_vol <- vol_comparison %>%
  select(maturity_date, type, monthly_vol_pre, monthly_vol_post) %>%
  pivot_longer(cols = c(monthly_vol_pre, monthly_vol_post),
               names_to = "period", values_to = "monthly_vol") %>%
  mutate(
    period = ifelse(period == "monthly_vol_pre", "Pre-Storm (Jan 16)", "Post-Storm (Current)")
  ) %>%
  ggplot(aes(x = maturity_date, y = monthly_vol, color = period, linetype = period)) +
  geom_line(linewidth = 1) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_color_manual(values = c("Pre-Storm (Jan 16)" = "blue", "Post-Storm (Current)" = "red")) +
  labs(
    title = "Monthly Dollar Volatility: Pre-Storm vs Post-Storm",
    subtitle = "30-day trailing volatility ($/MWh monthly)",
    x = "Delivery Month",
    y = "Monthly Vol ($/MWh)",
    color = "Period",
    linetype = "Period"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_vol)

# Price comparison plot
p_price <- vol_comparison %>%
  select(maturity_date, type, price_pre, price_post) %>%
  pivot_longer(cols = c(price_pre, price_post),
               names_to = "period", values_to = "price") %>%
  mutate(
    period = ifelse(period == "price_pre", "Pre-Storm (Jan 16)", "Post-Storm (Current)")
  ) %>%
  ggplot(aes(x = maturity_date, y = price, color = period, linetype = period)) +
  geom_line(linewidth = 1) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_color_manual(values = c("Pre-Storm (Jan 16)" = "blue", "Post-Storm (Current)" = "red")) +
  labs(
    title = "Forward Curve: Pre-Storm vs Post-Storm",
    subtitle = "Price by delivery month ($/MWh)",
    x = "Delivery Month",
    y = "Price ($/MWh)",
    color = "Period",
    linetype = "Period"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_price)

# Volatility change plot
p_vol_change <- vol_comparison %>%
  ggplot(aes(x = maturity_date, y = vol_change, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_fill_manual(values = c("Peak" = "red", "Off-Peak" = "steelblue")) +
  labs(
    title = "Change in Monthly Volatility (Post-Storm - Pre-Storm)",
    subtitle = "Positive = volatility increased after storm",
    x = "Delivery Month",
    y = "Vol Change ($/MWh monthly)",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_vol_change)

# -----------------------------------------------------------
# 7. EXPORT COMPARISON
# -----------------------------------------------------------

write_csv(vol_comparison, paste0(cfg$file_path, "vol_comparison_prestorm_vs_poststorm.csv"))
cat("\nSaved:", paste0(cfg$file_path, "vol_comparison_prestorm_vs_poststorm.csv"), "\n")

cat("\n========================================\n")
cat("COMPARISON COMPLETE\n")
cat("========================================\n")
