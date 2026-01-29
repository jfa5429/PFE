# =========================================================
# COMPARE P95 FORWARD CURVES AT MAX VM DATE
# Run this AFTER running all three MC models
# =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

cfg <- list(
  file_path = "C:/Users/AllenFr/OneDrive - SmartestEnergy/Documents/"
)

cat("========================================\n")
cat("P95 FORWARD CURVE COMPARISON\n")
cat("At Max VM Date: December 31, 2027\n")
cat("========================================\n\n")

# -----------------------------------------------------------
# 1. LOAD CONTRACT SUMMARIES FROM EACH MODEL
# -----------------------------------------------------------

log_summary <- read_csv(paste0(cfg$file_path, "reworld_contract_summary.csv"), show_col_types = FALSE)
arith_summary <- read_csv(paste0(cfg$file_path, "reworld_contract_summary_arithmetic.csv"), show_col_types = FALSE)
dollar_summary <- read_csv(paste0(cfg$file_path, "reworld_contract_summary_dollar_vol.csv"), show_col_types = FALSE)

# -----------------------------------------------------------
# 2. FILTER TO CONTRACTS ACTIVE AT DEC 31, 2027
# -----------------------------------------------------------

max_vm_date <- as.Date("2027-12-31")

# Contracts with maturity > Dec 2027 are still active
active_log <- log_summary %>%
  filter(maturity_date > max_vm_date)

active_arith <- arith_summary %>%
  filter(maturity_date > max_vm_date)

active_dollar <- dollar_summary %>%
  filter(maturity_date > max_vm_date)

cat("Active contracts at", as.character(max_vm_date), ":\n")
cat("  Total:", nrow(active_log), "\n")
cat("  Peak:", sum(active_log$type == "Peak"), "\n")
cat("  Off-Peak:", sum(active_log$type == "Off-Peak"), "\n\n")

# -----------------------------------------------------------
# 3. BUILD COMPARISON TABLE
# -----------------------------------------------------------

# Note: The contract_summary files have P95 at LAST margin date before delivery
# For contracts delivering Jan 2028+, the last margin date is at or before Dec 2027
# So these P95 prices are approximately correct for the max VM date

comparison <- active_log %>%
  select(contract_id, maturity_date, type, mwhrs, strike, 
         price_p95_log = price_p95, pnl_p95_log = pnl_p95) %>%
  left_join(
    active_arith %>% select(contract_id, price_p95_arith = price_p95, pnl_p95_arith = pnl_p95),
    by = "contract_id"
  ) %>%
  left_join(
    active_dollar %>% select(contract_id, price_p95_dollar = price_p95, pnl_p95_dollar = pnl_p95),
    by = "contract_id"
  ) %>%
  arrange(maturity_date, type)

# -----------------------------------------------------------
# 4. SUMMARY STATISTICS
# -----------------------------------------------------------

cat("========================================\n")
cat("REMAINING VOLUMES BY YEAR\n")
cat("========================================\n")

volume_by_year <- comparison %>%
  mutate(year = format(maturity_date, "%Y")) %>%
  group_by(year, type) %>%
  summarise(
    contracts = n(),
    total_mwh = sum(mwhrs),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = type, values_from = c(contracts, total_mwh))

print(volume_by_year)

cat("\nTotal remaining MWh:", format(sum(comparison$mwhrs), big.mark = ","), "\n")

# -----------------------------------------------------------
# 5. PEAK CONTRACT P95 COMPARISON
# -----------------------------------------------------------

cat("\n========================================\n")
cat("PEAK CONTRACTS - P95 PRICE COMPARISON\n")
cat("========================================\n")

peak_comparison <- comparison %>%
  filter(type == "Peak") %>%
  select(maturity_date, mwhrs, strike, price_p95_log, price_p95_arith, price_p95_dollar) %>%
  mutate(
    upside_log = price_p95_log - strike,
    upside_arith = price_p95_arith - strike,
    upside_dollar = price_p95_dollar - strike
  )

print(peak_comparison, n = 30)

cat("\n--- Peak P95 Summary (first 2 years) ---\n")
peak_comparison %>%
  filter(maturity_date <= as.Date("2029-12-31")) %>%
  summarise(
    avg_strike = mean(strike),
    avg_p95_log = mean(price_p95_log),
    avg_p95_arith = mean(price_p95_arith),
    avg_p95_dollar = mean(price_p95_dollar),
    avg_upside_log = mean(upside_log),
    avg_upside_arith = mean(upside_arith),
    avg_upside_dollar = mean(upside_dollar)
  ) %>%
  print()

# -----------------------------------------------------------
# 6. OFF-PEAK CONTRACT P95 COMPARISON
# -----------------------------------------------------------

cat("\n========================================\n")
cat("OFF-PEAK CONTRACTS - P95 PRICE COMPARISON\n")
cat("========================================\n")

offpeak_comparison <- comparison %>%
  filter(type == "Off-Peak") %>%
  select(maturity_date, mwhrs, strike, price_p95_log, price_p95_arith, price_p95_dollar) %>%
  mutate(
    upside_log = price_p95_log - strike,
    upside_arith = price_p95_arith - strike,
    upside_dollar = price_p95_dollar - strike
  )

print(offpeak_comparison, n = 30)

# -----------------------------------------------------------
# 7. HIGH-IMPACT CONTRACTS (Top PNL Contributors)
# -----------------------------------------------------------

cat("\n========================================\n")
cat("TOP 20 CONTRACTS BY LOG RETURN P95 P&L\n")
cat("========================================\n")

comparison %>%
  arrange(desc(pnl_p95_log)) %>%
  select(maturity_date, type, mwhrs, strike, price_p95_log, price_p95_arith, price_p95_dollar, pnl_p95_log) %>%
  head(20) %>%
  print()

# -----------------------------------------------------------
# 8. CREATE VISUALIZATION
# -----------------------------------------------------------

# Reshape for plotting
plot_data <- comparison %>%
  select(maturity_date, type, strike, price_p95_log, price_p95_arith, price_p95_dollar) %>%
  pivot_longer(
    cols = c(price_p95_log, price_p95_arith, price_p95_dollar),
    names_to = "model",
    values_to = "price_p95"
  ) %>%
  mutate(
    model = case_when(
      model == "price_p95_log" ~ "Log Return",
      model == "price_p95_arith" ~ "Arithmetic",
      model == "price_p95_dollar" ~ "Dollar Vol"
    ),
    model = factor(model, levels = c("Arithmetic", "Log Return", "Dollar Vol"))
  )

# Peak forward curve comparison
p_peak_curve <- ggplot(plot_data %>% filter(type == "Peak"), 
                        aes(x = maturity_date)) +
  geom_line(aes(y = strike), color = "black", linewidth = 1.2, linetype = "dashed") +
  geom_line(aes(y = price_p95, color = model), linewidth = 1) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_color_manual(values = c("Arithmetic" = "red", "Log Return" = "blue", "Dollar Vol" = "green")) +
  labs(
    title = "Peak P95 Forward Curves at Dec 31, 2027",
    subtitle = "Dashed = Strike (current forward). Solid = P95 by model",
    x = "Delivery Month",
    y = "Price ($/MWh)",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_peak_curve)

# Off-Peak forward curve comparison
p_offpeak_curve <- ggplot(plot_data %>% filter(type == "Off-Peak"), 
                           aes(x = maturity_date)) +
  geom_line(aes(y = strike), color = "black", linewidth = 1.2, linetype = "dashed") +
  geom_line(aes(y = price_p95, color = model), linewidth = 1) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_color_manual(values = c("Arithmetic" = "red", "Log Return" = "blue", "Dollar Vol" = "green")) +
  labs(
    title = "Off-Peak P95 Forward Curves at Dec 31, 2027",
    subtitle = "Dashed = Strike (current forward). Solid = P95 by model",
    x = "Delivery Month",
    y = "Price ($/MWh)",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_offpeak_curve)

# -----------------------------------------------------------
# 9. VOLUME + EXPOSURE CHART
# -----------------------------------------------------------

# Create combined chart with volumes and P95 upside
volume_plot_data <- comparison %>%
  group_by(maturity_date) %>%
  summarise(
    total_mwh = sum(mwhrs),
    peak_mwh = sum(mwhrs[type == "Peak"]),
    offpeak_mwh = sum(mwhrs[type == "Off-Peak"]),
    pnl_log = sum(pnl_p95_log),
    pnl_arith = sum(pnl_p95_arith),
    pnl_dollar = sum(pnl_p95_dollar),
    .groups = "drop"
  )

p_volume_pnl <- ggplot(volume_plot_data, aes(x = maturity_date)) +
  geom_bar(aes(y = total_mwh / 1000), stat = "identity", fill = "steelblue", alpha = 0.5) +
  geom_bar(aes(y = peak_mwh / 1000), stat = "identity", fill = "red", alpha = 0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(
    name = "MWh (thousands)",
    labels = comma_format()
  ) +
  labs(
    title = "Remaining Volumes at Dec 31, 2027",
    subtitle = "Red = Peak, Blue overlay = Off-Peak",
    x = "Delivery Month"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_volume_pnl)

# -----------------------------------------------------------
# 10. EXPORT COMPARISON TABLE
# -----------------------------------------------------------

write_csv(comparison, paste0(cfg$file_path, "p95_model_comparison.csv"))
cat("Saved: p95_model_comparison.csv\n")

# -----------------------------------------------------------
# 11. FINAL SUMMARY
# -----------------------------------------------------------

cat("\n========================================\n")
cat("FINAL SUMMARY AT MAX VM DATE (Dec 31, 2027)\n")
cat("========================================\n")

total_summary <- comparison %>%
  summarise(
    contracts = n(),
    total_mwh = sum(mwhrs),
    sum_pnl_log = sum(pnl_p95_log),
    sum_pnl_arith = sum(pnl_p95_arith),
    sum_pnl_dollar = sum(pnl_p95_dollar)
  )

cat("\nActive contracts:", total_summary$contracts, "\n")
cat("Remaining MWh:", format(total_summary$total_mwh, big.mark = ","), "\n")
cat("\nSum of Individual P95 P&Ls (no correlation):\n")
cat("  Arithmetic:  $", format(round(total_summary$sum_pnl_arith / 1e6, 1), big.mark = ","), "M\n", sep = "")
cat("  Log Return:  $", format(round(total_summary$sum_pnl_log / 1e6, 1), big.mark = ","), "M\n", sep = "")
cat("  Dollar Vol:  $", format(round(total_summary$sum_pnl_dollar / 1e6, 1), big.mark = ","), "M\n", sep = "")

cat("\nCorrelated Portfolio P95 (from MC):\n")
cat("  Arithmetic:  $222.5M\n")
cat("  Log Return:  $193.2M\n")
cat("  Dollar Vol:  $160.3M\n")

cat("\nDiversification Benefit:\n")
cat("  Arithmetic:  $", format(round((total_summary$sum_pnl_arith - 222519238) / 1e6, 1), big.mark = ","), "M\n", sep = "")
cat("  Log Return:  $", format(round((total_summary$sum_pnl_log - 193175808) / 1e6, 1), big.mark = ","), "M\n", sep = "")
cat("  Dollar Vol:  $", format(round((total_summary$sum_pnl_dollar - 160316517) / 1e6, 1), big.mark = ","), "M\n", sep = "")

cat("\n========================================\n")
cat("COMPARISON COMPLETE\n")
cat("========================================\n")
