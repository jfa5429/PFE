
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(ggplot2)
  library(scales)
  library(MASS)     # mvrnorm
})

options(
  pillar.sigfig = 6,
  digits = 6
)

# ------------------------------------------------------------
# 1. CONFIG
# ------------------------------------------------------------

cfg <- list(
  file_path       = "c:/Users/JFA/OneDrive/Documents/mass.hub.26.28.csv",
  col_asof        = "As of Date",
  col_maturity    = "Maturity Date",
  col_curve_name  = "Peak Offpeak",   # "Peak" / "Off-Peak"
  col_curve_value = "Curve Value",
  
  start_maturity  = as.Date("2026-01-01"),
  end_maturity    = as.Date("2028-12-31"),
  
  
  # PFE valuation dates (year-ends)
  pfe_dates       = as.Date(c("2025-12-31", "2026-12-31", "2027-12-31")),
  
  # Minimum non-NA daily observations required per maturity
  min_obs_days    = 30L
)

# ------------------------------------------------------------
# 2. LOAD RAW CURVE DATA
# ------------------------------------------------------------

curve_raw <- read_csv(cfg$file_path, show_col_types = FALSE) %>%
  rename(
    as_of_raw      = !!cfg$col_asof,
    maturity_raw   = !!cfg$col_maturity,
    curve_name_raw = !!cfg$col_curve_name,
    curve_value    = !!cfg$col_curve_value
  ) %>%
  mutate(
    as_of_date    = as.Date(as_of_raw,    format = "%m/%d/%Y"),
    maturity_date = as.Date(maturity_raw, format = "%m/%d/%Y")
  )

stopifnot(all(!is.na(curve_raw$as_of_date)))
stopifnot(all(!is.na(curve_raw$maturity_date)))

# ------------------------------------------------------------
# 3. BUILD HISTORICAL ATC (PEAK + OFF-PEAK)
#    ATC = Peak * (80/168) + Off-Peak * (88/168)
# ------------------------------------------------------------

peak_data <- curve_raw %>%
  filter(curve_name_raw == "Peak") %>%
  transmute(
    as_of_date,
    maturity_date,
    peak_price = curve_value
  )

offpeak_data <- curve_raw %>%
  filter(curve_name_raw == "Off-Peak") %>%
  transmute(
    as_of_date,
    maturity_date,
    offpeak_price = curve_value
  )

historical_atc <- full_join(
  peak_data,
  offpeak_data,
  by = c("as_of_date", "maturity_date")
) %>%
  mutate(
    atc_price = (peak_price * 80 + offpeak_price * 88) / 168
  ) %>%
  filter(
    maturity_date >= cfg$start_maturity,
    maturity_date <= cfg$end_maturity
  ) %>%
  arrange(maturity_date, as_of_date)

stopifnot(nrow(historical_atc) > 0)

head(historical_atc)


# ------------------------------------------------------------
# 4. CURRENT FORWARD CURVE + TRAILING VOL STATS
# ------------------------------------------------------------




# 4a. Current ATC forward curve (latest as_of_date across all maturities)
(valuation_date <- max(historical_atc$as_of_date, na.rm = TRUE))

current_curve <- historical_atc %>%
  filter(as_of_date == valuation_date) %>%
  distinct(maturity_date, atc_price) %>%
  arrange(maturity_date) %>%
  rename(current_atc = atc_price)

current_curve

# 4b. Build daily % returns and $ changes by maturity
historical_changes <- historical_atc %>%
  dplyr::select(as_of_date, maturity_date, atc_price) %>%
  arrange(maturity_date, as_of_date) %>%
  group_by(maturity_date) %>%
  mutate(
    dollar_change = atc_price - lag(atc_price),
    #pct_return    = atc_price / lag(atc_price) - 1
    log_return  = log(atc_price / lag(atc_price))
  ) %>%
  ungroup()


# 4c. For each maturity, take the last N observations and compute std dev
vol_lookback_n <- 30L  

# trailing vol over last N observations per maturity
vol_summary <- historical_changes %>%
  filter(!is.na(dollar_change), !is.na(log_return)) %>%
  group_by(maturity_date) %>%
  arrange(as_of_date, .by_group = TRUE) %>%
  slice_tail(n = vol_lookback_n) %>%
  summarise(
    sd_log_return    = sd(log_return,    na.rm = TRUE),  # daily log-vol (unitless)
    sd_dollar_change = sd(dollar_change, na.rm = TRUE),  # daily $ vol
    n_obs_used       = n(),
    .groups = "drop"
  ) %>%
  mutate(
    # annualization factor (trading days)
    ann_factor             = sqrt(252),
    # annualized log-return vol (unitless)
    sd_log_return_ann      = sd_log_return * ann_factor,
    # same as % per year
    sd_log_return_ann_pct  = sd_log_return_ann * 100,
    # annualized $ vol
    sd_dollar_change_ann   = sd_dollar_change * ann_factor
  )

# 4d. Join current curve with vol stats
forward_vol_table <- current_curve %>%
  left_join(vol_summary, by = "maturity_date") %>%
  arrange(maturity_date)

forward_vol_table %>% print(n = 50)

# 4e. Export to CSV
#write_csv(
#  forward_vol_table,
#  "c:/Users/JFA/OneDrive/Documents/mass_hub_forward_vol_summary.csv"
#)







# ------------------------------------------------------------
# 6. P05 / P95 FORWARD PRICES (LOGNORMAL, CENTERED ON FORWARD)
#    for ALL PFE dates in cfg$pfe_dates
# ------------------------------------------------------------

z05 <- qnorm(0.05)   # -1.645
z95 <- qnorm(0.95)   # +1.645

# forward_vol_table already has:
#   maturity_date, current_atc, sd_log_return_ann, ...

# 6a. Build PFE price distribution for each (PFE date, maturity)
pfe_prices <- forward_vol_table %>%
  mutate(
    # end of *delivery* month (e.g., Jan-2026 -> 2026-01-31)
    delivery_end = ceiling_date(maturity_date, "month") - 1
  ) %>%
  # cross with all PFE dates
  tidyr::crossing(pfe_date = cfg$pfe_dates) %>%
  mutate(
    # days from PFE date to end of delivery month
    days_exposure = as.numeric(delivery_end - pfe_date),
    # clamp at 0 if PFE is after delivery end
    T_years       = pmax(days_exposure / 365, 0),
    
    # horizon log-vol; sd_log_return_ann is annual
    sigma_h       = sd_log_return_ann * sqrt(T_years),
    
    # simple constant IR assumption (annualized)
    ir            = 0.02,
    drift         = ir * T_years,
    
    # GBM-style lognormal quantiles:
    # log S_T ~ log(F) + (ir - 0.5*sigma_h^2) + sigma_h * Z
    # => E[S_T] = F * exp(ir * T_years), if you interpret current_atc as F
    fwd_p05       = current_atc * exp(drift - 0.5 * sigma_h^2 + sigma_h * z05),
    fwd_p95       = current_atc * exp(drift - 0.5 * sigma_h^2 + sigma_h * z95)
  ) %>%
  arrange(pfe_date, maturity_date)

pfe_prices %>% print(n = 50)


# ------------------------------------------------------------
# 7. DEAL VOLUMES + PFE PNL (P05 / P95)
# ------------------------------------------------------------

mw_table <- tribble(
  ~maturity_date, ~mw,
  as.Date("2026-01-01"), 10.2,
  as.Date("2026-02-01"), 10.3,
  as.Date("2026-03-01"), 11.3,
  as.Date("2026-04-01"), 12.2,
  as.Date("2026-05-01"), 11.4,
  as.Date("2026-06-01"), 8.2,
  as.Date("2026-07-01"), 5.1,
  as.Date("2026-08-01"), 2.7,
  as.Date("2026-09-01"), 4.5,
  as.Date("2026-10-01"), 5.0,
  as.Date("2026-11-01"), 6.4,
  as.Date("2026-12-01"), 9.8,
  
  as.Date("2027-01-01"), 10.2,
  as.Date("2027-02-01"), 10.3,
  as.Date("2027-03-01"), 11.3,
  as.Date("2027-04-01"), 12.2,
  as.Date("2027-05-01"), 11.4,
  as.Date("2027-06-01"), 8.2,
  as.Date("2027-07-01"), 5.1,
  as.Date("2027-08-01"), 2.7,
  as.Date("2027-09-01"), 4.5,
  as.Date("2027-10-01"), 5.0,
  as.Date("2027-11-01"), 6.4,
  as.Date("2027-12-01"), 9.8,
  
  as.Date("2028-01-01"), 10.2,
  as.Date("2028-02-01"), 10.3,
  as.Date("2028-03-01"), 11.3,
  as.Date("2028-04-01"), 12.2,
  as.Date("2028-05-01"), 11.4,
  as.Date("2028-06-01"), 8.2,
  as.Date("2028-07-01"), 5.1,
  as.Date("2028-08-01"), 2.7,
  as.Date("2028-09-01"), 4.5,
  as.Date("2028-10-01"), 5.0,
  as.Date("2028-11-01"), 6.4,
  as.Date("2028-12-01"), 9.8
)

# Attach volumes and compute MWh + PFE PnL
pfe_with_volumes <- pfe_prices %>%
  left_join(mw_table, by = "maturity_date") %>%
  mutate(
    days_per_month = days_in_month(maturity_date),
    mwhrs          = mw * days_per_month * 24,
    pnl_p05        = mwhrs * (fwd_p05 - current_atc),
    pnl_p95        = mwhrs * (fwd_p95 - current_atc)
  )

pfe_with_volumes %>% print(n = 80)

# Portfolio-level PFE by date (sum over all months)
pfe_portfolio <- pfe_with_volumes %>%
  group_by(pfe_date) %>%
  summarise(
    total_pnl_p05 = sum(pnl_p05, na.rm = TRUE),
    total_pnl_p95 = sum(pnl_p95, na.rm = TRUE),
    .groups = "drop"
  )

pfe_portfolio


# ------------------------------------------------------------
# 8. EXPORT TO CSV (DETAIL + PORTFOLIO SUMMARY)
# ------------------------------------------------------------

write_csv(
  pfe_with_volumes,
  "c:/Users/JFA/OneDrive/Documents/mass_hub_pfe_detail.csv"
)

write_csv(
  pfe_portfolio,
  "c:/Users/JFA/OneDrive/Documents/mass_hub_pfe_portfolio_summary.csv"
)
