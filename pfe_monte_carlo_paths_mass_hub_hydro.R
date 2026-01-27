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
  file_path       = "C:/Users/AllenFr/OneDrive - SmartestEnergy/Documents/mass.hub.26.28.csv",
  col_asof        = "As of Date",
  col_maturity    = "Maturity Date",
  col_curve_name  = "Peak Offpeak",
  col_curve_value = "Curve Value",
  start_maturity  = as.Date("2026-01-01"),
  end_maturity    = as.Date("2028-12-31"),
  min_obs_days    = 30L,
  n_sims          = 10000L,
  ir_annual       = 0.02,
  # Volatility term structure parameters
  # These will be calibrated from historical data if vol_term_structure = TRUE
  # Otherwise, defaults below are used
  vol_term_structure = FALSE,  # Set to FALSE to use static volatilities
  use_seasonal_dynamic = FALSE,  # Set to FALSE to use pure calibrated (no seasonality)
  vol_base_multiplier = 2.0,   # Default: Multiplier for spot month (if not calibrated)
  vol_decay_rate = 0.10,       # Default: Decay per month (if not calibrated)
  vol_baseline_months = 24     # Baseline horizon for calibration
)

# -----------------------------------------------------------
# 2. LOAD RAW CURVE DATA
# -----------------------------------------------------------

curve_raw <- read_csv(cfg$file_path, show_col_types = FALSE) %>%
  rename(
    as_of_raw      = !!cfg$col_asof,
    maturity_raw   = !!cfg$col_maturity,
    curve_name_raw = !!cfg$col_curve_name,
    curve_value    = !!cfg$col_curve_value
  ) %>%
  mutate(
    as_of_date    = as.Date(as_of_raw, format = "%m/%d/%Y"),
    maturity_date = as.Date(maturity_raw, format = "%m/%d/%Y")
  )

stopifnot(all(!is.na(curve_raw$as_of_date)))
stopifnot(all(!is.na(curve_raw$maturity_date)))

# -----------------------------------------------------------
# 3. BUILD HISTORICAL ATC (PEAK + OFF-PEAK)
# -----------------------------------------------------------

peak_data <- curve_raw %>%
  filter(curve_name_raw == "Peak") %>%
  transmute(as_of_date, maturity_date, peak_price = curve_value)

offpeak_data <- curve_raw %>%
  filter(curve_name_raw == "Off-Peak") %>%
  transmute(as_of_date, maturity_date, offpeak_price = curve_value)

historical_atc <- full_join(peak_data, offpeak_data, by = c("as_of_date", "maturity_date")) %>%
  mutate(atc_price = (peak_price * 80 + offpeak_price * 88) / 168) %>%
  filter(maturity_date >= cfg$start_maturity, maturity_date <= cfg$end_maturity) %>%
  arrange(maturity_date, as_of_date)

stopifnot(nrow(historical_atc) > 0)

# -----------------------------------------------------------
# 4. CURRENT FORWARD CURVE + VOLATILITY STATS
# -----------------------------------------------------------

valuation_date <- max(historical_atc$as_of_date, na.rm = TRUE)
cat("Valuation date:", as.character(valuation_date), "\n")

current_curve <- historical_atc %>%
  filter(as_of_date == valuation_date) %>%
  distinct(maturity_date, atc_price) %>%
  arrange(maturity_date) %>%
  rename(current_atc = atc_price)

# Build log returns
historical_changes <- historical_atc %>%
  dplyr::select(as_of_date, maturity_date, atc_price) %>%
  arrange(maturity_date, as_of_date) %>%
  group_by(maturity_date) %>%
  mutate(log_return = log(atc_price / lag(atc_price))) %>%
  ungroup()

# Trailing volatility
vol_lookback_n <- 30L

vol_summary <- historical_changes %>%
  filter(!is.na(log_return)) %>%
  group_by(maturity_date) %>%
  arrange(as_of_date, .by_group = TRUE) %>%
  slice_tail(n = vol_lookback_n) %>%
  summarise(
    sd_log_return = sd(log_return, na.rm = TRUE),
    n_obs_used    = n(),
    .groups       = "drop"
  ) %>%
  mutate(
    ann_factor         = sqrt(252),
    sd_log_return_ann  = sd_log_return * ann_factor,
    # Monthly volatility for path simulation
    sd_log_return_monthly = sd_log_return_ann / sqrt(12)
  )

forward_vol_table <- current_curve %>%
  left_join(vol_summary, by = "maturity_date") %>%
  arrange(maturity_date)

cat("\nForward curve with volatility:\n")
print(forward_vol_table, n = 50)

# -----------------------------------------------------------
# 5. BUILD CORRELATION MATRIX FROM LOG RETURNS
# -----------------------------------------------------------

returns_wide <- historical_changes %>%
  filter(!is.na(log_return)) %>%
  dplyr::select(as_of_date, maturity_date, log_return) %>%
  pivot_wider(names_from = maturity_date, values_from = log_return) %>%
  arrange(as_of_date)

returns_matrix <- returns_wide %>%
  dplyr::select(-as_of_date) %>%
  as.matrix()

cor_matrix <- cor(returns_matrix, use = "pairwise.complete.obs")

cat("\nCorrelation matrix dimensions:", dim(cor_matrix), "\n")

# -----------------------------------------------------------
# 5b. CALIBRATE VOLATILITY TERM STRUCTURE
# -----------------------------------------------------------

cat("\nCalibrating volatility term structure from historical data...\n")

# For each maturity month, calculate volatility at different times-to-delivery
vol_term_data <- historical_changes %>%
  filter(!is.na(log_return)) %>%
  mutate(
    months_to_delivery = interval(as_of_date, maturity_date) %/% months(1)
  ) %>%
  filter(months_to_delivery >= 0) %>%  # Only forward-looking
  group_by(maturity_date, months_to_delivery) %>%
  # Take rolling window of last 30 observations at each time-to-delivery
  arrange(as_of_date, .by_group = TRUE) %>%
  mutate(row_num = row_number()) %>%
  filter(row_num >= max(row_num) - 29) %>%  # Last 30 obs
  summarise(
    vol_daily = sd(log_return, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 10) %>%  # Require at least 10 observations
  mutate(
    vol_ann = vol_daily * sqrt(252),
    vol_monthly = vol_ann / sqrt(12)
  )

# Average volatility by months_to_delivery
vol_by_horizon <- vol_term_data %>%
  group_by(months_to_delivery) %>%
  summarise(
    avg_vol_monthly = mean(vol_monthly, na.rm = TRUE),
    n_contracts = n(),
    .groups = "drop"
  ) %>%
  filter(months_to_delivery <= 36) %>%  # Limit to reasonable horizons
  arrange(months_to_delivery)

cat("\nObserved volatilities by time to delivery:\n")
print(vol_by_horizon, n = 36)

# Fit exponential decay: vol = base * exp(-decay * months)
# Using nonlinear least squares
if (nrow(vol_by_horizon) >= 3 && cfg$vol_term_structure) {
  
  # Fit the model
  fit <- nls(
    avg_vol_monthly ~ base * exp(-decay * months_to_delivery),
    data = vol_by_horizon,
    start = list(base = max(vol_by_horizon$avg_vol_monthly), decay = 0.05),
    control = nls.control(maxiter = 100, warnOnly = TRUE)
  )
  
  fitted_base <- coef(fit)["base"]
  fitted_decay <- coef(fit)["decay"]
  
  cat("\n========================================\n")
  cat("CALIBRATED VOLATILITY TERM STRUCTURE\n")
  cat("========================================\n")
  cat("Base volatility (spot):", round(fitted_base, 6), "\n")
  cat("Decay rate:", round(fitted_decay, 4), "per month\n")
  
  # Calculate multipliers relative to far-dated baseline
  baseline_months <- 24  # Use 2-year as baseline
  baseline_vol <- fitted_base * exp(-fitted_decay * baseline_months)
  
  cat("\nMultipliers relative to", baseline_months, "month baseline:\n")
  horizons_example <- c(0, 1, 3, 6, 12, 18, 24, 30, 36)
  for (h in horizons_example) {
    vol_h <- fitted_base * exp(-fitted_decay * h)
    mult <- vol_h / baseline_vol
    cat(sprintf("  %2d months: %.2fx\n", h, mult))
  }
  
  # Update config with fitted parameters
  cfg$vol_base_multiplier <- fitted_base / baseline_vol
  cfg$vol_decay_rate <- fitted_decay
  cfg$vol_baseline_months <- baseline_months
  
  cat("\nUpdated config:\n")
  cat("  vol_base_multiplier:", round(cfg$vol_base_multiplier, 3), "\n")
  cat("  vol_decay_rate:", round(cfg$vol_decay_rate, 4), "\n")
  
  # Plot observed vs fitted
  fitted_curve <- tibble(
    months_to_delivery = seq(0, max(vol_by_horizon$months_to_delivery), by = 0.5),
    fitted_vol = fitted_base * exp(-fitted_decay * months_to_delivery)
  )
  
  p_vol_term <- ggplot() +
    geom_point(data = vol_by_horizon, aes(x = months_to_delivery, y = avg_vol_monthly, size = n_contracts),
               alpha = 0.6, color = "steelblue") +
    geom_line(data = fitted_curve, aes(x = months_to_delivery, y = fitted_vol),
              color = "red", linewidth = 1) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Volatility Term Structure (Calibrated)",
      subtitle = sprintf("Fitted: vol = %.4f * exp(-%.4f * months)", fitted_base, fitted_decay),
      x = "Months to Delivery",
      y = "Monthly Volatility",
      size = "# Contracts"
    ) +
    theme_minimal()
  
  print(p_vol_term)
  
  # Compare static day-one vs calibrated term structure
  cat("\n--- Comparison: Static Day-One vs Calibrated Term Structure ---\n")
  
  # Static day-one: months to delivery from valuation date
  static_vol_curve <- forward_vol_table %>%
    mutate(
      months_to_delivery = interval(valuation_date, maturity_date) %/% months(1),
      vol_source = "Static (Day 1)"
    ) %>%
    dplyr::select(months_to_delivery, monthly_vol = sd_log_return_monthly, vol_source)
  
  # Calibrated curve at same horizons
  calibrated_vol_curve <- tibble(
    months_to_delivery = static_vol_curve$months_to_delivery,
    monthly_vol = fitted_base * exp(-fitted_decay * months_to_delivery),
    vol_source = "Calibrated (Dynamic)"
  )
  
  combined_curves <- bind_rows(static_vol_curve, calibrated_vol_curve)
  
  p_comparison <- ggplot(combined_curves, aes(x = months_to_delivery, y = monthly_vol, color = vol_source)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = c("Static (Day 1)" = "steelblue", "Calibrated (Dynamic)" = "red")) +
    labs(
      title = "Volatility Term Structure Comparison",
      subtitle = "Static day-one volatilities vs calibrated dynamic term structure",
      x = "Months to Delivery",
      y = "Monthly Volatility",
      color = "Volatility Source"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p_comparison)
  
  # --- Add Seasonal + Time-to-Delivery Model (if enabled) ---
  if (cfg$use_seasonal_dynamic) {
    cat("\n--- Calibrating Seasonal + Time-to-Delivery Model ---\n")
    
    # Extract base seasonal volatilities from far-dated observations
    # Use volatilities at longer horizons (e.g., 18-30 months) as seasonal baseline
    seasonal_baseline <- vol_term_data %>%
      filter(months_to_delivery >= 18, months_to_delivery <= 30) %>%
      mutate(month = month(maturity_date)) %>%
      group_by(month) %>%
      summarise(
        base_vol = mean(vol_monthly, na.rm = TRUE),
        n_obs = n(),
        .groups = "drop"
      )
    
    # If insufficient data, use static day-one volatilities as seasonal baseline
    if (nrow(seasonal_baseline) < 12) {
      cat("Insufficient far-dated observations - using static volatilities as seasonal baseline\n")
      seasonal_baseline <- forward_vol_table %>%
        mutate(month = month(maturity_date)) %>%
        group_by(month) %>%
        summarise(
          base_vol = mean(sd_log_return_monthly, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    cat("\nSeasonal base volatilities by month:\n")
    print(seasonal_baseline)
    
    # Create seasonal + dynamic curve for comparison
    seasonal_dynamic_curve <- forward_vol_table %>%
      mutate(
        months_to_delivery = interval(valuation_date, maturity_date) %/% months(1),
        month = month(maturity_date)
      ) %>%
      left_join(seasonal_baseline, by = "month") %>%
      mutate(
        # Apply time-to-delivery decay to seasonal baseline
        monthly_vol = base_vol * exp(-fitted_decay * months_to_delivery),
        vol_source = "Seasonal + Dynamic"
      ) %>%
      dplyr::select(months_to_delivery, monthly_vol, vol_source)
    
    # Combine all three curves
    all_curves <- bind_rows(
      static_vol_curve,
      calibrated_vol_curve,
      seasonal_dynamic_curve
    )
    
    p_all_comparison <- ggplot(all_curves, aes(x = months_to_delivery, y = monthly_vol, color = vol_source)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = c(
        "Static (Day 1)" = "steelblue", 
        "Calibrated (Dynamic)" = "red",
        "Seasonal + Dynamic" = "darkgreen"
      )) +
      labs(
        title = "Volatility Term Structure Comparison - Three Methods",
        subtitle = "Static vs Calibrated vs Seasonal+Dynamic",
        x = "Months to Delivery",
        y = "Monthly Volatility",
        color = "Volatility Source"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    print(p_all_comparison)
    
    # Store seasonal baseline for use in simulation
    cfg$seasonal_baseline <- seasonal_baseline
    
    cat("\nSeasonal + Dynamic model calibrated.\n")
  }
  
} else {
  cat("\nInsufficient data for calibration - using default parameters\n")
}

# -----------------------------------------------------------
# 6. DEAL VOLUMES
# -----------------------------------------------------------

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

# -----------------------------------------------------------
# 7. PATH-BASED MONTE CARLO SIMULATION
# -----------------------------------------------------------

set.seed(1)

# Build contract table with all needed info
contracts <- forward_vol_table %>%
  left_join(mw_table, by = "maturity_date") %>%
  mutate(
    mwhrs = mw * days_in_month(maturity_date) * 24,
    strike = current_atc,  # Lock in strike at deal inception
    # End of delivery month (when contract finishes pricing)
    delivery_end = ceiling_date(maturity_date, "month") - 1
  ) %>%
  arrange(maturity_date)

n_contracts <- nrow(contracts)
n_sims <- cfg$n_sims

# Margin evaluation dates: end of each month from Dec 2025 through Nov 2028
# Generate first of each month, then get last day
first_of_months <- seq(as.Date("2025-12-01"), as.Date("2028-11-01"), by = "month")
margin_dates <- ceiling_date(first_of_months, "month") - 1
n_periods <- length(margin_dates)

cat("\nNumber of margin periods:", n_periods, "\n")
cat("Margin dates:", as.character(margin_dates[1]), "to", as.character(margin_dates[n_periods]), "\n")
cat("All margin dates:\n")
print(margin_dates)

# Monthly drift
drift_monthly <- cfg$ir_annual / 12

# Initialize price matrix: rows = sims, cols = contracts
# Start with current forward prices
sim_prices <- matrix(
  rep(contracts$current_atc, each = n_sims),
  nrow = n_sims,
  ncol = n_contracts
)

# Matrix to store MTM at each margin date: rows = sims, cols = margin dates
mtm_matrix <- matrix(0, nrow = n_sims, ncol = n_periods)

# Store simulated prices at each margin date for curve visualization
# List of matrices: each element is n_sims x n_contracts
price_history <- vector("list", n_periods)

# Get monthly volatilities and correlation matrix
monthly_vols <- contracts$sd_log_return_monthly
contract_dates <- as.character(contracts$maturity_date)

# Simulate paths
cat("\nSimulating paths...\n")

for (t in 1:n_periods) {
  margin_date <- margin_dates[t]
  
  # Identify active contracts (still have price risk after this margin date)
  # A contract finishes pricing at the end of its delivery month - no more risk after that
  active_idx <- which(contracts$delivery_end > margin_date)
  n_active <- length(active_idx)
  
  if (n_active == 0) {
    mtm_matrix[, t] <- 0
    price_history[[t]] <- sim_prices  # Store final prices
    next
  }
  
  # Generate correlated shocks for active contracts
  active_names <- contract_dates[active_idx]
  cor_subset <- cor_matrix[active_names, active_names, drop = FALSE]
  
  # Handle single contract case
  if (n_active == 1) {
    Z <- matrix(rnorm(n_sims), ncol = 1)
  } else {
    Z <- mvrnorm(n = n_sims, mu = rep(0, n_active), Sigma = cor_subset)
  }
  
  # Update prices for active contracts
  for (i in 1:n_active) {
    idx <- active_idx[i]
    
    # Start with baseline volatility (from historical data at baseline horizon)
    if (cfg$vol_term_structure && exists("fitted_base") && exists("fitted_decay")) {
      
      # Months from margin_date to delivery month start
      months_to_delivery <- interval(margin_date, contracts$maturity_date[idx]) %/% months(1)
      months_to_delivery <- max(0, months_to_delivery)
      
      # Check if seasonal + dynamic model is enabled
      if (!is.null(cfg$use_seasonal_dynamic) && cfg$use_seasonal_dynamic && !is.null(cfg$seasonal_baseline)) {
        # Seasonal + Dynamic: base_seasonal * exp(-decay * months)
        delivery_month <- month(contracts$maturity_date[idx])
        seasonal_base <- cfg$seasonal_baseline$base_vol[cfg$seasonal_baseline$month == delivery_month]
        
        if (length(seasonal_base) == 0) {
          # Fallback if month not found
          seasonal_base <- mean(cfg$seasonal_baseline$base_vol)
        }
        
        sigma_m <- seasonal_base * exp(-fitted_decay * months_to_delivery)
        
      } else {
        # Pure calibrated term structure (no seasonality)
        sigma_m <- fitted_base * exp(-fitted_decay * months_to_delivery)
      }
      
    } else if (cfg$vol_term_structure) {
      # Use default term structure parameters
      sigma_m <- monthly_vols[idx]
      months_to_delivery <- interval(margin_date, contracts$maturity_date[idx]) %/% months(1)
      months_to_delivery <- max(0, months_to_delivery)
      vol_multiplier <- cfg$vol_base_multiplier * exp(-cfg$vol_decay_rate * months_to_delivery)
      sigma_m <- sigma_m * vol_multiplier
      
    } else {
      # No term structure adjustment
      sigma_m <- monthly_vols[idx]
    }
    
    # Lognormal evolution: P_new = P_old * exp(drift - 0.5*sigma^2 + sigma*Z)
    sim_prices[, idx] <- sim_prices[, idx] * exp(
      drift_monthly - 0.5 * sigma_m^2 + sigma_m * Z[, i]
    )
  }
  
  # Store prices at this margin date
  price_history[[t]] <- sim_prices
  
  # Calculate MTM for this margin date (only active contracts)
  # MTM = sum of MWh * (simulated price - strike)
  mtm <- rep(0, n_sims)
  for (idx in active_idx) {
    mtm <- mtm + contracts$mwhrs[idx] * (sim_prices[, idx] - contracts$strike[idx])
  }
  
  mtm_matrix[, t] <- mtm
}

cat("Simulation complete.\n")

# Show volatility term structure if enabled
if (cfg$vol_term_structure) {
  cat("\n========================================\n")
  cat("VOLATILITY TERM STRUCTURE (Used in Simulation)\n")
  
  if (!is.null(cfg$use_seasonal_dynamic) && cfg$use_seasonal_dynamic) {
    cat("Model: SEASONAL + DYNAMIC\n")
  } else {
    cat("Model: CALIBRATED DYNAMIC (no seasonality)\n")
  }
  
  cat("========================================\n")
  
  months_example <- c(0, 1, 3, 6, 12, 18, 24, 36)
  
  if (exists("fitted_base") && exists("fitted_decay")) {
    # Show calibrated curve
    if (!is.null(cfg$use_seasonal_dynamic) && cfg$use_seasonal_dynamic && !is.null(cfg$seasonal_baseline)) {
      # Show seasonal + dynamic for a few example months
      cat("\nExample volatilities by delivery month and time-to-delivery:\n")
      
      example_months <- c(1, 4, 7, 10)  # Jan, Apr, Jul, Oct
      for (m in example_months) {
        seasonal_base <- cfg$seasonal_baseline$base_vol[cfg$seasonal_baseline$month == m]
        if (length(seasonal_base) == 0) seasonal_base <- mean(cfg$seasonal_baseline$base_vol)
        
        cat(sprintf("\n%s (Month %d):\n", month.name[m], m))
        for (h in months_example) {
          vol_h <- seasonal_base * exp(-fitted_decay * h)
          cat(sprintf("  %2d months: %.2f%%\n", h, vol_h * 100))
        }
      }
      
    } else {
      # Show pure calibrated curve
      vols <- fitted_base * exp(-fitted_decay * months_example)
      baseline_vol <- fitted_base * exp(-fitted_decay * cfg$vol_baseline_months)
      
      term_struct <- tibble(
        months_to_delivery = months_example,
        monthly_vol = vols,
        annual_vol = vols * sqrt(12),
        multiplier_vs_baseline = vols / baseline_vol
      )
      
      cat("\nCalibrated from historical data:\n")
      print(term_struct)
    }
    
  } else {
    # Show default curve
    multipliers <- cfg$vol_base_multiplier * exp(-cfg$vol_decay_rate * months_example)
    
    term_struct <- tibble(
      months_to_delivery = months_example,
      vol_multiplier = multipliers,
      relative_to_baseline = multipliers / multipliers[length(multipliers)]
    )
    
    cat("\nUsing default parameters:\n")
    print(term_struct)
  }
  
  cat("\n")
}

# -----------------------------------------------------------
# 8. CALCULATE PFE PROFILE
# -----------------------------------------------------------

# Calculate month-over-month changes in MTM
# change_matrix[sim, t] = MTM[t] - MTM[t-1]
change_matrix <- matrix(0, nrow = n_sims, ncol = n_periods)
change_matrix[, 1] <- mtm_matrix[, 1]  # First period change from 0
for (t in 2:n_periods) {
  change_matrix[, t] <- mtm_matrix[, t] - mtm_matrix[, t - 1]
}

# Calculate no-correlation P95: sum of individual month P95 P&Ls
vm_p95_no_corr <- numeric(n_periods)

for (t in 1:n_periods) {
  margin_date <- margin_dates[t]
  active_idx <- which(contracts$delivery_end > margin_date)
  
  if (length(active_idx) == 0) {
    vm_p95_no_corr[t] <- 0
    next
  }
  
  # Sum of individual P95 P&Ls (no correlation effect)
  sum_p95 <- 0
  for (idx in active_idx) {
    price_p95 <- quantile(price_history[[t]][, idx], probs = 0.95)
    sum_p95 <- sum_p95 + contracts$mwhrs[idx] * (price_p95 - contracts$strike[idx])
  }
  vm_p95_no_corr[t] <- sum_p95
}

# For each margin date, calculate:
# - Variation margin: MTM based on price moves to date (P50 as expected, P95 as stress)
# - 1-month PFE: P95 of the change over next month
# - Total margin: Variation margin + 1-month PFE buffer

pfe_profile <- tibble(
  margin_date = margin_dates,
  n_active_contracts = sapply(margin_dates, function(d) sum(contracts$delivery_end > d)),
  
  # Variation Margin (cumulative MTM to date)
  vm_p50 = apply(mtm_matrix, 2, quantile, probs = 0.50),
  vm_p95 = apply(mtm_matrix, 2, quantile, probs = 0.95),
  
  # No-correlation P95 (sum of individual month P95s)
  vm_p95_no_corr = vm_p95_no_corr,
  
  # Diversification benefit
  diversification_benefit = vm_p95_no_corr - apply(mtm_matrix, 2, quantile, probs = 0.95),
  
  # 1-Month PFE (P95 of change over next month)
  # For margin date t, this is the P95 of change from t to t+1
  one_month_pfe_p95 = c(
    apply(change_matrix[, 2:n_periods, drop = FALSE], 2, quantile, probs = 0.95),
    0  # No forward PFE at last period
  ),
  
  # Total Margin Requirement = Current VM + 1-month buffer
  total_margin_p50 = vm_p50 + one_month_pfe_p95,
  total_margin_p95 = vm_p95 + one_month_pfe_p95,
  
  # Additional stats
  vm_p05 = apply(mtm_matrix, 2, quantile, probs = 0.05),
  vm_mean = apply(mtm_matrix, 2, mean),
  one_month_change_mean = apply(change_matrix, 2, mean),
  one_month_change_sd = apply(change_matrix, 2, sd)
)

cat("\n========================================\n")
cat("PFE MARGIN PROFILE (Monthly)\n")
cat("========================================\n")
cat("\nVariation Margin (VM): MTM based on price moves to date\n")
cat("vm_p95_no_corr: Sum of individual month P95s (no diversification)\n")
cat("diversification_benefit: vm_p95_no_corr - vm_p95\n")
cat("1-Month PFE: P95 of potential adverse move over next month\n")
cat("Total Margin: VM + 1-Month PFE buffer\n\n")

# Display key columns
pfe_profile %>%
  dplyr::select(margin_date, n_active_contracts, vm_p95, vm_p95_no_corr, 
                diversification_benefit, one_month_pfe_p95, total_margin_p95) %>%
  print(n = 50)

# Key summary stats
max_vm_p95 <- max(pfe_profile$vm_p95)
max_vm_date <- pfe_profile$margin_date[which.max(pfe_profile$vm_p95)]
max_total <- max(pfe_profile$total_margin_p95)
max_total_date <- pfe_profile$margin_date[which.max(pfe_profile$total_margin_p95)]

cat("\n========================================\n")
cat("SUMMARY\n")
cat("========================================\n")
cat("Maximum VM P95:", round(max_vm_p95, 0), "at", as.character(max_vm_date), "\n")
cat("Maximum Total Margin P95:", round(max_total, 0), "at", as.character(max_total_date), "\n")
cat("Contracts active at max total:", pfe_profile$n_active_contracts[which.max(pfe_profile$total_margin_p95)], "\n")

# Diversification summary
max_div_benefit <- max(pfe_profile$diversification_benefit)
max_div_date <- pfe_profile$margin_date[which.max(pfe_profile$diversification_benefit)]
avg_div_pct <- mean(pfe_profile$diversification_benefit / pfe_profile$vm_p95_no_corr, na.rm = TRUE) * 100

cat("\nDiversification benefit:\n")
cat("Maximum:", round(max_div_benefit, 0), "at", as.character(max_div_date), "\n")
cat("Average as % of no-corr P95:", round(avg_div_pct, 1), "%\n")

# -----------------------------------------------------------
# 8b. DETAILED BREAKDOWN AT PEAK MARGIN DATE
# -----------------------------------------------------------

# Find the period index for the peak VM P95 date
peak_period <- which(margin_dates == max_vm_date)

cat("\n========================================\n")
cat("DETAILED BREAKDOWN AT PEAK MARGIN DATE:", as.character(max_vm_date), "\n")
cat("========================================\n\n")

# Get active contracts at peak date
peak_active_idx <- which(contracts$delivery_end > max_vm_date)
n_peak_active <- length(peak_active_idx)

# Get the price matrix for active contracts at peak
peak_prices <- price_history[[peak_period]][, peak_active_idx, drop = FALSE]

# Calculate portfolio P&L for each simulation
peak_pnl <- rep(0, n_sims)
for (i in 1:n_peak_active) {
  idx <- peak_active_idx[i]
  peak_pnl <- peak_pnl + contracts$mwhrs[idx] * (peak_prices[, i] - contracts$strike[idx])
}

# Find the simulation path(s) at the P95 level
p95_value <- quantile(peak_pnl, 0.95)
# Get the simulation closest to P95
p95_sim_idx <- which.min(abs(peak_pnl - p95_value))

# For each month, calculate what percentile its price is at in the P95 simulation
prices_at_p95_sim <- peak_prices[p95_sim_idx, ]

# Calculate percentile rank for each month's price
percentile_at_p95 <- numeric(n_peak_active)
for (i in 1:n_peak_active) {
  percentile_at_p95[i] <- mean(peak_prices[, i] <= prices_at_p95_sim[i]) * 100
}

# Calculate price and P&L quantiles for each active contract
peak_detail <- tibble(
  maturity_date = contracts$maturity_date[peak_active_idx],
  mw = contracts$mw[peak_active_idx],
  mwhrs = contracts$mwhrs[peak_active_idx],
  strike = contracts$strike[peak_active_idx],
  price_p05 = apply(peak_prices, 2, quantile, probs = 0.05),
  price_p50 = apply(peak_prices, 2, quantile, probs = 0.50),
  price_p95 = apply(peak_prices, 2, quantile, probs = 0.95),
  price_at_portfolio_p95 = prices_at_p95_sim,
  percentile_at_portfolio_p95 = round(percentile_at_p95, 1),
  pnl_p95 = mwhrs * (price_p95 - strike),
  pnl_at_portfolio_p95 = mwhrs * (prices_at_p95_sim - strike)
)

print(peak_detail, n = 50)

cat("\n--- Summary ---\n")
cat("Total MWh:", round(sum(peak_detail$mwhrs), 0), "\n")
cat("Sum of individual month P95 P&L:", round(sum(peak_detail$pnl_p95), 0), "\n")
cat("Portfolio P95 (with correlation):", round(p95_value, 0), "\n")
cat("Actual P&L in P95 simulation:", round(sum(peak_detail$pnl_at_portfolio_p95), 0), "\n")
cat("Diversification benefit:", round(sum(peak_detail$pnl_p95) - p95_value, 0), "\n")

cat("\nPercentile distribution at portfolio P95:\n")
cat("Min:", min(peak_detail$percentile_at_portfolio_p95), "\n")
cat("Max:", max(peak_detail$percentile_at_portfolio_p95), "\n")
cat("Mean:", round(mean(peak_detail$percentile_at_portfolio_p95), 1), "\n")

# Export peak detail to CSV
write_csv(peak_detail, "C:/Users/AllenFr/OneDrive - SmartestEnergy/Documents/mass_hub_peak_margin_detail.csv")

# -----------------------------------------------------------
# 9. PLOT PFE PROFILE
# -----------------------------------------------------------

p <- ggplot(pfe_profile, aes(x = margin_date)) +
  geom_ribbon(aes(ymin = vm_p05, ymax = vm_p95), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = vm_p50), color = "darkblue", linewidth = 1) +
  geom_line(aes(y = vm_p95), color = "red", linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = total_margin_p95), color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "PFE Margin Profile - Monthly Evaluation",
    subtitle = "Counterparty margin requirement (your MTM gain)",
    x = "Margin Date",
    y = "Mark-to-Market ($)",
    caption = "Shaded: VM P05-P95. Blue = VM Median. Red dashed = VM P95. Dark red = Total Margin P95 (VM + 1-month PFE)."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)

# -----------------------------------------------------------
# 10. FORWARD CURVE PLOTS AT EACH MARGIN DATE
# -----------------------------------------------------------

cat("\nGenerating forward curve plots...\n")

# Function to create forward curve plot for a given margin date
plot_forward_curve <- function(t, margin_date, price_matrix, contracts) {
  
  # Get active contracts for this margin date
  active_idx <- which(contracts$delivery_end > margin_date)
  
  if (length(active_idx) == 0) return(NULL)
  
  # Calculate price quantiles for each active contract
  curve_data <- tibble(
    maturity_date = contracts$maturity_date[active_idx],
    current_atc = contracts$strike[active_idx],
    price_p05 = apply(price_matrix[, active_idx, drop = FALSE], 2, quantile, probs = 0.05),
    price_p25 = apply(price_matrix[, active_idx, drop = FALSE], 2, quantile, probs = 0.25),
    price_p50 = apply(price_matrix[, active_idx, drop = FALSE], 2, quantile, probs = 0.50),
    price_p75 = apply(price_matrix[, active_idx, drop = FALSE], 2, quantile, probs = 0.75),
    price_p95 = apply(price_matrix[, active_idx, drop = FALSE], 2, quantile, probs = 0.95)
  )
  
  p <- ggplot(curve_data, aes(x = maturity_date)) +
    geom_ribbon(aes(ymin = price_p05, ymax = price_p95), fill = "steelblue", alpha = 0.3) +
    geom_ribbon(aes(ymin = price_p25, ymax = price_p75), fill = "steelblue", alpha = 0.5) +
    geom_line(aes(y = price_p50), color = "darkblue", linewidth = 1) +
    geom_line(aes(y = current_atc), color = "black", linewidth = 1, linetype = "dashed") +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste("Forward Curve at", format(margin_date, "%b %d, %Y")),
      subtitle = paste(length(active_idx), "active contracts remaining"),
      x = "Delivery Month",
      y = "Price ($/MWh)",
      caption = "Black dashed = Original deal price (Current ATC). Blue = Simulated P50. Shaded = P05-P95 (light), P25-P75 (dark)."
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Generate and save plots in grids of 6
plot_list <- list()

for (t in 1:n_periods) {
  p <- plot_forward_curve(t, margin_dates[t], price_history[[t]], contracts)
  if (!is.null(p)) {
    plot_list[[length(plot_list) + 1]] <- p
  }
}

# Arrange plots in grids of 6
n_plots <- length(plot_list)
plots_per_grid <- 6
n_grids <- ceiling(n_plots / plots_per_grid)

for (g in 1:n_grids) {
  start_idx <- (g - 1) * plots_per_grid + 1
  end_idx <- min(g * plots_per_grid, n_plots)
  
  grid_plots <- plot_list[start_idx:end_idx]
  
  cat("\nPrinting forward curve grid", g, "of", n_grids, "\n")
  grid_plot <- do.call(grid.arrange, c(grid_plots, ncol = 2))
  # Note: grid.arrange prints automatically
}

cat("\nAll forward curve grids printed.\n")

# Also export forward curve data to CSV
curve_export_list <- list()

for (t in 1:n_periods) {
  margin_date <- margin_dates[t]
  active_idx <- which(contracts$delivery_end > margin_date)
  
  if (length(active_idx) == 0) next
  
  curve_data <- tibble(
    margin_date = margin_date,
    maturity_date = contracts$maturity_date[active_idx],
    current_atc = contracts$strike[active_idx],
    price_p05 = apply(price_history[[t]][, active_idx, drop = FALSE], 2, quantile, probs = 0.05),
    price_p50 = apply(price_history[[t]][, active_idx, drop = FALSE], 2, quantile, probs = 0.50),
    price_p95 = apply(price_history[[t]][, active_idx, drop = FALSE], 2, quantile, probs = 0.95)
  )
  
  curve_export_list[[t]] <- curve_data
}

curve_export <- bind_rows(curve_export_list)
write_csv(curve_export, "C:/Users/AllenFr/OneDrive - SmartestEnergy/Documents/mass_hub_forward_curves_by_margin_date.csv")
cat("Forward curve data exported.\n")

# -----------------------------------------------------------
# 11. EXPORT RESULTS
# -----------------------------------------------------------

write_csv(pfe_profile, "C:/Users/AllenFr/OneDrive - SmartestEnergy/Documents/mass_hub_pfe_margin_profile.csv")

cat("\nResults exported.\n")

# -----------------------------------------------------------
# 12. VOLATILITY MODEL DIAGNOSTIC AT PEAK PFE
# -----------------------------------------------------------

if (cfg$vol_term_structure && exists("fitted_base") && exists("fitted_decay")) {
  
  cat("\n========================================\n")
  cat("VOLATILITY MODEL DIAGNOSTIC\n")
  cat("Peak PFE Date:", as.character(max_vm_date), "\n")
  cat("========================================\n")
  
  # Get active contracts at peak
  peak_active_idx <- which(contracts$delivery_end > max_vm_date)
  
  # Calculate volatilities under both models
  vol_comparison <- tibble(
    maturity_date = contracts$maturity_date[peak_active_idx],
    delivery_month = month(contracts$maturity_date[peak_active_idx]),
    month_name = month.name[delivery_month],
    mwhrs = contracts$mwhrs[peak_active_idx],
    months_to_delivery = interval(max_vm_date, contracts$maturity_date[peak_active_idx]) %/% months(1)
  )
  
  # Pure Calibrated volatility
  vol_comparison <- vol_comparison %>%
    mutate(
      vol_calibrated = fitted_base * exp(-fitted_decay * months_to_delivery)
    )
  
  # Seasonal + Dynamic volatility (if available)
  if (!is.null(cfg$seasonal_baseline)) {
    vol_comparison <- vol_comparison %>%
      left_join(cfg$seasonal_baseline %>% rename(delivery_month = month, seasonal_base = base_vol), 
                by = "delivery_month") %>%
      mutate(
        vol_seasonal_dynamic = seasonal_base * exp(-fitted_decay * months_to_delivery),
        vol_diff = vol_seasonal_dynamic - vol_calibrated,
        vol_ratio = vol_seasonal_dynamic / vol_calibrated
      )
    
    cat("\nVolatility comparison at peak PFE (", nrow(vol_comparison), "active contracts):\n", sep = "")
    vol_comparison %>%
      dplyr::select(maturity_date, month_name, months_to_delivery, mwhrs, 
                    vol_calibrated, vol_seasonal_dynamic, vol_ratio) %>%
      print(n = 50)
    
    # Summary statistics
    cat("\n--- Volatility Summary ---\n")
    cat("Calibrated (Dynamic):\n")
    cat("  Average vol:", round(mean(vol_comparison$vol_calibrated) * 100, 2), "%\n")
    cat("  Weighted avg (by MWh):", round(weighted.mean(vol_comparison$vol_calibrated, vol_comparison$mwhrs) * 100, 2), "%\n")
    cat("  Range:", round(min(vol_comparison$vol_calibrated) * 100, 2), "% to", 
        round(max(vol_comparison$vol_calibrated) * 100, 2), "%\n")
    
    cat("\nSeasonal + Dynamic:\n")
    cat("  Average vol:", round(mean(vol_comparison$vol_seasonal_dynamic) * 100, 2), "%\n")
    cat("  Weighted avg (by MWh):", round(weighted.mean(vol_comparison$vol_seasonal_dynamic, vol_comparison$mwhrs) * 100, 2), "%\n")
    cat("  Range:", round(min(vol_comparison$vol_seasonal_dynamic) * 100, 2), "% to", 
        round(max(vol_comparison$vol_seasonal_dynamic) * 100, 2), "%\n")
    
    # Seasonal loading analysis
    cat("\n--- Seasonal Loading (MWh by Season) ---\n")
    seasonal_loading <- vol_comparison %>%
      mutate(
        season = case_when(
          delivery_month %in% c(12, 1, 2) ~ "Winter (Dec-Feb)",
          delivery_month %in% c(3, 4, 5) ~ "Spring (Mar-May)",
          delivery_month %in% c(6, 7, 8) ~ "Summer (Jun-Aug)",
          delivery_month %in% c(9, 10, 11) ~ "Fall (Sep-Nov)"
        )
      ) %>%
      group_by(season) %>%
      summarise(
        total_mwhrs = sum(mwhrs),
        pct_of_total = sum(mwhrs) / sum(vol_comparison$mwhrs) * 100,
        avg_vol_calibrated = mean(vol_calibrated),
        avg_vol_seasonal = mean(vol_seasonal_dynamic),
        .groups = "drop"
      )
    
    print(seasonal_loading)
    
    # High-volume months analysis
    cat("\n--- High-Volume Months (Jan-May) Analysis ---\n")
    high_vol_months <- vol_comparison %>%
      filter(delivery_month %in% c(1, 2, 3, 4, 5)) %>%
      summarise(
        n_contracts = n(),
        total_mwhrs = sum(mwhrs),
        pct_of_portfolio = sum(mwhrs) / sum(vol_comparison$mwhrs) * 100,
        avg_vol_calibrated = mean(vol_calibrated),
        avg_vol_seasonal = mean(vol_seasonal_dynamic),
        weighted_vol_calibrated = weighted.mean(vol_calibrated, mwhrs),
        weighted_vol_seasonal = weighted.mean(vol_seasonal_dynamic, mwhrs)
      )
    
    cat("Jan-May contracts:", high_vol_months$n_contracts, "of", nrow(vol_comparison), "\n")
    cat("Jan-May MWh:", round(high_vol_months$total_mwhrs, 0), 
        "(", round(high_vol_months$pct_of_portfolio, 1), "% of portfolio)\n")
    cat("Jan-May avg volatility:\n")
    cat("  Calibrated:", round(high_vol_months$avg_vol_calibrated * 100, 2), "%\n")
    cat("  Seasonal+Dynamic:", round(high_vol_months$avg_vol_seasonal * 100, 2), "%\n")
    cat("Jan-May weighted volatility (by MWh):\n")
    cat("  Calibrated:", round(high_vol_months$weighted_vol_calibrated * 100, 2), "%\n")
    cat("  Seasonal+Dynamic:", round(high_vol_months$weighted_vol_seasonal * 100, 2), "%\n")
    
    # Plot volatility comparison
    p_vol_diagnostic <- ggplot(vol_comparison, aes(x = months_to_delivery)) +
      geom_point(aes(y = vol_calibrated * 100, size = mwhrs, color = "Calibrated"), alpha = 0.6) +
      geom_point(aes(y = vol_seasonal_dynamic * 100, size = mwhrs, color = "Seasonal+Dynamic"), alpha = 0.6) +
      scale_color_manual(values = c("Calibrated" = "red", "Seasonal+Dynamic" = "darkgreen")) +
      scale_size_continuous(range = c(2, 10), labels = scales::comma) +
      labs(
        title = paste("Volatility at Peak PFE Date:", max_vm_date),
        subtitle = "Point size = MWh. High volumes concentrated in Jan-May (high-vol months)",
        x = "Months to Delivery",
        y = "Monthly Volatility (%)",
        color = "Model",
        size = "MWh"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    print(p_vol_diagnostic)
    
    cat("\n--- Key Insight ---\n")
    cat("Higher PFE with Seasonal+Dynamic despite lower average volatility likely due to:\n")
    cat("1. High volumes (", round(high_vol_months$pct_of_portfolio, 0), "%) concentrated in Jan-May\n", sep = "")
    cat("2. These high-volume months have higher seasonal base volatilities\n")
    cat("3. At peak PFE, many Jan-May contracts are near delivery (high time-to-delivery multiplier)\n")
    cat("4. Tail risk (P95) is driven by these high-vol × high-volume months\n")
    cat("5. Low-vol summer months don't offset in the tail due to lower volumes\n")
    
  } else {
    cat("\nSeasonal baseline not available for comparison.\n")
  }
}
