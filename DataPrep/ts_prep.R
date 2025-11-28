#!/usr/bin/env Rscript
# Time-Series Data Preparation
# Creates stationary differenced target series and time-safe splits for AR/MA/ARIMA modeling

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(tseries)
})

# Source the data loading function from model_pipeline.R
# Note: Assumes script is run from project root directory
source("DataPrep/model_pipeline.R")

# ============================================================================
# Stationary Target Series Preparation
# ============================================================================

prepare_stationary_target <- function(data, target_col = "spy_close") {
  # Prepare a stationary differenced series for time-series modeling
  # Tests both returns (r_t) and differenced returns (diff_r_t) for stationarity
  # Returns the simplest stationary series
  
  message("Preparing stationary target series...")
  
  if (!target_col %in% names(data)) {
    stop("Target column '", target_col, "' not found in data")
  }
  
  df <- data %>%
    arrange(date) %>%
    filter(!is.na(!!sym(target_col)), !!sym(target_col) > 0) %>%
    mutate(
      # Log prices
      log_price = log(!!sym(target_col)),
      
      # Returns: r_t = log_price_t - log_price_{t-1}
      returns = c(NA, diff(log_price)),
      
      # Differenced returns: diff_r_t = r_t - r_{t-1}
      diff_returns = c(NA, diff(returns))
    ) %>%
    filter(!is.na(returns))  # Remove first row with NA return
  
  # Test stationarity of returns
  returns_clean <- df$returns[!is.na(df$returns)]
  if (length(returns_clean) < 50) {
    stop("Insufficient data for stationarity tests")
  }
  
  message("  Testing stationarity of returns (r_t)...")
  adf_returns <- tryCatch({
    adf.test(returns_clean, alternative = "stationary")
  }, error = function(e) {
    message("    ADF test error: ", e$message)
    NULL
  })
  
  kpss_returns <- tryCatch({
    kpss.test(returns_clean, null = "Trend")
  }, error = function(e) {
    message("    KPSS test error: ", e$message)
    NULL
  })
  
  returns_stationary <- FALSE
  if (!is.null(adf_returns) && !is.null(kpss_returns)) {
    # Returns are stationary if ADF rejects non-stationarity (p < 0.05) 
    # and KPSS does not reject stationarity (p > 0.05)
    returns_stationary <- (adf_returns$p.value < 0.05) && (kpss_returns$p.value > 0.05)
    message("    Returns - ADF p-value: ", round(adf_returns$p.value, 4), 
            ", KPSS p-value: ", round(kpss_returns$p.value, 4))
    message("    Returns stationary: ", returns_stationary)
  }
  
  # Test stationarity of differenced returns if returns are not stationary
  diff_returns_clean <- df$diff_returns[!is.na(df$diff_returns)]
  diff_stationary <- FALSE
  d_used <- 0
  
  if (!returns_stationary && length(diff_returns_clean) >= 50) {
    message("  Testing stationarity of differenced returns (diff_r_t)...")
    adf_diff <- tryCatch({
      adf.test(diff_returns_clean, alternative = "stationary")
    }, error = function(e) {
      message("    ADF test error: ", e$message)
      NULL
    })
    
    kpss_diff <- tryCatch({
      kpss.test(diff_returns_clean, null = "Trend")
    }, error = function(e) {
      message("    KPSS test error: ", e$message)
      NULL
    })
    
    if (!is.null(adf_diff) && !is.null(kpss_diff)) {
      diff_stationary <- (adf_diff$p.value < 0.05) && (kpss_diff$p.value > 0.05)
      message("    Differenced returns - ADF p-value: ", round(adf_diff$p.value, 4),
              ", KPSS p-value: ", round(kpss_diff$p.value, 4))
      message("    Differenced returns stationary: ", diff_stationary)
    }
  }
  
  # Choose the target series
  if (returns_stationary) {
    target_series <- df$returns
    d_used <- 0
    message("\n  Using returns (r_t) as target series (d=0)")
  } else if (diff_stationary) {
    target_series <- df$diff_returns
    d_used <- 1
    message("\n  Using differenced returns (diff_r_t) as target series (d=1)")
  } else {
    # Default to returns even if tests are inconclusive
    target_series <- df$returns
    d_used <- 0
    warning("Stationarity tests inconclusive. Defaulting to returns (d=0)")
    message("\n  Defaulting to returns (r_t) as target series (d=0)")
  }
  
  # Create final data frame with target series
  result_df <- df %>%
    mutate(target_series = target_series) %>%
    filter(!is.na(target_series)) %>%
    select(date, target_series, returns, diff_returns, log_price)
  
  message("  Final target series: ", length(result_df$target_series), " observations")
  message("  Date range: ", min(result_df$date), " to ", max(result_df$date))
  
  return(list(
    data = result_df,
    d = d_used,
    target_name = if (d_used == 0) "returns" else "diff_returns",
    stationarity_tests = list(
      returns = list(adf = adf_returns, kpss = kpss_returns, stationary = returns_stationary),
      diff_returns = list(adf = if(exists("adf_diff")) adf_diff else NULL,
                         kpss = if(exists("kpss_diff")) kpss_diff else NULL,
                         stationary = diff_stationary)
    )
  ))
}

# ============================================================================
# Time-Safe Splits
# ============================================================================

create_time_splits <- function(data, start_date = NULL, end_date = NULL, 
                               holdout_date = NULL, min_train_size = 100) {
  # Create time-safe train/test split and rolling-origin folds for backtesting
  # Ensures no future data leakage
  
  message("Creating time-safe splits...")
  
  # Filter by date range if provided
  if (!is.null(start_date)) {
    data <- data %>% filter(date >= as.Date(start_date))
  }
  if (!is.null(end_date)) {
    data <- data %>% filter(date <= as.Date(end_date))
  }
  
  data <- data %>% arrange(date)
  
  if (nrow(data) < min_train_size + 20) {
    stop("Insufficient data for train/test split. Need at least ", min_train_size + 20, " observations")
  }
  
  # Determine holdout date
  if (is.null(holdout_date)) {
    # Default: use last 20% of data as test
    holdout_idx <- floor(nrow(data) * 0.8)
    holdout_date <- data$date[holdout_idx]
    message("  Auto-selected holdout date: ", holdout_date, " (80/20 split)")
  } else {
    holdout_date <- as.Date(holdout_date)
  }
  
  # Train/test split
  train_data <- data %>% filter(date < holdout_date)
  test_data <- data %>% filter(date >= holdout_date)
  
  message("  Training: ", nrow(train_data), " obs (", 
          min(train_data$date), " to ", max(train_data$date), ")")
  message("  Test: ", nrow(test_data), " obs (", 
          min(test_data$date), " to ", max(test_data$date), ")")
  
  # Create rolling-origin folds for backtesting
  # Each fold: expanding training window, 1-step-ahead forecast
  n_test <- nrow(test_data)
  folds <- list()
  
  for (i in 1:n_test) {
    # Training data: all data before this test point
    if (i == 1) {
      train_subset <- train_data
    } else {
      # Include previous test points in training for expanding window
      train_subset <- bind_rows(train_data, test_data[1:(i-1), ])
    }
    
    # Test point
    test_point <- test_data[i, ]
    
    folds[[i]] <- list(
      train_data = train_subset,
      test_data = test_point,
      test_idx = i,
      test_date = test_point$date
    )
  }
  
  message("  Created ", length(folds), " rolling-origin folds for backtesting")
  
  return(list(
    train_data = train_data,
    test_data = test_data,
    folds = folds,
    holdout_date = holdout_date
  ))
}

# ============================================================================
# Main Preparation Function
# ============================================================================

prepare_ts_data <- function(file_path = NULL, start_date = NULL, end_date = NULL,
                            holdout_date = NULL, target_col = "spy_close") {
  # Main function to load data and prepare stationary target with time-safe splits
  
  # Load data
  raw_data <- load_data(file_path)
  
  # Prepare stationary target
  prep_result <- prepare_stationary_target(raw_data, target_col = target_col)
  
  # Create time-safe splits
  splits <- create_time_splits(
    prep_result$data,
    start_date = start_date,
    end_date = end_date,
    holdout_date = holdout_date
  )
  
  return(list(
    data = prep_result$data,
    d = prep_result$d,
    target_name = prep_result$target_name,
    stationarity_tests = prep_result$stationarity_tests,
    splits = splits
  ))
}

