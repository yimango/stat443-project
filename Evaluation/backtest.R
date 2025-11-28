#!/usr/bin/env Rscript
# Generic Backtesting Utilities
# 
# Provides generic rolling-origin backtesting framework that ensures no future data leakage
# Can be used with any model that provides fit_fun and forecast_fun interfaces

suppressPackageStartupMessages({
  library(tidyverse)
})

# ============================================================================
# Generic Backtesting Driver
# ============================================================================

generic_backtest <- function(series_data, splits, fit_fun, forecast_fun, 
                             model_name = "model", ...) {
  # Generic rolling-origin backtesting function
  # 
  # Args:
  #   series_data: data frame with 'date' and 'target_series' columns
  #   splits: time splits from create_time_splits
  #   fit_fun: function(train_series, ...) that returns a fitted model object
  #   forecast_fun: function(fit, h=1, ...) that returns forecast value(s)
  #   model_name: name of the model (for labeling results)
  #   ...: additional arguments to pass to fit_fun
  #
  # Returns:
  #   Data frame with date, actual, forecast, error, and model_name columns
  
  message("Running backtest for ", model_name, "...")
  
  folds <- splits$folds
  n_folds <- length(folds)
  
  results <- list()
  
  for (i in 1:n_folds) {
    fold <- folds[[i]]
    
    # Extract training series (only past data - no future leakage)
    train_series <- fold$train_data$target_series
    train_series <- train_series[!is.na(train_series)]
    
    if (length(train_series) < 10) {
      # Skip if insufficient data
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual = fold$test_data$target_series,
        forecast = NA_real_,
        error = NA_real_,
        model_name = model_name
      )
      next
    }
    
    tryCatch({
      # Fit model on training data only (no future data)
      fit <- fit_fun(train_series, ...)
      
      # Generate 1-step-ahead forecast
      forecast_val <- forecast_fun(fit, h = 1)
      actual_val <- fold$test_data$target_series
      
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual = actual_val,
        forecast = forecast_val,
        error = actual_val - forecast_val,
        model_name = model_name
      )
      
    }, error = function(e) {
      # Skip this fold if fitting/forecasting fails
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual = fold$test_data$target_series,
        forecast = NA_real_,
        error = NA_real_,
        model_name = model_name
      )
    })
    
    if (i %% 50 == 0 && n_folds > 50) {
      message("    Completed ", i, " / ", n_folds, " folds")
    }
  }
  
  result_df <- bind_rows(results) %>%
    filter(!is.na(forecast))  # Remove failed forecasts
  
  message("  ", model_name, " backtest complete: ", nrow(result_df), " successful forecasts")
  
  return(result_df)
}

# ============================================================================
# Model-Specific Wrappers
# ============================================================================

run_backtest_ar <- function(series_data, splits, max_p = 8) {
  # Wrapper for AR model backtesting
  source("Models/ar_model.R", local = TRUE)
  
  fit_fun <- function(train_series, max_p) {
    fit_ar(train_series, max_p = max_p)
  }
  
  forecast_fun <- function(fit, h = 1) {
    forecast_ar(fit, h = h)
  }
  
  return(generic_backtest(series_data, splits, fit_fun, forecast_fun, 
                         model_name = "AR", max_p = max_p))
}

run_backtest_ma <- function(series_data, splits, d = 0, max_q = 8) {
  # Wrapper for MA model backtesting
  source("Models/ma_model.R", local = TRUE)
  
  fit_fun <- function(train_series, d, max_q) {
    fit_ma(train_series, d = d, max_q = max_q)
  }
  
  forecast_fun <- function(fit, h = 1) {
    forecast_ma(fit, h = h)
  }
  
  return(generic_backtest(series_data, splits, fit_fun, forecast_fun, 
                         model_name = "MA", d = d, max_q = max_q))
}

run_backtest_arima <- function(series_data, splits, d = 0, max_p = 5, max_q = 5) {
  # Wrapper for ARIMA model backtesting
  source("Models/arima_model.R", local = TRUE)
  
  fit_fun <- function(train_series, d, max_p, max_q) {
    fit_arima(train_series, d = d, max_p = max_p, max_q = max_q)
  }
  
  forecast_fun <- function(fit, h = 1) {
    forecast_arima(fit, h = h)
  }
  
  return(generic_backtest(series_data, splits, fit_fun, forecast_fun, 
                         model_name = "ARIMA", d = d, max_p = max_p, max_q = max_q))
}

# ============================================================================
# Direct Model Backtest Functions (Alternative Interface)
# ============================================================================

# These functions call the model-specific backtest functions directly
# They're provided as an alternative to the generic interface

backtest_all_models <- function(series_data, splits, d = 0, 
                               ar_max_p = 8, ma_max_q = 8, 
                               arima_max_p = 5, arima_max_q = 5) {
  # Run backtests for all models (AR, MA, ARIMA)
  #
  # Returns:
  #   Combined data frame with results from all models
  
  message("Running backtests for all models...")
  
  # Source model files
  source("Models/ar_model.R", local = TRUE)
  source("Models/ma_model.R", local = TRUE)
  source("Models/arima_model.R", local = TRUE)
  
  # Run backtests
  results_ar <- backtest_ar(series_data, splits, max_p = ar_max_p)
  results_ma <- backtest_ma(series_data, splits, d = d, max_q = ma_max_q)
  results_arima <- backtest_arima(series_data, splits, d = d, 
                                  max_p = arima_max_p, max_q = arima_max_q)
  
  # Combine results
  combined <- bind_rows(
    results_ar %>% mutate(model_name = "AR"),
    results_ma %>% mutate(model_name = "MA"),
    results_arima %>% mutate(model_name = "ARIMA")
  )
  
  message("All backtests complete")
  
  return(combined)
}

