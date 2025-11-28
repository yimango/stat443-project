#!/usr/bin/env Rscript
# AR(p) Model Module
# 
# This module implements Autoregressive models of order p: AR(p)
# Target: stationary differenced series (returns or differenced returns)
# 
# Model form: y_t = c + φ_1*y_{t-1} + φ_2*y_{t-2} + ... + φ_p*y_{t-p} + ε_t
# where ε_t ~ white noise
#
# Backtesting: Rolling-origin scheme ensures forecasts at time t+1 use only data ≤ t

suppressPackageStartupMessages({
  library(tidyverse)
  library(forecast)
  library(tseries)
})

# ============================================================================
# Model Fitting
# ============================================================================

fit_ar <- function(train_series, max_p = 8, method = "ML") {
  # Fit AR(p) model with automatic order selection
  # Uses PACF to guide selection and AIC/BIC for final choice
  #
  # Args:
  #   train_series: numeric vector of training data (stationary series)
  #   max_p: maximum AR order to consider
  #   method: fitting method ("ML" for maximum likelihood, "CSS" for conditional sum of squares)
  #
  # Returns:
  #   List with fitted model, selected p, AIC/BIC, and PACF info
  
  if (length(train_series) < max_p + 10) {
    stop("Insufficient data for AR model. Need at least ", max_p + 10, " observations")
  }
  
  # Remove any NA values
  train_series <- train_series[!is.na(train_series)]
  
  message("  Fitting AR models up to order p = ", max_p)
  
  # Compute PACF to guide selection
  pacf_result <- pacf(train_series, plot = FALSE, lag.max = min(max_p, length(train_series) - 1))
  pacf_values <- pacf_result$acf[, 1, 1]
  pacf_lags <- pacf_result$lag[, 1, 1]
  
  # Find significant PACF lags (outside 95% confidence interval)
  ci <- 1.96 / sqrt(length(train_series))
  significant_lags <- which(abs(pacf_values) > ci)
  
  if (length(significant_lags) > 0) {
    suggested_p <- max(significant_lags)
    message("    PACF suggests p ≈ ", suggested_p, " (significant lags: ", 
            paste(significant_lags, collapse = ", "), ")")
  } else {
    suggested_p <- 1
    message("    PACF shows no significant lags, defaulting to p = 1")
  }
  
  # Fit models for different p values
  aic_values <- numeric(max_p + 1)
  bic_values <- numeric(max_p + 1)
  models <- list()
  
  for (p in 0:max_p) {
    tryCatch({
      fit <- Arima(train_series, order = c(p, 0, 0), method = method)
      aic_values[p + 1] <- AIC(fit)
      bic_values[p + 1] <- BIC(fit)
      models[[p + 1]] <- fit
    }, error = function(e) {
      aic_values[p + 1] <- Inf
      bic_values[p + 1] <- Inf
      models[[p + 1]] <- NULL
    })
  }
  
  # Select best model by BIC (more conservative than AIC)
  best_idx <- which.min(bic_values)
  best_p <- best_idx - 1
  
  if (is.infinite(bic_values[best_idx])) {
    stop("Failed to fit any AR model")
  }
  
  best_model <- models[[best_idx]]
  
  message("    Best AR(p) model: p = ", best_p, 
          " (AIC = ", round(aic_values[best_idx], 2),
          ", BIC = ", round(bic_values[best_idx], 2), ")")
  
  return(list(
    model = best_model,
    p = best_p,
    aic = aic_values[best_idx],
    bic = bic_values[best_idx],
    all_aic = aic_values,
    all_bic = bic_values,
    pacf_values = pacf_values,
    pacf_lags = pacf_lags,
    suggested_p = suggested_p
  ))
}

# ============================================================================
# Forecasting
# ============================================================================

forecast_ar <- function(fit, h = 1, new_data = NULL) {
  # Generate 1-step-ahead forecast from AR model
  #
  # Args:
  #   fit: fitted AR model (from fit_ar)
  #   h: forecast horizon (typically 1 for backtesting)
  #   new_data: optional new data (not used for AR, but kept for interface consistency)
  #
  # Returns:
  #   Forecast value(s)
  
  if (is.null(fit$model)) {
    stop("Invalid model object")
  }
  
  fc <- forecast(fit$model, h = h)
  return(as.numeric(fc$mean[1]))  # Return 1-step-ahead forecast
}

# ============================================================================
# Diagnostics
# ============================================================================

diagnose_ar <- function(fit, plot_file = NULL) {
  # Perform diagnostic checks on AR model residuals
  #
  # Args:
  #   fit: fitted AR model (from fit_ar)
  #   plot_file: optional path to save diagnostic plots
  #
  # Returns:
  #   List with diagnostic statistics and plots
  
  if (is.null(fit$model)) {
    stop("Invalid model object")
  }
  
  residuals <- residuals(fit$model)
  
  # Ljung-Box test for residual autocorrelation
  lb_test <- Box.test(residuals, lag = 10, type = "Ljung-Box")
  
  # Jarque-Bera test for normality
  jb_test <- tryCatch({
    jarque.bera.test(residuals)
  }, error = function(e) {
    NULL
  })
  
  # ACF and PACF of residuals
  acf_resid <- acf(residuals, plot = FALSE, lag.max = 20)
  pacf_resid <- pacf(residuals, plot = FALSE, lag.max = 20)
  
  diagnostics <- list(
    ljung_box = lb_test,
    jarque_bera = jb_test,
    acf_residuals = acf_resid,
    pacf_residuals = pacf_resid,
    residual_mean = mean(residuals, na.rm = TRUE),
    residual_sd = sd(residuals, na.rm = TRUE)
  )
  
  # Create diagnostic plots if file path provided
  if (!is.null(plot_file)) {
    png(plot_file, width = 1200, height = 800)
    par(mfrow = c(2, 2))
    
    # ACF of residuals
    acf(residuals, main = "ACF of AR Residuals", lag.max = 20)
    
    # PACF of residuals
    pacf(residuals, main = "PACF of AR Residuals", lag.max = 20)
    
    # Q-Q plot
    qqnorm(residuals, main = "Q-Q Plot of AR Residuals")
    qqline(residuals)
    
    # Histogram
    hist(residuals, main = "Histogram of AR Residuals", breaks = 30, freq = FALSE)
    curve(dnorm(x, mean = mean(residuals, na.rm = TRUE), 
                sd = sd(residuals, na.rm = TRUE)), 
          add = TRUE, col = "red", lwd = 2)
    
    dev.off()
    message("    Diagnostic plots saved to ", plot_file)
  }
  
  message("    Ljung-Box test p-value: ", round(lb_test$p.value, 4))
  if (!is.null(jb_test)) {
    message("    Jarque-Bera test p-value: ", round(jb_test$p.value, 4))
  }
  
  return(diagnostics)
}

# ============================================================================
# Backtesting
# ============================================================================

backtest_ar <- function(series_data, splits, max_p = 8) {
  # Perform rolling-origin backtest for AR model
  # Ensures no future data leakage: forecast at t+1 uses only data ≤ t
  #
  # Args:
  #   series_data: data frame with 'date' and 'target_series' columns
  #   splits: time splits from create_time_splits
  #   max_p: maximum AR order to consider
  #
  # Returns:
  #   Data frame with date, actual, forecast_ar, and error columns
  
  message("Running AR backtest...")
  
  folds <- splits$folds
  n_folds <- length(folds)
  
  results <- list()
  selected_ps <- numeric(n_folds)
  
  for (i in 1:n_folds) {
    fold <- folds[[i]]
    
    # Extract training series (only past data)
    train_series <- fold$train_data$target_series
    
    # Remove NA values
    train_series <- train_series[!is.na(train_series)]
    
    if (length(train_series) < max_p + 10) {
      # Skip if insufficient data
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual = fold$test_data$target_series,
        forecast_ar = NA_real_,
        error = NA_real_,
        p_used = NA_real_
      )
      next
    }
    
    tryCatch({
      # Fit AR model on training data only
      fit_result <- fit_ar(train_series, max_p = max_p)
      
      # Generate 1-step-ahead forecast
      forecast_val <- forecast_ar(fit_result, h = 1)
      actual_val <- fold$test_data$target_series
      
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual = actual_val,
        forecast_ar = forecast_val,
        error = actual_val - forecast_val,
        p_used = fit_result$p
      )
      
      selected_ps[i] <- fit_result$p
      
    }, error = function(e) {
      message("    Error in fold ", i, ": ", e$message)
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual = fold$test_data$target_series,
        forecast_ar = NA_real_,
        error = NA_real_,
        p_used = NA_real_
      )
    })
    
    if (i %% 50 == 0) {
      message("    Completed ", i, " / ", n_folds, " folds")
    }
  }
  
  result_df <- bind_rows(results) %>%
    filter(!is.na(forecast_ar))  # Remove failed forecasts
  
  message("  AR backtest complete: ", nrow(result_df), " successful forecasts")
  message("  Average p selected: ", round(mean(selected_ps, na.rm = TRUE), 2))
  
  return(result_df)
}

