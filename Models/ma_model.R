#!/usr/bin/env Rscript
# MA(q) Model Module
# 
# This module implements Moving Average models of order q: MA(q)
# Target: stationary differenced series (returns or differenced returns)
# 
# Model form: y_t = c + ε_t + θ_1*ε_{t-1} + θ_2*ε_{t-2} + ... + θ_q*ε_{t-q}
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

fit_ma <- function(train_series, d = 0, max_q = 8, method = "ML") {
  # Fit MA(q) model with automatic order selection
  # Uses ACF to guide selection and AIC/BIC for final choice
  #
  # Args:
  #   train_series: numeric vector of training data (stationary series)
  #   d: differencing order (0 for stationary series, 1 if differenced)
  #   max_q: maximum MA order to consider
  #   method: fitting method ("ML" for maximum likelihood, "CSS" for conditional sum of squares)
  #
  # Returns:
  #   List with fitted model, selected q, AIC/BIC, and ACF info
  
  if (length(train_series) < max_q + 10) {
    stop("Insufficient data for MA model. Need at least ", max_q + 10, " observations")
  }
  
  # Remove any NA values
  train_series <- train_series[!is.na(train_series)]
  
  message("  Fitting MA models up to order q = ", max_q, " (d = ", d, ")")
  
  # Compute ACF to guide selection
  acf_result <- acf(train_series, plot = FALSE, lag.max = min(max_q, length(train_series) - 1))
  acf_values <- acf_result$acf[, 1, 1]
  acf_lags <- acf_result$lag[, 1, 1]
  
  # Find significant ACF lags (outside 95% confidence interval)
  ci <- 1.96 / sqrt(length(train_series))
  significant_lags <- which(abs(acf_values[-1]) > ci)  # Exclude lag 0
  
  if (length(significant_lags) > 0) {
    suggested_q <- max(significant_lags)
    message("    ACF suggests q ≈ ", suggested_q, " (significant lags: ", 
            paste(significant_lags, collapse = ", "), ")")
  } else {
    suggested_q <- 1
    message("    ACF shows no significant lags, defaulting to q = 1")
  }
  
  # Fit models for different q values
  aic_values <- numeric(max_q + 1)
  bic_values <- numeric(max_q + 1)
  models <- list()
  
  for (q in 0:max_q) {
    tryCatch({
      fit <- Arima(train_series, order = c(0, d, q), method = method)
      aic_values[q + 1] <- AIC(fit)
      bic_values[q + 1] <- BIC(fit)
      models[[q + 1]] <- fit
    }, error = function(e) {
      aic_values[q + 1] <- Inf
      bic_values[q + 1] <- Inf
      models[[q + 1]] <- NULL
    })
  }
  
  # Select best model by BIC (more conservative than AIC)
  best_idx <- which.min(bic_values)
  best_q <- best_idx - 1
  
  if (is.infinite(bic_values[best_idx])) {
    stop("Failed to fit any MA model")
  }
  
  best_model <- models[[best_idx]]
  
  message("    Best MA(q) model: q = ", best_q, 
          " (AIC = ", round(aic_values[best_idx], 2),
          ", BIC = ", round(bic_values[best_idx], 2), ")")
  
  return(list(
    model = best_model,
    q = best_q,
    d = d,
    aic = aic_values[best_idx],
    bic = bic_values[best_idx],
    all_aic = aic_values,
    all_bic = bic_values,
    acf_values = acf_values,
    acf_lags = acf_lags,
    suggested_q = suggested_q
  ))
}

# ============================================================================
# Forecasting
# ============================================================================

forecast_ma <- function(fit, h = 1, new_data = NULL) {
  # Generate 1-step-ahead forecast from MA model
  #
  # Args:
  #   fit: fitted MA model (from fit_ma)
  #   h: forecast horizon (typically 1 for backtesting)
  #   new_data: optional new data (not used for MA, but kept for interface consistency)
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

diagnose_ma <- function(fit, plot_file = NULL) {
  # Perform diagnostic checks on MA model residuals
  #
  # Args:
  #   fit: fitted MA model (from fit_ma)
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
    acf(residuals, main = "ACF of MA Residuals", lag.max = 20)
    
    # PACF of residuals
    pacf(residuals, main = "PACF of MA Residuals", lag.max = 20)
    
    # Q-Q plot
    qqnorm(residuals, main = "Q-Q Plot of MA Residuals")
    qqline(residuals)
    
    # Histogram
    hist(residuals, main = "Histogram of MA Residuals", breaks = 30, freq = FALSE)
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

backtest_ma <- function(series_data, splits, d = 0, max_q = 8) {
  # Perform rolling-origin backtest for MA model
  # Ensures no future data leakage: forecast at t+1 uses only data ≤ t
  #
  # Args:
  #   series_data: data frame with 'date' and 'target_series' columns
  #   splits: time splits from create_time_splits
  #   d: differencing order
  #   max_q: maximum MA order to consider
  #
  # Returns:
  #   Data frame with date, actual, forecast_ma, and error columns
  
  message("Running MA backtest...")
  
  folds <- splits$folds
  n_folds <- length(folds)
  
  results <- list()
  selected_qs <- numeric(n_folds)
  
  for (i in 1:n_folds) {
    fold <- folds[[i]]
    
    # Extract training series (only past data)
    train_series <- fold$train_data$target_series
    
    # Remove NA values
    train_series <- train_series[!is.na(train_series)]
    
    if (length(train_series) < max_q + 10) {
      # Skip if insufficient data
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual = fold$test_data$target_series,
        forecast_ma = NA_real_,
        error = NA_real_,
        q_used = NA_real_
      )
      next
    }
    
    tryCatch({
      # Fit MA model on training data only
      fit_result <- fit_ma(train_series, d = d, max_q = max_q)
      
      # Generate 1-step-ahead forecast
      forecast_val <- forecast_ma(fit_result, h = 1)
      actual_val <- fold$test_data$target_series
      
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual = actual_val,
        forecast_ma = forecast_val,
        error = actual_val - forecast_val,
        q_used = fit_result$q
      )
      
      selected_qs[i] <- fit_result$q
      
    }, error = function(e) {
      message("    Error in fold ", i, ": ", e$message)
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual = fold$test_data$target_series,
        forecast_ma = NA_real_,
        error = NA_real_,
        q_used = NA_real_
      )
    })
    
    if (i %% 50 == 0) {
      message("    Completed ", i, " / ", n_folds, " folds")
    }
  }
  
  result_df <- bind_rows(results) %>%
    filter(!is.na(forecast_ma))  # Remove failed forecasts
  
  message("  MA backtest complete: ", nrow(result_df), " successful forecasts")
  message("  Average q selected: ", round(mean(selected_qs, na.rm = TRUE), 2))
  
  return(result_df)
}

