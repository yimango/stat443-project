#!/usr/bin/env Rscript
# ARIMA(p,d,q) Model Module - Box-Jenkins Methodology
# 
# This module implements ARIMA models following Box-Jenkins methodology
# Target: stationary differenced series (returns or differenced returns)
# 
# Model form: (1 - φ_1*B - ... - φ_p*B^p)(1-B)^d y_t = c + (1 + θ_1*B + ... + θ_q*B^q)ε_t
# where B is the backshift operator, d is differencing order, ε_t ~ white noise
#
# Box-Jenkins steps:
# 1. Identification: Use ACF/PACF to suggest (p,d,q)
# 2. Estimation: Fit models and select by AIC/BIC
# 3. Diagnostics: Check residuals for white noise
#
# Backtesting: Rolling-origin scheme ensures forecasts at time t+1 use only data ≤ t

suppressPackageStartupMessages({
  library(tidyverse)
  library(forecast)
  library(tseries)
})

# ============================================================================
# Series Inspection (Box-Jenkins Step 1: Identification)
# ============================================================================

inspect_series <- function(series, plot_file = NULL) {
  # Inspect series using ACF/PACF and stationarity tests
  # Box-Jenkins identification step
  #
  # Args:
  #   series: numeric vector of time series data
  #   plot_file: optional path to save inspection plots
  #
  # Returns:
  #   List with ACF/PACF results and stationarity test results
  
  series <- series[!is.na(series)]
  
  if (length(series) < 50) {
    stop("Insufficient data for series inspection. Need at least 50 observations")
  }
  
  message("  Inspecting series (Box-Jenkins identification)...")
  
  # Compute ACF and PACF
  acf_result <- acf(series, plot = FALSE, lag.max = min(20, length(series) - 1))
  pacf_result <- pacf(series, plot = FALSE, lag.max = min(20, length(series) - 1))
  
  # Stationarity tests
  adf_test <- tryCatch({
    adf.test(series, alternative = "stationary")
  }, error = function(e) NULL)
  
  kpss_test <- tryCatch({
    kpss.test(series, null = "Trend")
  }, error = function(e) NULL)
  
  # Suggest AR and MA orders based on ACF/PACF
  ci <- 1.96 / sqrt(length(series))
  
  # PACF suggests AR order (cutoff after first significant lag)
  pacf_values <- pacf_result$acf[, 1, 1]
  significant_pacf <- which(abs(pacf_values) > ci)
  suggested_p <- if (length(significant_pacf) > 0) max(significant_pacf) else 0
  
  # ACF suggests MA order (cutoff after first significant lag)
  acf_values <- acf_result$acf[-1, 1, 1]  # Exclude lag 0
  significant_acf <- which(abs(acf_values) > ci)
  suggested_q <- if (length(significant_acf) > 0) max(significant_acf) else 0
  
  message("    Suggested AR order (from PACF): p ≈ ", suggested_p)
  message("    Suggested MA order (from ACF): q ≈ ", suggested_q)
  
  if (!is.null(adf_test)) {
    message("    ADF test p-value: ", round(adf_test$p.value, 4))
  }
  if (!is.null(kpss_test)) {
    message("    KPSS test p-value: ", round(kpss_test$p.value, 4))
  }
  
  # Create inspection plots if file path provided
  if (!is.null(plot_file)) {
    png(plot_file, width = 1200, height = 800)
    par(mfrow = c(2, 2))
    
    # Time series plot
    plot(series, type = "l", main = "Time Series", xlab = "Time", ylab = "Value")
    
    # ACF
    acf(series, main = "ACF", lag.max = 20)
    
    # PACF
    pacf(series, main = "PACF", lag.max = 20)
    
    # Histogram
    hist(series, main = "Distribution", breaks = 30, freq = FALSE)
    
    dev.off()
    message("    Inspection plots saved to ", plot_file)
  }
  
  return(list(
    acf = acf_result,
    pacf = pacf_result,
    adf_test = adf_test,
    kpss_test = kpss_test,
    suggested_p = suggested_p,
    suggested_q = suggested_q
  ))
}

# ============================================================================
# Model Fitting (Box-Jenkins Step 2: Estimation)
# ============================================================================

fit_arima <- function(train_series, d = 0, max_p = 5, max_q = 5, 
                     method = "ML", use_auto = FALSE) {
  # Fit ARIMA(p,d,q) model with automatic order selection
  # Box-Jenkins estimation step
  #
  # Args:
  #   train_series: numeric vector of training data (stationary series)
  #   d: differencing order (from stationarity tests)
  #   max_p: maximum AR order to consider
  #   max_q: maximum MA order to consider
  #   method: fitting method ("ML" for maximum likelihood)
  #   use_auto: if TRUE, also try forecast::auto.arima as sanity check
  #
  # Returns:
  #   List with fitted model, selected (p,d,q), AIC/BIC, and inspection results
  
  if (length(train_series) < max(max_p, max_q) + 10) {
    stop("Insufficient data for ARIMA model")
  }
  
  # Remove any NA values
  train_series <- train_series[!is.na(train_series)]
  
  message("  Fitting ARIMA models (d = ", d, ")...")
  
  # Step 1: Identification (inspect series)
  inspection <- inspect_series(train_series)
  
  # Step 2: Estimation (grid search over p and q)
  message("    Searching over p ∈ [0, ", max_p, "], q ∈ [0, ", max_q, "]")
  
  best_aic <- Inf
  best_bic <- Inf
  best_model <- NULL
  best_p <- 0
  best_q <- 0
  all_results <- list()
  
  for (p in 0:max_p) {
    for (q in 0:max_q) {
      tryCatch({
        fit <- Arima(train_series, order = c(p, d, q), method = method)
        aic_val <- AIC(fit)
        bic_val <- BIC(fit)
        
        all_results[[length(all_results) + 1]] <- list(
          p = p, q = q, aic = aic_val, bic = bic_val, model = fit
        )
        
        # Select by BIC (more conservative)
        if (bic_val < best_bic) {
          best_bic <- bic_val
          best_aic <- aic_val
          best_model <- fit
          best_p <- p
          best_q <- q
        }
      }, error = function(e) {
        # Skip this combination if it fails
      })
    }
  }
  
  if (is.null(best_model)) {
    stop("Failed to fit any ARIMA model")
  }
  
  message("    Best ARIMA(", best_p, ",", d, ",", best_q, ") model:")
  message("      AIC = ", round(best_aic, 2), ", BIC = ", round(best_bic, 2))
  
  # Optional: Compare with auto.arima
  auto_model <- NULL
  if (use_auto) {
    tryCatch({
      auto_fit <- auto.arima(train_series, d = d, max.p = max_p, max.q = max_q,
                            stepwise = FALSE, approximation = FALSE)
      auto_aic <- AIC(auto_fit)
      auto_bic <- BIC(auto_fit)
      message("    auto.arima suggests ARIMA(", auto_fit$arma[1], ",", d, ",", 
              auto_fit$arma[2], ")")
      message("      AIC = ", round(auto_aic, 2), ", BIC = ", round(auto_bic, 2))
      auto_model <- auto_fit
    }, error = function(e) {
      message("    auto.arima failed: ", e$message)
    })
  }
  
  return(list(
    model = best_model,
    p = best_p,
    d = d,
    q = best_q,
    aic = best_aic,
    bic = best_bic,
    all_results = all_results,
    inspection = inspection,
    auto_model = auto_model
  ))
}

# ============================================================================
# Forecasting
# ============================================================================

forecast_arima <- function(fit, h = 1, new_data = NULL) {
  # Generate 1-step-ahead forecast from ARIMA model
  #
  # Args:
  #   fit: fitted ARIMA model (from fit_arima)
  #   h: forecast horizon (typically 1 for backtesting)
  #   new_data: optional new data (not used for ARIMA, but kept for interface consistency)
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
# Diagnostics (Box-Jenkins Step 3: Diagnostic Checking)
# ============================================================================

diagnose_arima <- function(fit, plot_file = NULL) {
  # Perform diagnostic checks on ARIMA model residuals
  # Box-Jenkins diagnostic checking step
  #
  # Args:
  #   fit: fitted ARIMA model (from fit_arima)
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
    acf(residuals, main = "ACF of ARIMA Residuals", lag.max = 20)
    
    # PACF of residuals
    pacf(residuals, main = "PACF of ARIMA Residuals", lag.max = 20)
    
    # Q-Q plot
    qqnorm(residuals, main = "Q-Q Plot of ARIMA Residuals")
    qqline(residuals)
    
    # Histogram
    hist(residuals, main = "Histogram of ARIMA Residuals", breaks = 30, freq = FALSE)
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
  
  # Check if residuals are white noise (p-value > 0.05 suggests white noise)
  if (lb_test$p.value > 0.05) {
    message("    ✓ Residuals appear to be white noise (Ljung-Box p > 0.05)")
  } else {
    message("    ✗ Residuals show autocorrelation (Ljung-Box p < 0.05)")
  }
  
  return(diagnostics)
}

# ============================================================================
# Backtesting
# ============================================================================

backtest_arima <- function(series_data, splits, d = 0, max_p = 5, max_q = 5) {
  # Perform rolling-origin backtest for ARIMA model
  # Ensures no future data leakage: forecast at t+1 uses only data ≤ t
  #
  # Args:
  #   series_data: data frame with 'date' and 'target_series' columns
  #   splits: time splits from create_time_splits
  #   d: differencing order
  #   max_p: maximum AR order to consider
  #   max_q: maximum MA order to consider
  #
  # Returns:
  #   Data frame with date, actual, forecast_arima, and error columns
  
  message("Running ARIMA backtest...")
  
  folds <- splits$folds
  n_folds <- length(folds)
  
  results <- list()
  selected_ps <- numeric(n_folds)
  selected_qs <- numeric(n_folds)
  
  for (i in 1:n_folds) {
    fold <- folds[[i]]
    
    # Extract training series (only past data)
    train_series <- fold$train_data$target_series
    
    # Remove NA values
    train_series <- train_series[!is.na(train_series)]
    
    if (length(train_series) < max(max_p, max_q) + 10) {
      # Skip if insufficient data
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual = fold$test_data$target_series,
        forecast_arima = NA_real_,
        error = NA_real_,
        p_used = NA_real_,
        q_used = NA_real_
      )
      next
    }
    
    tryCatch({
      # Fit ARIMA model on training data only
      fit_result <- fit_arima(train_series, d = d, max_p = max_p, max_q = max_q)
      
      # Generate 1-step-ahead forecast
      forecast_val <- forecast_arima(fit_result, h = 1)
      actual_val <- fold$test_data$target_series
      
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual = actual_val,
        forecast_arima = forecast_val,
        error = actual_val - forecast_val,
        p_used = fit_result$p,
        q_used = fit_result$q
      )
      
      selected_ps[i] <- fit_result$p
      selected_qs[i] <- fit_result$q
      
    }, error = function(e) {
      message("    Error in fold ", i, ": ", e$message)
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual = fold$test_data$target_series,
        forecast_arima = NA_real_,
        error = NA_real_,
        p_used = NA_real_,
        q_used = NA_real_
      )
    })
    
    if (i %% 50 == 0) {
      message("    Completed ", i, " / ", n_folds, " folds")
    }
  }
  
  result_df <- bind_rows(results) %>%
    filter(!is.na(forecast_arima))  # Remove failed forecasts
  
  message("  ARIMA backtest complete: ", nrow(result_df), " successful forecasts")
  message("  Average (p,q) selected: (", round(mean(selected_ps, na.rm = TRUE), 2), 
          ", ", round(mean(selected_qs, na.rm = TRUE), 2), ")")
  
  return(result_df)
}

