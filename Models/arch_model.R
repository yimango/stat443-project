#!/usr/bin/env Rscript
# ARCH Model Module - Volatility Forecasting
# 
# This module implements ARCH (Autoregressive Conditional Heteroskedasticity) models
# Focus: Forecasting volatility (conditional variance) of returns
# 
# ARCH is a special case of GARCH: ARCH(q) = GARCH(0,q)
# Model form: 
#   r_t = μ + ε_t, where ε_t = σ_t * z_t, z_t ~ i.i.d.(0,1)
#   σ²_t = ω + Σ(α_i * ε²_{t-i})
# 
# Backtesting: Rolling-origin scheme ensures volatility forecasts at time t+1 use only data ≤ t

suppressPackageStartupMessages({
  library(tidyverse)
  library(rugarch)
  library(tseries)
})

# ============================================================================
# Model Fitting
# ============================================================================

fit_arch <- function(train_series, max_q = 5, distribution = "norm") {
  # Fit ARCH(q) model with automatic order selection
  # ARCH(q) is equivalent to GARCH(0,q)
  # Focuses on volatility modeling
  #
  # Args:
  #   train_series: numeric vector of returns (stationary series)
  #   max_q: maximum ARCH order to consider
  #   distribution: error distribution ("norm", "std" for t-distribution, "ged" for GED)
  #
  # Returns:
  #   List with fitted model, selected q, AIC/BIC, and volatility estimates
  
  if (length(train_series) < 100) {
    stop("Insufficient data for ARCH model. Need at least 100 observations")
  }
  
  # Remove any NA values
  train_series <- train_series[!is.na(train_series)]
  
  message("  Fitting ARCH models (max order: q=", max_q, ")...")
  
  # Grid search over ARCH orders (GARCH(0,q))
  best_aic <- Inf
  best_bic <- Inf
  best_model <- NULL
  best_q <- 1
  all_results <- list()
  
  for (q in 1:max_q) {
    tryCatch({
      # Specify ARCH model (GARCH(0,q))
      spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(0, q)),
        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
        distribution.model = distribution
      )
      
      # Fit model
      fit <- ugarchfit(spec, train_series, solver = "hybrid", solver.control = list(trace = 0))
      
      # Check convergence
      if (fit@fit$convergence == 0) {
        aic_val <- infocriteria(fit)[1]  # AIC
        bic_val <- infocriteria(fit)[2]  # BIC
        
        all_results[[length(all_results) + 1]] <- list(
          q = q, aic = aic_val, bic = bic_val, model = fit
        )
        
        # Select by BIC (more conservative)
        if (bic_val < best_bic) {
          best_bic <- bic_val
          best_aic <- aic_val
          best_model <- fit
          best_q <- q
        }
      }
    }, error = function(e) {
      # Skip this order if it fails
    })
  }
  
  if (is.null(best_model)) {
    stop("Failed to fit any ARCH model")
  }
  
  message("    Best ARCH(", best_q, ") model:")
  message("      AIC = ", round(best_aic, 2), ", BIC = ", round(best_bic, 2))
  
  # Extract fitted volatility
  fitted_volatility <- as.numeric(sigma(best_model))
  
  return(list(
    model = best_model,
    q = best_q,
    aic = best_aic,
    bic = best_bic,
    fitted_volatility = fitted_volatility,
    distribution = distribution
  ))
}

# ============================================================================
# Volatility Forecasting
# ============================================================================

forecast_arch_volatility <- function(fit, h = 1, n.ahead = NULL) {
  # Generate volatility forecasts from ARCH model
  #
  # Args:
  #   fit: fitted ARCH model (from fit_arch)
  #   h: forecast horizon (number of steps ahead)
  #   n.ahead: alias for h (for consistency)
  #
  # Returns:
  #   Volatility forecast(s) - conditional standard deviation
  
  if (is.null(n.ahead)) {
    n.ahead <- h
  }
  
  if (is.null(fit$model)) {
    stop("Invalid ARCH model object")
  }
  
  # Generate forecast
  fc <- ugarchforecast(fit$model, n.ahead = n.ahead)
  
  # Extract volatility forecasts (sigma)
  volatility_forecast <- as.numeric(sigma(fc))
  
  return(volatility_forecast)
}

# ============================================================================
# Diagnostics
# ============================================================================

diagnose_arch <- function(fit, plot_file = NULL) {
  # Perform diagnostic checks on ARCH model
  #
  # Args:
  #   fit: fitted ARCH model (from fit_arch)
  #   plot_file: optional path to save diagnostic plots
  #
  # Returns:
  #   List with diagnostic test results
  
  if (is.null(fit$model)) {
    stop("Invalid ARCH model object")
  }
  
  # Extract standardized residuals
  residuals_std <- residuals(fit$model, standardize = TRUE)
  residuals_std <- as.numeric(residuals_std)
  residuals_std <- residuals_std[!is.na(residuals_std)]
  
  # Ljung-Box test on standardized residuals
  lb_test <- Box.test(residuals_std, lag = 10, type = "Ljung-Box")
  
  # Ljung-Box test on squared standardized residuals (check for remaining ARCH effects)
  lb_squared <- Box.test(residuals_std^2, lag = 10, type = "Ljung-Box")
  
  # Jarque-Bera test for normality
  jb_test <- jarque.bera.test(residuals_std)
  
  message("    Ljung-Box test (residuals) p-value: ", round(lb_test$p.value, 4))
  message("    Ljung-Box test (squared residuals) p-value: ", round(lb_squared$p.value, 4))
  message("    Jarque-Bera test p-value: ", round(jb_test$p.value, 4))
  
  # Create diagnostic plots
  if (!is.null(plot_file)) {
    png(plot_file, width = 1400, height = 1000)
    par(mfrow = c(3, 2))
    
    # Standardized residuals
    plot(residuals_std, type = "l", main = "Standardized Residuals", 
         xlab = "Time", ylab = "Residuals")
    abline(h = 0, col = "red", lty = 2)
    
    # ACF of standardized residuals
    acf(residuals_std, main = "ACF of Standardized Residuals", lag.max = 20)
    
    # ACF of squared standardized residuals
    acf(residuals_std^2, main = "ACF of Squared Standardized Residuals", lag.max = 20)
    
    # Q-Q plot
    qqnorm(residuals_std, main = "Q-Q Plot of Standardized Residuals")
    qqline(residuals_std, col = "red")
    
    # Histogram
    hist(residuals_std, breaks = 30, freq = FALSE, 
         main = "Distribution of Standardized Residuals", xlab = "Residuals")
    curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lwd = 2)
    
    # Fitted volatility
    fitted_vol <- fit$fitted_volatility
    plot(fitted_vol, type = "l", main = "Fitted Conditional Volatility", 
         xlab = "Time", ylab = "Volatility")
    
    dev.off()
    message("    Diagnostic plots saved to ", plot_file)
  }
  
  # Check if model is adequate
  adequate <- (lb_test$p.value > 0.05) && (lb_squared$p.value > 0.05)
  
  if (adequate) {
    message("    ✓ Model appears adequate (residuals are white noise)")
  } else {
    if (lb_test$p.value <= 0.05) {
      message("    ✗ Residuals show autocorrelation (Ljung-Box p < 0.05)")
    }
    if (lb_squared$p.value <= 0.05) {
      message("    ✗ Squared residuals show autocorrelation (remaining ARCH effects)")
    }
  }
  
  return(list(
    lb_residuals = lb_test,
    lb_squared = lb_squared,
    jb_test = jb_test,
    adequate = adequate
  ))
}

# ============================================================================
# Backtesting Function
# ============================================================================

backtest_arch <- function(series_data, splits, max_q = 5) {
  # Rolling-origin backtesting for ARCH model
  # Forecasts volatility (not returns)
  #
  # Args:
  #   series_data: data frame with 'date' and 'target_series' columns
  #   splits: time splits from create_time_splits
  #   max_q: maximum ARCH order
  #
  # Returns:
  #   Data frame with date, actual_volatility, forecast_volatility, error, model_name
  
  source("Models/arch_model.R", local = TRUE)
  
  folds <- splits$folds
  n_folds <- length(folds)
  
  results <- list()
  
  message("Running ARCH volatility backtest...")
  
  for (i in 1:n_folds) {
    fold <- folds[[i]]
    
    # Extract training series
    train_series <- fold$train_data$target_series
    train_series <- train_series[!is.na(train_series)]
    
    if (length(train_series) < 100) {
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual_volatility = NA_real_,
        forecast_volatility = NA_real_,
        error = NA_real_,
        model_name = "ARCH"
      )
      next
    }
    
    tryCatch({
      # Fit ARCH model on training data
      fit_result <- fit_arch(train_series, max_q = max_q)
      
      # Generate 1-step-ahead volatility forecast
      vol_forecast <- forecast_arch_volatility(fit_result, h = 1)
      
      # Actual volatility (use absolute return as proxy)
      actual_return <- fold$test_data$target_series
      actual_vol <- abs(actual_return)
      
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual_volatility = actual_vol,
        forecast_volatility = vol_forecast[1],
        error = actual_vol - vol_forecast[1],
        model_name = "ARCH"
      )
      
    }, error = function(e) {
      message("    Error in fold ", i, ": ", e$message)
      results[[i]] <- tibble(
        date = fold$test_data$date,
        actual_volatility = NA_real_,
        forecast_volatility = NA_real_,
        error = NA_real_,
        model_name = "ARCH"
      )
    })
    
    if (i %% 50 == 0) {
      message("    Completed ", i, " / ", n_folds, " folds")
    }
  }
  
  result_df <- bind_rows(results) %>%
    filter(!is.na(forecast_volatility))
  
  message("  ARCH backtest complete: ", nrow(result_df), " successful forecasts")
  
  return(result_df)
}

