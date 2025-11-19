#!/usr/bin/env Rscript
# SPY Forecasting Pipeline with Exogenous Market Features
# Leakage-safe implementation using CRAN packages only

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(glmnet)
  library(forecast)
  library(KFAS)
  library(tseries)
  library(yardstick)
  library(patchwork)
})

# ============================================================================
# 1) Config & IO
# ============================================================================

load_data <- function(file_path = NULL) {
  # Load merged CSV with market data, or load individual files from r_ready_data/
  # Expected columns: date, spy_close, spy_open, spy_high, spy_low, spy_volume,
  #                  vix_close, ux1_close, us10y_yield, hy_spread
  
  # If file_path is provided and exists, use it
  if (!is.null(file_path) && file.exists(file_path)) {
    message("Loading data from: ", file_path)
    data <- read_csv(file_path, show_col_types = FALSE) %>%
      mutate(date = as.Date(date)) %>%
      arrange(date) %>%
      mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))
    
    message("Loaded ", nrow(data), " rows from ", min(data$date), " to ", max(data$date))
    return(data)
  }
  
  # Otherwise, load and merge individual files from r_ready_data/
  message("Loading individual data files from r_ready_data/ directory...")
  
  # Try multiple possible paths (current dir, parent dir, etc.)
  possible_dirs <- c(
    "r_ready_data",
    "../r_ready_data",
    file.path(getwd(), "r_ready_data"),
    file.path(dirname(getwd()), "r_ready_data")
  )
  
  data_dir <- NULL
  for (dir in possible_dirs) {
    if (dir.exists(dir)) {
      data_dir <- dir
      break
    }
  }
  
  if (is.null(data_dir)) {
    stop("Data directory not found. Tried: ", paste(possible_dirs, collapse = ", "),
         "\nPlease provide a valid file_path or ensure r_ready_data/ exists")
  }
  
  message("  Using data directory: ", normalizePath(data_dir))
  
  # Define file mappings
  file_mappings <- list(
    "SPY_US_Equity.csv" = list(
      date_col = "Dates",
      mappings = c(
        "spy_close" = "PX_LAST",
        "spy_open" = "OPEN",
        "spy_high" = "PX_HIGH",
        "spy_low" = "PX_LOW",
        "spy_volume" = "VOLUME"
      )
    ),
    "VIX_Index.csv" = list(
      date_col = "Dates",
      mappings = c("vix_close" = "PX_LAST")
    ),
    "UX1_Index.csv" = list(
      date_col = "Dates",
      mappings = c("ux1_close" = "PX_LAST")
    ),
    "USGG10YR_Index.csv" = list(
      date_col = "Dates",
      mappings = c("us10y_yield" = "PX_LAST")
    ),
    "USOHHYTO_Index.csv" = list(
      date_col = "Dates",
      mappings = c("hy_spread" = "PX_LAST")
    )
  )
  
  # Load and merge all files
  merged_data <- NULL
  
  for (filename in names(file_mappings)) {
    file_path_full <- file.path(data_dir, filename)
    
    if (!file.exists(file_path_full)) {
      warning("File not found: ", file_path_full, " - skipping")
      next
    }
    
    message("  Loading ", filename)
    mapping <- file_mappings[[filename]]
    
    # Read the file
    df <- read_csv(file_path_full, show_col_types = FALSE) %>%
      mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))
    
    # Rename date column
    if (mapping$date_col %in% names(df)) {
      df <- df %>% rename(date = !!mapping$date_col)
      df$date <- as.Date(df$date)
    } else {
      warning("Date column '", mapping$date_col, "' not found in ", filename)
      next
    }
    
    # Select and rename columns according to mapping
    cols_to_select <- c("date", mapping$mappings)
    available_cols <- cols_to_select[cols_to_select %in% names(df)]
    
    if (length(available_cols) == 0) {
      warning("No mappable columns found in ", filename)
      next
    }
    
    df_selected <- df %>% select(all_of(available_cols))
    
    # Rename columns
    for (new_name in names(mapping$mappings)) {
      old_name <- mapping$mappings[new_name]
      if (old_name %in% names(df_selected)) {
        df_selected <- df_selected %>% rename(!!new_name := !!old_name)
      }
    }
    
    # Merge with existing data
    if (is.null(merged_data)) {
      merged_data <- df_selected
    } else {
      merged_data <- merged_data %>%
        full_join(df_selected, by = "date")
    }
  }
  
  if (is.null(merged_data)) {
    stop("No data files could be loaded from ", data_dir)
  }
  
  # Sort by date and remove duplicates
  merged_data <- merged_data %>%
    arrange(date) %>%
    distinct(date, .keep_all = TRUE)
  
  message("Loaded and merged ", nrow(merged_data), " rows from ", 
          min(merged_data$date, na.rm = TRUE), " to ", 
          max(merged_data$date, na.rm = TRUE))
  
  # Check which columns are present
  expected_cols <- c("date", "spy_close", "spy_open", "spy_high", "spy_low", 
                     "spy_volume", "vix_close", "ux1_close", "us10y_yield", "hy_spread")
  missing_cols <- setdiff(expected_cols, names(merged_data))
  if (length(missing_cols) > 0) {
    warning("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  return(merged_data)
}

make_weekly <- function(data) {
  # Convert daily data to weekly (non-overlapping Fri->Fri or last trading day).
  message("Converting to weekly frequency...")
  
  # Create week identifier (Friday as week end)
  data_weekly <- data %>%
    mutate(week_end = floor_date(date, unit = "week", week_start = 6)) %>%
    group_by(week_end) %>%
    # Use last trading day's values for levels
    summarise(
      date = max(date),
      spy_close = last(na.omit(spy_close)),
      spy_open = first(na.omit(spy_open)),
      spy_high = max(na.omit(spy_high)),
      spy_low = min(na.omit(spy_low)),
      spy_volume = mean(na.omit(spy_volume)),
      vix_close = last(na.omit(vix_close)),
      ux1_close = last(na.omit(ux1_close)),
      us10y_yield = last(na.omit(us10y_yield)),
      hy_spread = last(na.omit(hy_spread)),
      .groups = "drop"
    ) %>%
    arrange(week_end) %>%
    select(-week_end)
  
  message("Weekly data: ", nrow(data_weekly), " weeks")
  return(data_weekly)
}

# ============================================================================
# 2) Feature Engineering (No Leakage)
# ============================================================================

engineer_features <- function(data) {
  # Create leakage-safe features. All predictors at time t use only info up to t.
  # Target: R_{t+1} = diff(log(spy_close)) aligned so predictors at t forecast t+1.
  message("Engineering features...")
  
  # Check which columns are available
  has_hy_spread <- "hy_spread" %in% names(data)
  has_ohlc <- all(c("spy_high", "spy_low", "spy_close") %in% names(data))
  
  df <- data %>%
    arrange(date) %>%
    # Target: log returns (lead-aligned)
    mutate(
      log_spy = log(spy_close),
      R = c(diff(log_spy), NA)  # R_t = log(close_t) - log(close_{t-1})
    ) %>%
    # Shift R forward so R[t] corresponds to return from t-1 to t
    # We want predictors at t to forecast R_{t+1}
    mutate(
      R_next = lead(R, 1)  # R_next[t] = R[t+1], so predictors at t forecast R_next[t]
    )
  
  # Build predictors (all using info only up to time t)
  df <- df %>%
    mutate(
      # dVIX: change in VIX
      dVIX = vix_close - lag(vix_close, 1),
      
      # Slope and dSlope
      slope = ux1_close - vix_close,
      dSlope = slope - lag(slope, 1),
      
      # d10Y: change in 10Y yield
      d10Y = us10y_yield - lag(us10y_yield, 1),
      
      # dHY: change in HY spread (if available)
      dHY = if (has_hy_spread) {
        hy_spread - lag(hy_spread, 1)
      } else {
        NA_real_
      },
      
      # Volume surprise: log volume - rolling mean
      log_vol = log(pmax(spy_volume, 1e-6)),  # Avoid log(0)
      vol_surp = log_vol - rollmean(log_vol, k = 20, align = "right", fill = NA),
      
      # Range: (high - low) / close
      range = if (has_ohlc) {
        (spy_high - spy_low) / spy_close
      } else {
        NA_real_
      },
      
      # Momentum: lagged returns
      R_lag1 = lag(R, 1),
      R_lag2 = lag(R, 2)
    )
  
  # Drop rows where target is missing
  df <- df %>% filter(!is.na(R_next))
  
  # Build predictor matrix (exclude target and date)
  predictor_cols <- c("dVIX", "slope", "dSlope", "d10Y", "dHY", "vol_surp", "range", "R", "R_lag1", "R_lag2")
  available_predictors <- predictor_cols[predictor_cols %in% names(df)]
  
  # Check for >20% NA and drop those columns
  na_frac <- df %>%
    select(all_of(available_predictors)) %>%
    summarise(across(everything(), ~ mean(is.na(.x))))
  
  valid_predictors <- available_predictors[na_frac[1, ] < 0.20]
  dropped <- setdiff(available_predictors, valid_predictors)
  
  if (length(dropped) > 0) {
    warning("Dropped predictors with >20% NA: ", paste(dropped, collapse = ", "))
  }
  
  message("Using ", length(valid_predictors), " predictors: ", paste(valid_predictors, collapse = ", "))
  
  return(list(
    data = df,
    predictors = valid_predictors
  ))
}

# ============================================================================
# 3) Stationarity Report
# ============================================================================

stationarity_report <- function(data, predictors, output_dir) {
  # Run ADF and KPSS tests on raw and differenced series.
  # Save report to CSV.
  message("Running stationarity tests...")
  
  results <- list()
  
  for (pred in predictors) {
    if (!pred %in% names(data)) next
    
    series <- data[[pred]]
    series <- series[!is.na(series)]
    
    if (length(series) < 10) next
    
    # Test levels
    tryCatch({
      adf_level <- adf.test(series, alternative = "stationary")
      kpss_level <- kpss.test(series, null = "Trend")
      
      results[[length(results) + 1]] <- tibble(
        variable = pred,
        transformation = "level",
        adf_pvalue = adf_level$p.value,
        adf_stat = adf_level$statistic,
        kpss_pvalue = kpss_level$p.value,
        kpss_stat = kpss_level$statistic
      )
    }, error = function(e) {
      message("Error testing ", pred, " (level): ", e$message)
    })
    
    # Test first difference
    if (length(series) > 1) {
      diff_series <- diff(series)
      tryCatch({
        adf_diff <- adf.test(diff_series, alternative = "stationary")
        kpss_diff <- kpss.test(diff_series, null = "Trend")
        
        results[[length(results) + 1]] <- tibble(
          variable = pred,
          transformation = "diff",
          adf_pvalue = adf_diff$p.value,
          adf_stat = adf_diff$statistic,
          kpss_pvalue = kpss_diff$p.value,
          kpss_stat = kpss_diff$statistic
        )
      }, error = function(e) {
        message("Error testing ", pred, " (diff): ", e$message)
      })
    }
  }
  
  if (length(results) > 0) {
    report <- bind_rows(results)
    
    # Print summary
    print(report)
    
    # Save
    write_csv(report, file.path(output_dir, "stationarity_report.csv"))
    message("Stationarity report saved to ", file.path(output_dir, "stationarity_report.csv"))
  }
  
  return(invisible(NULL))
}

# ============================================================================
# 4) Time-Safe Splits
# ============================================================================

time_splits <- function(data, start_date, end_date, holdout_date, freq = "daily") {
  # Create expanding-window backtest with fixed holdout.
  # For CV: rolling origin with expanding training, fixed validation blocks.
  message("Creating time-safe splits...")
  message("  Start: ", start_date, ", End: ", end_date, ", Holdout: ", holdout_date)
  
  # Filter to date range
  data <- data %>%
    filter(date >= as.Date(start_date), date <= as.Date(end_date))
  
  holdout_date <- as.Date(holdout_date)
  
  # Training: all data before holdout
  train_data <- data %>% filter(date < holdout_date)
  test_data <- data %>% filter(date >= holdout_date)
  
  message("  Training: ", nrow(train_data), " obs (", min(train_data$date), " to ", max(train_data$date), ")")
  message("  Test: ", nrow(test_data), " obs (", min(test_data$date), " to ", max(test_data$date), ")")
  
  # For CV: create rolling origin folds
  # Each fold: expanding training window, fixed validation block
  n_train <- nrow(train_data)
  val_block_size <- if (freq == "daily") 60 else 12  # 60 days or 12 weeks
  
  # Create 5 folds
  n_folds <- 5
  folds <- list()
  
  for (i in 1:n_folds) {
    # Minimum training size: at least 2*val_block_size
    min_train_size <- max(100, 2 * val_block_size)
    
    # Validation start: progressively later
    val_start_idx <- min_train_size + (i - 1) * val_block_size
    
    if (val_start_idx + val_block_size > n_train) {
      # Not enough data for this fold, skip
      next
    }
    
    train_idx <- 1:val_start_idx
    val_idx <- (val_start_idx + 1):min(val_start_idx + val_block_size, n_train)
    
    folds[[length(folds) + 1]] <- list(
      train_idx = train_idx,
      val_idx = val_idx
    )
  }
  
  message("  Created ", length(folds), " CV folds")
  
  return(list(
    train_data = train_data,
    test_data = test_data,
    cv_folds = folds,
    val_block_size = val_block_size
  ))
}

# ============================================================================
# 5) Elastic Net Selection
# ============================================================================

enet_select <- function(train_data, predictors, cv_folds, output_dir) {
  # Elastic Net with time-safe CV. Select alpha and lambda, then compute stability.
  message("Running Elastic Net selection...")
  
  # Prepare data
  X_train <- train_data %>%
    select(all_of(predictors)) %>%
    as.matrix()
  y_train <- train_data$R_next
  
  # Remove rows with any NA
  complete <- complete.cases(X_train) & !is.na(y_train)
  X_train <- X_train[complete, , drop = FALSE]
  y_train <- y_train[complete]
  
  # Standardize (store for later use)
  X_mean <- colMeans(X_train, na.rm = TRUE)
  X_sd <- apply(X_train, 2, sd, na.rm = TRUE)
  X_sd[X_sd == 0] <- 1  # Avoid division by zero
  
  X_train_scaled <- scale(X_train, center = X_mean, scale = X_sd)
  
  # Grid search over alpha
  alpha_grid <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  best_alpha <- NULL
  best_lambda <- NULL
  best_rmse <- Inf
  all_results <- list()
  
  for (alpha in alpha_grid) {
    message("  Testing alpha = ", alpha)
    
    # Create time-safe CV folds for glmnet
    foldid <- rep(NA, length(y_train))
    for (f in seq_along(cv_folds)) {
      fold <- cv_folds[[f]]
      # Map indices to actual row numbers in complete data
      train_idx_fold <- which(complete)[fold$train_idx]
      val_idx_fold <- which(complete)[fold$val_idx]
      
      # Only use folds where we have enough data
      if (length(train_idx_fold) < 10 || length(val_idx_fold) < 5) next
      
      foldid[val_idx_fold] <- f
    }
    
    # Remove rows not in any fold
    valid_fold <- !is.na(foldid)
    if (sum(valid_fold) < 10) next
    
    # Run CV
    tryCatch({
      cv_fit <- cv.glmnet(
        X_train_scaled[valid_fold, , drop = FALSE],
        y_train[valid_fold],
        alpha = alpha,
        foldid = foldid[valid_fold],
        standardize = FALSE  # Already standardized
      )
      
      rmse_min <- min(sqrt(cv_fit$cvm))
      
      all_results[[length(all_results) + 1]] <- tibble(
        alpha = alpha,
        lambda = cv_fit$lambda.min,
        rmse = rmse_min
      )
      
      if (rmse_min < best_rmse) {
        best_rmse <- rmse_min
        best_alpha <- alpha
        best_lambda <- cv_fit$lambda.min
      }
    }, error = function(e) {
      message("    Error with alpha = ", alpha, ": ", e$message)
    })
  }
  
  if (is.null(best_alpha)) {
    stop("Elastic Net selection failed")
  }
  
  message("  Best: alpha = ", best_alpha, ", lambda = ", best_lambda, ", RMSE = ", round(best_rmse, 6))
  
  # Fit final model on full training set
  final_fit <- glmnet(
    X_train_scaled,
    y_train,
    alpha = best_alpha,
    lambda = best_lambda,
    standardize = FALSE
  )
  
  # Compute stability: fraction of CV folds where coefficient != 0
  coef_stability <- rep(0, length(predictors))
  names(coef_stability) <- predictors
  
  for (f in seq_along(cv_folds)) {
    fold <- cv_folds[[f]]
    train_idx_fold <- which(complete)[fold$train_idx]
    
    if (length(train_idx_fold) < 10) next
    
    tryCatch({
      fold_fit <- glmnet(
        X_train_scaled[train_idx_fold, , drop = FALSE],
        y_train[train_idx_fold],
        alpha = best_alpha,
        lambda = best_lambda,
        standardize = FALSE
      )
      
      coefs <- as.numeric(coef(fold_fit))[-1]  # Exclude intercept
      coef_stability <- coef_stability + (coefs != 0)
    }, error = function(e) {
      # Skip this fold
    })
  }
  
  n_folds_used <- length(cv_folds)
  coef_stability <- coef_stability / n_folds_used
  
  # Select predictors with stability >= 0.6
  selected_predictors <- predictors[coef_stability >= 0.6]
  
  message("  Selected ", length(selected_predictors), " predictors (stability >= 0.6)")
  message("    ", paste(selected_predictors, collapse = ", "))
  
  return(list(
    selected_predictors = selected_predictors,
    alpha = best_alpha,
    lambda = best_lambda,
    X_mean = X_mean,
    X_sd = X_sd,
    stability = coef_stability
  ))
}

# ============================================================================
# 6) ARIMAX Model
# ============================================================================

fit_predict_arimax <- function(train_data, test_data, selected_predictors, X_mean, X_sd, output_dir) {
  # Fit ARIMAX on training, walk-forward forecast on validation & test.
  message("Fitting ARIMAX model...")
  
  # Prepare training data
  X_train <- train_data %>%
    select(all_of(selected_predictors)) %>%
    as.matrix()
  y_train <- train_data$R_next
  
  complete_train <- complete.cases(X_train) & !is.na(y_train)
  X_train <- X_train[complete_train, , drop = FALSE]
  y_train <- y_train[complete_train]
  
  X_train_scaled <- scale(X_train, center = X_mean[selected_predictors], scale = X_sd[selected_predictors])
  
  # Try ARIMAX models with different AR orders
  best_aic <- Inf
  best_model <- NULL
  best_p <- 0
  
  for (p in c(0, 1, 2)) {
    tryCatch({
      # Fit linear regression with ARIMA errors
      # y = X*beta + e, where e ~ ARIMA(p,0,0)
      lm_fit <- lm(y_train ~ X_train_scaled - 1)
      residuals <- residuals(lm_fit)
      
      if (p == 0) {
        arima_fit <- Arima(residuals, order = c(0, 0, 0))
      } else {
        arima_fit <- Arima(residuals, order = c(p, 0, 0))
      }
      
      aic_val <- AIC(arima_fit) + AIC(lm_fit)  # Approximate
      
      if (aic_val < best_aic) {
        best_aic <- aic_val
        best_p <- p
        best_model <- list(lm = lm_fit, arima = arima_fit)
      }
    }, error = function(e) {
      # Skip this p
    })
  }
  
  if (is.null(best_model)) {
    stop("ARIMAX fitting failed")
  }
  
  message("  Best AR order: p = ", best_p)
  
  # Save diagnostics
  png(file.path(output_dir, "diagnostics_arimax.png"), width = 1200, height = 800)
  par(mfrow = c(2, 2))
  acf(best_model$arima$residuals, main = "ACF of ARIMAX Residuals")
  pacf(best_model$arima$residuals, main = "PACF of ARIMAX Residuals")
  qqnorm(best_model$arima$residuals, main = "Q-Q Plot")
  qqline(best_model$arima$residuals)
  hist(best_model$arima$residuals, main = "Residuals Histogram", breaks = 30)
  dev.off()
  
  # Ljung-Box test
  lb_test <- Box.test(best_model$arima$residuals, lag = 10, type = "Ljung-Box")
  message("  Ljung-Box p-value: ", round(lb_test$p.value, 4))
  
  # Walk-forward forecasting on test set
  # Use only data up to t-1 to forecast t (leakage-safe)
  forecasts <- numeric(nrow(test_data))
  
  for (i in 1:nrow(test_data)) {
    # Training data: all data before this test point
    if (i == 1) {
      train_subset <- train_data
    } else {
      train_subset <- bind_rows(train_data, test_data[1:(i-1), ])
    }
    
    X_train_sub <- train_subset %>%
      select(all_of(selected_predictors)) %>%
      as.matrix()
    y_train_sub <- train_subset$R_next
    
    complete_sub <- complete.cases(X_train_sub) & !is.na(y_train_sub)
    if (sum(complete_sub) < 20) {
      forecasts[i] <- NA
      next
    }
    
    X_train_sub <- X_train_sub[complete_sub, , drop = FALSE]
    y_train_sub <- y_train_sub[complete_sub]
    
    X_train_sub_scaled <- scale(X_train_sub, center = X_mean[selected_predictors], scale = X_sd[selected_predictors])
    
    # Test point
    X_test_i <- test_data[i, selected_predictors, drop = FALSE] %>% as.matrix()
    if (any(is.na(X_test_i))) {
      forecasts[i] <- NA
      next
    }
    X_test_i_scaled <- scale(X_test_i, center = X_mean[selected_predictors], scale = X_sd[selected_predictors])
    
    tryCatch({
      lm_fit_i <- lm(y_train_sub ~ X_train_sub_scaled - 1)
      res_i <- residuals(lm_fit_i)
      
      if (best_p == 0) {
        arima_fit_i <- Arima(res_i, order = c(0, 0, 0))
      } else {
        arima_fit_i <- Arima(res_i, order = c(best_p, 0, 0))
      }
      
      linear_pred <- sum(coef(lm_fit_i) * as.numeric(X_test_i_scaled))
      arima_fc <- forecast(arima_fit_i, h = 1)$mean[1]
      
      forecasts[i] <- linear_pred + arima_fc
    }, error = function(e) {
      forecasts[i] <- NA
    })
  }
  
  return(list(
    forecasts = forecasts,
    model = best_model,
    p = best_p
  ))
}

# ============================================================================
# 7) Time-Varying Parameter (Kalman Filter)
# ============================================================================

fit_predict_tvp <- function(train_data, test_data, selected_predictors, X_mean, X_sd, output_dir) {
  # Fit TVP model using KFAS, walk-forward forecast.
  message("Fitting Time-Varying Parameter model...")
  
  # Prepare training data
  X_train <- train_data %>%
    select(all_of(selected_predictors)) %>%
    as.matrix()
  y_train <- train_data$R_next
  
  complete_train <- complete.cases(X_train) & !is.na(y_train)
  X_train <- X_train[complete_train, , drop = FALSE]
  y_train <- y_train[complete_train]
  
  X_train_scaled <- scale(X_train, center = X_mean[selected_predictors], scale = X_sd[selected_predictors])
  
  n_train <- length(y_train)
  n_pred <- length(selected_predictors)
  
  # Build SSM: y_t = X_t %*% theta_t + v_t, theta_t = theta_{t-1} + w_t
  # Observation equation: y_t = Z_t * alpha_t + epsilon_t
  # State equation: alpha_t = T_t * alpha_{t-1} + eta_t
  
  # Z matrix: time-varying (X_t at each time)
  Z <- array(0, dim = c(1, n_pred, n_train))
  for (t in 1:n_train) {
    Z[1, , t] <- X_train_scaled[t, ]
  }
  
  # T matrix: identity (random walk)
  T_mat <- diag(n_pred)
  
  # R matrix: identity (all states have noise)
  R <- diag(n_pred)
  
  # H: observation variance (scalar)
  H <- matrix(NA)
  
  # Q: state variance (diagonal, one per coefficient)
  Q <- diag(NA, n_pred)
  
  # Initial state: use OLS estimate
  lm_init <- lm(y_train ~ X_train_scaled - 1)
  a1 <- coef(lm_init)
  P1 <- diag(100, n_pred)  # Diffuse prior
  
  # Build model
  model <- SSModel(
    y_train ~ -1 + SSMcustom(
      Z = Z,
      T = T_mat,
      R = R,
      Q = Q,
      a1 = a1,
      P1 = P1
    ),
    H = H
  )
  
  # Estimate parameters (log(Q) and log(H))
  # Use BFGS
  Q_est <- NULL
  H_est <- NULL
  tryCatch({
    fit <- fitSSM(model, inits = c(rep(-2, n_pred), -2), method = "BFGS")
    message("  Model fitted successfully")
    # Extract estimated parameters
    Q_est <- fit$model$Q[1:n_pred, 1:n_pred, 1]
    H_est <- fit$model$H[1, 1, 1]
  }, error = function(e) {
    message("  Fitting failed, using default parameters: ", e$message)
    # Use default small variances
    Q_est <- diag(0.01, n_pred)
    H_est <- 0.01
    fit <- model
  })
  
  # Smooth and filter (if fit succeeded)
  coeff_paths <- NULL
  if (!is.null(Q_est)) {
    tryCatch({
      kfs <- KFS(fit$model)
      # Extract coefficient paths - a is (n_train+1) x n_pred matrix
      # First row is initial state, rows 2:(n_train+1) are filtered states
      if (!is.null(kfs$a) && ncol(kfs$a) >= n_pred) {
        # Extract filtered states (skip first row which is initial state)
        if (nrow(kfs$a) > 1) {
          coeff_paths <- as.matrix(kfs$a[2:(n_train+1), 1:n_pred, drop = FALSE])
          colnames(coeff_paths) <- selected_predictors
        } else {
          # Fallback: use smoothed states if available
          if (!is.null(kfs$alphahat) && ncol(kfs$alphahat) >= n_pred) {
            coeff_paths <- as.matrix(kfs$alphahat[, 1:n_pred, drop = FALSE])
            colnames(coeff_paths) <- selected_predictors
          }
        }
      }
    }, error = function(e) {
      message("  KFS failed: ", e$message)
    })
  }
  
  # If coefficient paths extraction failed, create dummy paths
  if (is.null(coeff_paths) || nrow(coeff_paths) == 0 || ncol(coeff_paths) != n_pred) {
    message("  Using OLS coefficients as proxy for coefficient paths")
    coeff_paths <- matrix(rep(coef(lm_init), each = n_train), 
                          nrow = n_train, ncol = n_pred)
    colnames(coeff_paths) <- selected_predictors
  }
  
  # Plot coefficient paths (only if we have valid paths)
  if (!is.null(coeff_paths) && nrow(coeff_paths) > 0 && ncol(coeff_paths) == n_pred) {
    tryCatch({
      png(file.path(output_dir, "tvp_coeff_paths.png"), width = 1200, height = 800)
      par(mfrow = c(ceiling(n_pred / 2), 2))
      for (i in 1:n_pred) {
        if (i <= ncol(coeff_paths)) {
          plot(coeff_paths[, i], type = "l", main = selected_predictors[i], 
               xlab = "Time", ylab = "Coefficient")
        }
      }
      dev.off()
      message("  Coefficient paths plot saved")
    }, error = function(e) {
      message("  Failed to plot coefficient paths: ", e$message)
    })
  } else {
    message("  Skipping coefficient paths plot (invalid dimensions)")
  }
  
  # Walk-forward forecasting on test set
  forecasts <- numeric(nrow(test_data))
  
  for (i in 1:nrow(test_data)) {
    # Training data: all data before this test point
    if (i == 1) {
      train_subset <- train_data
    } else {
      train_subset <- bind_rows(train_data, test_data[1:(i-1), ])
    }
    
    X_train_sub <- train_subset %>%
      select(all_of(selected_predictors)) %>%
      as.matrix()
    y_train_sub <- train_subset$R_next
    
    complete_sub <- complete.cases(X_train_sub) & !is.na(y_train_sub)
    if (sum(complete_sub) < 20) {
      forecasts[i] <- NA
      next
    }
    
    X_train_sub <- X_train_sub[complete_sub, , drop = FALSE]
    y_train_sub <- y_train_sub[complete_sub]
    
    X_train_sub_scaled <- scale(X_train_sub, center = X_mean[selected_predictors], scale = X_sd[selected_predictors])
    
    n_sub <- length(y_train_sub)
    
    # Test point
    X_test_i <- test_data[i, selected_predictors, drop = FALSE] %>% as.matrix()
    if (any(is.na(X_test_i))) {
      forecasts[i] <- NA
      next
    }
    X_test_i_scaled <- scale(X_test_i, center = X_mean[selected_predictors], scale = X_sd[selected_predictors])
    
    tryCatch({
      # Build SSM for this window
      Z_sub <- array(0, dim = c(1, n_pred, n_sub))
      for (t in 1:n_sub) {
        Z_sub[1, , t] <- X_train_sub_scaled[t, ]
      }
      
      # Use estimated Q and H from full training (reuse from outer scope)
      # For speed, use previous estimates rather than re-estimating
      Q_est_use <- if (is.null(Q_est)) diag(0.01, n_pred) else Q_est
      H_est_use <- if (is.null(H_est)) 0.01 else H_est
      
      # Initial state
      lm_init_sub <- lm(y_train_sub ~ X_train_sub_scaled - 1)
      a1_sub <- coef(lm_init_sub)
      P1_sub <- diag(100, n_pred)
      
      model_sub <- SSModel(
        y_train_sub ~ -1 + SSMcustom(
          Z = Z_sub,
          T = T_mat,
          R = R,
          Q = Q_est_use,
          a1 = a1_sub,
          P1 = P1_sub
        ),
        H = H_est_use
      )
      
      # Filter
      kfs_sub <- KFS(model_sub)
      
      # One-step forecast: use last filtered state
      last_state <- kfs_sub$a[n_sub + 1, ]
      forecast_i <- sum(last_state * as.numeric(X_test_i_scaled))
      
      forecasts[i] <- forecast_i
    }, error = function(e) {
      forecasts[i] <- NA
    })
  }
  
  return(list(
    forecasts = forecasts,
    coeff_paths = coeff_paths
  ))
}

# ============================================================================
# 8) Evaluation
# ============================================================================

evaluate_oos <- function(test_data, forecasts_arimax, forecasts_tvp, output_dir) {
  # Compute OOS metrics and create plots.
  message("Evaluating out-of-sample performance...")
  
  actual <- test_data$R_next
  dates <- test_data$date
  
  # Remove NA forecasts
  valid_arimax <- !is.na(forecasts_arimax) & !is.na(actual)
  valid_tvp <- !is.na(forecasts_tvp) & !is.na(actual)
  
  # Metrics
  metrics <- list()
  
  # ARIMAX
  if (sum(valid_arimax) > 0) {
    act_arimax <- actual[valid_arimax]
    fc_arimax <- forecasts_arimax[valid_arimax]
    
    rmse_arimax <- sqrt(mean((act_arimax - fc_arimax)^2))
    mae_arimax <- mean(abs(act_arimax - fc_arimax))
    mape_arimax <- mean(abs((act_arimax - fc_arimax) / pmax(abs(act_arimax), 1e-6))) * 100
    dir_acc_arimax <- mean(sign(act_arimax) == sign(fc_arimax)) * 100
    
    # Long/flat strategy
    positions_arimax <- ifelse(fc_arimax > 0, 1, 0)
    returns_arimax <- positions_arimax * act_arimax
    avg_return_arimax <- mean(returns_arimax)
    hit_rate_arimax <- mean(returns_arimax > 0) * 100
    
    metrics[[length(metrics) + 1]] <- tibble(
      model = "ARIMAX",
      RMSE = rmse_arimax,
      MAE = mae_arimax,
      MAPE = mape_arimax,
      directional_accuracy = dir_acc_arimax,
      avg_return = avg_return_arimax,
      hit_rate = hit_rate_arimax
    )
  }
  
  # TVP
  if (sum(valid_tvp) > 0) {
    act_tvp <- actual[valid_tvp]
    fc_tvp <- forecasts_tvp[valid_tvp]
    
    rmse_tvp <- sqrt(mean((act_tvp - fc_tvp)^2))
    mae_tvp <- mean(abs(act_tvp - fc_tvp))
    mape_tvp <- mean(abs((act_tvp - fc_tvp) / pmax(abs(act_tvp), 1e-6))) * 100
    dir_acc_tvp <- mean(sign(act_tvp) == sign(fc_tvp)) * 100
    
    # Long/flat strategy
    positions_tvp <- ifelse(fc_tvp > 0, 1, 0)
    returns_tvp <- positions_tvp * act_tvp
    avg_return_tvp <- mean(returns_tvp)
    hit_rate_tvp <- mean(returns_tvp > 0) * 100
    
    metrics[[length(metrics) + 1]] <- tibble(
      model = "TVP",
      RMSE = rmse_tvp,
      MAE = mae_tvp,
      MAPE = mape_tvp,
      directional_accuracy = dir_acc_tvp,
      avg_return = avg_return_tvp,
      hit_rate = hit_rate_tvp
    )
  }
  
  metrics_df <- bind_rows(metrics)
  print(metrics_df)
  write_csv(metrics_df, file.path(output_dir, "oos_metrics.csv"))
  message("Metrics saved to ", file.path(output_dir, "oos_metrics.csv"))
  
  # Plots
  if (sum(valid_arimax) > 0 && sum(valid_tvp) > 0) {
    # Find common valid indices
    common_valid <- valid_arimax & valid_tvp
    if (sum(common_valid) > 0) {
      # Forecast vs Actual
      plot_data <- tibble(
        date = dates[common_valid],
        actual = actual[common_valid],
        arimax = forecasts_arimax[common_valid],
        tvp = forecasts_tvp[common_valid]
      )
    
    p1 <- plot_data %>%
      ggplot(aes(x = date)) +
      geom_line(aes(y = actual, color = "Actual"), linewidth = 0.8) +
      geom_line(aes(y = arimax, color = "ARIMAX"), linewidth = 0.6, alpha = 0.7) +
      geom_line(aes(y = tvp, color = "TVP"), linewidth = 0.6, alpha = 0.7) +
      labs(title = "Forecast vs Actual (Test Window)", 
           x = "Date", y = "Return", color = "Series") +
      theme_minimal()
    
    # Cumulative PnL (use common valid indices)
    positions_arimax_common <- ifelse(forecasts_arimax[common_valid] > 0, 1, 0)
    positions_tvp_common <- ifelse(forecasts_tvp[common_valid] > 0, 1, 0)
    returns_arimax_common <- positions_arimax_common * actual[common_valid]
    returns_tvp_common <- positions_tvp_common * actual[common_valid]
    
    cum_pnl_arimax <- cumsum(returns_arimax_common)
    cum_pnl_tvp <- cumsum(returns_tvp_common)
    
    p2 <- tibble(
      date = dates[common_valid],
      arimax = cum_pnl_arimax,
      tvp = cum_pnl_tvp
    ) %>%
      pivot_longer(cols = c(arimax, tvp), names_to = "model", values_to = "cum_pnl") %>%
      ggplot(aes(x = date, y = cum_pnl, color = model)) +
      geom_line(linewidth = 0.8) +
      labs(title = "Cumulative PnL (Long/Flat Strategy)", 
           x = "Date", y = "Cumulative Return", color = "Model") +
      theme_minimal() +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
    
    p_combined <- p1 / p2
    ggsave(file.path(output_dir, "forecast_vs_actual.png"), p_combined, 
           width = 12, height = 10, dpi = 300)
    
    ggsave(file.path(output_dir, "cum_pnl.png"), p2, 
           width = 10, height = 6, dpi = 300)
    
      message("Plots saved to ", output_dir)
    } else {
      message("Warning: No common valid forecasts for plotting")
    }
  }
  
  return(metrics_df)
}

# ============================================================================
# 9) Main Function
# ============================================================================

main <- function(freq = NULL, start = NULL, end = NULL, holdout = NULL, 
                 out = NULL, data = NULL) {
  # Parse command-line arguments (if not provided as function arguments)
  args <- commandArgs(trailingOnly = TRUE)
  
  # Initialize from function arguments or defaults
  if (is.null(freq)) {
    freq <- "daily"
  }
  # Convert dates to character (handles Date objects and character strings)
  start_date <- if (is.null(start)) NULL else as.character(start)
  end_date <- if (is.null(end)) NULL else as.character(end)
  holdout_date <- if (is.null(holdout)) NULL else as.character(holdout)
  output_dir <- if (is.null(out)) "outputs" else out
  data_file <- if (is.null(data)) "data/market_data_2015_2025.csv" else data
  
  # Parse command-line arguments (only if function arguments weren't provided)
  if (is.null(start_date) || is.null(end_date) || is.null(holdout_date)) {
    i <- 1
    while (i <= length(args)) {
      if (args[i] == "--freq" && i < length(args)) {
        freq <- args[i + 1]
        i <- i + 2
      } else if (args[i] == "--start" && i < length(args) && is.null(start_date)) {
        start_date <- args[i + 1]
        i <- i + 2
      } else if (args[i] == "--end" && i < length(args) && is.null(end_date)) {
        end_date <- args[i + 1]
        i <- i + 2
      } else if (args[i] == "--holdout" && i < length(args) && is.null(holdout_date)) {
        holdout_date <- args[i + 1]
        i <- i + 2
      } else if (args[i] == "--out" && i < length(args) && output_dir == "outputs") {
        output_dir <- args[i + 1]
        i <- i + 2
      } else if (args[i] == "--data" && i < length(args) && data_file == "data/market_data_2015_2025.csv") {
        data_file <- args[i + 1]
        i <- i + 2
      } else {
        i <- i + 1
      }
    }
  }
  
  if (!freq %in% c("daily", "weekly")) {
    stop("--freq must be 'daily' or 'weekly'")
  }
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
  
  message(paste(rep("=", 50), collapse = ""))
  message("SPY Forecasting Pipeline")
  message("Frequency: ", freq)
  
  # 1. Load data first (needed to auto-detect dates if not provided)
  if (is.null(data_file) || data_file == "data/market_data_2015_2025.csv") {
    # Try to load from r_ready_data/ if default path doesn't exist
    if (is.null(data_file) || !file.exists(data_file)) {
      data <- load_data()  # Will auto-detect r_ready_data/
    } else {
      data <- load_data(data_file)
    }
  } else {
    data <- load_data(data_file)
  }
  
  # If dates are not provided, use the full range of the data
  if (is.null(start_date)) {
    start_date <- as.character(min(data$date, na.rm = TRUE))
    message("Using start_date from data: ", start_date)
  }
  if (is.null(end_date)) {
    end_date <- as.character(max(data$date, na.rm = TRUE))
    message("Using end_date from data: ", end_date)
  }
  if (is.null(holdout_date)) {
    # Default: use last 365 days as holdout
    holdout_date <- as.character(max(data$date, na.rm = TRUE) - 365)
    message("Using holdout_date (1 year before end): ", holdout_date)
  }
  
  message("Date range: ", start_date, " to ", end_date)
  message("Holdout start: ", holdout_date)
  message(paste(rep("=", 50), collapse = ""))
  
  # 2. Convert to weekly if needed
  if (freq == "weekly") {
    data <- make_weekly(data)
  }
  
  # 3. Feature engineering
  fe_result <- engineer_features(data)
  data <- fe_result$data
  predictors <- fe_result$predictors
  
  # 4. Stationarity report
  stationarity_report(data, predictors, output_dir)
  
  # 5. Time-safe splits
  splits <- time_splits(data, start_date, end_date, holdout_date, freq)
  train_data <- splits$train_data
  test_data <- splits$test_data
  cv_folds <- splits$cv_folds
  
  # 6. Elastic Net selection
  enet_result <- enet_select(train_data, predictors, cv_folds, output_dir)
  selected_predictors <- enet_result$selected_predictors
  X_mean <- enet_result$X_mean
  X_sd <- enet_result$X_sd
  
  # 7. ARIMAX
  arimax_result <- fit_predict_arimax(
    train_data, test_data, selected_predictors, X_mean, X_sd, output_dir
  )
  
  # 8. TVP
  tvp_result <- fit_predict_tvp(
    train_data, test_data, selected_predictors, X_mean, X_sd, output_dir
  )
  
  # 9. Evaluation
  metrics <- evaluate_oos(
    test_data, 
    arimax_result$forecasts, 
    tvp_result$forecasts, 
    output_dir
  )
  
  message("\n", paste(rep("=", 50), collapse = ""))
  message("Pipeline completed successfully!")
  message(paste(rep("=", 50), collapse = ""))
}

# Run main
if (!interactive()) {
  main()
}

