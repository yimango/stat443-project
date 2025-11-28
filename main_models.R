#!/usr/bin/env Rscript
# Main Time-Series Modeling Pipeline
# 
# Orchestrates the complete AR/MA/ARIMA modeling workflow:
# 1. Data preparation and stationary target creation
# 2. Rolling-origin backtesting for AR, MA, and ARIMA models
# 3. Model evaluation, comparison, and visualization
#
# Usage:
#   Rscript main_models.R [--start YYYY-MM-DD] [--end YYYY-MM-DD] [--holdout YYYY-MM-DD] [--out OUTPUT_DIR]

suppressPackageStartupMessages({
  library(tidyverse)
})

# ============================================================================
# Configuration
# ============================================================================

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)

start_date <- NULL
end_date <- NULL
holdout_date <- NULL
output_dir <- "outputs"

i <- 1
while (i <= length(args)) {
  if (args[i] == "--start" && i < length(args)) {
    start_date <- args[i + 1]
    i <- i + 2
  } else if (args[i] == "--end" && i < length(args)) {
    end_date <- args[i + 1]
    i <- i + 2
  } else if (args[i] == "--holdout" && i < length(args)) {
    holdout_date <- args[i + 1]
    i <- i + 2
  } else if (args[i] == "--out" && i < length(args)) {
    output_dir <- args[i + 1]
    i <- i + 2
  } else {
    i <- i + 1
  }
}

# ============================================================================
# Main Pipeline
# ============================================================================

main <- function() {
  message(paste(rep("=", 70), collapse = ""))
  message("Time-Series AR/MA/ARIMA Modeling Pipeline")
  message("Box-Jenkins Methodology with Proper Backtesting")
  message(paste(rep("=", 70), collapse = ""))
  
  # Step 1: Data Preparation
  message("\n[Step 1] Preparing data and stationary target series...")
  source("DataPrep/ts_prep.R", local = TRUE)
  
  prep_result <- prepare_ts_data(
    file_path = NULL,  # Auto-detect from r_ready_data/
    start_date = start_date,
    end_date = end_date,
    holdout_date = holdout_date,
    target_col = "spy_close"
  )
  
  message("\n  Target series: ", prep_result$target_name)
  message("  Differencing order: d = ", prep_result$d)
  message("  Total observations: ", nrow(prep_result$data))
  
  # Step 2: Load model modules
  message("\n[Step 2] Loading model modules...")
  source("Models/ar_model.R", local = TRUE)
  source("Models/ma_model.R", local = TRUE)
  source("Models/arima_model.R", local = TRUE)
  
  # Step 3: Initial model fitting on training data (for diagnostics)
  message("\n[Step 3] Fitting initial models on training data...")
  
  train_series <- prep_result$splits$train_data$target_series
  train_series <- train_series[!is.na(train_series)]
  
  # Fit AR model
  message("\n  Fitting AR model...")
  ar_fit <- fit_ar(train_series, max_p = 8)
  message("    Selected AR(", ar_fit$p, ") - AIC: ", round(ar_fit$aic, 2), 
          ", BIC: ", round(ar_fit$bic, 2))
  
  # Fit MA model
  message("\n  Fitting MA model...")
  ma_fit <- fit_ma(train_series, d = prep_result$d, max_q = 8)
  message("    Selected MA(", ma_fit$q, ") - AIC: ", round(ma_fit$aic, 2), 
          ", BIC: ", round(ma_fit$bic, 2))
  
  # Fit ARIMA model
  message("\n  Fitting ARIMA model (Box-Jenkins)...")
  arima_fit <- fit_arima(train_series, d = prep_result$d, max_p = 5, max_q = 5)
  message("    Selected ARIMA(", arima_fit$p, ",", prep_result$d, ",", arima_fit$q, 
          ") - AIC: ", round(arima_fit$aic, 2), 
          ", BIC: ", round(arima_fit$bic, 2))
  
  # Step 4: Model diagnostics
  message("\n[Step 4] Running model diagnostics...")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  message("\n  AR model diagnostics...")
  ar_diag <- diagnose_ar(ar_fit, file.path(output_dir, "ar_diagnostics.png"))
  
  message("\n  MA model diagnostics...")
  ma_diag <- diagnose_ma(ma_fit, file.path(output_dir, "ma_diagnostics.png"))
  
  message("\n  ARIMA model diagnostics...")
  arima_diag <- diagnose_arima(arima_fit, file.path(output_dir, "arima_diagnostics.png"))
  
  # Step 5: Rolling-origin backtesting
  message("\n[Step 5] Running rolling-origin backtests (no future data leakage)...")
  
  source("Evaluation/backtest.R", local = TRUE)
  
  message("\n  Backtesting AR model...")
  results_ar <- backtest_ar(prep_result$data, prep_result$splits, max_p = 8)
  
  message("\n  Backtesting MA model...")
  results_ma <- backtest_ma(prep_result$data, prep_result$splits, 
                           d = prep_result$d, max_q = 8)
  
  message("\n  Backtesting ARIMA model...")
  results_arima <- backtest_arima(prep_result$data, prep_result$splits, 
                                  d = prep_result$d, max_p = 5, max_q = 5)
  
  # Combine results
  all_results <- bind_rows(
    results_ar %>% mutate(model_name = "AR"),
    results_ma %>% mutate(model_name = "MA"),
    results_arima %>% mutate(model_name = "ARIMA")
  )
  
  message("\n  Total backtest forecasts: ", nrow(all_results))
  
  # Step 6: Evaluation and visualization
  message("\n[Step 6] Evaluating models and generating plots...")
  
  source("Evaluation/evaluate_models.R", local = TRUE)
  
  eval_result <- evaluate_all_models(all_results, output_dir = output_dir)
  
  # Step 7: Summary
  message("\n", paste(rep("=", 70), collapse = ""))
  message("Pipeline Summary")
  message(paste(rep("=", 70), collapse = ""))
  
  message("\nTarget Series:")
  message("  Type: ", prep_result$target_name)
  message("  Differencing: d = ", prep_result$d)
  message("  Training obs: ", nrow(prep_result$splits$train_data))
  message("  Test obs: ", nrow(prep_result$splits$test_data))
  
  message("\nSelected Models:")
  message("  AR(", ar_fit$p, ") - AIC: ", round(ar_fit$aic, 2), 
          ", BIC: ", round(ar_fit$bic, 2))
  message("  MA(", ma_fit$q, ") - AIC: ", round(ma_fit$aic, 2), 
          ", BIC: ", round(ma_fit$bic, 2))
  message("  ARIMA(", arima_fit$p, ",", prep_result$d, ",", arima_fit$q, 
          ") - AIC: ", round(arima_fit$aic, 2), 
          ", BIC: ", round(arima_fit$bic, 2))
  
  message("\nOut-of-Sample Performance:")
  print(eval_result$metrics)
  
  message("\nOutputs saved to: ", output_dir)
  message(paste(rep("=", 70), collapse = ""))
  message("Pipeline completed successfully!")
  message(paste(rep("=", 70), collapse = ""))
  
  return(list(
    prep = prep_result,
    fits = list(ar = ar_fit, ma = ma_fit, arima = arima_fit),
    diagnostics = list(ar = ar_diag, ma = ma_diag, arima = arima_diag),
    backtest_results = all_results,
    evaluation = eval_result
  ))
}

# Run main function
if (!interactive()) {
  result <- main()
}

