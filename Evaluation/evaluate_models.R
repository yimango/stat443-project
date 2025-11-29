#!/usr/bin/env Rscript
# Model Evaluation and Plotting
# 
# Computes metrics, performs model comparisons, and generates diagnostic plots
# for AR, MA, and ARIMA model backtest results

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
})

# ============================================================================
# Metrics Computation
# ============================================================================

compute_metrics <- function(backtest_results) {
  # Compute forecast accuracy metrics for each model
  #
  # Args:
  #   backtest_results: data frame with columns: date, actual, forecast, model_name, error
  #
  # Returns:
  #   Data frame with metrics per model
  
  message("Computing forecast accuracy metrics...")
  
  # Ensure numeric types and filter valid data
  # Handle both standardized 'forecast' column and model-specific columns (forecast_ar, forecast_ma, forecast_arima)
  
  # Check if forecast column already exists (from standardized results)
  if ("forecast" %in% names(backtest_results)) {
    # Use existing forecast column
    metrics_data <- backtest_results %>%
      filter(!is.na(.data$forecast), !is.na(.data$actual), !is.na(.data$error)) %>%
      mutate(
        actual = as.numeric(.data$actual),
        forecast = as.numeric(.data$forecast),
        error = as.numeric(.data$error)
      ) %>%
      filter(!is.na(actual), !is.na(forecast), !is.na(error))
  } else {
    # Handle model-specific columns (legacy support)
    forecast_cols <- c("forecast_ar", "forecast_ma", "forecast_arima")
    existing_cols <- forecast_cols[forecast_cols %in% names(backtest_results)]
    
    if (length(existing_cols) > 0) {
      # Create unified forecast column from model-specific columns
      metrics_data <- backtest_results %>%
        mutate(forecast = coalesce(!!!syms(existing_cols))) %>%
        filter(!is.na(forecast), !is.na(.data$actual), !is.na(.data$error)) %>%
        mutate(
          actual = as.numeric(.data$actual),
          forecast = as.numeric(forecast),
          error = as.numeric(.data$error)
        ) %>%
        filter(!is.na(actual), !is.na(forecast), !is.na(error))
    } else {
      stop("No forecast column found. Expected 'forecast' or one of: forecast_ar, forecast_ma, forecast_arima")
    }
  }
  
  metrics <- metrics_data %>%
    group_by(model_name) %>%
    summarise(
      n_forecasts = n(),
      rmse = sqrt(mean(error^2, na.rm = TRUE)),
      mae = mean(abs(error), na.rm = TRUE),
      mape = mean(abs(error / pmax(abs(actual), 1e-6)), na.rm = TRUE) * 100,
      mean_error = mean(error, na.rm = TRUE),
      sd_error = sd(error, na.rm = TRUE),
      # Directional accuracy - handle cases where actual or forecast might be zero
      sign_match = sign(actual) == sign(forecast),
      directional_accuracy = mean(sign_match, na.rm = TRUE) * 100,
      # Sign-based metrics
      correct_direction = sum(sign_match, na.rm = TRUE),
      total_direction = n(),
      .groups = "drop"
    ) %>%
    arrange(rmse)
  
  print(metrics)
  
  return(metrics)
}

# ============================================================================
# Diebold-Mariano Test (Model Comparison)
# ============================================================================

dm_test_simple <- function(errors1, errors2) {
  # Simple implementation of Diebold-Mariano test
  # Tests H0: E[loss1] = E[loss2] vs H1: E[loss1] != E[loss2]
  #
  # Args:
  #   errors1, errors2: forecast errors from two models (must be aligned)
  #
  # Returns:
  #   List with test statistic and p-value
  
  if (length(errors1) != length(errors2)) {
    stop("Error vectors must have the same length")
  }
  
  # Use squared errors as loss function
  loss_diff <- errors1^2 - errors2^2
  
  # Test statistic
  n <- length(loss_diff)
  mean_diff <- mean(loss_diff, na.rm = TRUE)
  se_diff <- sd(loss_diff, na.rm = TRUE) / sqrt(n)
  
  if (se_diff == 0) {
    return(list(statistic = 0, pvalue = 1))
  }
  
  dm_stat <- mean_diff / se_diff
  
  # Two-sided p-value (approximate normal distribution)
  pvalue <- 2 * (1 - pnorm(abs(dm_stat)))
  
  return(list(
    statistic = dm_stat,
    pvalue = pvalue,
    mean_diff = mean_diff
  ))
}

compare_models <- function(backtest_results) {
  # Perform pairwise Diebold-Mariano tests between models
  #
  # Args:
  #   backtest_results: data frame with columns: date, actual, forecast, model_name, error
  #
  # Returns:
  #   Data frame with pairwise comparison results
  
  message("Performing pairwise model comparisons (Diebold-Mariano tests)...")
  
  models <- unique(backtest_results$model_name)
  n_models <- length(models)
  
  if (n_models < 2) {
    message("  Need at least 2 models for comparison")
    return(NULL)
  }
  
  # Align results by date for fair comparison
  # Use .data$ to avoid conflict with forecast() function
  results_wide <- backtest_results %>%
    filter(!is.na(.data$forecast), !is.na(.data$actual)) %>%
    mutate(error = as.numeric(.data$error)) %>%
    filter(!is.na(error)) %>%
    select(date, model_name, error) %>%
    pivot_wider(names_from = model_name, values_from = error)
  
  comparisons <- list()
  
  for (i in 1:(n_models - 1)) {
    for (j in (i + 1):n_models) {
      model1 <- models[i]
      model2 <- models[j]
      
      # Get aligned errors
      errors1 <- as.numeric(results_wide[[model1]])
      errors2 <- as.numeric(results_wide[[model2]])
      
      # Remove rows with NA in either model
      valid <- !is.na(errors1) & !is.na(errors2)
      errors1 <- errors1[valid]
      errors2 <- errors2[valid]
      
      if (length(errors1) < 10) {
        next
      }
      
      # Perform DM test
      dm_result <- dm_test_simple(errors1, errors2)
      
      # Determine which model is better (lower RMSE)
      rmse1 <- sqrt(mean(errors1^2, na.rm = TRUE))
      rmse2 <- sqrt(mean(errors2^2, na.rm = TRUE))
      
      comparisons[[length(comparisons) + 1]] <- tibble(
        model1 = model1,
        model2 = model2,
        dm_statistic = dm_result$statistic,
        dm_pvalue = dm_result$pvalue,
        rmse1 = rmse1,
        rmse2 = rmse2,
        better_model = ifelse(rmse1 < rmse2, model1, model2),
        significant = dm_result$pvalue < 0.05
      )
    }
  }
  
  if (length(comparisons) > 0) {
    comparison_df <- bind_rows(comparisons)
    print(comparison_df)
    return(comparison_df)
  } else {
    return(NULL)
  }
}

# ============================================================================
# Plotting Functions
# ============================================================================

plot_forecast_comparison <- function(backtest_results, output_file = NULL) {
  # Plot actual vs forecasted values for all models
  #
  # Args:
  #   backtest_results: data frame with backtest results
  #   output_file: optional path to save plot
  #
  # Returns:
  #   ggplot object
  
  plot_data <- backtest_results %>%
    filter(!is.na(.data$forecast), !is.na(.data$actual)) %>%
    arrange(date)
  
  p <- plot_data %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.8) +
    geom_line(aes(y = forecast, color = "Forecast"), linewidth = 0.6, alpha = 0.7) +
    facet_wrap(~ model_name, ncol = 1, scales = "free_y") +
    labs(
      title = "Forecast vs Actual by Model",
      x = "Date",
      y = "Value",
      color = "Series"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  if (!is.null(output_file)) {
    ggsave(output_file, p, width = 12, height = 8, dpi = 300)
    message("  Forecast comparison plot saved to ", output_file)
  }
  
  return(p)
}

plot_residuals <- function(backtest_results, output_file = NULL) {
  # Plot residual diagnostics for all models
  #
  # Args:
  #   backtest_results: data frame with backtest results
  #   output_file: optional path to save plot
  #
  # Returns:
  #   ggplot object
  
  plot_data <- backtest_results %>%
    filter(!is.na(error)) %>%
    arrange(date)
  
  p1 <- plot_data %>%
    ggplot(aes(x = date, y = error, color = model_name)) +
    geom_line(linewidth = 0.5, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~ model_name, ncol = 1, scales = "free_y") +
    labs(
      title = "Residuals Over Time",
      x = "Date",
      y = "Forecast Error",
      color = "Model"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  p2 <- plot_data %>%
    ggplot(aes(x = error, fill = model_name)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~ model_name, ncol = 1) +
    labs(
      title = "Error Distribution",
      x = "Forecast Error",
      y = "Density",
      fill = "Model"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  p <- p1 / p2
  
  if (!is.null(output_file)) {
    ggsave(output_file, p, width = 12, height = 10, dpi = 300)
    message("  Residual plots saved to ", output_file)
  }
  
  return(p)
}

plot_error_comparison <- function(backtest_results, output_file = NULL) {
  # Plot error distribution comparison across models
  #
  # Args:
  #   backtest_results: data frame with backtest results
  #   output_file: optional path to save plot
  #
  # Returns:
  #   ggplot object
  
  plot_data <- backtest_results %>%
    filter(!is.na(error))
  
  p <- plot_data %>%
    ggplot(aes(x = model_name, y = abs(error), fill = model_name)) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = "Absolute Forecast Error by Model",
      x = "Model",
      y = "Absolute Error",
      fill = "Model"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  if (!is.null(output_file)) {
    ggsave(output_file, p, width = 10, height = 6, dpi = 300)
    message("  Error comparison plot saved to ", output_file)
  }
  
  return(p)
}

plot_cumulative_errors <- function(backtest_results, output_file = NULL) {
  # Plot cumulative forecast errors (pseudo-PnL)
  #
  # Args:
  #   backtest_results: data frame with backtest results
  #   output_file: optional path to save plot
  #
  # Returns:
  #   ggplot object
  
  plot_data <- backtest_results %>%
    filter(!is.na(error)) %>%
    arrange(date) %>%
    group_by(model_name) %>%
    mutate(cumulative_error = cumsum(error)) %>%
    ungroup()
  
  p <- plot_data %>%
    ggplot(aes(x = date, y = cumulative_error, color = model_name)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
    labs(
      title = "Cumulative Forecast Error (Pseudo-PnL)",
      x = "Date",
      y = "Cumulative Error",
      color = "Model"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  if (!is.null(output_file)) {
    ggsave(output_file, p, width = 12, height = 6, dpi = 300)
    message("  Cumulative error plot saved to ", output_file)
  }
  
  return(p)
}

plot_metrics_summary <- function(metrics, output_file = NULL) {
  # Create bar plots comparing metrics across models
  #
  # Args:
  #   metrics: data frame from compute_metrics
  #   output_file: optional path to save plot
  #
  # Returns:
  #   ggplot object
  
  metrics_long <- metrics %>%
    select(model_name, rmse, mae, mape, directional_accuracy) %>%
    pivot_longer(cols = c(rmse, mae, mape, directional_accuracy),
                 names_to = "metric", values_to = "value")
  
  p <- metrics_long %>%
    ggplot(aes(x = model_name, y = value, fill = model_name)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    facet_wrap(~ metric, scales = "free_y", ncol = 2) +
    labs(
      title = "Model Performance Metrics",
      x = "Model",
      y = "Value",
      fill = "Model"
    ) +
    theme_minimal() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (!is.null(output_file)) {
    ggsave(output_file, p, width = 12, height = 8, dpi = 300)
    message("  Metrics summary plot saved to ", output_file)
  }
  
  return(p)
}

# ============================================================================
# Main Evaluation Pipeline
# ============================================================================

evaluate_all_models <- function(backtest_results, output_dir = "outputs") {
  # Complete evaluation pipeline: metrics, comparisons, and plots
  #
  # Args:
  #   backtest_results: data frame with backtest results
  #   output_dir: directory to save outputs
  #
  # Returns:
  #   List with metrics, comparisons, and plot objects
  
  message("Running complete evaluation pipeline...")
  
  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Compute metrics
  metrics <- compute_metrics(backtest_results)
  
  # Save metrics
  metrics_file <- file.path(output_dir, "model_comparison_metrics.csv")
  write_csv(metrics, metrics_file)
  message("  Metrics saved to ", metrics_file)
  
  # Model comparisons
  comparisons <- compare_models(backtest_results)
  if (!is.null(comparisons)) {
    comp_file <- file.path(output_dir, "model_comparisons.csv")
    write_csv(comparisons, comp_file)
    message("  Model comparisons saved to ", comp_file)
  }
  
  # Generate plots
  message("  Generating plots...")
  
  p1 <- plot_forecast_comparison(backtest_results, 
                                file.path(output_dir, "forecast_comparison.png"))
  p2 <- plot_residuals(backtest_results, 
                      file.path(output_dir, "residual_diagnostics.png"))
  p3 <- plot_error_comparison(backtest_results, 
                             file.path(output_dir, "error_comparison.png"))
  p4 <- plot_cumulative_errors(backtest_results, 
                             file.path(output_dir, "cumulative_errors.png"))
  p5 <- plot_metrics_summary(metrics, 
                             file.path(output_dir, "metrics_summary.png"))
  
  message("Evaluation complete!")
  
  return(list(
    metrics = metrics,
    comparisons = comparisons,
    plots = list(
      forecast_comparison = p1,
      residuals = p2,
      error_comparison = p3,
      cumulative_errors = p4,
      metrics_summary = p5
    )
  ))
}

