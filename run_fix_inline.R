# Copy and paste this into your R console to fix your results

library(tidyverse)

message("=== Fixing forecast column names in result$backtest_results ===")

# Check if result exists
if (!exists("result")) {
  stop("Please run result <- main() first")
}

if (!"backtest_results" %in% names(result)) {
  stop("result$backtest_results not found. Please check your result object structure.")
}

# Get the backtest results
all_results <- result$backtest_results

message("\nOriginal columns: ", paste(names(all_results), collapse = ", "))

# Create a single forecast column from whichever model-specific column has a value
forecast_cols <- c("forecast_ar", "forecast_ma", "forecast_arima")
existing_cols <- forecast_cols[forecast_cols %in% names(all_results)]

if (length(existing_cols) > 0) {
  message("\nFound forecast columns: ", paste(existing_cols, collapse = ", "))
  
  # Use coalesce to combine all forecast columns into one
  all_results <- all_results %>%
    mutate(forecast = coalesce(!!!syms(existing_cols))) %>%
    select(-all_of(existing_cols))
  
  message("✅ Combined forecast columns into single 'forecast' column")
} else if ("forecast" %in% names(all_results)) {
  message("\n✅ 'forecast' column already exists - no fix needed")
} else {
  stop("No forecast columns found! Expected: forecast_ar, forecast_ma, or forecast_arima")
}

# Remove extra columns if they exist
all_results <- all_results %>%
  select(-any_of(c("p_used", "q_used")))

message("\nFixed columns: ", paste(names(all_results), collapse = ", "))
message("Total rows: ", nrow(all_results))

# Update the result object
result$backtest_results <- all_results

message("\n✅ Updated result$backtest_results")

# Now run the evaluation
message("\n=== Running evaluation ===")
source("Evaluation/evaluate_models.R", local = TRUE)

eval_result <- evaluate_all_models(all_results, output_dir = "outputs")

# Update result with evaluation
result$evaluation <- eval_result

message("\n✅ Evaluation complete! Results saved to:")
message("  - result$evaluation$metrics")
message("  - result$evaluation$comparisons")
message("  - result$evaluation$plots")
message("  - Output files in outputs/ directory")

