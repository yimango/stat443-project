# Quick fix script to standardize forecast column names in existing results
# Run this after: result <- main()

# Fix script for your actual result object
# Run this in R console after: result <- main()

library(tidyverse)

# Fix the backtest_results in the result object
if (exists("result") && "backtest_results" %in% names(result)) {
  all_results <- result$backtest_results
  
  message("Original columns: ", paste(names(all_results), collapse = ", "))
  
  # Create a single forecast column from whichever model-specific column has a value
  forecast_cols <- c("forecast_ar", "forecast_ma", "forecast_arima")
  existing_cols <- forecast_cols[forecast_cols %in% names(all_results)]
  
  if (length(existing_cols) > 0) {
    # Use coalesce to combine all forecast columns into one
    all_results <- all_results %>%
      mutate(forecast = coalesce(!!!syms(existing_cols))) %>%
      select(-all_of(existing_cols))
    
    message("✅ Fixed forecast columns: ", paste(existing_cols, collapse = ", "), " -> forecast")
  }
  
  # Remove extra columns if they exist
  all_results <- all_results %>%
    select(-any_of(c("p_used", "q_used")))
  
  message("Fixed columns: ", paste(names(all_results), collapse = ", "))
  
  # Update the result object
  result$backtest_results <- all_results
  
  # Now you can run the evaluation
  source("Evaluation/evaluate_models.R", local = TRUE)
  eval_result <- evaluate_all_models(all_results, output_dir = "outputs")
  
  # Update result with evaluation
  result$evaluation <- eval_result
  
  message("\n✅ Evaluation complete! Results saved to result$evaluation")
} else {
  stop("Please run result <- main() first, or ensure result$backtest_results exists")
}

