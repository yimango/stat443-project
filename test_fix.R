# Toy test script to verify the column name fix works
# This creates fake backtest results with the problematic column names

library(tidyverse)

# Create toy backtest results with the same structure as real results
# Simulating what backtest_ar, backtest_ma, and backtest_arima return
toy_results_ar <- tibble(
  date = as.Date("2023-01-01") + 0:9,
  actual = rnorm(10, 0, 0.01),
  forecast_ar = rnorm(10, 0, 0.01),  # Note: forecast_ar, not forecast
  error = rnorm(10, 0, 0.001),
  p_used = rep(2, 10)
)

toy_results_ma <- tibble(
  date = as.Date("2023-01-01") + 0:9,
  actual = rnorm(10, 0, 0.01),
  forecast_ma = rnorm(10, 0, 0.01),  # Note: forecast_ma, not forecast
  error = rnorm(10, 0, 0.001),
  q_used = rep(1, 10)
)

toy_results_arima <- tibble(
  date = as.Date("2023-01-01") + 0:9,
  actual = rnorm(10, 0, 0.01),
  forecast_arima = rnorm(10, 0, 0.01),  # Note: forecast_arima, not forecast
  error = rnorm(10, 0, 0.001),
  p_used = rep(2, 10),
  q_used = rep(1, 10)
)

# Combine them (this is what main_models.R does)
toy_all_results <- bind_rows(
  toy_results_ar %>% mutate(model_name = "AR"),
  toy_results_ma %>% mutate(model_name = "MA"),
  toy_results_arima %>% mutate(model_name = "ARIMA")
)

message("Original column names: ", paste(names(toy_all_results), collapse = ", "))
message("\nChecking for forecast column: ", "forecast" %in% names(toy_all_results))
message("Has forecast_ar: ", "forecast_ar" %in% names(toy_all_results))
message("Has forecast_ma: ", "forecast_ma" %in% names(toy_all_results))
message("Has forecast_arima: ", "forecast_arima" %in% names(toy_all_results))

# Apply the fix
message("\n=== Applying fix ===")
all_results_fixed <- toy_all_results

# Create a single forecast column from whichever model-specific column has a value
# Use coalesce to pick the first non-NA value across all forecast columns
forecast_cols <- c("forecast_ar", "forecast_ma", "forecast_arima")
existing_cols <- forecast_cols[forecast_cols %in% names(all_results_fixed)]

if (length(existing_cols) > 0) {
  # Build coalesce expression dynamically
  coalesce_expr <- paste(existing_cols, collapse = ", ")
  all_results_fixed <- all_results_fixed %>%
    mutate(forecast = coalesce(!!!syms(existing_cols))) %>%
    select(-all_of(existing_cols))
  message("Fixed forecast columns: ", paste(existing_cols, collapse = ", "), " -> forecast")
}

# Remove extra columns
all_results_fixed <- all_results_fixed %>% select(-any_of(c("p_used", "q_used")))

message("\nFixed column names: ", paste(names(all_results_fixed), collapse = ", "))
message("Has forecast column: ", "forecast" %in% names(all_results_fixed))

# Test that evaluate_all_models would work (just test the filter part)
message("\n=== Testing filter operation ===")
test_filter <- all_results_fixed %>%
  filter(!is.na(.data$forecast), !is.na(.data$actual), !is.na(.data$error))

message("Filter successful! Rows after filter: ", nrow(test_filter))
message("\n✅ Fix verified! The column standardization works correctly.")

