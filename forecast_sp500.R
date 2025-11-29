#!/usr/bin/env Rscript
# Day-by-Day SP500 Forecasting Script
# Uses the best model (ARIMA) to generate forecasts

suppressPackageStartupMessages({
  library(tidyverse)
  library(forecast)
  library(tseries)
})

# ============================================================================
# Load Data and Model
# ============================================================================

message("=== SP500 Day-by-Day Forecasting ===")
message("Loading data and fitted model...")

# Source necessary files
source("DataPrep/ts_prep.R", local = TRUE)
source("Models/arima_model.R", local = TRUE)

# Load the data
prep_result <- prepare_ts_data(
  file_path = NULL,
  target_col = "spy_close"
)

# Get the full time series
full_series <- prep_result$data$target_series
full_series <- full_series[!is.na(full_series)]
dates <- prep_result$data$date[!is.na(prep_result$data$target_series)]

message("  Data loaded: ", length(full_series), " observations")
message("  Date range: ", min(dates), " to ", max(dates))
message("  Differencing order: d = ", prep_result$d)

# Fit the ARIMA model on all available data
message("\nFitting ARIMA model on full dataset...")
arima_fit <- fit_arima(full_series, d = prep_result$d, max_p = 5, max_q = 5)
message("  Selected ARIMA(", arima_fit$p, ",", prep_result$d, ",", arima_fit$q, ")")
message("  AIC: ", round(arima_fit$aic, 2), ", BIC: ", round(arima_fit$bic, 2))

# ============================================================================
# Generate Forecasts
# ============================================================================

message("\n=== Generating Day-by-Day Forecasts ===")

# Number of days to forecast ahead
n_days <- 30  # Forecast next 30 days

# Generate multi-step ahead forecasts using forecast package directly
# The forecast_arima function only returns 1-step, so we use forecast() directly
fc_result <- forecast(arima_fit$model, h = n_days)
forecasts <- as.numeric(fc_result$mean)

# Get prediction intervals (80% and 95%)
lower_80 <- as.numeric(fc_result$lower[, 1])  # 80% lower bound
upper_80 <- as.numeric(fc_result$upper[, 1])   # 80% upper bound
lower_95 <- as.numeric(fc_result$lower[, 2])   # 95% lower bound
upper_95 <- as.numeric(fc_result$upper[, 2])   # 95% upper bound

# Create forecast dates (next business days)
last_date <- max(dates)
forecast_dates <- seq(from = last_date + 1, by = "day", length.out = n_days)
# Filter to weekdays only (remove weekends)
forecast_dates <- forecast_dates[weekdays(forecast_dates) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
# Adjust n_days to match actual business days
n_days <- min(n_days, length(forecast_dates))
forecast_dates <- forecast_dates[1:n_days]
forecasts <- forecasts[1:n_days]
lower_80 <- lower_80[1:n_days]
upper_80 <- upper_80[1:n_days]
lower_95 <- lower_95[1:n_days]
upper_95 <- upper_95[1:n_days]

# Create forecast data frame with prediction intervals
forecast_df <- tibble(
  date = forecast_dates,
  forecast = forecasts,
  lower_80 = lower_80,
  upper_80 = upper_80,
  lower_95 = lower_95,
  upper_95 = upper_95,
  type = "forecast"
)

# Get the last few actual values for context
last_n <- 20
actual_df <- tibble(
  date = tail(dates, last_n),
  value = tail(full_series, last_n),
  type = "actual"
)

# Combine for plotting
plot_data <- bind_rows(
  actual_df %>% rename(forecast = value),
  forecast_df
)

# ============================================================================
# Analysis and Summary
# ============================================================================

message("\n=== Forecast Summary ===")
message("Forecasting ", length(forecasts), " days ahead")
message("First forecast date: ", min(forecast_dates))
message("Last forecast date: ", max(forecast_dates))

message("\nForecast Statistics:")
message("  Mean forecast: ", round(mean(forecasts), 6))
message("  Std dev: ", round(sd(forecasts), 6))
message("  Min: ", round(min(forecasts), 6))
message("  Max: ", round(max(forecasts), 6))

# Calculate cumulative return forecast
cumulative_return <- sum(forecasts)
message("\nCumulative return forecast (", length(forecasts), " days): ", 
        round(cumulative_return * 100, 2), "%")

# Directional forecast (positive/negative days)
positive_days <- sum(forecasts > 0)
negative_days <- sum(forecasts < 0)
message("  Positive days: ", positive_days, " (", 
        round(100 * positive_days / length(forecasts), 1), "%)")
message("  Negative days: ", negative_days, " (", 
        round(100 * negative_days / length(forecasts), 1), "%)")

# ============================================================================
# Visualization
# ============================================================================

message("\n=== Generating Forecast Plot ===")

# Create forecast plot with prediction intervals
p <- ggplot() +
  # Prediction intervals (95% and 80%)
  geom_ribbon(data = forecast_df, 
              aes(x = date, ymin = lower_95, ymax = upper_95), 
              fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_ribbon(data = forecast_df, 
              aes(x = date, ymin = lower_80, ymax = upper_80), 
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
  # Actual data
  geom_line(data = actual_df, 
            aes(x = date, y = value), 
            color = "steelblue", linewidth = 1) +
  geom_point(data = actual_df, 
             aes(x = date, y = value), 
             color = "steelblue", size = 2) +
  # Forecasts
  geom_line(data = forecast_df, 
            aes(x = date, y = forecast), 
            color = "red", linewidth = 1, linetype = "dashed") +
  geom_point(data = forecast_df, 
             aes(x = date, y = forecast), 
             color = "red", size = 2) +
  # Zero line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
  labs(
    title = "SP500 Day-by-Day Return Forecasts (ARIMA Model)",
    subtitle = paste("Forecasting", length(forecasts), "days ahead from", last_date, 
                     "| Shaded areas: 80% (darker) and 95% (lighter) prediction intervals"),
    x = "Date",
    y = "Daily Return",
    caption = "Blue: Actual Returns | Red: Forecast Returns with Prediction Intervals"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 9, color = "gray50")
  )

# Save plot
output_file <- "outputs/sp500_forecast.png"
ggsave(output_file, p, width = 14, height = 8, dpi = 300)
message("  Forecast plot saved to: ", output_file)

# ============================================================================
# Save Forecasts to CSV
# ============================================================================

forecast_output <- forecast_df %>%
  mutate(
    forecast_return = forecast,
    lower_80 = lower_80,
    upper_80 = upper_80,
    lower_95 = lower_95,
    upper_95 = upper_95,
    cumulative_return = cumsum(forecast),
    direction = ifelse(forecast > 0, "Up", "Down")
  ) %>%
  select(date, forecast_return, lower_80, upper_80, lower_95, upper_95, 
         cumulative_return, direction)

csv_file <- "outputs/sp500_forecasts.csv"
write_csv(forecast_output, csv_file)
message("  Forecasts saved to: ", csv_file)

# Print first few forecasts
message("\nFirst 10 Day Forecasts:")
print(head(forecast_output, 10))

message("\n=== Forecasting Complete ===")
message("Forecasts available in: ", csv_file)
message("Visualization available in: ", output_file)

# Store results in global environment for interactive use
if (interactive()) {
  forecast_results <<- list(
    model = arima_fit,
    forecasts = forecast_output,
    plot = p,
    prep_result = prep_result
  )
  message("\nResults stored in 'forecast_results' object")
}

