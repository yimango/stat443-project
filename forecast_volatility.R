#!/usr/bin/env Rscript
# Volatility Forecasting Script
# Uses GARCH and ARCH models to forecast SP500 volatility

suppressPackageStartupMessages({
  library(tidyverse)
  library(rugarch)
  library(tseries)
})

# ============================================================================
# Load Data and Models
# ============================================================================

message("=== SP500 Volatility Forecasting ===")
message("Loading data and volatility models...")

# Source necessary files
source("DataPrep/ts_prep.R", local = TRUE)
source("Models/garch_model.R", local = TRUE)
source("Models/arch_model.R", local = TRUE)

# Load the data
prep_result <- prepare_ts_data(
  file_path = NULL,
  target_col = "spy_close"
)

# Get the full time series (returns)
full_series <- prep_result$data$target_series
full_series <- full_series[!is.na(full_series)]
dates <- prep_result$data$date[!is.na(prep_result$data$target_series)]

message("  Data loaded: ", length(full_series), " observations")
message("  Date range: ", min(dates), " to ", max(dates))

# ============================================================================
# Fit Volatility Models
# ============================================================================

message("\n=== Fitting Volatility Models ===")

# Fit GARCH model
message("\nFitting GARCH model...")
garch_fit <- fit_garch(full_series, max_p = 2, max_q = 2)
message("  Selected GARCH(", garch_fit$p, ",", garch_fit$q, ")")
message("  AIC: ", round(garch_fit$aic, 2), ", BIC: ", round(garch_fit$bic, 2))

# Fit ARCH model
message("\nFitting ARCH model...")
arch_fit <- fit_arch(full_series, max_q = 5)
message("  Selected ARCH(", arch_fit$q, ")")
message("  AIC: ", round(arch_fit$aic, 2), ", BIC: ", round(arch_fit$bic, 2))

# Compare models
if (garch_fit$bic < arch_fit$bic) {
  best_model <- "GARCH"
  best_fit <- garch_fit
  message("\n  Best model: GARCH (lower BIC)")
} else {
  best_model <- "ARCH"
  best_fit <- arch_fit
  message("\n  Best model: ARCH (lower BIC)")
}

# ============================================================================
# Generate Volatility Forecasts
# ============================================================================

message("\n=== Generating Volatility Forecasts ===")

# Number of days to forecast ahead
n_days <- 30

# Create forecast dates (next business days) first
last_date <- max(dates)
forecast_dates <- seq(from = last_date + 1, by = "day", length.out = n_days)
forecast_dates <- forecast_dates[weekdays(forecast_dates) %in% 
                                 c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
n_days_actual <- min(n_days, length(forecast_dates))
forecast_dates <- forecast_dates[1:n_days_actual]

# Generate forecasts from both models (for the actual number of business days)
garch_forecasts <- forecast_garch_volatility(garch_fit, h = n_days_actual)
arch_forecasts <- forecast_arch_volatility(arch_fit, h = n_days_actual)
best_forecasts <- if (best_model == "GARCH") garch_forecasts else arch_forecasts

# Ensure forecasts match dates length
garch_forecasts <- garch_forecasts[1:n_days_actual]
arch_forecasts <- arch_forecasts[1:n_days_actual]
best_forecasts <- best_forecasts[1:n_days_actual]

# Get recent historical volatility for context
recent_vol <- tail(garch_fit$fitted_volatility, 20)
recent_dates <- tail(dates, 20)

# Create forecast data frames
garch_df <- tibble(
  date = forecast_dates,
  forecast_volatility = garch_forecasts,
  model = "GARCH"
)

arch_df <- tibble(
  date = forecast_dates,
  forecast_volatility = arch_forecasts,
  model = "ARCH"
)

best_df <- tibble(
  date = forecast_dates,
  forecast_volatility = best_forecasts,
  model = best_model
)

# Historical volatility for plotting
historical_df <- tibble(
  date = recent_dates,
  volatility = recent_vol,
  type = "historical"
)

# ============================================================================
# Analysis and Summary
# ============================================================================

message("\n=== Volatility Forecast Summary ===")
message("Forecasting ", n_days_actual, " business days ahead")
message("First forecast date: ", min(forecast_dates))
message("Last forecast date: ", max(forecast_dates))

message("\nGARCH Model Forecasts:")
message("  Mean volatility: ", round(mean(garch_forecasts) * 100, 3), "%")
message("  Std dev: ", round(sd(garch_forecasts) * 100, 3), "%")
message("  Min: ", round(min(garch_forecasts) * 100, 3), "%")
message("  Max: ", round(max(garch_forecasts) * 100, 3), "%")

message("\nARCH Model Forecasts:")
message("  Mean volatility: ", round(mean(arch_forecasts) * 100, 3), "%")
message("  Std dev: ", round(sd(arch_forecasts) * 100, 3), "%")
message("  Min: ", round(min(arch_forecasts) * 100, 3), "%")
message("  Max: ", round(max(arch_forecasts) * 100, 3), "%")

message("\nRecent Historical Volatility (last 20 days):")
message("  Mean: ", round(mean(recent_vol) * 100, 3), "%")
message("  Current: ", round(tail(recent_vol, 1) * 100, 3), "%")

# Compare to historical
forecast_mean <- mean(best_forecasts)
historical_mean <- mean(recent_vol)
vol_change <- ((forecast_mean - historical_mean) / historical_mean) * 100

message("\nForecast vs Historical:")
message("  Forecast mean: ", round(forecast_mean * 100, 3), "%")
message("  Historical mean: ", round(historical_mean * 100, 3), "%")
message("  Change: ", round(vol_change, 1), "%")

# ============================================================================
# Visualization
# ============================================================================

message("\n=== Generating Volatility Forecast Plot ===")

# Combine all forecasts
all_forecasts <- bind_rows(
  garch_df %>% mutate(type = "forecast"),
  arch_df %>% mutate(type = "forecast"),
  best_df %>% mutate(type = "best_forecast")
)

# Create plot
p <- ggplot() +
  # Historical volatility
  geom_line(data = historical_df, 
            aes(x = date, y = volatility), 
            color = "steelblue", linewidth = 1, alpha = 0.7) +
  geom_point(data = historical_df, 
             aes(x = date, y = volatility), 
             color = "steelblue", size = 2) +
  # GARCH forecasts
  geom_line(data = garch_df, 
            aes(x = date, y = forecast_volatility, color = "GARCH"), 
            linewidth = 1, linetype = "dashed") +
  geom_point(data = garch_df, 
             aes(x = date, y = forecast_volatility, color = "GARCH"), 
             size = 2) +
  # ARCH forecasts
  geom_line(data = arch_df, 
            aes(x = date, y = forecast_volatility, color = "ARCH"), 
            linewidth = 1, linetype = "dashed") +
  geom_point(data = arch_df, 
             aes(x = date, y = forecast_volatility, color = "ARCH"), 
             size = 2) +
  # Best model (highlighted)
  geom_line(data = best_df, 
            aes(x = date, y = forecast_volatility, color = "Best Model"), 
            linewidth = 1.5, linetype = "solid") +
  scale_color_manual(
    values = c("GARCH" = "red", "ARCH" = "orange", "Best Model" = "darkgreen"),
    name = "Model"
  ) +
  labs(
    title = "SP500 Volatility Forecasts (GARCH vs ARCH)",
    subtitle = paste("Forecasting", n_days_actual, "days ahead from", last_date,
                     "| Best model:", best_model),
    x = "Date",
    y = "Conditional Volatility (Daily)",
    caption = paste("Blue: Historical Volatility | Red: GARCH Forecast | Orange: ARCH Forecast | Green: Best Model (", best_model, ")")
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

# Save plot
output_file <- "outputs/volatility_forecast.png"
ggsave(output_file, p, width = 16, height = 8, dpi = 300)
message("  Volatility forecast plot saved to: ", output_file)

# ============================================================================
# Save Forecasts to CSV
# ============================================================================

forecast_output <- best_df %>%
  mutate(
    garch_forecast = garch_forecasts,
    arch_forecast = arch_forecasts,
    best_forecast = forecast_volatility,
    best_model = best_model,
    historical_mean = historical_mean,
    forecast_change_pct = ((forecast_volatility - historical_mean) / historical_mean) * 100
  ) %>%
  select(date, best_model, best_forecast, garch_forecast, arch_forecast, 
         historical_mean, forecast_change_pct)

csv_file <- "outputs/volatility_forecasts.csv"
write_csv(forecast_output, csv_file)
message("  Forecasts saved to: ", csv_file)

# Print first few forecasts
message("\nFirst 10 Day Volatility Forecasts:")
print(head(forecast_output, 10))

message("\n=== Volatility Forecasting Complete ===")
message("Forecasts available in: ", csv_file)
message("Visualization available in: ", output_file)

# Return forecast data
if (interactive()) {
  volatility_results <<- list(
    garch_model = garch_fit,
    arch_model = arch_fit,
    best_model = best_model,
    forecasts = forecast_output,
    plot = p,
    prep_result = prep_result
  )
  message("\nResults stored in 'volatility_results' object")
}

