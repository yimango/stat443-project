#!/usr/bin/env Rscript
# Comprehensive Analysis of SP500 Day-by-Day Forecasts

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
})

message("=== SP500 Forecast Analysis ===")

# Load forecasts
forecasts <- read_csv("outputs/sp500_forecasts.csv", show_col_types = FALSE)

message("\nForecast Period: ", min(forecasts$date), " to ", max(forecasts$date))
message("Total forecast days: ", nrow(forecasts))

# ============================================================================
# 1. Forecast Statistics
# ============================================================================

message("\n=== Forecast Statistics ===")
message("Mean daily return forecast: ", round(mean(forecasts$forecast_return) * 100, 3), "%")
message("Std dev: ", round(sd(forecasts$forecast_return) * 100, 3), "%")
message("Min forecast: ", round(min(forecasts$forecast_return) * 100, 3), "%")
message("Max forecast: ", round(max(forecasts$forecast_return) * 100, 3), "%")

# Volatility estimate (from prediction intervals)
avg_volatility <- mean((forecasts$upper_95 - forecasts$lower_95) / 4)  # Approximate
message("Estimated daily volatility: ", round(avg_volatility * 100, 3), "%")

# ============================================================================
# 2. Directional Analysis
# ============================================================================

message("\n=== Directional Forecast Analysis ===")
up_days <- sum(forecasts$direction == "Up")
down_days <- sum(forecasts$direction == "Down")
message("Up days: ", up_days, " (", round(100 * up_days / nrow(forecasts), 1), "%)")
message("Down days: ", down_days, " (", round(100 * down_days / nrow(forecasts), 1), "%)")

# Expected return by direction
up_returns <- forecasts %>% filter(direction == "Up") %>% pull(forecast_return)
down_returns <- forecasts %>% filter(direction == "Down") %>% pull(forecast_return)
message("Average up day return: ", round(mean(up_returns) * 100, 3), "%")
message("Average down day return: ", round(mean(down_returns) * 100, 3), "%")

# ============================================================================
# 3. Cumulative Return Analysis
# ============================================================================

message("\n=== Cumulative Return Analysis ===")
final_cumulative <- tail(forecasts$cumulative_return, 1)
message("Total cumulative return forecast: ", round(final_cumulative * 100, 2), "%")
message("Annualized return (approx): ", round(final_cumulative * 100 * (252 / nrow(forecasts)), 2), "%")

# Best and worst cumulative returns
max_cumulative <- max(forecasts$cumulative_return)
min_cumulative <- min(forecasts$cumulative_return)
max_date <- forecasts$date[which.max(forecasts$cumulative_return)]
min_date <- forecasts$date[which.min(forecasts$cumulative_return)]
message("Peak cumulative return: ", round(max_cumulative * 100, 2), "% on ", max_date)
message("Lowest cumulative return: ", round(min_cumulative * 100, 2), "% on ", min_date)

# ============================================================================
# 4. Risk Analysis (Prediction Intervals)
# ============================================================================

message("\n=== Risk Analysis (Prediction Intervals) ===")

# Calculate how often forecasts are outside intervals (should be rare if model is good)
# For 95% interval, expect ~5% outside; for 80%, expect ~20%

# Calculate potential losses (worst case scenarios)
worst_case_95 <- min(forecasts$lower_95)
worst_case_80 <- min(forecasts$lower_80)
message("Worst case (95% interval): ", round(worst_case_95 * 100, 3), "% daily return")
message("Worst case (80% interval): ", round(worst_case_80 * 100, 3), "% daily return")

# Calculate potential gains (best case scenarios)
best_case_95 <- max(forecasts$upper_95)
best_case_80 <- max(forecasts$upper_80)
message("Best case (95% interval): ", round(best_case_95 * 100, 3), "% daily return")
message("Best case (80% interval): ", round(best_case_80 * 100, 3), "% daily return")

# Average interval width (uncertainty measure)
avg_width_95 <- mean(forecasts$upper_95 - forecasts$lower_95)
avg_width_80 <- mean(forecasts$upper_80 - forecasts$lower_80)
message("Average 95% interval width: ", round(avg_width_95 * 100, 3), "%")
message("Average 80% interval width: ", round(avg_width_80 * 100, 3), "%")

# ============================================================================
# 5. Weekly Summary
# ============================================================================

message("\n=== Weekly Forecast Summary ===")
forecasts_weekly <- forecasts %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(
    n_days = n(),
    avg_return = mean(forecast_return),
    cumulative_return = sum(forecast_return),
    up_days = sum(direction == "Up"),
    down_days = sum(direction == "Down"),
    .groups = "drop"
  )

print(forecasts_weekly)

# ============================================================================
# 6. Visualization
# ============================================================================

message("\n=== Generating Analysis Plots ===")

# Plot 1: Daily forecasts with intervals
p1 <- forecasts %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "red", alpha = 0.1) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "red", alpha = 0.2) +
  geom_line(aes(y = forecast_return), color = "red", linewidth = 1) +
  geom_point(aes(y = forecast_return, color = direction), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Up" = "green", "Down" = "red")) +
  labs(
    title = "SP500 Daily Return Forecasts with Prediction Intervals",
    x = "Date",
    y = "Daily Return (%)",
    color = "Direction"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: Cumulative returns
p2 <- forecasts %>%
  ggplot(aes(x = date, y = cumulative_return)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Cumulative Return Forecast",
    x = "Date",
    y = "Cumulative Return (%)"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

# Plot 3: Direction distribution
p3 <- forecasts %>%
  count(direction) %>%
  ggplot(aes(x = direction, y = n, fill = direction)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("Up" = "green", "Down" = "red")) +
  labs(
    title = "Forecast Direction Distribution",
    x = "Direction",
    y = "Number of Days"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 4: Forecast distribution
p4 <- forecasts %>%
  ggplot(aes(x = forecast_return)) +
  geom_histogram(bins = 15, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(forecasts$forecast_return), 
             linetype = "dashed", color = "green") +
  labs(
    title = "Distribution of Forecast Returns",
    x = "Daily Return (%)",
    y = "Frequency"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_minimal()

# Combine plots
combined_plot <- (p1 / p2) | (p3 / p4)
combined_plot <- combined_plot + 
  plot_annotation(
    title = "SP500 Day-by-Day Forecast Analysis",
    subtitle = paste("Forecast period:", min(forecasts$date), "to", max(forecasts$date)),
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave("outputs/forecast_analysis.png", combined_plot, width = 16, height = 10, dpi = 300)
message("  Analysis plot saved to: outputs/forecast_analysis.png")

# ============================================================================
# 7. Trading Strategy Implications
# ============================================================================

message("\n=== Trading Strategy Implications ===")

# Simple strategy: buy on positive forecast, sell on negative
# (This is simplified - real strategies need more considerations)

positive_forecasts <- forecasts %>% filter(forecast_return > 0)
negative_forecasts <- forecasts %>% filter(forecast_return < 0)

message("Days with positive forecast: ", nrow(positive_forecasts))
message("  Average positive forecast: ", round(mean(positive_forecasts$forecast_return) * 100, 3), "%")
message("Days with negative forecast: ", nrow(negative_forecasts))
message("  Average negative forecast: ", round(mean(negative_forecasts$forecast_return) * 100, 3), "%")

# Expected return if following forecasts
expected_return <- sum(forecasts$forecast_return)
message("\nExpected total return (if following forecasts): ", 
        round(expected_return * 100, 2), "%")

message("\n=== Analysis Complete ===")
message("All outputs saved to outputs/ directory")

