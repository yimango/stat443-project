# SP500 Time-Series Forecasting Project

## Project Overview

This project implements a comprehensive time-series forecasting pipeline for SP500 daily returns using AR, MA, and ARIMA models following the Box-Jenkins methodology. The pipeline includes proper data preparation, model selection, backtesting, evaluation, and day-by-day forecasting.

## Project Structure

```
stat443-project/
├── DataPrep/
│   ├── ts_prep.R              # Time-series data preparation and stationarity testing
│   └── model_pipeline.R       # Data loading and feature engineering
├── Models/
│   ├── ar_model.R             # Autoregressive (AR) model implementation
│   ├── ma_model.R             # Moving Average (MA) model implementation
│   └── arima_model.R          # ARIMA model implementation (Box-Jenkins)
├── Evaluation/
│   ├── backtest.R             # Rolling-origin backtesting framework
│   └── evaluate_models.R      # Model evaluation, metrics, and comparisons
├── main_models.R              # Main pipeline orchestrator
├── forecast_sp500.R           # Day-by-day forecasting script
├── analyze_forecasts.R        # Forecast analysis and visualization
├── r_ready_data/              # Input data files (CSV format)
│   ├── SPY_US_Equity.csv
│   ├── VIX_Index.csv
│   ├── UX1_Index.csv
│   ├── USGG10YR_Index.csv
│   └── USOHHYTO_Index.csv
└── outputs/                   # Generated outputs
    ├── Diagnostic plots
    ├── Forecast plots
    ├── Metrics CSV files
    └── Forecast results
```

## Methodology

### 1. Data Preparation (`DataPrep/ts_prep.R` and `DataPrep/model_pipeline.R`)

**Target Series Creation:**
- Loads SP500 (SPY) price data along with VIX, yield curve, and other market indicators
- Creates log returns: `r_t = log(P_t) - log(P_{t-1})`
- Tests stationarity using ADF and KPSS tests
- Uses returns directly (d=0) since they are already stationary

**Feature Engineering:**
- Creates 36 potential predictors across multiple categories:
  - **Basic Changes**: dVIX, slope, dSlope, d10Y, dHY
  - **Volatility Features**: realized_vol_5, realized_vol_20, vix_level, vix_ma, vix_dev
  - **Volume Features**: vol_surp, vol_ma_ratio
  - **Price/Technical Features**: range, ma_5, ma_20, ma_50, price_ma5_ratio, price_ma20_ratio, ma_cross, rsi
  - **Momentum Features**: R, R_lag1, R_lag2, R_lag3, R_lag5, cumret_5, cumret_20
  - **Cross-Asset Features**: vix_realized_ratio, yield_level, yield_ma, yield_dev, hy_level, hy_ma, hy_dev
  - **Interaction Features**: vix_r_interaction, vol_vol_interaction
- All features use only information up to time t (no future leakage)
- Drops predictors with >20% missing values

**Variable Selection (Elastic Net Regularization):**

**Method**: Elastic Net combines L1 (Lasso) and L2 (Ridge) regularization:
- Penalty: `α * ||β||₁ + (1-α) * ||β||₂²`
- α controls the mix: α=1 is pure Lasso, α=0 is pure Ridge

**Selection Process**:
1. **Normalization**: Z-score normalization (mean=0, SD=1) for all predictors
2. **Grid Search**: Tests α values: [0.1, 0.3, 0.5, 0.7, 0.9]
3. **Cross-Validation**: Time-safe 5-fold CV to select optimal λ for each α
4. **Selection Metric**: **RMSE** (Root Mean Squared Error) - minimizes prediction error
5. **Stability Score**: Fraction of CV folds where coefficient ≠ 0
   - Measures predictor reliability across different time periods
   - Higher stability = more consistent predictor
6. **Final Selection**: Predictors with stability ≥ 0.6

**Selected Predictors** (8 out of 36):
- `realized_vol_20` (stability: 0.6) - 20-day realized volatility
- `rsi` (stability: 0.8) - Relative Strength Index
- `R_lag5` (stability: 1.0) - 5-day lagged return
- `cumret_5` (stability: 1.0) - 5-day cumulative return
- `vix_realized_ratio` (stability: 1.0) - VIX to realized volatility ratio
- `hy_level` (stability: 1.0) - High-yield credit spread level
- `hy_dev` (stability: 0.8) - High-yield spread deviation from mean
- `vol_vol_interaction` (stability: 0.6) - Volume × volatility interaction

**Optimal Hyperparameters**:
- **α = 0.1** (closer to Ridge, prefers grouped selection)
- **λ = 0.00378** (regularization strength)
- **CV RMSE = 0.005036**

**Stationarity Testing for Predictors**:
- All predictors tested for stationarity using ADF and KPSS tests
- Tests performed on both level and differenced transformations
- Results saved to `outputs/stationarity_report.csv`
- Non-stationary predictors are differenced before use

**Why These Metrics?**
- **RMSE**: Standard metric for regression, penalizes large errors more
- **Stability Score**: Ensures predictors are robust across time periods (important for financial data)
- **Elastic Net**: Handles correlated predictors better than pure Lasso, selects relevant groups
- **Cross-Validation**: Time-safe CV ensures no future data leakage during selection

**Time-Safe Splits:**
- Training: 80% of data (2015-10-01 to 2023-10-23)
- Test: 20% of data (2023-10-24 to 2025-10-30)
- Creates rolling-origin folds for proper backtesting (no future data leakage)

### 2. Model Implementation (Box-Jenkins Methodology)

**AR Model (`Models/ar_model.R`):**
- Autoregressive model: `X_t = φ₁X_{t-1} + ... + φₚX_{t-p} + ε_t`
- Model selection: Tests orders p = 1 to 8 using AIC/BIC
- Uses PACF for initial order identification

**MA Model (`Models/ma_model.R`):**
- Moving Average model: `X_t = ε_t + θ₁ε_{t-1} + ... + θ_qε_{t-q}`
- Model selection: Tests orders q = 1 to 8 using AIC/BIC
- Uses ACF for initial order identification

**ARIMA Model (`Models/arima_model.R`):**
- ARIMA(p,d,q) model combining AR and MA components
- Box-Jenkins methodology:
  1. **Identification**: Uses ACF/PACF plots and stationarity tests
  2. **Estimation**: Grid search over p ∈ [0,5], q ∈ [0,5] with AIC/BIC
  3. **Diagnostic Checking**: Ljung-Box test for residual autocorrelation, Jarque-Bera for normality

### 3. Backtesting (`Evaluation/backtest.R`)

**Rolling-Origin Backtesting:**
- For each test point:
  1. Use only data up to that point (no future leakage)
  2. Fit model on expanding training window
  3. Generate 1-step-ahead forecast
  4. Compare to actual value
- Total: 528 test folds for each model

**Why This Matters:**
- Simulates real-world forecasting conditions
- Ensures no look-ahead bias
- Provides realistic performance estimates

### 4. Model Evaluation (`Evaluation/evaluate_models.R`)

**Metrics Computed:**
- **RMSE** (Root Mean Squared Error): Overall forecast accuracy
- **MAE** (Mean Absolute Error): Average forecast error magnitude
- **MAPE** (Mean Absolute Percentage Error): Percentage error
- **Directional Accuracy**: % of correct direction predictions
- **Diebold-Mariano Test**: Statistical comparison between models

**Visualizations:**
- Forecast vs Actual plots
- Residual diagnostics
- Error distributions
- Cumulative error (pseudo-PnL) plots
- Model comparison charts

## Model Selection Results

### Performance Comparison

| Model | RMSE | MAE | MAPE | Directional Accuracy |
|-------|------|-----|------|---------------------|
| **ARIMA** | **0.0100** | 0.00655 | 6439% | 49.6% |
| AR | 0.0101 | 0.00653 | 6969% | 51.1% |
| MA | 0.0101 | 0.00640 | 2838% | **53.4%** |

### Selected Model: ARIMA(2,0,2)

**Why ARIMA?**
- Lowest RMSE (best overall forecast accuracy)
- Combines benefits of both AR and MA components
- Better captures complex return dynamics
- Model diagnostics acceptable (some residual autocorrelation, but within acceptable range)

**Model Characteristics:**
- AIC: -16,287.1
- BIC: -16,251.85
- Differencing: d = 0 (returns already stationary)
- AR order: p = 2
- MA order: q = 2

## Day-by-Day Forecasting

### Implementation (`forecast_sp500.R`)

**Process:**
1. Load full historical data (2015-10-01 to 2025-10-30)
2. Fit ARIMA(2,0,2) on complete dataset
3. Generate multi-step ahead forecasts (30 days)
4. Include prediction intervals (80% and 95%)
5. Create visualizations and export results

### Forecast Results (Oct 31 - Nov 28, 2025)

**Summary Statistics:**
- **Forecast Period**: 21 business days
- **Cumulative Return**: +1.12%
- **Annualized Return**: ~13.4%
- **Mean Daily Return**: +0.053%
- **Daily Volatility**: ~1.09%

**Directional Forecast:**
- Up days: 17 (81%)
- Down days: 4 (19%)
- Average up day: +0.073%
- Average down day: -0.033%

**Risk Assessment:**
- 95% Prediction Interval: -2.21% to +2.34% daily
- 80% Prediction Interval: -1.46% to +1.59% daily
- Average uncertainty (95% interval width): 4.36%

**Weekly Breakdown:**
- Week 1: +0.19% (1 day)
- Week 2: +0.23% (4 up, 1 down)
- Week 3: +0.20% (3 up, 2 down)
- Week 4: +0.23% (4 up, 1 down)
- Week 5: +0.27% (5 up, 0 down)

## Key Findings

### 1. Variable Selection Results
- **36 predictors** created from market data (VIX, yields, volatility, momentum, etc.)
- **8 predictors selected** using Elastic Net (22% selection rate)
- **High stability predictors** (stability = 1.0): R_lag5, cumret_5, vix_realized_ratio, hy_level
- **Selected predictors represent**:
  - Momentum (lagged returns, cumulative returns)
  - Volatility (realized volatility, VIX ratios)
  - Credit conditions (high-yield spreads)
  - Technical indicators (RSI)
- **Optimal regularization**: α = 0.1 (Ridge-like, groups correlated features)

### 2. Model Performance
- All three models (AR, MA, ARIMA) show similar RMSE (~0.010)
- ARIMA slightly outperforms in overall accuracy
- MA has best directional accuracy (53.4%)
- Models struggle with high MAPE due to small return magnitudes

### 2. Forecast Characteristics
- **Bullish Short-Term Outlook**: 81% of forecast days are positive
- **Low Volatility Environment**: Average daily moves around 0.05-0.07%
- **Consistent Growth Pattern**: Cumulative return increases steadily
- **Moderate Uncertainty**: Prediction intervals show reasonable confidence

### 3. Model Diagnostics
- **ARIMA Residuals**: Some autocorrelation detected (Ljung-Box p < 0.05)
- **Non-Normal Residuals**: Jarque-Bera test rejects normality (common in financial returns)
- **Stationarity**: Returns confirmed stationary (ADF p < 0.05, KPSS p > 0.05)

### 4. Limitations
- **Short-Term Focus**: Models designed for 1-step-ahead forecasts
- **No Exogenous Variables**: Pure time-series approach (could add VIX, yields, etc.)
- **Assumes Stationarity**: May not capture structural breaks or regime changes
- **High MAPE**: Percentage errors large due to small return values

## How to Run

### Prerequisites
```r
# Required R packages
install.packages(c("tidyverse", "forecast", "tseries", "patchwork", 
                   "zoo", "glmnet", "KFAS", "yardstick"))
```

### Main Pipeline
```r
# Run complete modeling pipeline
source("main_models.R")
result <- main()
```

This will:
1. Prepare data and test stationarity
2. Fit AR, MA, and ARIMA models
3. Run diagnostics
4. Perform rolling-origin backtesting
5. Evaluate and compare models
6. Generate all plots and metrics

### Generate Forecasts
```r
# Generate day-by-day SP500 forecasts
source("forecast_sp500.R")
```

### Analyze Forecasts
```r
# Comprehensive forecast analysis
source("analyze_forecasts.R")
```

## Output Files

### Diagnostic Plots
- `ar_diagnostics.png` - AR model residual diagnostics
- `ma_diagnostics.png` - MA model residual diagnostics
- `arima_diagnostics.png` - ARIMA model residual diagnostics

### Evaluation Plots
- `forecast_comparison.png` - Forecast vs actual for all models
- `residual_diagnostics.png` - Residual analysis across models
- `error_comparison.png` - Error distribution comparison
- `cumulative_errors.png` - Cumulative forecast errors (pseudo-PnL)
- `metrics_summary.png` - Model performance metrics visualization

### Forecast Outputs
- `sp500_forecast.png` - Day-by-day forecast visualization
- `forecast_analysis.png` - Comprehensive forecast analysis
- `sp500_forecasts.csv` - Detailed forecast table with prediction intervals

### Metrics Files
- `model_comparison_metrics.csv` - Detailed metrics for each model
- `model_comparisons.csv` - Diebold-Mariano test results
- `oos_metrics.csv` - Out-of-sample performance metrics
- `stationarity_report.csv` - Stationarity test results for all 36 predictors (ADF and KPSS tests on level and differenced series)

## Technical Details

### Data
- **Frequency**: Daily
- **Period**: 2015-09-30 to 2025-10-30
- **Total Observations**: 2,631 (after removing first NA return)
- **Target**: SP500 log returns (stationary, d=0)

### Model Selection Criteria
- **AIC** (Akaike Information Criterion): Balances fit and complexity
- **BIC** (Bayesian Information Criterion): Stronger penalty for complexity
- **Grid Search**: Exhaustive search over parameter space
- **Information Criteria**: Lower is better

### Backtesting Details
- **Method**: Rolling-origin (expanding window)
- **Forecast Horizon**: 1-step-ahead
- **Test Folds**: 528 (one per test observation)
- **No Future Leakage**: Each fold uses only past data

### Statistical Tests
- **ADF Test**: Augmented Dickey-Fuller test for unit roots
- **KPSS Test**: Kwiatkowski-Phillips-Schmidt-Shin test for stationarity
- **Ljung-Box Test**: Tests residual autocorrelation
- **Jarque-Bera Test**: Tests residual normality
- **Diebold-Mariano Test**: Compares forecast accuracy between models

### Variable Selection Details
- **Method**: Elastic Net Regularization (L1 + L2 penalty)
- **Selection Metric**: RMSE via cross-validation
- **Stability Metric**: Fraction of CV folds where coefficient ≠ 0
- **Selection Threshold**: Stability ≥ 0.6
- **Normalization**: Z-score (mean=0, SD=1) before regularization
- **Grid Search**: α ∈ [0.1, 0.3, 0.5, 0.7, 0.9]
- **CV Method**: Time-safe 5-fold cross-validation
- **Final Selection**: 8 predictors from 36 candidates

## Future Enhancements

1. **ARIMAX Models**: Include exogenous variables (VIX, yields, etc.)
2. **GARCH Models**: Model volatility clustering
3. **Regime-Switching**: Handle structural breaks
4. **Ensemble Methods**: Combine multiple models
5. **Machine Learning**: LSTM, XGBoost for comparison
6. **Real-Time Updates**: Automate daily forecast generation

## References

- Box, G. E. P., & Jenkins, G. M. (1976). *Time Series Analysis: Forecasting and Control*
- Hyndman, R. J., & Athanasopoulos, G. (2021). *Forecasting: principles and practice*
- Diebold, F. X., & Mariano, R. S. (1995). Comparing predictive accuracy

## Notes for Report Writing

### Key Sections to Include:

1. **Introduction**
   - SP500 forecasting importance
   - Time-series modeling approach
   - Box-Jenkins methodology

2. **Data and Methodology**
   - Data description and preprocessing
   - Stationarity testing
   - Model specifications (AR, MA, ARIMA)
   - Backtesting procedure

3. **Results**
   - Model selection (AIC/BIC comparison)
   - Out-of-sample performance metrics
   - Diagnostic checking results
   - Model comparison (Diebold-Mariano tests)

4. **Forecasting**
   - Day-by-day forecast generation
   - Prediction intervals
   - Forecast analysis and interpretation

5. **Discussion**
   - Model strengths and limitations
   - Forecast interpretation
   - Practical implications
   - Future improvements

6. **Conclusion**
   - Summary of findings
   - Model recommendations
   - Forecasting insights

### Important Numbers to Highlight:
- **Variable Selection**: 8 predictors selected from 36 candidates (22% selection rate)
- **Elastic Net**: α = 0.1, λ = 0.00378, CV RMSE = 0.005036
- **ARIMA(2,0,2)** selected model
- **RMSE**: 0.0100 (best among models)
- **528 backtest folds** per model
- **1.12% cumulative return** forecast (21 days)
- **81% positive day** forecast rate
- **4.36% average prediction interval** width

---

**Last Updated**: November 2025  
**Model Version**: ARIMA(2,0,2)  
**Forecast Period**: Oct 31 - Nov 28, 2025

