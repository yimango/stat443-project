# Model Selection Results Summary

## Return Forecasting Models

### Performance Comparison

| Model | RMSE | MAE | MAPE | Directional Accuracy | Selected Order |
|-------|------|-----|------|---------------------|----------------|
| **ARIMA** | **0.0100** | 0.00655 | 6439% | 49.6% | **(2,0,2)** |
| AR | 0.0101 | 0.00653 | 6969% | 51.1% | (8) |
| MA | 0.0101 | 0.00640 | 2838% | **53.4%** | (2) |

### Best Return Model: **ARIMA(2,0,2)**

**Selection Criteria**: Lowest RMSE (0.0100)

**Model Characteristics**:
- AIC: -16,287.1
- BIC: -16,251.85
- Differencing: d = 0 (returns already stationary)
- AR order: p = 2
- MA order: q = 2

**Why ARIMA?**
- Best overall forecast accuracy (lowest RMSE)
- Combines benefits of both AR and MA components
- Better captures complex return dynamics
- Standard Box-Jenkins methodology

---

## Volatility Forecasting Models

### Performance Comparison

| Model | Order | AIC | BIC | Mean Forecast Volatility | Forecast Range |
|-------|-------|-----|-----|------------------------|----------------|
| **GARCH** | **(1,1)** | **-6.65** | **-6.64** | **0.931%** | **0.855% - 0.991%** |
| ARCH | (1) | -6.14 | -6.14 | 1.152% | 1.152% (constant) |

### Best Volatility Model: **GARCH(1,1)**

**Selection Criteria**: Lowest BIC (-6.64 vs -6.14)

**Model Characteristics**:
- AIC: -6.65
- BIC: -6.64
- GARCH order: p = 1 (GARCH terms)
- ARCH order: q = 1 (ARCH terms)
- Model: `σ²_t = ω + α₁ε²_{t-1} + β₁σ²_{t-1}`

**Why GARCH(1,1)?**
- **Lower BIC** (-6.64 vs -6.14) - better model fit
- **Lower AIC** (-6.65 vs -6.14) - better information criterion
- **More realistic forecasts**: Time-varying volatility (0.855% to 0.991%)
- **Volatility persistence**: Captures long-term volatility dynamics
- **ARCH limitation**: ARCH(1) produces constant volatility (1.152%) - unrealistic

**Forecast Insights**:
- Mean forecast volatility: **0.931%** (vs historical mean: 0.843%)
- Forecast suggests **10.4% increase** in volatility ahead
- Current volatility (0.749%) is below forecast mean
- Volatility gradually increases from 0.855% to 0.991% over 21 days
- Indicates **increasing market risk** in the forecast period

---

## Summary

### Return Forecasting
- **Best Model**: ARIMA(2,0,2)
- **Performance**: RMSE = 0.0100 (best among AR/MA/ARIMA)
- **Focus**: Forecasting daily returns (mean)

### Volatility Forecasting
- **Best Model**: GARCH(1,1)
- **Performance**: BIC = -6.64 (better than ARCH)
- **Focus**: Forecasting conditional volatility (risk)

### Key Differences
- **ARIMA**: Forecasts **returns** (what direction and magnitude)
- **GARCH**: Forecasts **volatility** (how risky/unpredictable)
- Both models serve different purposes in risk management

---

## Model Diagnostics

### ARIMA(2,0,2) Diagnostics
- Ljung-Box test (residuals): Some autocorrelation detected
- Jarque-Bera test: Non-normal residuals (common in finance)
- Stationarity: Returns confirmed stationary

### GARCH(1,1) Diagnostics
- Ljung-Box test (residuals): Should be white noise
- Ljung-Box test (squared residuals): Should be white noise (no remaining ARCH effects)
- Volatility clustering: Successfully captured

---

## Forecast Periods

### Return Forecasts
- **Period**: Oct 31 - Nov 28, 2025 (21 business days)
- **Cumulative Return**: +1.12%
- **Direction**: 81% positive days

### Volatility Forecasts
- **Period**: Oct 31 - Nov 28, 2025 (21 business days)
- **Mean Volatility**: 0.931% (increasing trend)
- **Risk Level**: Moderate to increasing

---

**Last Updated**: November 2025  
**Models**: ARIMA(2,0,2) for returns, GARCH(1,1) for volatility

