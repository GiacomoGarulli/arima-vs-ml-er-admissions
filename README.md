# Why Machine Learning Models Struggle to Beat ARIMA: Simulations and Real Data

Computational code and reproducible experimental suite for the Bachelor's degree thesis in Statistics (*Scuola di Economia e Management, Università degli Studi di Firenze*, October 2026).

- **Candidate:** Giacomo Garulli
- **Supervisor:** Prof. Fabrizio Cipollini

---

## Overview

In operational time series forecasting, non-parametric tree ensembles (Random Forest, Gradient Boosting) are frequently adopted under the assumption that their flexibility will outperform classical linear models. However, in many real-world operational contexts characterized by strong calendar regularities, bounded dynamics, and post-holiday shock rebounds, classical linear models like ARIMA consistently match or exceed machine learning performance.

This project investigates the empirical and theoretical mechanisms behind this gap using a two-part framework:
1. **Real Data (Emergency Department Admissions):** 7 years of daily patient admissions at Careggi Hospital in Florence (2013–2019, $T = 2556$). We evaluate SARIMAX with calendar regressors against Random Forest and Gradient Boosting under direct and recursive multi-step forecasting strategies ($h = 1, 3, 7, 14, 30$).
2. **Controlled Monte Carlo Simulations:** Synthetic experiments under known Data Generating Processes (Gaussian White Noise, Random Walk, Seasonal Random Walk, AR(1), and MA(1)) designed to isolate specific failure modes, extrapolation limits, and inductive bias.

---

## Core Findings

- **Overfitting on Pure Noise (Simulation 1):**  
  On white noise series, tree-based models attempt to partition residual variance, inflating test RMSE by +4% to +21% depending on the number of lags. ARMA(1,1) cancels redundant autoregressive and moving average roots ($\phi_1 + \theta_1 \approx 0$), matching the theoretical sample mean with negligible variance penalty (+0.05%).
- **Bounded Extrapolation Failure (Simulations 2 & 3):**  
  Regression trees cannot predict outside the convex hull of the training target values. On non-stationary series (Random Walk), trees evaluated on raw levels fail as the series leaves historical bounds. First-differencing restores stationarity and prevents divergence, but discrete recursive integration over long horizons accumulates error variance.
- **The Moving Average Shock-Absorption Gap (Real Data):**  
  In the Careggi series, holiday shocks (e.g., Christmas) are followed by immediate patient volume rebounds. The linear ARIMA model absorbs these post-holiday corrections through its negative moving average parameter ($\hat{\theta}_1 = -0.81$). Tree ensembles lack an internal state for unobserved shock innovations; without explicit rebound dummy features, differenced recursive trees accumulate a persistent downward forecast bias.
- **Statistical Significance (Diebold-Mariano Tests):**  
  Across multi-step horizons up to 14 days, ARIMA statistically outperforms tree ensembles at the 5% level, even after conservative Bonferroni correction for multiple hypothesis testing.

---

## Notebooks

The empirical pipeline and simulations are organized across 9 self-contained Jupyter notebooks:

| Notebook | Topic | Description |
|---|---|---|
| `01_ARIMA(0,1,2).ipynb` | Classical Benchmark | SARIMAX(0,1,2) with calendar and holiday regressors, estimated via rolling-origin evaluation on the 2019 test set. |
| `02_sarimax_vs_r.ipynb` | Software Alignment | Verifies exact numerical parameter and likelihood equivalence between Python `statsmodels` and R `forecast::Arima`. |
| `03_ML_Direct.ipynb` | Direct ML Forecasting | Random Forest and Gradient Boosting fitted on levels and first differences with direct multi-step horizon targets. |
| `04_ML_Recursive.ipynb` | Recursive ML Forecasting | Autoregressive recursive multi-step forecasting with RF and GBM; compares levels vs. differenced targets. |
| `05_comparison.ipynb` | Evaluation & Tests | Comprehensive metric comparison (RMSE, MAE) and Diebold-Mariano pairwise tests with HLN correction. |
| `05b_diebold_mariano_deepdive.ipynb` | Diagnostic Deep Dive | Analysis of spectral variance estimation, Bartlett kernel weighting, and holiday rebound shock resolution. |
| `06_simulations_wn.ipynb` | Monte Carlo: White Noise | Evaluates root cancellation in ARMA(1,1) vs. variance inflation in Random Forest across lag configurations. |
| `07_simulations_rw.ipynb` | Monte Carlo: Random Walk | Demonstrates bounded extrapolation failure on raw levels and recovery via first-differencing and discrete inversion (`diffinv`). |
| `08_simulations_seasonal_rw.ipynb` | Monte Carlo: Seasonal RW | Weekly seasonal random walk ($s=7$), comparing seasonal differencing with seasonal naive benchmarks. |
| `09_simulations_ar_ma.ipynb` | Monte Carlo: AR vs. MA | Explores lag dimension sensitivity: staircase discretization in AR(1) vs. infinite autoregressive reconstruction in MA(1). |

---

## Project Structure

```text
arima-vs-ml-er-admissions/
├── 01_ARIMA(0,1,2).ipynb
├── 02_sarimax_vs_r.ipynb
├── 03_ML_Direct.ipynb
├── 04_ML_Recursive.ipynb
├── 05_comparison.ipynb
├── 05b_diebold_mariano_deepdive.ipynb
├── 06_simulations_wn.ipynb
├── 07_simulations_rw.ipynb
├── 08_simulations_seasonal_rw.ipynb
├── 09_simulations_ar_ma.ipynb
├── src/                             # Python package
│   ├── config.py                    # Paths, split dates, and seed constants
│   ├── data_prep.py                 # Feature engineering, lag generators, calendar dummy matrices
│   ├── models.py                    # Classical ARIMA wrappers
│   ├── ML_models.py                 # RF and GBM direct/recursive forecasting pipelines
│   ├── diebold_mariano.py           # Diebold-Mariano test with HLN correction and Bartlett spectral weighting
│   ├── simulations.py               # Synthetic DGP generators and recursive simulation engines
│   └── visualization.py             # Matplotlib publication formatting
├── R/                               # R routines used for calendar spline regressors
├── figures/                         # Generated figures organized by notebook
└── requirements.txt                 # Python dependencies
```

---

## Installation & Usage

### Setup

```bash
git clone https://github.com/GiacomoGarulli/arima-vs-ml-er-admissions.git
cd arima-vs-ml-er-admissions
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

### Reproducibility

- **Simulations:** Notebooks `06` through `09` are fully self-contained. They generate all synthetic series on execution using fixed random seeds.
- **Real Data:** Notebooks `01` through `05` contain pre-rendered cell outputs and diagnostic figures. Due to institutional data governance policies, the raw Excel file of Careggi admissions is retained locally and is available upon request for academic verification.

---

## License

This repository is distributed under the terms of the [MIT License](LICENSE).
