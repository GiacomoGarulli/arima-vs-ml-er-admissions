# Data Directory

This directory contains the datasets and precomputed model evaluation artifacts.

## Clinical Emergency Department Data

The empirical study is based on daily patient admission counts to the Emergency Department of Careggi University Hospital (*AOU Careggi*, Florence, Italy) covering the period from January 1, 2013 to December 31, 2019 ($T = 2556$ daily observations).

- **Estimation sample:** 2013-01-01 to 2018-12-31 ($N_{\text{train}} = 2191$ days)
- **Test evaluation sample:** 2019-01-01 to 2019-12-31 ($N_{\text{test}} = 365$ days)

### Data Availability Notice

Due to hospital data governance regulations, the raw clinical Excel spreadsheet (`PS-Careggi-2013-2020.xlsx`) is retained locally and is not distributed publicly. Researchers interested in reproducing the empirical results on the original series may request access for academic verification.

### Expected Data Format

If you have access to the raw data file, place `PS-Careggi-2013-2020.xlsx` in this directory. The data pipeline expects:
- Column `Data`: Date (daily frequency, `YYYY-MM-DD`).
- Column `Totale`: Integer count of total daily emergency admissions.

## Generated Artifacts

When executing the notebooks, this directory stores precomputed outputs:
- `model_data.csv`: Preprocessed dataset with engineered calendar features, holiday indicators, and spline regressors.
- `arima_forecasts.csv`: Out-of-sample multi-step forecasts from SARIMAX(0,1,2).
- `ml_direct_forecasts.csv` / `ml_direct_diff_forecasts.csv`: Forecasts from Random Forest and Gradient Boosting (direct strategy).
- `ml_recursive_forecasts.csv` / `ml_recursive_diff_forecasts.csv`: Forecasts from Random Forest and Gradient Boosting (recursive strategy).
- `dm_test_results.csv`: Test statistics and p-values for Diebold-Mariano comparisons.
