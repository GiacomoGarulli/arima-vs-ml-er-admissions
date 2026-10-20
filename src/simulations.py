"""Universal time series simulation framework and Monte Carlo routines.

Provides reusable data generating processes (DGPs), atomic model builders
(Naive, Mean, ARIMA/SARIMAX, RF on levels, RF on differences), and a single
general-purpose parallel Monte Carlo simulation engine.
Existing functions from src/evaluation and src/ML_models are strictly reused.
"""

import math
import warnings
import numpy as np
import pandas as pd
from statsmodels.tsa.statespace.sarimax import SARIMAX
from statsmodels.tools.sm_exceptions import ConvergenceWarning
from sklearn.ensemble import RandomForestRegressor
from joblib import Parallel, delayed

from src.evaluation import error_metrics, seasonal_naive
from src.ML_models import make_supervised, direct_lags, recursive_forecast


# ==============================================================================
# 1. DATA GENERATING PROCESSES (DGPs)
# ==============================================================================

def generate_white_noise(n_samples=2542, sigma=1.0, seed=None):
    """Generate Gaussian White Noise: y_t ~ N(0, sigma^2)."""
    rng = np.random.default_rng(seed)
    values = rng.normal(loc=0.0, scale=sigma, size=n_samples)
    return pd.Series(values, name="y")


def generate_random_walk(n_samples=2542, sigma=1.0, y0=0.0, seed=None):
    """Generate Gaussian Random Walk: y_t = y_{t-1} + eps_t."""
    rng = np.random.default_rng(seed)
    innovations = rng.normal(loc=0.0, scale=sigma, size=n_samples)
    values = np.cumsum(innovations) + y0
    return pd.Series(values, name="y")


def generate_seasonal_rw(n_samples=2542, period=7, sigma=1.0, seed=None):
    """Generate Seasonal Random Walk: y_t = y_{t-s} + eps_t."""
    rng = np.random.default_rng(seed)
    innovations = rng.normal(loc=0.0, scale=sigma, size=n_samples)
    values = np.zeros(n_samples)
    values[:period] = rng.normal(loc=0.0, scale=sigma, size=period)
    for t in range(period, n_samples):
        values[t] = values[t - period] + innovations[t]
    return pd.Series(values, name="y")


def generate_ar1(n_samples=2542, phi=0.8, sigma=1.0, seed=None):
    """Generate Gaussian stationary AR(1): y_t = phi * y_{t-1} + eps_t."""
    rng = np.random.default_rng(seed)
    innovations = rng.normal(loc=0.0, scale=sigma, size=n_samples)
    values = np.zeros(n_samples)
    values[0] = rng.normal(loc=0.0, scale=sigma / np.sqrt(1 - phi**2))
    for t in range(1, n_samples):
        values[t] = phi * values[t - 1] + innovations[t]
    return pd.Series(values, name="y")


def generate_ma1(n_samples=2542, theta=0.8, sigma=1.0, seed=None):
    """Generate Gaussian invertible MA(1): y_t = eps_t + theta * eps_{t-1}."""
    rng = np.random.default_rng(seed)
    innovations = rng.normal(loc=0.0, scale=sigma, size=n_samples + 1)
    values = innovations[1:] + theta * innovations[:-1]
    return pd.Series(values, name="y")


# ==============================================================================
# 2. ATOMIC MODEL BUILDERS (Reusable across ANY experiment and series)
# ==============================================================================

def model_mean():
    """Predict unconditional training sample mean."""
    def _forecast(y_train, y_full, n_test, seed):
        pred = np.full(n_test, y_train.mean())
        return pred, {}
    return _forecast


def model_naive(lag=1):
    """Predict value observed 'lag' steps ago using src.evaluation.seasonal_naive."""
    def _forecast(y_train, y_full, n_test, seed):
        pred = seasonal_naive(y_full, n_test, h=1, period=lag).to_numpy()
        return pred, {}
    return _forecast


def model_arima(
    order=(1, 0, 1),
    seasonal_order=(0, 0, 0, 0),
    trend="n",
    enforce_stationarity=True,
    enforce_invertibility=True,
    maxiter=200,
):
    """Fit SARIMAX on train and forecast via Kalman filter on full series."""
    def _forecast(y_train, y_full, n_test, seed):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", ConvergenceWarning)
            warnings.simplefilter("ignore", UserWarning)
            model = SARIMAX(
                y_train,
                order=order,
                seasonal_order=seasonal_order,
                trend=trend,
                enforce_stationarity=enforce_stationarity,
                enforce_invertibility=enforce_invertibility,
            )
            res = model.fit(disp=False, maxiter=maxiter)
            res_full = res.apply(endog=y_full, refit=False)
            pred = res_full.fittedvalues.iloc[-n_test:].to_numpy()
            info = {
                "phi_1": float(res.params.get("ar.L1", np.nan)),
                "theta_1": float(res.params.get("ma.L1", np.nan)),
                "intercept": float(res.params.get("intercept", res.params.get("const", np.nan))),
            }
        return pred, info
    return _forecast


def model_rf_levels(lags=[1], n_estimators=100):
    """Fit Random Forest on levels y with specified lag features."""
    def _forecast(y_train, y_full, n_test, seed):
        empty_df = pd.DataFrame(index=y_full.index)
        X_tr, y_tr, X_te, _ = make_supervised(empty_df, y_full, lags=lags, n_test=n_test)
        rf = RandomForestRegressor(n_estimators=n_estimators, random_state=seed, n_jobs=1)
        rf.fit(X_tr, y_tr)
        pred = rf.predict(X_te)
        return pred, {}
    return _forecast


def model_rf_diff(lags=[1], d=1, n_estimators=100):
    """Fit Random Forest on d-th order differences and re-integrate back to levels.
    
    Uses seasonal_naive(y_full, n_test, h=1, period=d) for discrete difference inversion.
    """
    def _forecast(y_train, y_full, n_test, seed):
        dy = y_full.diff(d).dropna()
        empty_df = pd.DataFrame(index=dy.index)
        X_tr, dy_tr, X_te, _ = make_supervised(empty_df, dy, lags=lags, n_test=n_test)
        rf = RandomForestRegressor(n_estimators=n_estimators, random_state=seed, n_jobs=1)
        rf.fit(X_tr, dy_tr)
        pred_diff = rf.predict(X_te)
        # Re-integration: y_hat_t = y_{t-d} + pred_diff_t
        base_level = seasonal_naive(y_full, n_test, h=1, period=d).to_numpy()
        pred_level = base_level + pred_diff
        return pred_level, {}
    return _forecast


def model_naive_multistep(period=1):
    """Multi-step seasonal naive forecast across requested horizons."""
    def _forecast(y_train, y_full, n_test, seed, horizons):
        preds = {
            h: seasonal_naive(y_full, n_test, h=h, period=period).to_numpy()
            for h in horizons
        }
        return preds, {}
    return _forecast


def model_arima_rw_multistep(order=(1, 1, 1), trend="n", maxiter=200):
    """ARIMA(1,1,1) multi-step forecast on Random Walk with parameter extraction."""
    def _forecast(y_train, y_full, n_test, seed, horizons):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", ConvergenceWarning)
            warnings.simplefilter("ignore", UserWarning)
            model = SARIMAX(
                y_train,
                order=order,
                trend=trend,
                enforce_stationarity=True,
                enforce_invertibility=True,
            )
            res = model.fit(disp=False, maxiter=maxiter)
            res_full = res.apply(endog=y_full, refit=False)
            phi = float(res.params.get("ar.L1", 0.0))
            y = y_full.to_numpy(dtype=float)
            fitted = res_full.fittedvalues.to_numpy()
            n = len(y)
            idx = np.arange(n - n_test, n)
            preds = {}
            for h in horizons:
                o = idx - h
                w1 = fitted[o + 1] - y[o]
                mult = h if abs(1 - phi) < 1e-10 else (1 - phi**h) / (1 - phi)
                preds[h] = y[o] + w1 * mult
            info = {
                "phi_1": float(res.params.get("ar.L1", np.nan)),
                "theta_1": float(res.params.get("ma.L1", np.nan)),
            }
        return preds, info
    return _forecast


def model_sarima_srw_multistep(order=(1, 0, 1), seasonal_order=(0, 1, 0, 7), trend="n", maxiter=200):
    """SARIMA(1,0,1)(0,1,0)_7 multi-step forecast on Seasonal Random Walk."""
    def _forecast(y_train, y_full, n_test, seed, horizons):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", ConvergenceWarning)
            warnings.simplefilter("ignore", UserWarning)
            model = SARIMAX(
                y_train,
                order=order,
                seasonal_order=seasonal_order,
                trend=trend,
                enforce_stationarity=True,
                enforce_invertibility=True,
            )
            res = model.fit(disp=False, maxiter=maxiter)
            res_full = res.apply(endog=y_full, refit=False)
            phi = float(res.params.get("ar.L1", 0.0))
            y = y_full.to_numpy(dtype=float)
            fitted = res_full.fittedvalues.to_numpy()
            n = len(y)
            idx = np.arange(n - n_test, n)
            preds = {}
            for h in horizons:
                o = idx - h
                w1 = fitted[o + 1] - y[o + 1 - 7]
                k = math.ceil(h / 7)
                pred = y[idx - 7 * k].copy()
                for i in range(k):
                    pred = pred + phi ** (h - 7 * i - 1) * w1
                preds[h] = pred
            info = {
                "phi_1": float(res.params.get("ar.L1", np.nan)),
                "theta_1": float(res.params.get("ma.L1", np.nan)),
            }
        return preds, info
    return _forecast


def model_arma_multistep(order=(1, 0, 1), trend="n", maxiter=200):
    """ARMA(1,1) multi-step forecast on stationary AR or MA series."""
    def _forecast(y_train, y_full, n_test, seed, horizons):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", ConvergenceWarning)
            warnings.simplefilter("ignore", UserWarning)
            model = SARIMAX(
                y_train,
                order=order,
                trend=trend,
                enforce_stationarity=True,
                enforce_invertibility=True,
            )
            res = model.fit(disp=False, maxiter=maxiter)
            res_full = res.apply(endog=y_full, refit=False)
            phi_est = float(res.params.get("ar.L1", 0.0))
            theta_est = float(res.params.get("ma.L1", 0.0))
            fitted = res_full.fittedvalues
            preds = {}
            for h in horizons:
                if h == 1:
                    preds[1] = fitted.iloc[-n_test:].to_numpy()
                else:
                    pred_h = (phi_est ** (h - 1)) * fitted.shift(h - 1).iloc[-n_test:].to_numpy()
                    preds[h] = pred_h
            info = {
                "phi_1": phi_est,
                "theta_1": theta_est,
            }
        return preds, info
    return _forecast


def model_rf_levels_multistep(lags=[1], n_estimators=100):
    """Multi-step recursive Random Forest on raw levels."""
    def _forecast(y_train, y_full, n_test, seed, horizons):
        empty_df = pd.DataFrame(index=y_full.index)
        X_tr, y_tr, X_te, _ = make_supervised(empty_df, y_full, lags=lags, n_test=n_test)
        rf = RandomForestRegressor(n_estimators=n_estimators, random_state=seed, n_jobs=1)
        rf.fit(X_tr, y_tr)
        preds = {
            h: recursive_forecast(rf, X_tr, X_te, h).to_numpy()
            for h in horizons
        }
        return preds, {}
    return _forecast


def recursive_forecast_steps(model, X_train, X_test, h):
    X_all = pd.concat([X_train, X_test])
    columns = X_all.columns
    lag_cols = {j: int(c.split("_")[1]) for j, c in enumerate(columns) if c.startswith("lag_")}
    first = len(X_train) - h + 1
    rows = first + np.arange(len(X_test))[:, None] + np.arange(h)
    paths = X_all.to_numpy(dtype=float)[rows]
    steps = np.empty((len(X_test), h))
    for s in range(h):
        pred = model.predict(pd.DataFrame(paths[:, s], columns=columns))
        steps[:, s] = pred
        for j, k in lag_cols.items():
            if s + k < h:
                paths[:, s + k, j] = pred
    return steps


def model_rf_diff_multistep(lags=[1], d=1, n_estimators=100):
    """Multi-step recursive Random Forest on d-th differences with difference inversion."""
    def _forecast(y_train, y_full, n_test, seed, horizons):
        dy = y_full.diff(d).dropna()
        empty_df = pd.DataFrame(index=dy.index)
        X_tr, dy_tr, X_te, _ = make_supervised(empty_df, dy, lags=lags, n_test=n_test)
        rf = RandomForestRegressor(n_estimators=n_estimators, random_state=seed, n_jobs=1)
        rf.fit(X_tr, dy_tr)
        preds = {}
        for h in horizons:
            steps = recursive_forecast_steps(rf, X_tr, X_te, h)
            k = math.ceil(h / d)
            cum = sum(steps[:, h - 1 - d * i] for i in range(k))
            base_level = seasonal_naive(y_full, n_test, h=h, period=d).to_numpy()
            preds[h] = base_level + cum
        return preds, {}
    return _forecast


# ==============================================================================
# 3. UNIVERSAL MONTE CARLO ENGINE
# ==============================================================================

def simulate_replication(rep_idx, dgp_fn, models, n_samples, n_test, seed, **dgp_kwargs):
    """Run a single replication across an arbitrary dictionary of models."""
    rep_seed = seed if seed is not None else rep_idx
    y = dgp_fn(n_samples=n_samples, seed=rep_seed, **dgp_kwargs)
    n_train = n_samples - n_test
    y_train = y.iloc[:n_train]
    y_test = y.iloc[n_train:]

    min_tr, max_tr = y_train.min(), y_train.max()
    oob_rate = float(((y_test < min_tr) | (y_test > max_tr)).mean())

    row = {
        "replication": rep_idx,
        "oob_rate": oob_rate,
    }

    for name, model_fn in models.items():
        pred, info = model_fn(y_train, y, n_test, rep_seed)
        m = error_metrics(y_test, pred)
        row[f"rmse_{name}"] = m["RMSE"]
        row[f"mae_{name}"] = m["MAE"]
        for k, v in info.items():
            if not np.isnan(v):
                row[k] = v

    return row


def run_simulation(
    dgp_fn,
    models,
    n_sim=500,
    n_samples=2542,
    n_test=364,
    base_seed=42,
    n_jobs=-1,
    **dgp_kwargs,
):
    """Execute a general Monte Carlo simulation experiment in parallel."""
    results = Parallel(n_jobs=n_jobs)(
        delayed(simulate_replication)(
            rep_idx=i,
            dgp_fn=dgp_fn,
            models=models,
            n_samples=n_samples,
            n_test=n_test,
            seed=base_seed + i if base_seed is not None else None,
            **dgp_kwargs,
        )
        for i in range(n_sim)
    )
    return pd.DataFrame(results)


def run_monte_carlo_wn(
    n_sim=500,
    n_samples=2542,
    n_test=364,
    sigma=1.0,
    n_jobs=-1,
    base_seed=42,
    n_estimators=100,
):
    """Convenience wrapper for White Noise simulation (retains exact backwards compatibility)."""
    models = {
        "mean": model_mean(),
        "arma11": model_arima(order=(1, 0, 1), trend="c", enforce_stationarity=True, enforce_invertibility=True),
        "rf_1lag": model_rf_levels(lags=[1], n_estimators=n_estimators),
        "rf_4lags": model_rf_levels(lags=direct_lags(1), n_estimators=n_estimators),
    }
    return run_simulation(
        dgp_fn=generate_white_noise,
        models=models,
        n_sim=n_sim,
        n_samples=n_samples,
        n_test=n_test,
        base_seed=base_seed,
        n_jobs=n_jobs,
        sigma=sigma,
    )


def simulate_replication_multistep(
    rep_idx, dgp_fn, models, n_samples, n_test, seed, horizons=[1, 3, 7, 14, 30], **dgp_kwargs
):
    """Run a single replication across models evaluating multiple forecast horizons h."""
    rep_seed = seed if seed is not None else rep_idx
    y = dgp_fn(n_samples=n_samples, seed=rep_seed, **dgp_kwargs)
    n_train = n_samples - n_test
    y_train = y.iloc[:n_train]
    y_test = y.iloc[n_train:]

    min_tr, max_tr = y_train.min(), y_train.max()
    oob_rate = float(((y_test < min_tr) | (y_test > max_tr)).mean())

    row = {
        "replication": rep_idx,
        "oob_rate": oob_rate,
    }

    for name, model_fn in models.items():
        preds_dict, info = model_fn(y_train, y, n_test, rep_seed, horizons)
        for h in horizons:
            pred_h = preds_dict[h]
            m = error_metrics(y_test, pred_h)
            row[f"rmse_{name}_h{h}"] = m["RMSE"]
            row[f"mae_{name}_h{h}"] = m["MAE"]
        for k, v in info.items():
            if not np.isnan(v):
                row[k] = v

    return row


def run_simulation_multistep(
    dgp_fn,
    models,
    n_sim=100,
    n_samples=2542,
    n_test=364,
    horizons=[1, 3, 7, 14, 30],
    base_seed=42,
    n_jobs=-1,
    **dgp_kwargs,
):
    """Execute a multi-horizon Monte Carlo simulation experiment in parallel."""
    results = Parallel(n_jobs=n_jobs)(
        delayed(simulate_replication_multistep)(
            rep_idx=i,
            dgp_fn=dgp_fn,
            models=models,
            n_samples=n_samples,
            n_test=n_test,
            seed=base_seed + i if base_seed is not None else None,
            horizons=horizons,
            **dgp_kwargs,
        )
        for i in range(n_sim)
    )
    return pd.DataFrame(results)
