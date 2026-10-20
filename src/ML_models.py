import math

import numpy as np
import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import TimeSeriesSplit


def direct_lags(h, period=7):
    """Lags h and h+1 plus the first two weekly lags that are not already among them."""
    k = math.ceil(h / period)
    weekly = [period * j for j in range(k, k + 3) if period * j not in (h, h + 1)][:2]
    return [h, h + 1] + weekly


def make_supervised(X, y, lags, n_test):
    X = X.copy()
    for k in lags:
        X[f"lag_{k}"] = y.shift(k)
    X, y = X.iloc[max(lags) :], y.iloc[max(lags) :]
    return X.iloc[:-n_test], y.iloc[:-n_test], X.iloc[-n_test:], y.iloc[-n_test:]


def recursive_forecast(model, X_train, X_test, h):
    """Recursive h-step-ahead forecast for every day of the test set.

    The origin for target day t is t - h: the lags that fall after the
    origin are unknown and are replaced by the model's own forecasts.
    """
    X_all = pd.concat([X_train, X_test])
    columns = X_all.columns
    lag_cols = {
        j: int(c.split("_")[1]) for j, c in enumerate(columns) if c.startswith("lag_")
    }

    # paths[i, s] = features of day (target_i - h + 1 + s), for s = 0, ..., h-1
    first = len(X_train) - h + 1
    rows = first + np.arange(len(X_test))[:, None] + np.arange(h)
    paths = X_all.to_numpy(dtype=float)[rows]

    for s in range(h):
        pred = model.predict(pd.DataFrame(paths[:, s], columns=columns))
        for j, k in lag_cols.items():
            if s + k < h:
                paths[:, s + k, j] = pred
    return pd.Series(pred, index=X_test.index)


def tune_gb_trees(X, y, max_trees=1500, n_splits=5, **gb_params):
    """Number of trees with the lowest mean RMSE over a TimeSeriesSplit of the training set."""
    cv_rmse = np.zeros((n_splits, max_trees))
    for i, (fit_idx, val_idx) in enumerate(TimeSeriesSplit(n_splits=n_splits).split(X)):
        gb = GradientBoostingRegressor(n_estimators=max_trees, **gb_params)
        gb.fit(X.iloc[fit_idx], y.iloc[fit_idx])
        y_val = y.iloc[val_idx].to_numpy()
        for j, pred in enumerate(gb.staged_predict(X.iloc[val_idx])):
            cv_rmse[i, j] = np.sqrt(np.mean((y_val - pred) ** 2))
    mean_rmse = cv_rmse.mean(axis=0)
    return int(mean_rmse.argmin()) + 1, mean_rmse
