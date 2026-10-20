import math

import numpy as np


def seasonal_naive(y, n_test, h, period=7):
    return y.shift(period * math.ceil(h / period)).iloc[-n_test:]


def error_metrics(y_true, y_pred):
    err = np.asarray(y_true, dtype=float) - np.asarray(y_pred, dtype=float)
    return {"RMSE": np.sqrt(np.mean(err**2)), "MAE": np.mean(np.abs(err))}
