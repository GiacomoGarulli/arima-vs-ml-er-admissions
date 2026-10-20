import warnings

import pandas as pd
import statsmodels.api as sm
from statsmodels.tools.sm_exceptions import ConvergenceWarning

MONTHS = [
    "jan",
    "feb",
    "mar",
    "apr",
    "may",
    "jun",
    "jul",
    "aug",
    "sep",
    "oct",
    "nov",
    "dec",
]


def calendar_day_zscores(resid):
    """Mean residual of each calendar day (month, day) over the years, with its z-score."""
    table = (
        resid.groupby([resid.index.month, resid.index.day])
        .mean()
        .to_frame("mean_resid")
    )
    table.index.names = ["month", "day"]
    table["z"] = (table["mean_resid"] - table["mean_resid"].mean()) / table[
        "mean_resid"
    ].std()
    return table.sort_values("z")


def special_day_dummies(index, days):
    columns = {
        f"{d}_{MONTHS[m - 1]}": ((index.month == m) & (index.day == d)).astype(float)
        for m, d in days
    }
    return pd.DataFrame(columns, index=index)


def build_exog(calendar, special_days, drop=("sh", "eh")):
    # Special-day dummies + calendar dummies
    special = special_day_dummies(calendar.index, special_days)
    cal = calendar.drop(columns=list(drop)).astype(float)
    cal.loc[special.sum(axis=1) > 0, :] = 0.0
    return pd.concat([special, cal], axis=1)


def fit_sarimax_with_exog(endog, exog, order, trend="n"):
    model = sm.tsa.SARIMAX(endog, exog=exog, order=order, trend=trend)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", ConvergenceWarning)
        results = model.fit(disp=False, maxiter=1000)
        if not results.mle_retvals["converged"]:
            # L-BFGS stopped early: restart it from the Nelder-Mead solution
            start = model.fit(disp=False, method="nm", maxiter=5000).params
            results = model.fit(start_params=start, disp=False, maxiter=1000)
    if not results.mle_retvals["converged"]:
        warnings.warn("SARIMAX estimation did not converge", ConvergenceWarning)
    return results


def rolling_forecast(results, y, exog, n_test, h, alpha=0.05):
    """h-step-ahead ex-post forecasts for the last n_test days.

    The parameters stay fixed at the training estimates; the forecast for
    day t only uses data up to t - h.
    """
    n_train = len(y) - n_test
    rows = []
    for t in range(n_train, len(y)):
        origin = t - h
        res_t = results.apply(
            endog=y.iloc[: origin + 1], exog=exog.iloc[: origin + 1], refit=False
        )
        fc = res_t.get_forecast(steps=h, exog=exog.iloc[origin + 1 : t + 1])
        lower, upper = fc.conf_int(alpha=alpha).iloc[-1]
        rows.append((fc.predicted_mean.iloc[-1], lower, upper))
    return pd.DataFrame(
        rows, index=y.index[n_train:], columns=["forecast", "lower", "upper"]
    )
