import warnings

import numpy as np
from scipy import stats


def diebold_mariano(
    y,
    f1,
    f2,
    h=1,
    loss="SE",
    alternative="two-sided",
):
    """Diebold-Mariano test for predictive accuracy comparison.

    Replicates R's forecast::dm.test and the .DieboldMariano helper function
    from TSA-Predict-Student-Functions.R:
    - Small-sample correction by Harvey, Leybourne, and Newbold (HLN, 1997).
    - Long-run variance estimation using sample autocovariances up to lag h - 1.
    - p-value computed from a Student's t distribution with n - 1 degrees of freedom.

    Parameters
    ----------
    y : array-like of shape (n,)
        Ground truth (realized values) of the time series.
    f1 : array-like of shape (n,)
        Forecasts from Model 1.
    f2 : array-like of shape (n,)
        Forecasts from Model 2.
    h : int, default=1
        Forecast horizon (steps ahead). Must be an integer >= 1.
    loss : {"SE", "AE", "LLE"}, default="SE"
        Loss function to evaluate forecast errors:
        - "SE" : Squared Error: (y - f)^2
        - "AE" : Absolute Error: |y - f|
        - "LLE": Log-Likelihood Error (quasi-likelihood): r - 1 - log(r), with r = y / f
    alternative : {"two-sided", "less", "greater"}, default="two-sided"
        Alternative hypothesis:
        - "two-sided": Model 1 and Model 2 have different accuracy (E[d] != 0).
        - "less"     : Model 1 is more accurate than Model 2 (E[d] < 0).
        - "greater"  : Model 2 is more accurate than Model 1 (E[d] > 0).

    Returns
    -------
    dict
        Dictionary containing:
        - "h": int, forecast horizon
        - "loss": str, loss function identifier
        - "statistic": float, HLN-adjusted Diebold-Mariano test statistic
        - "p_value": float, p-value from Student's t distribution with n - 1 d.o.f.
    """
    y = np.asarray(y, dtype=float)
    f1 = np.asarray(f1, dtype=float)
    f2 = np.asarray(f2, dtype=float)

    n = len(y)
    if len(f1) != n or len(f2) != n:
        raise ValueError(
            f"Input arrays must have equal length: len(y)={n}, len(f1)={len(f1)}, len(f2)={len(f2)}."
        )

    h = int(h)
    if h < 1:
        raise ValueError("Forecast horizon h must be at least 1.")
    if h > n:
        raise ValueError(f"Forecast horizon h ({h}) cannot exceed sample length ({n}).")

    loss_upper = loss.upper()
    if loss_upper == "SE":
        e1 = (y - f1) ** 2
        e2 = (y - f2) ** 2
    elif loss_upper == "AE":
        e1 = np.abs(y - f1)
        e2 = np.abs(y - f2)
    elif loss_upper == "LLE":
        if np.any(y <= 0) or np.any(f1 <= 0) or np.any(f2 <= 0):
            raise ValueError("For LLE loss, y, f1, and f2 must be strictly positive.")
        r1 = y / f1
        r2 = y / f2
        e1 = r1 - 1.0 - np.log(r1)
        e2 = r2 - 1.0 - np.log(r2)
    else:
        raise ValueError(f"Unsupported loss function: '{loss}'. Choose from 'SE', 'AE', 'LLE'.")

    # Loss differential: d_t = e_{1,t} - e_{2,t}
    d = e1 - e2
    d_mean = np.mean(d)
    d_centered = d - d_mean

    # Sample autocovariances with denominator n (identical to R's acf(..., type='covariance'))
    gamma0 = np.sum(d_centered ** 2) / n
    gamma_sum = 0.0
    for k in range(1, h):
        gamma_k = np.sum(d_centered[k:] * d_centered[:-k]) / n
        gamma_sum += gamma_k

    # Variance of the sample mean: Var(d_mean) = (gamma0 + 2 * sum_{k=1}^{h-1} gamma_k) / n
    d_var = (gamma0 + 2.0 * gamma_sum) / n

    if d_var <= 0:
        if h == 1:
            raise ValueError("Variance of DM statistic is zero.")
        warnings.warn("Estimated variance is non-positive. Proceeding with horizon h = 1.")
        return diebold_mariano(y, f1, f2, h=1, loss=loss, alternative=alternative)

    # Raw DM statistic
    stat_raw = d_mean / np.sqrt(d_var)

    # Harvey-Leybourne-Newbold (HLN, 1997) small-sample correction factor
    k_hln = np.sqrt((n + 1 - 2 * h + (h / n) * (h - 1)) / n)
    statistic = stat_raw * k_hln

    # p-value from Student's t distribution with n - 1 degrees of freedom
    alt = alternative.lower().replace("_", "").replace("-", "").replace(".", "")
    if alt in ("twosided", "two"):
        p_value = 2.0 * stats.t.cdf(-np.abs(statistic), df=n - 1)
    elif alt == "less":
        p_value = float(stats.t.cdf(statistic, df=n - 1))
    elif alt == "greater":
        p_value = float(stats.t.sf(statistic, df=n - 1))
    else:
        raise ValueError(
            f"Unsupported alternative hypothesis: '{alternative}'. "
            "Choose from 'two-sided', 'less', 'greater'."
        )

    return {
        "h": h,
        "loss": loss_upper,
        "statistic": float(statistic),
        "p_value": float(p_value),
    }
