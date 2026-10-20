import warnings

from statsmodels.stats.diagnostic import acorr_ljungbox
from statsmodels.tools.sm_exceptions import InterpolationWarning
from statsmodels.tsa.stattools import adfuller, kpss


def run_adf_test(series, title="ADF test", reg="c"):
    # H0: unit root
    stat, pvalue = adfuller(series.dropna(), regression=reg, autolag="AIC")[:2]
    verdict = "stationary" if pvalue < 0.05 else "non-stationary"
    print(f"{title:<20} | ADF stat:  {stat:8.4f} | p-value: {pvalue:.4f} | {verdict}")


def run_kpss_test(series, title="KPSS test", reg="c"):
    # H0: stationarity. statsmodels only reports p-values between 0.01 and 0.10
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", InterpolationWarning)
        stat, pvalue = kpss(series.dropna(), regression=reg, nlags="auto")[:2]
    p_text = (
        "< 0.01" if pvalue <= 0.01 else "> 0.10" if pvalue >= 0.10 else f"{pvalue:.4f}"
    )
    verdict = "non-stationary" if pvalue < 0.05 else "stationary"
    print(f"{title:<20} | KPSS stat: {stat:8.4f} | p-value: {p_text} | {verdict}")


def ljung_box_table(resid, model_df, lags=(5, 10, 15, 20)):
    # model_df = p + q of the ARMA part
    table = acorr_ljungbox(resid, lags=list(lags), model_df=model_df)
    table.index.name = "Lag"
    table.columns = ["Q statistic", "p-value"]
    return table.round(3)
