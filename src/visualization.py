import matplotlib.pyplot as plt
import statsmodels.api as sm
import pandas as pd
import numpy as np

MODEL_COLORS = {
    "ARIMA": "royalblue",
    "RF": "forestgreen",
    "GBM": "darkorange",
    "Seasonal naive": "dimgrey",
}


def plot_time_series(
    series, title="Time Series", xlabel="Time", ylabel="Value", ax=None, save_name=None
):
    create_fig = ax is None
    if create_fig:
        fig, ax = plt.subplots(figsize=(16, 6))

    ax.plot(series.index, series, color="black", linewidth=0.6)
    ax.grid(False)
    ax.set_facecolor("white")

    for spine in ax.spines.values():
        spine.set_color("black")
        spine.set_linewidth(1)

    ax.set_title(title, fontsize=16, fontweight="bold")
    ax.set_xlabel(xlabel, fontsize=12)
    ax.set_ylabel(ylabel, fontsize=12)

    if create_fig:
        plt.tight_layout()
        if save_name:
            plt.savefig(save_name, dpi=300, bbox_inches="tight")
        plt.show()


def plot_acf(
    series,
    lags=40,
    title="Autocorrelation Function (ACF)",
    ylim=None,
    ax=None,
    save_name=None,
):
    create_fig = ax is None
    if create_fig:
        fig, ax = plt.subplots(figsize=(6, 4))

    sm.graphics.tsa.plot_acf(
        series, lags=lags, ax=ax, color="black", vlines_kwargs={"colors": "black"}, zero=False
    )
    ax.set_title(title)
    ax.set_xlabel("Lag")
    ax.set_ylabel("ACF")

    if ylim is not None:
        ax.set_ylim(ylim)

    if create_fig:
        plt.tight_layout()
        if save_name:
            plt.savefig(save_name, dpi=300, bbox_inches="tight")
        plt.show()


def plot_pacf(
    series,
    lags=40,
    title="Partial Autocorrelation Function (PACF)",
    ylim=None,
    ax=None,
    save_name=None,
):
    create_fig = ax is None
    if create_fig:
        fig, ax = plt.subplots(figsize=(6, 4))

    sm.graphics.tsa.plot_pacf(
        series,
        lags=lags,
        method="ywm",
        ax=ax,
        color="black",
        vlines_kwargs={"colors": "black"},
        zero=False,
    )
    ax.set_title(title)
    ax.set_xlabel("Lag")
    ax.set_ylabel("PACF")

    if ylim is not None:
        ax.set_ylim(ylim)

    if create_fig:
        plt.tight_layout()
        if save_name:
            plt.savefig(save_name, dpi=300, bbox_inches="tight")
        plt.show()


def _save_and_show(fig, save_name):
    fig.tight_layout()
    if save_name:
        fig.savefig(save_name, dpi=300, bbox_inches="tight")
    plt.show()


def plot_inverse_roots(roots, ax, title):
    # inverse roots inside the unit circle, stationary (AR) / invertible (MA)
    inv = 1 / np.asarray(roots)
    ax.add_patch(
        plt.Circle(
            (0, 0), 1, color="firebrick", fill=False, linestyle="--", linewidth=1.5
        )
    )
    ax.scatter(inv.real, inv.imag, color="royalblue", edgecolor="black", s=50, zorder=5)
    ax.axhline(0, color="grey", linestyle=":", alpha=0.7)
    ax.axvline(0, color="grey", linestyle=":", alpha=0.7)
    ax.set_xlim(-1.2, 1.2)
    ax.set_ylim(-1.2, 1.2)
    ax.set_aspect("equal")
    ax.set_title(title, fontweight="bold")
    ax.set_xlabel("Real part")
    ax.set_ylabel("Imaginary part")


def plot_forecasts(observed, forecasts, title, interval=None, save_name=None):
    """observed: Series; forecasts: DataFrame with one column per model;
    interval: optional (lower, upper) pair of Series, drawn as a band."""
    fig, ax = plt.subplots(figsize=(16, 6))
    if interval is not None:
        ax.fill_between(
            observed.index,
            *interval,
            color="royalblue",
            alpha=0.15,
            label="ARIMA 95% CI"
        )
    for name, fc in forecasts.items():
        ax.plot(fc.index, fc, linewidth=1.5, label=name, color=MODEL_COLORS.get(name))
    ax.scatter(
        observed.index,
        observed,
        facecolors="none",
        edgecolors="black",
        linewidths=1.2,
        label="Observed",
        zorder=5,
    )
    ax.set_title(title, fontsize=16, fontweight="bold")
    ax.set_xlabel("Date", fontsize=12)
    ax.set_ylabel("ER admissions", fontsize=12)
    ax.grid(True, linestyle=":", alpha=0.6)
    ax.legend(loc="upper left")
    _save_and_show(fig, save_name)


def plot_importances(model, title, color="lightgreen", ax=None, save_name=None):
    importances = pd.Series(model.feature_importances_, index=model.feature_names_in_)
    create_fig = ax is None
    if create_fig:
        fig, ax = plt.subplots(figsize=(10, 5))

    importances.sort_values(ascending=False).plot(
        kind="bar", ax=ax, color=color, edgecolor="black"
    )
    ax.set_title(title, fontsize=12, fontweight="bold")
    ax.set_ylabel("Relative importance")
    ax.grid(axis="y", linestyle=":", alpha=0.7)
    plt.setp(ax.get_xticklabels(), rotation=45, ha="right")

    if create_fig:
        _save_and_show(fig, save_name)
