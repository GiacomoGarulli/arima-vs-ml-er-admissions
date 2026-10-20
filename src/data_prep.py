import pandas as pd


def load_and_validate_careggi(file_path, end_date="2020-01-01"):
    df = pd.read_excel(file_path)
    df["date"] = pd.to_datetime(df["date"])
    df = df.loc[df["date"] < end_date, ["date", "x"]].set_index("date").sort_index()

    if df.index.duplicated().any():
        raise ValueError("The dataset contains duplicated dates")
    missing = pd.date_range(df.index[0], df.index[-1], freq="D").difference(df.index)
    if len(missing) > 0:
        raise ValueError(f"{len(missing)} missing days, first one: {missing[0].date()}")
    if df["x"].isna().any():
        raise ValueError("Missing values in column 'x'")

    return df.asfreq("D")
