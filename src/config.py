HORIZONS = [1, 3, 7, 14, 30]
N_TEST = 365

RF_PARAMS = dict(n_estimators=1000, max_features=1 / 3, random_state=42, n_jobs=-1)
GB_PARAMS = dict(learning_rate=0.01, max_depth=3, subsample=0.8, random_state=42)
