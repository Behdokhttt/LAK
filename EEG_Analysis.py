import pandas as pd
import numpy as np
from scipy import signal

# Define EEG frequency bands
frequency_bands = {
    "delta": (1, 4),
    "theta": (4, 8),
    "alpha": (8, 12),
    "beta": (12, 30),
    "gamma": (30, 45)
}

# Overall range for total power calculation
total_freq_range = (1, 45)

# Presence threshold (10% of total power)
presence_threshold = 0.1

# Load data
df = pd.read_excel("data.xlsx")

# EEG signal channels
signals = ["AF7", "AF8", "TP9", "TP10"]

results = []

# Process each segment
for segment, seg_data in df.groupby("Question"):
    timestamps = seg_data["Timestamp"].values
    if len(timestamps) < 2:
        continue  # not enough samples
    
    # Estimate sampling frequency
    dt = np.median(np.diff(timestamps))
    fs = 1.0 / dt

    for sig in signals:
        signal_values = seg_data[sig].values

        # Compute PSD with Welch’s method
        freqs, psd = signal.welch(signal_values, fs=fs, nperseg=min(256, len(signal_values)))

        # Total power in 1–45 Hz range
        total_idx = np.logical_and(freqs >= total_freq_range[0], freqs <= total_freq_range[1])
        total_power = np.trapz(psd[total_idx], freqs[total_idx])

        row = {"segment": segment, "signal": sig, "total_power": total_power}

        # Band power + presence
        for band, (low, high) in frequency_bands.items():
            band_idx = np.logical_and(freqs >= low, freqs <= high)
            band_power = np.trapz(psd[band_idx], freqs[band_idx])
            ratio = band_power / total_power if total_power > 0 else 0
            presence = 1 if ratio >= presence_threshold else 0

            row[f"{band}_power"] = band_power
            row[f"{band}_presence"] = presence

        results.append(row)

# Save per-signal results
result_df = pd.DataFrame(results)
result_df.to_csv("eeg_analysis_results.csv", index=False)
print("Processing complete. Results saved to eeg_analysis_results.csv")

# Majority voting across signals
majority_results = []
for segment, group in result_df.groupby("segment"):
    majority_row = {"segment": segment}
    for band in frequency_bands.keys():
        count = group[f"{band}_presence"].sum()
        majority_row[f"{band}_majority"] = 1 if count > len(signals) / 2 else 0
    majority_results.append(majority_row)

majority_df = pd.DataFrame(majority_results)
majority_df.to_csv("eeg_majority_voting_results.csv", index=False)
print("Majority voting complete. Results saved to eeg_majority_voting_results.csv")
