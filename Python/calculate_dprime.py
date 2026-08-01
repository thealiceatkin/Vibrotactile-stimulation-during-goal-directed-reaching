#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul  9 14:12:52 2026

@author: aatkin
"""

"""
Calculate d' (d-prime) per participant, separately for two touch locations.

d' = z(hit rate) - z(false alarm rate)

INPUT CSV is expected to have one row per participant with these columns
(edit the names in the CONFIG section below to match your file exactly):

    participant_id          6-digit participant ID
    Hits_back                # hits, location 1
    FA_hand                  # false alarms, location 1
    Stim_Count_back        # trials WITH a stimulus, location 1
    noStim_Count_back         # trials WITHOUT a stimulus, location 1
    Hits_hand                # hits, location 2
    FA_hand                   # false alarms, location 2
    Stim_Count_hand        # trials WITH a stimulus, location 2
    noStim_Count_hand         # trials WITHOUT a stimulus, location 2

OUTPUT CSV (wide format, ready for R):

    participant_id, dprime_loc1, dprime_loc2

Extreme rate handling: if a hit rate or false alarm rate is exactly 0 or 1
(which would make z() +/-infinity), that single cell is corrected using the
Macmillan & Kaplan (1985) 1/(2N) rule:
    rate == 0  ->  1 / (2 * N)
    rate == 1  ->  1 - 1 / (2 * N)
where N is the relevant trial count (signal_trials for hits, noise_trials
for false alarms). Cells that are not 0 or 1 are left untouched. A log is
printed for every correction applied so you can audit them.
"""

import sys
from pathlib import Path

import pandas as pd
from scipy.stats import norm

# ---------------------------------------------------------------------------
# CONFIG - edit these to match your actual CSV column names
# ---------------------------------------------------------------------------
INPUT_CSV = "/Users/aatkin/Documents/Tactile_Suppression_Study/Signal_Detection/Hits_FA.csv"
OUTPUT_CSV = "dprime_results.csv"

PARTICIPANT_ID_COL = "Participant_ID"

LOCATIONS = {
    "back": {
        "hits": "Hits_back",
        "fa": "FA_back",
        "signal_trials": "Stim_Count_back",
        "noise_trials": "noStim_Count_back",
    },
    "hand": {
        "hits": "Hits_hand",
        "fa": "FA_hand",
        "signal_trials": "Stim_Count_hand",
        "noise_trials": "noStim_Count_hand",
    },
}
# ---------------------------------------------------------------------------


def corrected_rate(count, n, pid, label):
    """Return count/n, applying the Macmillan & Kaplan (1985) 1/(2N)
    correction only when the raw rate would be exactly 0 or 1."""
    if n == 0:
        print(f"  WARNING participant {pid}: {label} has 0 trials -> rate set to NaN")
        return float("nan")
    rate = count / n
    if rate == 0:
        corrected = 1 / (2 * n)
        print(f"  Corrected participant {pid} {label}: rate 0 -> {corrected:.4f}")
        return corrected
    if rate == 1:
        corrected = 1 - 1 / (2 * n)
        print(f"  Corrected participant {pid} {label}: rate 1 -> {corrected:.4f}")
        return corrected
    return rate


def compute_dprime_for_location(row, cols, pid, loc_name):
    hit_rate = corrected_rate(row[cols["hits"]], row[cols["signal_trials"]], pid, f"{loc_name} hit rate")
    fa_rate = corrected_rate(row[cols["fa"]], row[cols["noise_trials"]], pid, f"{loc_name} FA rate")
    if pd.isna(hit_rate) or pd.isna(fa_rate):
        return float("nan")
    return norm.ppf(hit_rate) - norm.ppf(fa_rate)


def main():
    in_path = Path(INPUT_CSV)
    if not in_path.exists():
        sys.exit(f"Input file not found: {in_path.resolve()}")

    df = pd.read_csv(in_path, dtype={PARTICIPANT_ID_COL: str})

    required_cols = [PARTICIPANT_ID_COL] + [c for loc in LOCATIONS.values() for c in loc.values()]
    missing = [c for c in required_cols if c not in df.columns]
    if missing:
        sys.exit(f"CSV is missing expected column(s): {missing}\nFound columns: {list(df.columns)}")

    results = {PARTICIPANT_ID_COL: df[PARTICIPANT_ID_COL]}

    for loc_name, cols in LOCATIONS.items():
        print(f"\nProcessing {loc_name}...")
        dprimes = []
        for _, row in df.iterrows():
            pid = row[PARTICIPANT_ID_COL]
            dprimes.append(compute_dprime_for_location(row, cols, pid, loc_name))
        results[f"dprime_{loc_name}"] = dprimes

    out_df = pd.DataFrame(results)
    out_df.to_csv(OUTPUT_CSV, index=False)
    print(f"\nDone. Wrote {len(out_df)} rows to {Path(OUTPUT_CSV).resolve()}")


if __name__ == "__main__":
    main()
