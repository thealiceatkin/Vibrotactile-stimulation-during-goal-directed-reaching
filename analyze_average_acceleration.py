#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul  2 09:25:55 2026

@author: aatkin
"""

"""
Average Acceleration Analysis
==============================
Calculates average acceleration during phase 5 of each trial, from the onset
of phase 5 up to the sample at which peak velocity is achieved. Averages
within participants for each combination of targetSize and distance.

Average acceleration is computed as:
    peak_velocity / (t_peak - t_start)

where:
    peak_velocity  = maximum instantaneous velocity across phase-5 sample pairs
    t_start        = timestamp of the first phase-5 sample
    t_peak         = timestamp of the later sample in the peak velocity pair

Units: cm/s²

Usage:
    python analyze_acceleration.py --experiment_dir /path/to/experiment_folder

Optional arguments:
    --sessions         Names of session sub-folders within experiment_dir to merge
    --warmup           Per-session warmup exclusion as session:N or session:half:TOTAL
    --output_file      Name of the output CSV file (default: acceleration_summary.csv)

Examples:
    # Single session
    python analyze_acceleration.py --experiment_dir ./data

    # Merge two sessions with different warmup exclusions
    python analyze_acceleration.py --experiment_dir ./data \\
        --sessions motor1_data motor2_data \\
        --warmup motor1_data:half:256 motor2_data:12
"""

import os
import re
import argparse
import numpy as np
import pandas as pd


# --- Column name constants ---
CURSOR_X_COL      = "cursorx_cm"
CURSOR_Y_COL      = "cursory_cm"
TIME_COL          = "time_s"
PHASE_COL         = "phase"
TARGET_SIZE_COL   = "targetSize"
DISTANCE_COL      = "distance"
MOVEMENT_TIME_COL = "movement_time"

ACCELERATION_PHASE = 5          # Only use samples from this phase

# --- Exclusion criteria ---
DEFAULT_WARMUP_TRIALS = 12      # Exclude the first N trials by trial number
MAX_MOVEMENT_TIME     = 2.5     # Exclude trials with movement_time exceeding this (seconds)


def parse_warmup_args(warmup_args: list) -> dict:
    """Parse --warmup arguments into a dict mapping session name -> warmup threshold.

    Each entry is either:
        session:N        — exclude trials with trial number <= N
        session:half:T   — exclude trials with trial number <= T // 2

    Returns a dict of {session_name: int}.
    """
    warmup_map = {}
    for entry in warmup_args:
        parts = entry.split(":")
        if len(parts) == 2:
            session, n = parts
            try:
                warmup_map[session] = int(n)
            except ValueError:
                raise ValueError(
                    f"Invalid --warmup entry '{entry}'. "
                    f"Expected format 'session:N' (e.g. session1:12)."
                )
        elif len(parts) == 3 and parts[1].lower() == "half":
            session, _, total = parts
            try:
                warmup_map[session] = int(total) // 2
            except ValueError:
                raise ValueError(
                    f"Invalid --warmup entry '{entry}'. "
                    f"Expected format 'session:half:TOTAL' (e.g. motor1_data:half:256)."
                )
        else:
            raise ValueError(
                f"Invalid --warmup entry '{entry}'. "
                f"Expected 'session:N' or 'session:half:TOTAL'."
            )
    return warmup_map


def extract_participant_id(folder_name: str) -> str:
    """Extract participant ID from a folder name.
    Assumes the participant ID is the leading portion of the folder name
    (up to the first underscore or space, or the full name if neither exists).
    Edit this function if your folder naming convention differs.
    """
    for sep in ["_", " "]:
        if sep in folder_name:
            return folder_name.split(sep)[0]
    return folder_name


def compute_average_acceleration(trial_df: pd.DataFrame) -> float | None:
    """Compute average acceleration from phase-5 onset to peak velocity.

    Steps:
        1. Isolate phase-5 samples.
        2. Compute instantaneous velocity for each consecutive sample pair.
        3. Identify the peak velocity and the timestamp of that sample pair.
        4. Compute average acceleration as:
               peak_velocity / (t_peak - t_start)
           where t_start is the first phase-5 timestamp and t_peak is the
           timestamp of the later sample in the peak velocity pair.

    Returns None if there are fewer than 2 phase-5 samples, required columns
    are missing, all time deltas are zero, or the time window is zero.
    """
    required = [CURSOR_X_COL, CURSOR_Y_COL, TIME_COL, PHASE_COL]
    if not all(c in trial_df.columns for c in required):
        return None

    phase5 = trial_df[trial_df[PHASE_COL] == ACCELERATION_PHASE].copy()
    if len(phase5) < 2:
        return None

    x = phase5[CURSOR_X_COL].to_numpy()
    y = phase5[CURSOR_Y_COL].to_numpy()
    t = phase5[TIME_COL].to_numpy()

    t_start = t[0]

    dx = np.diff(x)
    dy = np.diff(y)
    dt = np.diff(t)

    # Guard against zero or negative time deltas
    valid = dt > 0
    if not valid.any():
        return None

    velocities = np.full(len(dt), -np.inf)
    velocities[valid] = np.sqrt(dx[valid] ** 2 + dy[valid] ** 2) / dt[valid]

    peak_idx      = int(np.argmax(velocities))
    peak_velocity = velocities[peak_idx]
    t_peak        = t[peak_idx + 1]   # later sample of the peak velocity pair

    accel_duration = t_peak - t_start
    if accel_duration <= 0:
        return None

    return float(peak_velocity / accel_duration)


def process_participant_dir(participant_dir: str, session_label: str,
                             warmup_threshold: int) -> pd.DataFrame:
    """Process all valid trial CSV files in a single participant session folder.

    Returns a DataFrame with one row per included trial containing:
        - participant_id
        - session
        - targetSize, distance
        - average_acceleration
    """
    folder_name    = os.path.basename(participant_dir)
    participant_id = extract_participant_id(folder_name)

    trial_records = []
    trial_pattern = re.compile(r"^trial\d{4}\.csv$", re.IGNORECASE)
    csv_files     = sorted([f for f in os.listdir(participant_dir) if trial_pattern.match(f)])

    if not csv_files:
        print(f"  [WARNING] No trial CSV files found in: {participant_dir}")
        return pd.DataFrame()

    for fname in csv_files:
        # --- Exclusion 1: skip warmup trials ---
        trial_num = int(re.search(r"\d{4}", fname).group())
        if trial_num <= warmup_threshold:
            print(f"  [EXCLUDED] {fname} — warmup trial ({trial_num} <= {warmup_threshold}).")
            continue

        fpath = os.path.join(participant_dir, fname)
        try:
            trial_df = pd.read_csv(fpath)
        except Exception as e:
            print(f"  [WARNING] Could not read {fpath}: {e}")
            continue

        # --- Exclusion 2: skip trials with movement time exceeding threshold ---
        if MOVEMENT_TIME_COL in trial_df.columns:
            mt = trial_df.iloc[-1][MOVEMENT_TIME_COL]
            if pd.notna(mt) and mt > MAX_MOVEMENT_TIME:
                print(f"  [EXCLUDED] {fname} — movement time {mt:.3f}s exceeds {MAX_MOVEMENT_TIME}s.")
                continue
        else:
            print(f"  [WARNING] '{MOVEMENT_TIME_COL}' column not found in {fname} — skipping movement time check.")

        # --- Extract condition values ---
        missing_cols = [c for c in [TARGET_SIZE_COL, DISTANCE_COL] if c not in trial_df.columns]
        if missing_cols:
            print(f"  [WARNING] Missing columns {missing_cols} in {fname} — skipping.")
            continue

        last        = trial_df.iloc[-1]
        targetSize = last[TARGET_SIZE_COL]
        distance    = last[DISTANCE_COL]

        if pd.isna(targetSize) or pd.isna(distance):
            print(f"  [WARNING] NaN in targetSize or distance in {fname} — skipping.")
            continue

        # --- Compute average acceleration ---
        avg_accel = compute_average_acceleration(trial_df)
        if avg_accel is None:
            print(f"  [WARNING] Could not compute average acceleration for {fname} "
                  f"— fewer than 2 phase-{ACCELERATION_PHASE} samples, zero duration, "
                  f"or missing columns. Skipping.")
            continue

        trial_records.append({
            "participant_id":       participant_id,
            "session":              session_label,
            TARGET_SIZE_COL:        targetSize,
            DISTANCE_COL:           distance,
            "average_acceleration": avg_accel,
        })

    return pd.DataFrame(trial_records)


def collect_all_trials(experiment_dir: str, sessions: list,
                        warmup_map: dict) -> pd.DataFrame:
    """Collect trial data across all sessions and participants.

    If sessions is provided, each session folder is scanned for participant
    sub-folders. If sessions is empty, participant folders are read directly
    from experiment_dir (single-session behaviour).
    """
    all_trials_list = []

    if sessions:
        participant_sessions: dict = {}

        for session in sessions:
            session_dir = os.path.join(experiment_dir, session)
            if not os.path.isdir(session_dir):
                print(f"[WARNING] Session folder not found, skipping: {session_dir}")
                continue

            participant_dirs = sorted([
                os.path.join(session_dir, d)
                for d in os.listdir(session_dir)
                if os.path.isdir(os.path.join(session_dir, d))
            ])

            for p_dir in participant_dirs:
                pid = extract_participant_id(os.path.basename(p_dir))
                participant_sessions.setdefault(pid, []).append((session, p_dir))

        if not participant_sessions:
            raise RuntimeError("No participant folders found across the specified session folders.")

        for wm_session in warmup_map:
            if wm_session not in sessions:
                print(f"[WARNING] --warmup references '{wm_session}' which is not listed in --sessions. "
                      f"Check for typos — this warmup rule will have no effect.")

        for pid in sorted(participant_sessions.keys()):
            session_entries = participant_sessions[pid]
            print(f"\nProcessing participant: {pid}  ({len(session_entries)} session(s))")
            for session_label, p_dir in session_entries:
                if session_label not in warmup_map:
                    print(f"  [WARNING] No --warmup rule found for session '{session_label}' — "
                          f"falling back to default of {DEFAULT_WARMUP_TRIALS} trials.")
                warmup_threshold = warmup_map.get(session_label, DEFAULT_WARMUP_TRIALS)
                print(f"  Session '{session_label}': {p_dir}  (warmup exclusion: trial <= {warmup_threshold})")
                p_trials = process_participant_dir(p_dir, session_label, warmup_threshold)
                if not p_trials.empty:
                    all_trials_list.append(p_trials)

    else:
        participant_dirs = sorted([
            os.path.join(experiment_dir, d)
            for d in os.listdir(experiment_dir)
            if os.path.isdir(os.path.join(experiment_dir, d))
        ])

        if not participant_dirs:
            raise RuntimeError("No participant sub-folders found in the experiment directory.")

        warmup_threshold = warmup_map.get("session1", DEFAULT_WARMUP_TRIALS)
        for p_dir in participant_dirs:
            pid = extract_participant_id(os.path.basename(p_dir))
            print(f"Processing participant: {pid}  ({p_dir})  (warmup exclusion: trial <= {warmup_threshold})")
            p_trials = process_participant_dir(p_dir, "session1", warmup_threshold)
            if not p_trials.empty:
                all_trials_list.append(p_trials)

    if not all_trials_list:
        raise RuntimeError("No valid trial data found.")

    return pd.concat(all_trials_list, ignore_index=True)


def build_summary(all_trials: pd.DataFrame) -> pd.DataFrame:
    """Average acceleration within participants for each targetSize x distance combination.

    Output is wide-format (ANOVA-ready):
        - Rows: one per participant
        - Columns: one per unique targetSize x distance combination
          (labelled as targetSize_<val>_distance_<val>)
    """
    mean_accel = (
        all_trials
        .groupby(["participant_id", TARGET_SIZE_COL, DISTANCE_COL])["average_acceleration"]
        .mean()
        .reset_index()
        .rename(columns={"average_acceleration": "mean_average_acceleration"})
    )

    mean_accel["condition_label"] = mean_accel.apply(
        lambda r: f"targetSize_{r[TARGET_SIZE_COL]}_distance_{r[DISTANCE_COL]}", axis=1
    )

    summary = mean_accel.pivot(
        index="participant_id",
        columns="condition_label",
        values="mean_average_acceleration"
    )
    summary.columns.name = None
    summary = summary.reset_index()
    return summary


def main():
    parser = argparse.ArgumentParser(
        description="Average acceleration analysis by target size and distance."
    )
    parser.add_argument(
        "--experiment_dir",
        required=True,
        help="Path to the top-level experiment folder."
    )
    parser.add_argument(
        "--sessions",
        nargs="+",
        default=[],
        help=(
            "Names of session sub-folders within experiment_dir to merge "
            "(e.g. --sessions motor1_data motor2_data)."
        )
    )
    parser.add_argument(
        "--warmup",
        nargs="+",
        default=[],
        metavar="SESSION:N",
        help=(
            "Per-session warmup exclusion. Each entry is either:\n"
            "  session:N        — exclude trials with trial number <= N\n"
            "  session:half:T   — exclude the first half of T total trials\n"
            f"Sessions not listed fall back to the default of {DEFAULT_WARMUP_TRIALS} trials.\n"
            "Example: --warmup motor1_data:half:256 motor2_data:12"
        )
    )
    parser.add_argument(
        "--output_file",
        default="acceleration_summary.csv",
        help="Output CSV filename (saved in experiment_dir). Default: acceleration_summary.csv"
    )
    args = parser.parse_args()

    experiment_dir = os.path.abspath(args.experiment_dir)
    output_path    = os.path.join(experiment_dir, args.output_file)
    warmup_map     = parse_warmup_args(args.warmup)

    print(f"Experiment directory : {experiment_dir}")
    print(f"Sessions to merge    : {args.sessions if args.sessions else '(single-session mode)'}")
    print(f"Warmup exclusions    : {warmup_map if warmup_map else f'(default: {DEFAULT_WARMUP_TRIALS} trials for all sessions)'}")
    print(f"Output file          : {output_path}\n")

    if not os.path.isdir(experiment_dir):
        raise FileNotFoundError(f"Experiment directory not found: {experiment_dir}")

    all_trials = collect_all_trials(experiment_dir, args.sessions, warmup_map)
    print(f"\nTotal trials processed: {len(all_trials)}")

    combos = all_trials.groupby([TARGET_SIZE_COL, DISTANCE_COL]).size().reset_index(name="n_trials")
    print("\nCondition combinations found:")
    print(combos.to_string(index=False))

    summary = build_summary(all_trials)
    summary.to_csv(output_path, index=False)
    print(f"\nSummary saved to: {output_path}")
    print(summary.to_string(index=False))


if __name__ == "__main__":
    main()