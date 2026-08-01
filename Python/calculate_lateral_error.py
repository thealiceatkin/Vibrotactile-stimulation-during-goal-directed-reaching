#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 13 12:04:44 2026

@author: aatkin
"""

"""
Target Entry Error Analysis (Lateral Deviation)
=================================================
For each trial, finds the first phase-5 sample at which the cursor crosses
the near circumference of the target in the x-dimension, then calculates the
lateral error at that point — i.e. deviation perpendicular to the straight-line
path from start to target.

Method:
    1. Compute the unit vector from (startx_cm, starty_cm) to
       (targetx_cm, targety_cm). This defines the primary movement axis.
    2. Find the first phase-5 sample at which the cursor crosses the near
       circumference of the target along the primary movement axis.
    3. Compute the vector from the start position to the cursor position at
       that sample.
    4. Project this vector onto the axis perpendicular to the movement
       direction to obtain lateral deviation.
    5. Take the absolute value of the lateral deviation as the error.

The near circumference threshold is determined per trial by projecting the
target circumference onto the movement axis:
    threshold = dot(target_centre - start, movement_unit) - targetSize
The cursor is considered to have crossed this threshold when its projection
onto the movement axis equals or exceeds this value.

Standard deviation of lateral error is then computed within participants for
each combination of targetSize and distance.

Usage:
    python analyze_entry_error.py --experiment_dir /path/to/experiment_folder

Optional arguments:
    --sessions         Names of session sub-folders within experiment_dir to merge
    --warmup           Per-session warmup exclusion as session:N or session:half:TOTAL
    --output_file      Name of the output CSV file (default: entry_error_summary.csv)

Examples:
    # Single session
    python analyze_entry_error.py --experiment_dir ./data

    # Merge two sessions with different warmup exclusions
    python analyze_entry_error.py --experiment_dir ./data \\
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
TARGET_X_COL      = "targetx_cm"
TARGET_Y_COL      = "targety_cm"
TARGET_SIZE_COL   = "targetSize"
DISTANCE_COL      = "distance"
PHASE_COL         = "phase"
START_X_COL       = "startx_cm"
START_Y_COL       = "starty_cm"
MOVEMENT_TIME_COL = "movement_time"

ENTRY_PHASE = 5                 # Only use samples from this phase

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


def compute_entry_error(trial_df: pd.DataFrame, fname: str) -> float | None:
    """Compute absolute lateral deviation at the point the cursor crosses the
    near circumference of the target during phase 5.

    The coordinate frame is rotated so that the primary axis points from the
    start position to the target centre. The circumference threshold is defined
    along this axis as:
        threshold = distance_to_target - targetSize
    i.e. the projection of the near circumference onto the movement axis.

    The first phase-5 sample whose projection onto the movement axis meets or
    exceeds this threshold is used. Lateral error is the absolute component of
    the cursor-minus-start vector perpendicular to the movement axis.

    Returns None if required columns are missing, the movement vector is
    degenerate (zero length), no phase-5 samples exist, or the threshold is
    never crossed.
    """
    required = [CURSOR_X_COL, CURSOR_Y_COL, TARGET_X_COL, TARGET_Y_COL,
                TARGET_SIZE_COL, START_X_COL, START_Y_COL, PHASE_COL]
    missing = [c for c in required if c not in trial_df.columns]
    if missing:
        print(f"  [WARNING] Missing columns {missing} in {fname} — skipping.")
        return None

    # Read trial-level constants from the first row
    first       = trial_df.iloc[0]
    start_x     = first[START_X_COL]
    start_y     = first[START_Y_COL]
    target_x    = first[TARGET_X_COL]
    target_y    = first[TARGET_Y_COL]
    targetSize = first[TARGET_SIZE_COL]

    if any(pd.isna(v) for v in [start_x, start_y, target_x, target_y, targetSize]):
        print(f"  [WARNING] NaN in trial constants in {fname} — skipping.")
        return None

    if targetSize <= 0:
        print(f"  [WARNING] targetSize <= 0 in {fname} — skipping.")
        return None

    # Movement vector and unit vector (primary axis)
    move_vec = np.array([target_x - start_x, target_y - start_y])
    move_len = np.linalg.norm(move_vec)
    if move_len == 0:
        print(f"  [WARNING] Start position equals target position in {fname} — skipping.")
        return None

    move_unit = move_vec / move_len         # primary axis (toward target)
    perp_unit = np.array([-move_unit[1], move_unit[0]])  # perpendicular axis

    # Circumference threshold along the primary axis
    # = full distance to target centre minus the target radius
    threshold = move_len - targetSize

    # Isolate phase-5 samples
    phase5 = trial_df[trial_df[PHASE_COL] == ENTRY_PHASE]
    if phase5.empty:
        print(f"  [WARNING] No phase-{ENTRY_PHASE} samples in {fname} — skipping.")
        return None

    cursor_x = phase5[CURSOR_X_COL].to_numpy()
    cursor_y = phase5[CURSOR_Y_COL].to_numpy()

    # Project each phase-5 cursor position onto the movement axis
    # (relative to start position)
    rel_x = cursor_x - start_x
    rel_y = cursor_y - start_y
    projections = rel_x * move_unit[0] + rel_y * move_unit[1]

    # First sample where projection meets or exceeds the circumference threshold
    crossed = np.where(projections >= threshold)[0]
    if len(crossed) == 0:
        print(f"  [WARNING] Cursor never reached target circumference threshold "
              f"({threshold:.4f} cm along movement axis) during phase {ENTRY_PHASE} "
              f"in {fname} — skipping.")
        return None

    entry_idx = crossed[0]
    rel_vec   = np.array([rel_x[entry_idx], rel_y[entry_idx]])

    # Lateral deviation = projection onto perpendicular axis
    lateral_error = float(np.dot(rel_vec, perp_unit))
    return lateral_error


def process_participant_dir(participant_dir: str, session_label: str,
                             warmup_threshold: int) -> pd.DataFrame:
    """Process all valid trial CSV files in a single participant session folder.

    Returns a DataFrame with one row per included trial containing:
        - participant_id
        - session
        - targetSize, distance
        - entry_error_lateral
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

        # --- Compute lateral entry error ---
        error = compute_entry_error(trial_df, fname)
        if error is None:
            continue

        trial_records.append({
            "participant_id":       participant_id,
            "session":              session_label,
            TARGET_SIZE_COL:        targetSize,
            DISTANCE_COL:           distance,
            "entry_error_lateral":  error,
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
    """Compute SD of lateral entry error within participants for each
    targetSize x distance combination.

    Output is wide-format (ANOVA-ready):
        - Rows: one per participant
        - Columns: one per unique targetSize x distance combination
          (labelled as targetSize_<val>_distance_<val>)
    """
    sd_error = (
        all_trials
        .groupby(["participant_id", TARGET_SIZE_COL, DISTANCE_COL])["entry_error_lateral"]
        .std(ddof=1)
        .reset_index()
        .rename(columns={"entry_error_lateral": "sd_entry_error_lateral"})
    )

    sd_error["condition_label"] = sd_error.apply(
        lambda r: f"targetSize_{r[TARGET_SIZE_COL]}_distance_{r[DISTANCE_COL]}", axis=1
    )

    summary = sd_error.pivot(
        index="participant_id",
        columns="condition_label",
        values="sd_entry_error_lateral"
    )
    summary.columns.name = None
    summary = summary.reset_index()
    return summary


def main():
    parser = argparse.ArgumentParser(
        description="Target entry lateral error analysis (SD) by target size and distance."
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
        default="entry_error_summary.csv",
        help="Output CSV filename (saved in experiment_dir). Default: entry_error_summary.csv"
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
