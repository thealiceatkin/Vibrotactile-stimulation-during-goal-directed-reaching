#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 15 22:24:20 2026

@author: aatkin

"""

# pivot_summary.py

# Reads all CSV files from a folder (one per participant) and generates
# a Pivot-style summary table.

# CONFIGURATION — edit the variables in the CONFIG section below. """

import glob
import os
import pandas as pd
from openpyxl import load_workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter


# ──────────────────────────────────────────────────────────────────────────────
# CONFIG — edit these variables to match data
# ──────────────────────────────────────────────────────────────────────────────

# Folder containing CSV files (use "." for the current directory)
CSV_FOLDER = "/Users/aatkin/Documents/Tactile_Suppression_Study/motor1_data"

# Column in the CSVs whose values become COLUMNS (e.g. condition)
PIVOT_COLUMNS = ["targetSize", "distance"]

# Column(s) whose values become ROWS. Can be one string or a list.
# Example single:  PIVOT_ROWS = "block"
# Example multi:   PIVOT_ROWS = ["block", "trial_type"]
PIVOT_ROWS = "_participant"

# Column to summarise (ie. cell VALUES)
VALUE_COLUMN = "movement_time"

# Aggregation function applied to VALUE_COLUMN.
# Options: "mean", "median", "sum", "count", "std", "min", "max"
AGG_FUNC = "mean"

# FILTERS — dict of {column_name: value_or_list_of_values} to keep.
# Set to {} to keep all rows.
# Example: {"correct": 1}               → keep only correct trials
# Example: {"phase": ["test", "probe"]} → keep rows where phase is test or probe
FILTERS = {
    "trial_idx":      (">",  12),
    "movement_time":  (">", 0.1),
    "movement_time":  ("<", 2.5),
}

# Name of the output Excel file
OUTPUT_FILE = "pivot_summary.xlsx"

# ──────────────────────────────────────────────────────────────────────────────


# load the .csv files

def load_all_csvs(folder: str) -> pd.DataFrame:
    """Read every CSV in *folder* and concatenate into one DataFrame."""
    pattern = os.path.join(folder, "**", "motor1.csv")
    files = sorted(glob.glob(pattern, recursive=True))
    if not files:
        raise FileNotFoundError(f"No CSV files found in: {os.path.abspath(folder)}")
    
    frames = []
    for f in files:
        df = pd.read_csv(f)
        # Store the source filename (without extension) as a participant ID
        # Take only the first segment before an underscore: "P001_session1" → "P001"
        # df["_participant"] = os.path.basename(os.path.dirname(f)).split("_")[0]
        folder_name = os.path.basename(os.path.dirname(f))
        df["_participant"] = folder_name.split("_")[0]
        frames.append(df)
    
    combined = pd.concat(frames, ignore_index=True)
    print(f"Loaded {len(files)} file(s) → {len(combined):,} rows total.")
    return combined

# apply filters

def apply_filters(df: pd.DataFrame, filters: dict) -> pd.DataFrame:
    ops = {
        ">":  lambda col, val: df[col] >  val,
        ">=": lambda col, val: df[col] >= val,
        "<":  lambda col, val: df[col] <  val,
        "<=": lambda col, val: df[col] <= val,
        "==": lambda col, val: df[col] == val,
        "!=": lambda col, val: df[col] != val,
    }
    for col, condition in filters.items():
        if col not in df.columns:
            raise KeyError(f"Filter column '{col}' not found in data.")
        if isinstance(condition, (list, tuple)) and isinstance(condition[0], str) and condition[0] in ops:
            op, val = condition
            df = df[ops[op](col, val)]
        elif isinstance(condition, (list, tuple)):
            df = df[df[col].isin(condition)]   # original list-of-values behaviour
        else:
            df = df[df[col] == condition]       # original exact-match behaviour
    print(f"After filters: {len(df):,} rows remain.")
    return df

# build the dataframe out of columns, rows and values

def build_pivot(df: pd.DataFrame) -> pd.DataFrame:
    row_cols = [PIVOT_ROWS] if isinstance(PIVOT_ROWS, str) else list(PIVOT_ROWS)
    col_cols = [PIVOT_COLUMNS] if isinstance(PIVOT_COLUMNS, str) else list(PIVOT_COLUMNS)

    for col in row_cols + col_cols + [VALUE_COLUMN]:
        if col not in df.columns:
            raise KeyError(f"Column '{col}' not found. "
                           f"Available: {list(df.columns)}")

    pivot = df.pivot_table(
        index=row_cols,
        columns=col_cols,
        values=VALUE_COLUMN,
        aggfunc=AGG_FUNC,
    )

# Flatten multi-level column names into plain strings e.g. "congruent_1"
    if isinstance(pivot.columns, pd.MultiIndex):
        pivot.columns = ["_".join(str(v) for v in col).strip("_") for col in pivot.columns.to_flat_index()]
    else:
        pivot.columns = [str(c) for c in pivot.columns]

    pivot.columns.name = None
    pivot = pivot.reset_index()

    # Ensure all column names are plain Python strings (openpyxl requirement)
    pivot.columns = [str(c) for c in pivot.columns]
    return pivot

# Write the pivot table to Excel and apply formatting."""

def style_and_save(pivot: pd.DataFrame, output_path: str):
    
    pivot.columns = [str(c) for c in pivot.columns]  # guarantee no Index objects
    pivot.round(4).to_excel(output_path, index=False, sheet_name="Summary")

    wb = load_workbook(output_path)
    ws = wb["Summary"]

    # ── colours ──────────────────────────────────────────────────────────────
    HEADER_FILL   = PatternFill("solid", fgColor="2F5597")   # dark blue
    SUBHEAD_FILL  = PatternFill("solid", fgColor="D6E4F7")   # light blue
    ALT_ROW_FILL  = PatternFill("solid", fgColor="F2F7FF")   # very light blue
    WHITE_FILL    = PatternFill("solid", fgColor="FFFFFF")

    thin = Side(style="thin", color="B8CCE4")
    border = Border(left=thin, right=thin, top=thin, bottom=thin)

    row_col_count = 1 if isinstance(PIVOT_ROWS, str) else len(PIVOT_ROWS)
    total_cols = ws.max_column
    total_rows = ws.max_row

    # ── header row ───────────────────────────────────────────────────────────
    for cell in ws[1]:
        cell.font      = Font(name="Arial", bold=True, color="FFFFFF", size=11)
        cell.fill      = HEADER_FILL
        cell.alignment = Alignment(horizontal="center", vertical="center",
                                   wrap_text=True)
        cell.border    = border

    # shade the row-index header cells slightly differently
    for c in range(1, row_col_count + 1):
        ws.cell(1, c).fill = PatternFill("solid", fgColor="1F3864")

    # ── data rows ────────────────────────────────────────────────────────────
    for row_idx in range(2, total_rows + 1):
        fill = ALT_ROW_FILL if row_idx % 2 == 0 else WHITE_FILL
        for col_idx in range(1, total_cols + 1):
            cell = ws.cell(row_idx, col_idx)
            cell.border = border
            cell.font   = Font(name="Arial", size=10)
            # row-index columns: light blue background, left-aligned
            if col_idx <= row_col_count:
                cell.fill      = SUBHEAD_FILL
                cell.font      = Font(name="Arial", bold=True, size=10)
                cell.alignment = Alignment(horizontal="left", vertical="center")
            else:
                cell.fill      = fill
                cell.alignment = Alignment(horizontal="center", vertical="center")

    # ── column widths ────────────────────────────────────────────────────────
    for col_idx in range(1, total_cols + 1):
        col_letter = get_column_letter(col_idx)
        max_len = max(
            len(str(ws.cell(r, col_idx).value or ""))
            for r in range(1, total_rows + 1)
        )
        ws.column_dimensions[col_letter].width = min(max(max_len + 4, 12), 30)

    ws.row_dimensions[1].height = 32   # taller header row
    ws.freeze_panes = "A2"             # freeze header

    # ── metadata sheet ───────────────────────────────────────────────────────
    meta = wb.create_sheet("Config")
    meta_rows = [
        ("Setting", "Value"),
        ("CSV folder",     os.path.abspath(CSV_FOLDER)),
        ("Pivot rows",     str(PIVOT_ROWS)),
        ("Pivot columns",  PIVOT_COLUMNS),
        ("Value column",   VALUE_COLUMN),
        ("Aggregation",    AGG_FUNC),
        ("Filters",        str(FILTERS) if FILTERS else "(none)"),
    ]
    for r, (k, v) in enumerate(meta_rows, start=1):
        meta.cell(r, 1).value = k
        meta.cell(r, 2).value = v
        if r == 1:
            for c in (1, 2):
                meta.cell(r, c).font = Font(bold=True, name="Arial")
                meta.cell(r, c).fill = HEADER_FILL
                meta.cell(r, c).font = Font(bold=True, color="FFFFFF", name="Arial")
    meta.column_dimensions["A"].width = 20
    meta.column_dimensions["B"].width = 50

    wb.save(output_path)
    print(f"Saved → {os.path.abspath(output_path)}")


def main():
    df = load_all_csvs(CSV_FOLDER)
    df = apply_filters(df, FILTERS)
    pivot = build_pivot(df)
    style_and_save(pivot, OUTPUT_FILE)


if __name__ == "__main__":
    main()