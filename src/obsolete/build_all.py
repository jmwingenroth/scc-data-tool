#!/usr/bin/env python3
import argparse, json
from pathlib import Path
import pandas as pd
from datetime import datetime

RAW_PARAM_COLS = ["XSC", "XTE", "XSL", "XPH", "XHE", "XAG", "XEN", "XCI", "XAD", "XDR", "YEA"]
GASES = ["CO2", "N2O", "CH4"]
SECTOR_KEYS = ["XHE", "XAG", "XEN", "XCI"]

OMIT_COLS = set("""
POP_LOW POP POP_HIGH GDP_LOW GDP GDP_HIGH EMI_LOW EMI EMI_HIGH NOE_LOW NOE NOE_HIGH
MEM_LOW MEM MEM_HIGH TEM_LOW TEM TEM_HIGH SEA_LOW SEA SEA_HIGH OPH_LOW OPH OPH_HIGH
CON_LOW CON CON_HIGH NOC_LOW NOC NOC_HIGH MEC_LOW MEC MEC_HIGH DAC_LOW DAC DAC_HIGH
DAN_LOW DAN DAN_HIGH DAM_LOW DAM DAM_HIGH CO2 N2O CH4
""".split())  # PRO retained

def _to_float(x):
    try:
        return float(x)
    except Exception:
        return pd.NA

def _parse_json_cell(s):
    s = (s or "").strip()
    if not s:
        return {}
    try:
        return json.loads(s)
    except Exception:
        return {}

def normalize_columns(df: pd.DataFrame) -> pd.DataFrame:
    df.columns = [c.strip().lstrip("\ufeff") for c in df.columns]
    return df

def map_existing(df: pd.DataFrame, wanted: list[str]) -> list[str]:
    have = set(df.columns)
    missing = [c for c in wanted if c not in have]
    if missing:
        raise ValueError(f"Missing required columns: {missing}. Found: {list(df.columns)}")
    return wanted

def parse_keys_from_first_row(df: pd.DataFrame, pro_col: str = "PRO") -> list[str]:
    if pro_col not in df.columns:
        raise ValueError(f"Expected column '{pro_col}' not found.")
    for v in df[pro_col]:
        s = (v or "").strip()
        if s:
            d = json.loads(s)
            return list(d.keys())
    raise ValueError("Could not find a non-empty PRO cell to infer keys.")

def expand_gas(df: pd.DataFrame, gas: str) -> None:
    """Expands CO2/N2O/CH4 JSON into numeric columns."""
    original_col = f"{gas}_JSON"
    if original_col not in df.columns:
        return
    parsed = df[original_col].apply(_parse_json_cell)
    x_list = parsed.apply(lambda d: d.get("X", []))
    df[f"{gas}_LOW"]  = x_list.apply(lambda arr: _to_float(arr[0]) if len(arr) > 0 else pd.NA)
    df[f"{gas}"]      = x_list.apply(lambda arr: _to_float(arr[1]) if len(arr) > 1 else pd.NA)
    df[f"{gas}_HIGH"] = x_list.apply(lambda arr: _to_float(arr[2]) if len(arr) > 2 else pd.NA)
    df[f"{gas}_CE"]   = x_list.apply(lambda arr: _to_float(arr[3]) if len(arr) > 3 else pd.NA)
    for sk in SECTOR_KEYS:
        df[f"{gas}_{sk}"] = parsed.apply(lambda d: _to_float(d.get(sk)) if sk in d else pd.NA)

def get_average_value(row, key):
    """Return the correct 'average' value for this key from the row."""
    if key in GASES:
        json_col = f"{key}_JSON"
        if json_col in row and isinstance(row[json_col], str) and row[json_col].strip():
            try:
                data = json.loads(row[json_col])
                x_vals = data.get("X", [])
                if len(x_vals) > 1:
                    return _to_float(x_vals[1])
            except Exception:
                return pd.NA
        return pd.NA
    else:
        return _to_float(row.get(key, pd.NA))

def explode_key(df: pd.DataFrame, key: str, param_cols: list[str], pro_col: str = "PRO") -> pd.DataFrame:
    """Explode the PRO JSON for one key into rows, adding average per source row."""
    all_rows = []
    for i, row in df.iterrows():
        s = (row.get(pro_col) or "").strip()
        if not s:
            continue
        try:
            data = json.loads(s)
            pairs = data.get(key, [])
        except Exception:
            continue
        avg_val = get_average_value(row, key)
        for xy in pairs:
            if len(xy) != 2:
                continue
            try:
                x_val = _to_float(xy[0])
                y_val = _to_float(xy[1])
                record = {col: row[col] for col in param_cols}
                record.update({"x": x_val, "average": avg_val, "y": y_val})
                all_rows.append(record)
            except Exception:
                continue
    if not all_rows:
        return pd.DataFrame(columns=param_cols + ["x", "average", "y"])
    return pd.DataFrame(all_rows)

def main():
    ap = argparse.ArgumentParser(description="Expand gas and PRO columns into structured CSVs with averages.")
    ap.add_argument("--input", default="main.csv", help="Path to input CSV (default: main.csv)")
    args = ap.parse_args()

    in_path = Path(args.input)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    out_dir = Path(f"dist_output_{timestamp}")
    out_dir.mkdir(parents=True, exist_ok=True)

    df = pd.read_csv(in_path, dtype=str, keep_default_na=False, engine="python")
    df = normalize_columns(df)

    # rename gas JSONs
    for gas in GASES:
        if gas in df.columns:
            df.rename(columns={gas: f"{gas}_JSON"}, inplace=True)

    # expand gas columns
    for gas in GASES:
        expand_gas(df, gas)

    # Write cleaned main file
    drop_cols = [f"{gas}_JSON" for gas in GASES if f"{gas}_JSON" in df.columns]
    if "PRO" in df.columns:
        drop_cols.append("PRO")
    final_df = df.drop(columns=drop_cols)
    final_main_path = out_dir / "new-main.csv"
    final_df.to_csv(final_main_path, index=False)
    print(f"✅ Wrote cleaned main file: {final_main_path.resolve()}")

    # Expand PRO to per-key CSVs
    param_cols = map_existing(df, RAW_PARAM_COLS)
    keys = parse_keys_from_first_row(df, pro_col="PRO")
    keys = list(dict.fromkeys(keys))

    # Keep PRO + param columns + any scalar columns matching PRO keys
    keep = set(param_cols + ["PRO"] + keys + [f"{gas}_JSON" for gas in GASES])
    df_pro = df[[c for c in df.columns if c in keep]]

    for key in keys:
        out_df = explode_key(df_pro, key, param_cols, pro_col="PRO")
        out_df.to_csv(out_dir / f"{key}.csv", index=False)

    print(f"✅ Wrote {len(keys)} PRO files to: {out_dir.resolve()}")

if __name__ == "__main__":
    main()
