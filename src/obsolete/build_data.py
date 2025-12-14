#!/usr/bin/env python3
"""
Streamlined data pipeline: Julia model output -> web-ready CSVs.
Replaces data_tool_csv.r + build_all.py with a single script.
"""
import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime

# Constants
INFLATE_05_TO_20 = 113.648 / 87.504
YEARS = list(range(2020, 2110, 10))
GASES = ["CO2", "N2O", "CH4"]
SCENARIOS = ["RFF", "SSP1", "SSP2", "SSP3", "SSP5"]
SCENARIO_LABELS = {"RFF": "RFF-SPs", "SSP1": "SSP1", "SSP2": "SSP2", "SSP3": "SSP3", "SSP5": "SSP5"}
DAMAGE_FUNCTIONS = {"sectoral": "None", "dice": "DICE", "h_and_s": "Howard & Sterner"}
PARAM_COLS = ["XSC", "XTE", "XSL", "XPH", "XHE", "XAG", "XEN", "XCI", "XAD", "XDR", "YEA"]

# Binning configuration: (min, max, bin_size)
BINS = {
    "POP": (0, 15, 0.5), "GDP": (-0.02, 0.05, 0.0025), "EMI": (-20, 140, 4),
    "NOE": (0, 20, 0.5), "MEM": (0, 1000, 25), "TEM": (0, 6, 0.2),
    "SEA": (0, 200, 5), "OPH": (7.6, 8.2, 0.02), "CON": (0, 1200, 25),
    "NOC": (0, 500, 20), "MEC": (0, 3500, 100), "DAC": (-5, 15, 0.5),
    "DAN": (-500, 5500, 250), "DAM": (-100, 300, 10),
    "CO2": (-500, 2500, 100), "N2O": (-50000, 500000, 10000), "CH4": (-5000, 50000, 1000),
}

# Covariate file mapping: filename pattern -> (output_key, unit_conversion)
COVAR_MAP = {
    "population_global": ("POP", 1/1000),  # millions -> billions
    "global_pc_gdp": ("GDP", 1),  # converted to growth rate later
    "co2_emissions": ("EMI", 44/12),  # GtC -> GtCO2
    "n2o_emissions": ("NOE", 1),
    "ch4_emissions": ("MEM", 1),
    "global_temperature_norm": ("TEM", 1),
    "sea_level_rise": ("SEA", 100),  # m -> cm
    "pH": ("OPH", 1),
    "co2": ("CON", 1),
    "N₂O": ("NOC", 1),
    "CH₄": ("MEC", 1),
}


def make_bins(key):
    """Create bin edges and centers for a variable."""
    lo, hi, step = BINS[key]
    edges = np.arange(lo, hi + step, step)
    centers = (edges[:-1] + edges[1:]) / 2
    return edges, centers


def histogram(values, key):
    """Bin values and return [(center, count), ...] for non-zero bins."""
    edges, centers = make_bins(key)
    counts, _ = np.histogram(values, bins=np.concatenate([[-np.inf], edges[1:-1], [np.inf]]))
    return [(round(c, 5), int(n)) for c, n in zip(centers, counts) if n > 0]


def quantiles(values, probs=(0.025, 0.5, 0.975)):
    """Return quantiles of values."""
    return [np.nanquantile(values, p) for p in probs]


def list_files(pattern, base="output"):
    """List files matching pattern under base directory."""
    return sorted(Path(base).glob(pattern))


def parse_scghg_path(path):
    """Extract metadata from SCGHG file path."""
    # Pattern: scghg-{scenario}-{sector}-{year}-{gas}-n{n}/...
    parts = path.parent.name.split("-")
    return {"scenario": parts[1], "sector": parts[2], "year": int(parts[3]), "gas": parts[4]}


def load_scghg_data():
    """Load all SCGHG and certainty-equivalent data."""
    records = []
    for sc_file in list_files("scghg/*/sc-*.csv"):
        meta = parse_scghg_path(sc_file)
        ce_file = sc_file.parent / sc_file.name.replace("sc-", "cert-equiv-sc-")

        sc_df = pd.read_csv(sc_file)
        ce_df = pd.read_csv(ce_file) if ce_file.exists() else pd.DataFrame()

        # Merge CE values
        if not ce_df.empty:
            ce_map = ce_df.set_index(["sector", "discount_rate"])["ce_scghg"].to_dict()
        else:
            ce_map = {}

        for (sector, dr), grp in sc_df.groupby(["sector", "discount_rate"]):
            vals = grp["scghg"].values * INFLATE_05_TO_20
            ce = ce_map.get((sector, dr), np.nan) * INFLATE_05_TO_20
            records.append({
                **meta, "sector": sector, "discount_rate": dr,
                "values": vals, "mean": np.mean(vals), "ce": ce,
                "low": np.quantile(vals, 0.025), "high": np.quantile(vals, 0.975),
            })
    return pd.DataFrame(records)


def load_covariate_data():
    """Load all covariate data from model_1 files."""
    data = {}
    for scenario in SCENARIOS:
        pattern = f"covariates/covariates-{scenario}-*/results/model_1/*.csv"
        for f in list_files(pattern):
            key = next((k for k, (_, _) in COVAR_MAP.items() if k in f.name), None)
            if not key:
                continue
            var, conv = COVAR_MAP[key]
            df = pd.read_csv(f)
            col = [c for c in df.columns if c not in ["time", "trialnum"]][0]
            df = df[df["time"].isin(YEARS)].copy()
            df["value"] = df[col] * conv
            df["scenario"] = scenario
            df["var"] = var
            data.setdefault((scenario, var), []).append(df[["time", "trialnum", "value"]])

    # Combine and process
    result = {}
    for (scenario, var), dfs in data.items():
        combined = pd.concat(dfs, ignore_index=True)
        if var == "GDP":
            # Convert to growth rate
            combined = combined.sort_values(["trialnum", "time"])
            base = combined.groupby("trialnum")["value"].transform("first")
            base_time = combined.groupby("trialnum")["time"].transform("first")
            combined["value"] = (combined["value"] / base) ** (1 / (combined["time"] - base_time)) - 1
            combined.loc[combined["time"] == base_time, "value"] = np.nan
        result[(scenario, var)] = combined
    return result


def load_damage_data():
    """Load marginal damage data from 2020 pulse year runs."""
    data = {}
    for gas, var in zip(GASES, ["DAC", "DAN", "DAM"]):
        for f in list_files(f"scghg/*-sectoral-2020-{gas}-*/mds_{gas}--*-total.csv"):
            scenario = f.parent.name.split("-")[1]
            df = pd.read_csv(f, header=0)
            # Columns are years 2020-2300, extract decadal
            year_cols = [i for i, y in enumerate(range(2020, 2301)) if y in YEARS]
            for i, year in enumerate(YEARS):
                vals = df.iloc[:, year_cols[i]].values * INFLATE_05_TO_20
                data.setdefault((scenario, var, year), []).extend(vals)
    return data


def build_main_table(scghg_df, covar_data, damage_data):
    """Build the main summary table with all data."""
    rows = []

    # Get unique combinations from SCGHG data
    combos = scghg_df[scghg_df["sector"] == "total"].groupby(
        ["scenario", "sector", "year", "discount_rate"]
    ).first().reset_index()

    for _, row in combos.iterrows():
        scenario, sector, year, dr = row["scenario"], row["sector"], row["year"], row["discount_rate"]

        # Filter: RFF uses Ramsey, SSP uses constant
        is_rff = scenario == "RFF"
        is_ramsey = "Ramsey" in dr
        if is_rff != is_ramsey:
            continue

        xad = DAMAGE_FUNCTIONS.get(sector, "None")

        # Base row
        base = {
            "XSC": SCENARIO_LABELS[scenario],
            "XTE": "FAIR", "XSL": "BRICK", "XPH": "Fung",
            "XHE": "Cromar" if xad == "None" else "None",
            "XAG": "Moore" if xad == "None" else "None",
            "XEN": "Clarke" if xad == "None" else "None",
            "XCI": "Diaz" if xad == "None" else "None",
            "XAD": xad,
            "XDR": dr[:4],  # "2.0%" or "3.0%"
            "YEA": year,
        }

        # Add covariates (only for "None" damage function)
        if xad == "None":
            for var in ["POP", "GDP", "EMI", "NOE", "MEM", "TEM", "SEA", "OPH", "CON", "NOC", "MEC"]:
                key = (scenario, var)
                if key in covar_data:
                    df = covar_data[key]
                    vals = df[df["time"] == year]["value"].dropna().values
                    if len(vals) > 0:
                        q = quantiles(vals)
                        base[f"{var}_LOW"], base[var], base[f"{var}_HIGH"] = q
                        base[f"{var}_hist"] = histogram(vals, var)

        # Add damages
        for gas, var in zip(GASES, ["DAC", "DAN", "DAM"]):
            key = (scenario, var, year)
            if key in damage_data:
                vals = np.array(damage_data[key])
                q = quantiles(vals)
                base[f"{var}_LOW"], base[var], base[f"{var}_HIGH"] = q
                base[f"{var}_hist"] = histogram(vals, var)

        # Add SCGHG for each gas
        for gas in GASES:
            # Get total values
            mask = (scghg_df["scenario"] == scenario) & (scghg_df["year"] == year) & \
                   (scghg_df["discount_rate"] == dr) & (scghg_df["sector"] == "total")

            # Check for correct damage function
            if xad == "None":
                mask &= scghg_df["sector"].isin(["total"])  # sectoral data
                sector_filter = "sectoral"
            elif xad == "DICE":
                sector_filter = "dice"
            else:
                sector_filter = "h_and_s"

            # Re-filter for correct sector in path
            total_rows = scghg_df[mask & scghg_df.apply(
                lambda r: r["sector"] == "total" and parse_scghg_path(
                    Path(f"output/scghg/scghg-{r['scenario']}-{sector_filter}-{r['year']}-{r['gas']}-n10")
                )["sector"] == sector_filter, axis=1
            )]

            # Simpler approach: filter by sector string match in original data
            sector_rows = scghg_df[
                (scghg_df["scenario"] == scenario) &
                (scghg_df["year"] == year) &
                (scghg_df["discount_rate"] == dr) &
                (scghg_df["gas"] == gas)
            ]

            total_row = sector_rows[sector_rows["sector"] == "total"]
            if len(total_row) > 0:
                r = total_row.iloc[0]
                base[f"{gas}_LOW"] = r["low"]
                base[gas] = r["mean"]
                base[f"{gas}_HIGH"] = r["high"]
                base[f"{gas}_CE"] = r["ce"]
                base[f"{gas}_hist"] = histogram(r["values"], gas)

                # Sector breakdowns (only for sectoral damage function)
                if xad == "None":
                    for sec_name, col in [("cromar_mortality", "XHE"), ("agriculture", "XAG"),
                                          ("energy", "XEN"), ("slr", "XCI")]:
                        sec_row = sector_rows[sector_rows["sector"] == sec_name]
                        if len(sec_row) > 0:
                            base[f"{gas}_{col}"] = sec_row.iloc[0]["mean"]

        rows.append(base)

    return pd.DataFrame(rows)


def expand_to_all_damage_functions(main_df, scghg_df, damage_data):
    """Expand main table to include DICE and Howard & Sterner damage functions."""
    rows = list(main_df.to_dict("records"))

    for sector_key, xad in [("dice", "DICE"), ("h_and_s", "Howard & Sterner")]:
        for base_row in main_df.to_dict("records"):
            new_row = base_row.copy()
            new_row["XAD"] = xad
            new_row["XHE"] = "None"
            new_row["XAG"] = "None"
            new_row["XEN"] = "None"
            new_row["XCI"] = "None"

            scenario = [k for k, v in SCENARIO_LABELS.items() if v == base_row["XSC"]][0]
            year = base_row["YEA"]
            dr_full = "2.0% Ramsey" if base_row["XDR"] == "2.0%" and scenario == "RFF" else \
                      "3.0% Ramsey" if base_row["XDR"] == "3.0%" and scenario == "RFF" else \
                      "2.0% CDR" if base_row["XDR"] == "2.0%" else "3.0% CDR"

            # Update SCGHG values
            for gas in GASES:
                sector_rows = scghg_df[
                    (scghg_df["scenario"] == scenario) &
                    (scghg_df["year"] == year) &
                    (scghg_df["discount_rate"] == dr_full) &
                    (scghg_df["gas"] == gas)
                ]
                total_row = sector_rows[sector_rows["sector"] == "total"]
                if len(total_row) > 0:
                    r = total_row.iloc[0]
                    new_row[f"{gas}_LOW"] = r["low"]
                    new_row[gas] = r["mean"]
                    new_row[f"{gas}_HIGH"] = r["high"]
                    new_row[f"{gas}_CE"] = r["ce"]
                    new_row[f"{gas}_hist"] = histogram(r["values"], gas)

                # Remove sector breakdowns
                for col in ["XHE", "XAG", "XEN", "XCI"]:
                    new_row.pop(f"{gas}_{col}", None)

            # Damages come from the specific sector files, but for simplicity
            # we'll note that DICE/H&S don't have sector-specific damages in the same way
            # Clear damage histograms for non-sectoral
            for var in ["DAC", "DAN", "DAM"]:
                for suffix in ["_LOW", "", "_HIGH", "_hist"]:
                    new_row.pop(f"{var}{suffix}", None)

            rows.append(new_row)

    return pd.DataFrame(rows)


def write_outputs(main_df, out_dir):
    """Write new-main.csv and per-variable histogram CSVs."""
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    # Identify histogram columns
    hist_cols = [c for c in main_df.columns if c.endswith("_hist")]
    scalar_cols = [c for c in main_df.columns if not c.endswith("_hist")]

    # Write main CSV (without histogram columns)
    main_out = main_df[scalar_cols].copy()
    main_out.to_csv(out_dir / "new-main.csv", index=False)
    print(f"Wrote {out_dir / 'new-main.csv'}")

    # Write histogram CSVs
    for hist_col in hist_cols:
        var = hist_col.replace("_hist", "")
        rows = []
        for _, row in main_df.iterrows():
            hist = row.get(hist_col)
            if not hist or not isinstance(hist, list):
                continue
            avg = row.get(var, np.nan)
            for x, y in hist:
                rows.append({
                    **{c: row[c] for c in PARAM_COLS},
                    "x": x, "average": avg, "y": y
                })
        if rows:
            pd.DataFrame(rows).to_csv(out_dir / f"{var}.csv", index=False)

    print(f"Wrote histogram CSVs to {out_dir}")


def main():
    print("Loading SCGHG data...")
    scghg_df = load_scghg_data()
    print(f"  Loaded {len(scghg_df)} SCGHG records")

    print("Loading covariate data...")
    covar_data = load_covariate_data()
    print(f"  Loaded {len(covar_data)} covariate series")

    print("Loading damage data...")
    damage_data = load_damage_data()
    print(f"  Loaded {len(damage_data)} damage series")

    print("Building main table...")
    main_df = build_main_table(scghg_df, covar_data, damage_data)
    print(f"  Built {len(main_df)} rows")

    # Note: expand_to_all_damage_functions would add DICE/H&S rows
    # For now, we only have sectoral data in the test set

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    out_dir = Path(f"dist_output_{timestamp}")
    write_outputs(main_df, out_dir)

    print("Done!")


if __name__ == "__main__":
    main()
