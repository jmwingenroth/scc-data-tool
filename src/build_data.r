#!/usr/bin/env Rscript
# Streamlined data pipeline: Julia model output -> web-ready CSVs
# Replaces data_tool_csv.r + build_all.py

library(tidyverse)

# Constants
INFLATE <- 113.648 / 87.504
YEARS <- seq(2020, 2100, 10)
GASES <- c("CO2", "N2O", "CH4")
PARAM_COLS <- c("XSC", "XTE", "XSL", "XPH", "XHE", "XAG", "XEN", "XCI", "XAD", "XDR", "YEA")

SCENARIO_LABELS <- c(RFF = "RFF-SPs", SSP1 = "SSP1", SSP2 = "SSP2", SSP3 = "SSP3", SSP5 = "SSP5")
DAMAGE_LABELS <- c(
  sectoral = "Rennert et al.",
  dice = "DICE",
  h_and_s = "Howard & Sterner"
)

# Binning config: list(min, max, step)
BINS <- list(
  POP = c(0, 15, 0.5), GDP = c(-0.02, 0.05, 0.0025), EMI = c(-20, 140, 4),
  NOE = c(0, 20, 0.5), MEM = c(0, 1000, 25), TEM = c(0, 6, 0.2),
  SEA = c(0, 200, 5), OPH = c(7.6, 8.2, 0.02), CON = c(0, 1200, 25),
  NOC = c(0, 500, 20), MEC = c(0, 3500, 100), DAC = c(-5, 15, 0.5),
  DAN = c(-500, 5500, 250), DAM = c(-100, 300, 10),
  CO2 = c(-500, 2500, 100), N2O = c(-5e4, 5e5, 1e4), CH4 = c(-5e3, 5e4, 1e3)
)

# Covariate mapping: filename_pattern -> c(output_key, conversion_factor)
COVAR_MAP <- list(
  population_global = c("POP", 1/1000),
  global_pc_gdp = c("GDP", 1),
  co2_emissions = c("EMI", 44/12),
  n2o_emissions = c("NOE", 1),
  ch4_emissions = c("MEM", 1),
  global_temperature_norm = c("TEM", 1),
  sea_level_rise = c("SEA", 100),
  pH = c("OPH", 1),
  `co2` = c("CON", 1),
  `N₂O` = c("NOC", 1),
  `CH₄` = c("MEC", 1)
)

# Helper: create histogram bins
make_hist <- function(vals, key) {
  cfg <- BINS[[key]]
  breaks <- seq(cfg[1], cfg[2], cfg[3])
  centers <- (breaks[-length(breaks)] + breaks[-1]) / 2
  counts <- hist(vals, breaks = c(-Inf, breaks[-c(1, length(breaks))], Inf), plot = FALSE)$counts
  tibble(x = centers, y = counts) %>% filter(y > 0)
}

# Helper: parse SCGHG path
parse_path <- function(path) {
  parts <- str_split(basename(dirname(path)), "-")[[1]]
  list(scenario = parts[2], sector = parts[3], year = as.integer(parts[4]), gas = parts[5])
}

# Load SCGHG data
load_scghg <- function() {
  sc_files <- list.files("output/scghg", "^sc-.*\\.csv$", recursive = TRUE, full.names = TRUE)

  map_dfr(sc_files, function(f) {
    meta <- parse_path(f)
    ce_file <- str_replace(f, "sc-", "cert-equiv-sc-")

    sc <- read_csv(f, show_col_types = FALSE) %>%
      mutate(scghg = scghg * INFLATE)

    ce <- if (file.exists(ce_file)) {
      read_csv(ce_file, show_col_types = FALSE) %>% mutate(ce_scghg = ce_scghg * INFLATE)
    } else tibble()

    sc %>%
      group_by(sector, discount_rate) %>%
      summarise(
        values = list(scghg), mean = mean(scghg),
        low = quantile(scghg, 0.025), high = quantile(scghg, 0.975),
        .groups = "drop"
      ) %>%
      left_join(ce, by = c("sector", "discount_rate")) %>%
      mutate(scenario = meta$scenario, dmg_sector = meta$sector,
             year = meta$year, gas = meta$gas)
  })
}

# Load covariate data
load_covariates <- function() {
  scenarios <- names(SCENARIO_LABELS)

  map_dfr(scenarios, function(scen) {
    pattern <- sprintf("output/covariates/covariates-%s-*/results/model_1/*.csv", scen)
    files <- Sys.glob(pattern)

    map_dfr(files, function(f) {
      fname <- basename(f)
      match <- names(COVAR_MAP)[map_lgl(names(COVAR_MAP), ~str_detect(fname, .x))]
      if (length(match) == 0) return(tibble())

      cfg <- COVAR_MAP[[match[1]]]
      var <- cfg[1]; conv <- as.numeric(cfg[2])

      read_csv(f, show_col_types = FALSE) %>%
        filter(time %in% YEARS) %>%
        mutate(value = .[[2]] * conv, scenario = scen, var = var) %>%
        select(scenario, var, time, trialnum, value)
    })
  }) %>%
    # Convert GDP to growth rate
    group_by(scenario, var, trialnum) %>%
    mutate(value = if_else(
      var == "GDP" & time > min(time),
      (value / first(value))^(1/(time - first(time))) - 1,
      if_else(var == "GDP", NA_real_, value)
    )) %>%
    ungroup()
}

# Load damage data
load_damages <- function() {
  gas_var_map <- c(CO2 = "DAC", N2O = "DAN", CH4 = "DAM")

  map_dfr(names(gas_var_map), function(gas) {
    var <- gas_var_map[gas]
    pattern <- sprintf("output/scghg/*-sectoral-2020-%s-*/mds_%s--*-total.csv", gas, gas)
    files <- Sys.glob(pattern)

    map_dfr(files, function(f) {
      meta <- parse_path(f)
      df <- read_csv(f, show_col_types = FALSE)
      year_cols <- seq(1, 81, 10)  # 2020, 2030, ..., 2100

      map_dfr(seq_along(YEARS), function(i) {
        tibble(
          scenario = meta$scenario, var = var, year = YEARS[i],
          value = df[[year_cols[i]]] * INFLATE
        )
      })
    })
  })
}

# Build main output
build_main <- function(scghg, covars, damages) {
  # Filter SCGHG: RFF+Ramsey or SSP+constant
  scghg_filt <- scghg %>%
    filter(
      (scenario == "RFF" & str_detect(discount_rate, "Ramsey")) |
      (scenario != "RFF" & str_detect(discount_rate, "CDR"))
    ) %>%
    mutate(XDR = str_replace(str_sub(discount_rate, 1, 4), "\\.0", ""))

  # Build one row per (scenario, sector, year, discount_rate) for total
  base <- scghg_filt %>%
    filter(sector == "total") %>%
    distinct(scenario, dmg_sector, year, XDR) %>%
    mutate(
      XSC = SCENARIO_LABELS[scenario],
      XAD = DAMAGE_LABELS[dmg_sector],
      XTE = "FAIR", XSL = "BRICK", XPH = "Fung",
      XHE = if_else(XAD == "Rennert et al.", "Cromar", ""),
      XAG = if_else(XAD == "Rennert et al.", "Moore", ""),
      XEN = if_else(XAD == "Rennert et al.", "Clarke", ""),
      XCI = if_else(XAD == "Rennert et al.", "Diaz", ""),
      YEA = paste0(year, "/01/01")
    )

  # Add SCGHG values for each gas
  for (g in GASES) {
    gas_data <- scghg_filt %>% filter(gas == g)

    # Total values
    totals <- gas_data %>%
      filter(sector == "total") %>%
      select(scenario, dmg_sector, year, XDR,
             !!paste0(g, "_LOW") := low, !!g := mean,
             !!paste0(g, "_HIGH") := high, !!paste0(g, "_CE") := ce_scghg,
             !!paste0(g, "_values") := values)

    base <- left_join(base, totals, by = c("scenario", "dmg_sector", "year", "XDR"))

    # Sector breakdowns (only for sectoral)
    if (any(base$XAD == "None")) {
      sectors <- gas_data %>%
        filter(sector %in% c("cromar_mortality", "agriculture", "energy", "slr")) %>%
        mutate(col = case_when(
          sector == "cromar_mortality" ~ "XHE",
          sector == "agriculture" ~ "XAG",
          sector == "energy" ~ "XEN",
          sector == "slr" ~ "XCI"
        )) %>%
        select(scenario, dmg_sector, year, XDR, col, mean) %>%
        pivot_wider(names_from = col, values_from = mean, names_prefix = paste0(g, "_"))

      base <- left_join(base, sectors, by = c("scenario", "dmg_sector", "year", "XDR"))
    }
  }

  # Add covariates (only for sectoral/"None")
  covar_summary <- covars %>%
    group_by(scenario, var, time) %>%
    summarise(
      low = quantile(value, 0.025, na.rm = TRUE),
      mid = median(value, na.rm = TRUE),
      high = quantile(value, 0.975, na.rm = TRUE),
      values = list(value),
      .groups = "drop"
    ) %>%
    rename(year = time)

  for (v in unique(covars$var)) {
    v_data <- covar_summary %>%
      filter(var == v) %>%
      select(scenario, year,
             !!paste0(v, "_LOW") := low, !!v := mid,
             !!paste0(v, "_HIGH") := high, !!paste0(v, "_values") := values)

    base <- left_join(base, v_data, by = c("scenario", "year"))
  }

  # Add damages
  dmg_summary <- damages %>%
    group_by(scenario, var, year) %>%
    summarise(
      low = quantile(value, 0.025), mid = median(value), high = quantile(value, 0.975),
      values = list(value), .groups = "drop"
    )

  for (v in unique(damages$var)) {
    v_data <- dmg_summary %>%
      filter(var == v) %>%
      select(scenario, year,
             !!paste0(v, "_LOW") := low, !!v := mid,
             !!paste0(v, "_HIGH") := high, !!paste0(v, "_values") := values)

    base <- left_join(base, v_data, by = c("scenario", "year"))
  }

  base
}

# Write outputs
write_outputs <- function(main_df) {
  out_dir <- "output/web_ready/"
  dir.create(out_dir, showWarnings = FALSE)

  # Identify value columns (those ending in _values)
  val_cols <- names(main_df)[str_detect(names(main_df), "_values$")]
  drop_cols <- c(val_cols, "scenario", "dmg_sector", "year")

  # Column order matching Python output
  col_order <- c(PARAM_COLS,
    "CO2_LOW", "CO2", "CO2_HIGH", "CO2_CE", "CO2_XHE", "CO2_XAG", "CO2_XEN", "CO2_XCI",
    "N2O_LOW", "N2O", "N2O_HIGH", "N2O_CE", "N2O_XHE", "N2O_XAG", "N2O_XEN", "N2O_XCI",
    "CH4_LOW", "CH4", "CH4_HIGH", "CH4_CE", "CH4_XHE", "CH4_XAG", "CH4_XEN", "CH4_XCI",
    "POP_LOW", "POP", "POP_HIGH", "GDP_LOW", "GDP", "GDP_HIGH",
    "EMI_LOW", "EMI", "EMI_HIGH", "NOE_LOW", "NOE", "NOE_HIGH",
    "MEM_LOW", "MEM", "MEM_HIGH", "TEM_LOW", "TEM", "TEM_HIGH",
    "SEA_LOW", "SEA", "SEA_HIGH", "OPH_LOW", "OPH", "OPH_HIGH",
    "CON_LOW", "CON", "CON_HIGH", "NOC_LOW", "NOC", "NOC_HIGH",
    "MEC_LOW", "MEC", "MEC_HIGH", "DAC_LOW", "DAC", "DAC_HIGH",
    "DAN_LOW", "DAN", "DAN_HIGH", "DAM_LOW", "DAM", "DAM_HIGH")
  available_cols <- setdiff(names(main_df), drop_cols)
  col_order <- col_order[col_order %in% available_cols]

  # Write main CSV
  main_out <- main_df %>% select(all_of(col_order))
  write_csv(main_out, file.path(out_dir, "new-main.csv"), na = "", quote = "all")
  cat("Wrote", file.path(out_dir, "new-main.csv"), "\n")

  # Write histogram CSVs
  for (vc in val_cols) {
    var <- str_replace(vc, "_values$", "")
    avg_col <- var

    hist_rows <- main_df %>%
      select(all_of(PARAM_COLS), average = !!avg_col, values = !!vc) %>%
      filter(!map_lgl(values, is.null)) %>%
      mutate(
        average = round(average, 4),
        hist = map2(values, var, ~{
          if (is.null(.x) || all(is.na(.x))) return(tibble())
          make_hist(na.omit(.x), .y)
        })
      ) %>%
      select(-values) %>%
      unnest(hist, keep_empty = FALSE) %>%
      select(all_of(PARAM_COLS), x, average, y) %>%
      # Sort rows to match reference file order (BEFORE converting to character)
      arrange(
        XSC,
        factor(XAD, levels = c("Rennert et al.", "Howard & Sterner", "DICE")),
        XDR,
        YEA,
        x
      ) %>%
      # Convert numeric columns to character for proper quoting
      mutate(
        x = as.character(x),
        average = as.character(average),
        y = as.character(y)
      )

    if (nrow(hist_rows) > 0) {
      # Rename CON to CONC for Windows compatibility (CON is reserved)
      filename <- if_else(var == "CON", "CONC", var)
      write_csv(hist_rows, file.path(out_dir, paste0(filename, ".csv")), quote = "all")
    }
  }

  cat("Wrote histogram CSVs to", out_dir, "\n")
  out_dir
}

# Main
main <- function() {
  cat("Loading SCGHG data...\n")
  scghg <- load_scghg()
  cat("  Loaded", nrow(scghg), "records\n")

  cat("Loading covariate data...\n")
  covars <- load_covariates()
  cat("  Loaded", nrow(covars), "observations\n")

  cat("Loading damage data...\n")
  damages <- load_damages()
  cat("  Loaded", nrow(damages), "observations\n")

  cat("Building main table...\n")
  main_df <- build_main(scghg, covars, damages)
  cat("  Built", nrow(main_df), "rows\n")

  cat("Writing outputs...\n")
  write_outputs(main_df)

  cat("Done!\n")
}

main()
