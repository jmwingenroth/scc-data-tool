# Jordan Wingenroth
# 11/15/2021

library(tidyverse)
library(data.table)

### Parameters~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Inflation adjustment from https://github.com/anthofflab/paper-scc-give/blob/main/src/price_level_inflator.jl, accessed 02/28/2022:
  # (10/25/2021) BEA Table 1.1.9, line 1 GDP annual values as linked here: https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=3&isuri=1&select_all_years=0&nipa_table_list=13&series=a&first_year=2005&last_year=2020&scale=-99&categories=survey&thetable=
inflate_05_to_20 = 113.648 / 87.504

n_bins <- 20

scenarios <- c("RFF-SPs", "SSP1", "SSP2", "SSP3", "SSP5") # alphabetical order

gases <- c("CH4", "CO2", "N2O")

years <- seq(2020, 2100, by = 10)

limits <- list(POP = c(0,     15000),   # Millions
               GDP = c(-.02,  .05),     # Growth rate
               EMI = c(0,     75),      # Gt CO2
               NOE = c(0,     60),      # Mt N2O
               MEM = c(0,     1500),    # Mt CH4
               TEM = c(0,     6),       # Anomaly (K)
               SEA = c(0,     200),     # cm
               OPH = c(7.6,   8.2),     # pH (no units)
               CON = c(0,     1000),    # ppm
               NOC = c(0,     1000),    # ppb
               MEC = c(0,     3000),    # ppb
               DAC = c(-5,    20),      # 2020 USD
               DAN = c(-50,   300),     # 2020 USD
               DAM = c(-5,    30),      # 2020 USD
               CO2 = c(0,     1000),    # 2020 USD
               N20 = c(0,     1e5),     # 2020 USD
               CH4 = c(0,     1e4))     # 2020 USD

breaks <- lapply(limits, function(x) {
  seq(x[1], x[2], length.out = 21)[-c(1,21)]
})

### Handy Functions~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bracket_3 <- function(a,b,c) {
  paste0("[",round(a,4),",",round(b,4),",",round(c,4),"]")
}

bracket_4 <- function(a,b,c,d) {
  paste0("[",round(a,4),",",round(b,4),",",round(c,4),",",round(d,4),"]")
}

SCC_format <- function(X = bracket_4(0,0,0,0), 
                       XHE = 0, 
                       XAG = 0, 
                       XEN = 0, 
                       XCI = 0, 
                       XOT = 0) {

  paste0('{"X":',X,
         ',"XHE":',round(XHE,4),
         ',"XAG":',round(XAG,4),
         ',"XEN":',round(XEN,4),
         ',"XCI":',round(XCI,4),
         ',"XOT":',round(XOT,4),
         '}')

}

list_files_scghg <- function(pattern) {
  list.files(path = "output/scghg",
             pattern = pattern,
             recursive = TRUE,
             full.names = TRUE,)
}

### Socioeconomic and Physical Variables~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# List files
covar_files <- list.files(path = "output/covariates",
           recursive = TRUE,
           full.names = TRUE) %>%
  .[str_detect(., "model_1")]

file_key <- c("MEC", "CON", "SEA", "NOC", "OPH", 
              "GDP", "MEM", "EMI", "NOE", "POP", 
              "TEM") %>%
  expand_grid(XSC = scenarios, var = .) 

file_key$conversions <- rep(c(1, 1, 100, 1, 1,
                              1, 1, 44/12, 1, 1, # GDP (YPC) growth rate is unitless
                              1), length(scenarios))

# Read, rename, and convert units of data
covar_data <- lapply(covar_files, read_csv, show_col_types = FALSE) %>%
  lapply(function(x) filter(x, time %in% years))

for (i in 1:length(covar_data)) {
  names(covar_data[[i]])[2] <- file_key$var[i]
  covar_data[[i]][,2] <- file_key$conversions[i] * covar_data[[i]][,2]
}

# Reformat YPC data to growth rate
gdp_idx <- which(str_detect(covar_files, "global_pc_gdp"))
covar_data[gdp_idx] <- covar_data[gdp_idx] %>% 
  lapply(function(x) {
    x %>%
      group_by(trialnum) %>%
      mutate(GDP = (GDP/first(GDP)) ^ (1/(time-first(time))) - 1)
  })

# Transform to histogram format
covar_hist <- list()
for (i in 1:length(covar_data)) {

  temp_breaks <- breaks[[which(names(breaks)==names(covar_data[[i]])[2])]]
  temp_limits <- limits[[which(names(limits)==names(covar_data[[i]])[2])]]

  covar_hist[[i]] <- covar_data[[i]] %>%
    rename(var = 2) %>%
    mutate(across(2, ~ cut(.x, breaks = c(-Inf,
                                          temp_breaks,
                                          Inf), 
                               labels = round(c(temp_limits[1], 
                                                temp_breaks + (temp_limits[2] - temp_limits[1])/40),4)))) %>%
    group_by(time, var) %>%
    tally() %>%
    arrange(time, var) %>%
    mutate(str = paste0('["',var,'","',n,'"]')) %>%
    summarise(PRO = paste0(str, collapse = ",")) %>%
    ungroup() %>%
    mutate(YEA = time) %>%
    select(YEA, PRO) %>%
    transmute(XSC = file_key$XSC[i], 
              YEA, 
              x = file_key$var[i], 
              PRO_x = paste0('"',names(covar_data[[i]])[2],'":[',PRO,']'))

}

covar_hist_tidy <- bind_rows(covar_hist) %>% 
  pivot_wider(names_from = x, values_from = PRO_x) %>%
  transmute(XSC, YEA,
            PRO_covar = paste(POP, GDP, EMI, NOE, MEM, CON, NOC, MEC, TEM, SEA, OPH,
                              sep = ","))

# Take quantiles and format
covar_tidy <- lapply(covar_data, function(x) {
  x %>%
    group_by(time) %>%
    summarise(across(1, .fns = list(bot = ~ quantile(.x, .025),
                                    mid = ~ median(.x),
                                    top = ~ quantile(.x, .975)))) %>%
    rename(YEA = time, v1 = 2, v2 = 3, v3 = 4) %>%
    transmute(YEA, var = bracket_3(v1, v2, v3))
})

for (i in 1:length(covar_tidy)) {
  colnames(covar_tidy[[i]]) <- c("YEA", file_key$var[i])
  covar_tidy[[i]]$XSC <- file_key$XSC[i]
}

# Pivot wider
covar_final <- bind_rows(covar_tidy) %>%
  select(XSC, YEA, everything()) %>%
  group_by(XSC, YEA) %>%
  summarise(across(MEC:TEM, .fns = ~max(.x, na.rm = TRUE))) %>%
  left_join(covar_hist_tidy) %>%
  select(XSC, YEA, POP, GDP, EMI, NOE, MEM, TEM, SEA, OPH, CON, NOC, MEC, PRO_covar)

### Undiscounted Damages~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

columns <- c("DAC", "DAN", "DAM")

# List files (only from 2020 runs)
dam_files <- list(list_files_scghg("mds_CO2"),
                  list_files_scghg("mds_N2O"),
                  list_files_scghg("mds_CH4")) %>%
  lapply(., function(x) x[str_detect(x, "2020")])

# Read data (read_csv was slow)
dam_data <- list()
for(i in 1:length(dam_files)) {
  dam_data[[i]] <- lapply(dam_files[[i]], fread)
  names(dam_data[[i]]) <- dam_files[[i]]
}

# Tidy data and adjust for inflation
dam_tidy <- list()
for(i in 1:length(dam_data)) {
  dam_tidy[[i]] <- lapply(dam_data[[i]], function(x) {
    y <- tibble(x)[,seq(1, 81, by = 10)]
    colnames(y) <- seq(2020, 2100, by = 10)
    y %>%
      mutate(trialnum = row_number()) %>%
      pivot_longer(`2020`:`2100`, names_to = "YEA") %>%
      mutate(YEA = as.numeric(YEA),
             value = value*inflate_05_to_20)
  })
  names(dam_tidy[[i]]) <- dam_files[[i]]
}

# Extract different damage functions 
# (DICE was combined with sectoral for computational efficiency)
H_S_damages <- lapply(dam_tidy, function(x) {
  x[str_detect(names(x), "h_and_s.*total")]
})

sectoral_damages <- lapply(dam_tidy, function(x) {
  x[!str_detect(names(x), "h_and_s") & !str_detect(names(x), "total")]
})

DICE_plus_sectoral <- lapply(dam_tidy, function(x) {
  x[str_detect(names(x), "sectoral.*total")]
})

# Aggregate sectoral damages
agg_sectoral <- list()
for (i in 1:length(sectoral_damages)) {
  agg_sectoral[[i]] <- list()
  sectoral_damages[[i]] <- lapply(sectoral_damages[[i]], function(x) as.matrix(x))
  n <- length(sectoral_damages[[i]])/length(scenarios)
  for (j in 1:length(scenarios)) {

    agg_sectoral[[i]][[j]] <- Reduce('+', sectoral_damages[[i]][(n*(j-1) + 1):(n*j)])
    agg_sectoral[[i]][[j]][,"YEA"] <- agg_sectoral[[i]][[j]][,"YEA"]/n 
    agg_sectoral[[i]][[j]][,"trialnum"] <- agg_sectoral[[i]][[j]][,"trialnum"]/n 
  
  }
  agg_sectoral[[i]] <- lapply(agg_sectoral[[i]], as_tibble)
}

# Subtract sectoral damages from DICE+sectoral
DICE_damages <- list()
for (i in 1:length(agg_sectoral)) {
  DICE_damages[[i]] <- list()
  for (j in 1:length(agg_sectoral[[i]])) {

    DICE_damages[[i]][[j]] <- DICE_plus_sectoral[[i]][[j]]
    DICE_damages[[i]][[j]]$value <- DICE_plus_sectoral[[i]][[j]]$value - agg_sectoral[[i]][[j]]$value

  }
}

# Fix names
for (i in 1:length(agg_sectoral)) {
  names(agg_sectoral[[i]]) <- scenarios
  names(DICE_damages[[i]]) <- scenarios
  names(H_S_damages[[i]]) <- scenarios
}

# Take quantiles (keep data for histogram) and reformat
quantiles_damages <- function(x, name) {
  x %>%
    group_by(YEA) %>%
    summarise(across(value, .fns = list(bot = ~ quantile(.x, .025),
                                        mid = ~ median(.x),
                                        top = ~ quantile(.x, .975)))) %>%
    mutate(bracket = bracket_3(value_bot, value_mid, value_top)) %>%
    select(YEA, !!quo_name(name) := bracket)
}

q_agg_sectoral <- list()
q_DICE_damages <- list()
q_H_S_damages <-  list()
damages_tidy <- list()
for (i in 1:length(agg_sectoral)) {

  q_agg_sectoral[[i]] <- lapply(agg_sectoral[[i]], 
                                quantiles_damages,
                                name = columns[i]) %>%
    bind_rows()
  q_agg_sectoral[[i]]$XSC <- rep(scenarios, each = length(years))
  q_agg_sectoral[[i]]$XAD <- "None"

  q_DICE_damages[[i]] <- lapply(DICE_damages[[i]], 
                                quantiles_damages,
                                name = columns[i]) %>%
    bind_rows()
  q_DICE_damages[[i]]$XSC <- rep(scenarios, each = length(years))
  q_DICE_damages[[i]]$XAD <- "DICE"

  q_H_S_damages[[i]] <- lapply(H_S_damages[[i]], 
                                quantiles_damages,
                                name = columns[i]) %>%
    bind_rows()
  q_H_S_damages[[i]]$XSC <- rep(scenarios, each = length(years))
  q_H_S_damages[[i]]$XAD <- "Howard & Sterner"

  damages_tidy[[i]] <- bind_rows(q_agg_sectoral[[i]], 
                                 q_DICE_damages[[i]], 
                                 q_H_S_damages[[i]]) %>%
    select(XSC, XAD, YEA, everything())

}

damages_final <- damages_tidy[[1]] %>%
  left_join(damages_tidy[[2]]) %>%
  left_join(damages_tidy[[3]])

### SCGHG~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Still need to add processing code for certainty-equivalent SCGHG

# List files
scghg_files <- list_files_scghg("sc-")

# Read data and adjust for inflation
scghg_data <- lapply(scghg_files, read_csv, show_col_types = FALSE) %>%
  lapply(., mutate, scghg = scghg*inflate_05_to_20, 
                    discount_rate = str_replace(discount_rate, "CDR", "constant"))

# Extract H&S, sectoral, and DICE data
H_S_idx <- which(str_detect(scghg_files, "h_and_s"))
sec_dice_idx <- which(str_detect(scghg_files, "sectoral_and_dice"))

H_S_scghg <- scghg_data[H_S_idx] %>% 
  lapply(., filter, sector == "total")

sectoral_scghg <- scghg_data[sec_dice_idx] %>% 
  lapply(., filter, sector != "total")

DICE_scghg <- scghg_data[sec_dice_idx] %>% 
  lapply(., group_by, sector, discount_rate) %>%
  lapply(., mutate, trialnum = row_number()) %>%
  lapply(., pivot_wider, names_from = sector, values_from = scghg) %>%
  lapply(., transmute, sector = "total", 
                       discount_rate, 
                       scghg = total - slr - agriculture - energy - cromar_mortality)

# Take quantiles and reformat
q_H_S_scghg <-  list()
q_sectoral_scghg <- list()
q_DICE_scghg <- list()
for (i in 1:length(H_S_scghg)) {

  q_H_S_scghg[[i]] <- H_S_scghg[[i]] %>%
    group_by(discount_rate) %>%
    summarise(across(scghg, .fns = list(bot = ~ quantile(.x, .025),
                                        mid = ~ mean(.x),
                                        top = ~ quantile(.x, .975)))) %>%
    transmute(XSC = scenarios[ceiling(i*length(scenarios)/length(H_S_scghg))],
              XAD = "Howard & Sterner", 
              XDR = discount_rate,
              YEA = years[(ceiling(i/3) - 1) %% length(years) + 1],
              gas = gases[(i - 1)%%3 + 1],
              value = SCC_format(bracket_4(scghg_bot, 
                                           scghg_mid, 
                                           scghg_top, 
                                           scghg_mid*.96))) # placeholder 

  q_DICE_scghg[[i]] <- DICE_scghg[[i]] %>%
    group_by(discount_rate) %>%
    summarise(across(scghg, .fns = list(bot = ~ quantile(.x, .025),
                                        mid = ~ mean(.x),
                                        top = ~ quantile(.x, .975)))) %>%
    transmute(XSC = scenarios[ceiling(i*length(scenarios)/length(DICE_scghg))],
              XAD = "DICE", 
              XDR = discount_rate,
              YEA = years[(ceiling(i/3) - 1) %% length(years) + 1],
              gas = gases[(i - 1)%%3 + 1],
              value = SCC_format(bracket_4(scghg_bot, 
                                           scghg_mid, 
                                           scghg_top, 
                                           scghg_mid*.96))) # placeholder 

  q_sectoral_scghg[[i]] <- sectoral_scghg[[i]] %>%
    group_by(sector, discount_rate) %>%
    summarise(across(scghg, .fns = list(bot = ~ quantile(.x, .025),
                                        mid = ~ mean(.x),
                                        top = ~ quantile(.x, .975))), 
              .groups = "drop") %>%
    group_by(discount_rate) %>%
    mutate(across(scghg_bot:scghg_top, .fns = list(sum = ~ sum(.x)))) %>%
    select(-scghg_bot, -scghg_top) %>%
    pivot_wider(names_from = sector, values_from = scghg_mid) %>%
    ungroup() %>%
    transmute(XSC = scenarios[ceiling(i*length(scenarios)/length(sectoral_scghg))],
              XAD = "None", 
              XDR = discount_rate,
              YEA = years[(ceiling(i/3) - 1) %% length(years) + 1],
              gas = gases[(i - 1)%%3 + 1],
              value = SCC_format(bracket_4(scghg_bot_sum, 
                                           scghg_mid_sum, 
                                           scghg_top_sum, 
                                           scghg_mid_sum*.96), # placeholder
                                   XAG = agriculture,
                                   XHE = cromar_mortality,
                                   XEN = energy,
                                   XCI = slr))
                                                                             
}

scghg_final <- lapply(list(q_H_S_scghg, 
                           q_DICE_scghg, 
                           q_sectoral_scghg), function(x) {

  x %>%
    bind_rows() %>%
    pivot_wider(names_from = gas, values_from = value)

}) %>%
  bind_rows()

### Combine and add indexing columns~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_but_PRO <- right_join(covar_final, damages_final) %>%
  right_join(scghg_final) %>%
  mutate(XTE = "FAIR", XSL = "BRICK", XPH = "Fung", XOT = "None",
         XHE = if_else(XAD == "None", "Cromar", "None"),
         XAG = if_else(XAD == "None", "Moore", "None"),
         XEN = if_else(XAD == "None", "Clarke", "None"),
         XCI = if_else(XAD == "None", "Diaz", "None")) %>%
  select(XSC, XTE, XSL, XPH, XHE, XAG, XEN, XCI, XOT, XAD, XDR, YEA, 
         POP, GDP, EMI, NOE, MEM, TEM, SEA, OPH, CON, NOC, MEC, 
         DAC, DAN, DAM, CO2, N2O, CH4) %>%
  arrange(XSC, desc(XAD), XDR, YEA)

### Histogram~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Covariates

lapply(covar_data, function(x) names(x)[2])
bins[1]
covar_data
# Use covar_data, agg_sectoral, DICE_damages, H_S_damages, ...

### Export~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write_csv(all_but_PRO, "test.csv", quote = "none", escape = "none")
