# Jordan Wingenroth
# 11/15/2021

library(tidyverse)
library(data.table)

### Parameters~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Inflation adjustment from https://github.com/anthofflab/paper-scc-give/blob/main/src/price_level_inflator.jl, accessed 02/28/2022:
  # (10/25/2021) BEA Table 1.1.9, line 1 GDP annual values as linked here: https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=3&isuri=1&select_all_years=0&nipa_table_list=13&series=a&first_year=2005&last_year=2020&scale=-99&categories=survey&thetable=
inflate_05_to_20 = 113.648 / 87.504

n_bins <- 20

scenarios <- c("RFF-SPs", "SSP1", "SSP2", "SSP3", "SSP5")

years <- seq(2020, 2100, by = 10)

limits <- list(POP = c(0,     15000),
               GDP = c(-.02,  .05),
               EMI = c(0,     75),
               NOE = c(0,     60),
               MEM = c(0,     1500),
               TEM = c(0,     6),
               SEA = c(0,     200),
               OPH = c(7.6,   8.2),
               CON = c(0,     1000),
               NOC = c(0,     1000),
               MEC = c(0,     3000),
               DAC = c(-5,    20),
               DAN = c(-50,   300),
               DAM = c(-5,    30),
               CO2 = c(0,     1000),
               N20 = c(0,     1e5),
               CH4 = c(0,     1e4))

bins <- lapply(limits, function(x) {
  seq(x[1], x[2], by = (x[2] - x[1]) / (2*n_bins))[seq(2, 2*n_bins, by = 2)]
})

### Handy Functions~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bracket_3 <- function(a,b,c) {
  paste0("[",round(a,4),",",round(b,4),",",round(c,4),"]")
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

# Read data
covar_data <- lapply(covar_files, read_csv) %>%
  lapply(function(x) filter(x, time %in% years))

# Reformat YPC data to growth rate
gdp_idx <- which(str_detect(covar_files, "global_pc_gdp"))
covar_data[gdp_idx] <- covar_data[gdp_idx] %>% 
  lapply(function(x) {
    x %>%
      group_by(trialnum) %>%
      rename(ypc = global_pc_gdp) %>%
      mutate(ypc = (ypc/first(ypc)) ^ (1/(time-first(time))) - 1)
  })

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
  select(XSC, YEA, POP, GDP, EMI, NOE, MEM, TEM, SEA, OPH, CON, NOC, MEC)

### Undiscounted Damages~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

columns <- c("DAC", "DAN", "DAM")

# List files (only from 2020 runs)
list_files_damages <- function(pattern) {
  list.files(path = "output/scghg",
             pattern = pattern,
             recursive = TRUE,
             full.names = TRUE,)
}

dam_files <- list(list_files_damages("mds_CO2"),
                  list_files_damages("mds_N2O"),
                  list_files_damages("mds_CH4")) %>%
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

  damages_tidy[[i]] <- bind_rows(q_agg_sectoral[[i]], q_DICE_damages[[i]], q_H_S_damages[[i]]) %>%
    select(XSC, XAD, YEA, everything())

}

damages_final <- damages_tidy[[1]] %>%
  left_join(damages_tidy[[2]]) %>%
  left_join(damages_tidy[[3]])

### SCGHG~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remember certainty-equivalent
# Remember to correct for 2005-2020 inflation

### Histogram~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use covar_data, agg_sectoral, DICE_damages, H_S_damages, ...

### Combine and add indexing columns~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

specs <- list(
  XSC = c('RFF', 'SSP'),
  XTE = 'FAIR',
  XSL = 'BRICK',
  XPH = 'Fung',
  XHE = c('Cromar', 'None'),
  XAG = c('Moore', 'None'),
  XEN = c('Clarke', 'None'),
  XCI = 'None',
  XOT = 'None',
  XAD = c('DICE','None'),
  XDR = c('2% constant', '3% constant', '2% Ramsey', '3% Ramsey'),
  YEA = c(2020, 2022, seq(2030,2100,by=10))
)

csv <- expand.grid(specs) %>%
  tibble %>%
  filter((XHE=='Cromar' & XAG=='Moore' & XEN=='Clarke' & XAD=='None') | 
           (XHE=='None' & XAG=='None' & XEN=='None' & XAD=='DICE')) %>%
  arrange(XSC,XHE,XDR,YEA)

