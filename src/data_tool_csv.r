# Jordan Wingenroth
# 11/15/2021

library(tidyverse)
library(data.table)

### Handy Functions~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bracket_3 <- function(a,b,c) {
  paste0("[",round(a,4),",",round(b,4),",",round(c,4),"]")
}

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
      mutate(global_pc_gdp = (global_pc_gdp / first(global_pc_gdp))^(1/(time-first(time))) - 1)
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
for(i in 1:length(dam_files)) dam_data[[i]] <- lapply(dam_files[[i]], fread)

# Take quantiles
dam_tidy <- list()
for(i in 1:length(dam_data)) {

  dam_tidy[[i]] <- lapply(dam_data[[i]], function(x) {
    y <- tibble(x)[,seq(1, 81, by = 10)]
    colnames(y) <- seq(2020, 2100, by = 10)
    y %>%
      pivot_longer(`2020`:`2100`, names_to = "YEA") %>%
      group_by(YEA) %>%
      summarise(across(value, .fns = list(bot = ~ quantile(.x, .025)*inflate_05_to_20,
                                      mid = ~ median(.x)*inflate_05_to_20,
                                      top = ~ quantile(.x, .975)*inflate_05_to_20)))
  })
  
  names(dam_tidy[[i]]) <- dam_files[[i]]

}

# Extract different damage functions 
# (DICE was combined with sectoral for computational efficiency)
H_S_damages <- lapply(dam_tidy, function(x) {
  x[str_detect(names(x), "h_and_s.*total")]
})



# INDEXING COLUMNS--------------------------------------------------------------

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

# INTERMEDIATE VARIABLES--------------------------------------------------------

time_quantiles <- function(x, var, div_fac = 1) {
  
  x %>%
    filter(time %in% specs$YEA) %>%
    group_by(time) %>%
    summarise_at(vars(var), funs(p025 = quantile(., .025)/div_fac,
                                 p500 = median(.)/div_fac,
                                 p975 = quantile(., .975)/div_fac))
  
}

files <- expand.grid(a = 'consulting-2021-epa-rffmodel/output/Data_Tool/MCS200_', 
                     b = c('RFF', 
                           'SSP'), 
                     c = '_aggregate_2020/results/model_1/',
                     d = c('Socioeconomic_population_global.csv',
                           'Socioeconomic_gdp_global.csv',
                           'Socioeconomic_co2_emissions.csv',
                           'temperature_T.csv',
                           'global_sea_level_sea_level_rise.csv',
                           'OceanPH_pH.csv',
                           'co2_cycle_co2.csv')
) %>%
  transmute(str = paste0(a,b,c,d)) %>%
  unlist()

colnames <- c('POP', 'GDP', 'EMI', 'TEM', 'SEA', 'OPH', 'CON')

int_data <- lapply(files, read_csv)

data <- int_data %>%
  lapply(function(x) rename(x, 'time' = 1, 'var' = 2, 'trial' = 3)) %>%
  lapply(function(x) time_quantiles(x, var = 'var'))

for (i in 1:length(data)) data[[i]]$socio <- str_sub(files[i],54,56)

data2 <- list()

for (i in 1:(length(data)/2)) data2[[i]] <- bind_rows(data[[2*i-1]],data[[2*i]])

unit_multiplication <- c(1, .001, 44/12, 1, 100, 1, 1)

for (i in 1:length(data2)) {
  
  data2[[i]]$p025 <- data2[[i]]$p025 * unit_multiplication[i]
  data2[[i]]$p500 <- data2[[i]]$p500 * unit_multiplication[i]
  data2[[i]]$p975 <- data2[[i]]$p975 * unit_multiplication[i]
  
}

################################################################################
################################################################################
`%*=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 * e2))
###INSERT DUMMY DATA HERE### (N2O em in Mt is modeled as CO2 in Gt divided by 2; CH4 em (Mt) is 10*CO2 (Gt))
data2[[8]] <- data2[[3]]
data2[[8]]$p025 %*=% .5
data2[[8]]$p500 %*=% .5
data2[[8]]$p975 %*=% .5

data2[[9]] <- data2[[3]]
data2[[9]]$p025 %*=% 10
data2[[9]]$p500 %*=% 10
data2[[9]]$p975 %*=% 10

data2[[10]] <- data2[[7]] # atmo conc: n2o (ppb) = co2 (ppm) times .75; ditto ch4 = co2 times 4
data2[[10]]$p025 %*=% .75
data2[[10]]$p500 %*=% .75
data2[[10]]$p975 %*=% .75

data2[[11]] <- data2[[7]]
data2[[11]]$p025 %*=% 4
data2[[11]]$p500 %*=% 4
data2[[11]]$p975 %*=% 4
###INSERT DUMMY DATA HERE###
################################################################################
################################################################################

for (i in 1:length(colnames)) {
  
  data2[[i]][colnames[i]] <- paste0('[',data2[[i]]$p025,',',data2[[i]]$p500,',',data2[[i]]$p975,']')
  
}

###PROCESS DUMMY DATA HERE###
extra_colnames <- c("NOE", "MEM", "NOC", "MEC")
for (i in 1:length(extra_colnames)) {
  
  data2[[i+7]][extra_colnames[i]] <- paste0('[',data2[[i+7]]$p025,',',data2[[i+7]]$p500,',',data2[[i+7]]$p975,']')
  
}
###PROCESS DUMMY DATA HERE###

data2 <- lapply(data2, function(x) select(x, -contains('p', ignore.case = FALSE)))

for (i in 1:length(data2)) csv <- left_join(csv, data2[[i]], by = c('YEA' = 'time', 'XSC' = 'socio'))

# DAMAGES-----------------------------------------------------------------------

files <- expand.grid(a = 'consulting-2021-epa-rffmodel/output/Data_Tool/MCS200_', 
                     b = c('RFF', 
                           'SSP'), 
                     c = c('_aggregate', '_sectoral'),
                     d = '_2020/mcs_mds_main_n200.csv') %>%
  transmute(str = paste0(a,b,c,d)) %>%
  unlist()

XAD <- c('DICE', 'None')

dam_data <- lapply(files, read_csv)

data <- dam_data %>%
  lapply(function(x) rename(x, 'trial' = 1, 'time' = 2, 'damages' = 3)) %>%
  lapply(function(x) time_quantiles(x, var = 'damages'))

for (i in 1:length(data)) data[[i]]$socio <- str_sub(files[i],54,56)

data2 <- list()

for (i in 1:(length(data)/2)) data2[[i]] <- bind_rows(data[[2*i-1]],data[[2*i]])

for (i in 1:length(data2)) data2[[i]]$XAD <- XAD[i]

data3 <- bind_rows(data2) %>%
  mutate(DAC = paste0('[',p025,',',p500,',',p975,']'),
         DAN = paste0('[',p025*80,',',p500*80,',',p975*80,']'),  #DUMMY DATA!!!!
         DAM = paste0('[',p025*6,',',p500*6,',',p975*6,']')) %>% #DUMMY DATA!!!!
  select(-contains('p', ignore.case = FALSE))

csv <- left_join(csv, data3, by = c('YEA' = 'time', 'XSC' = 'socio', 'XAD' = 'XAD'))

# SCC---------------------------------------------------------------------------

files <- expand.grid(a = 'consulting-2021-epa-rffmodel/output/Data_Tool/MCS200_', 
                     b = c('RFF', 
                           'SSP'), 
                     c = c('_aggregate_', '_sectoral_'),
                     y = specs$YEA,
                     d = '/mcs_runs_n200.csv') %>%
  transmute(str = paste0(a,b,c,y,d)) %>%
  unlist()

scc_data <- lapply(files, read_csv)

data <- scc_data %>%
  lapply(function(x) mutate(x, XDR = factor(prtp, labels = c('2% Ramsey',
                                                             '3% Ramsey',
                                                             '2% constant',
                                                             '3% constant')))) %>%
  lapply(function(x) select(x, scc, XDR)) %>%
  lapply(function(x) group_by(x, XDR)) %>%
  lapply(function(x) summarise(x, 
                               p025 = quantile(scc, .025),
                               p500 = mean(scc),
                               p975 = quantile(scc, .975)))

names(data) <- files

year <- str_sub(str_extract(files, '\\d{4}/'),,4)

sectoral <- if_else(str_detect(files, 'sectoral'),'None', 'DICE')

socio <- if_else(str_detect(files, 'RFF'),'RFF', 'SSP')

for (i in 1:length(data)) {
  
  data[[i]]$YEA <- as.numeric(year[i])
  data[[i]]$XAD <- sectoral[i]
  data[[i]]$XSC <- socio[i]
  
}

data2 <- bind_rows(data) %>%
  mutate(CO2 = paste0('{"X":[',
                      p025,
                      ',',
                      p500,
                      ',',
                      p975,
                      '],"XHE":"',
                      p500*.6, # dummy data because I wasn't able to run sector-specific
                      '","XAG":"',
                      p500*.3,
                      '","XEN":"',
                      p500*.1,
                      '","XCI":"',
                      0,
                      '","XOT":"',
                      0,
                      '"}'),
         N2O = paste0('{"X":[', # DUMMY DATA!!!!!!!
                      p025*75,
                      ',',
                      p500*75,
                      ',',
                      p975*75,
                      '],"XHE":"',
                      p500*.6*75, # dummy data because I wasn't able to run sector-specific
                      '","XAG":"',
                      p500*.3*75,
                      '","XEN":"',
                      p500*.1*75,
                      '","XCI":"',
                      0,
                      '","XOT":"',
                      0,
                      '"}'),
         CH4 = paste0('{"X":[', # DUMMY DATA!!!!!!!
                      p025*7,
                      ',',
                      p500*7,
                      ',',
                      p975*7,
                      '],"XHE":"',
                      p500*.6*7, # dummy data because I wasn't able to run sector-specific
                      '","XAG":"',
                      p500*.3*7,
                      '","XEN":"',
                      p500*.1*7,
                      '","XCI":"',
                      0,
                      '","XOT":"',
                      0,
                      '"}')) %>%
  select(-contains('p', ignore.case = FALSE))

csv <- left_join(csv, data2, by = c('XDR', 'YEA', 'XAD', 'XSC'))

# PRO (HISTOGRAMS)--------------------------------------------------------------

### INT-------------------------------------------------------------------------

data <- int_data %>%
  lapply(function(x) rename(x, 'time' = 1, 'var' = 2, 'trial' = 3)) %>%
  lapply(function(x) filter(x, time %in% specs$YEA))

for (i in 1:length(data)) data[[i]]$socio <- if_else(i%%2==1, 'RFF', 'SSP')

data2 <- list()

for (i in 1:(length(data)/2)) data2[[i]] <- bind_rows(data[[2*i-1]], data[[2*i]])

for (i in 1:length(data2)) {
  
  data2[[i]] <- data2[[i]] %>%
    mutate(var = signif(var/max(var), 2)*max(var)*unit_multiplication[i]) # 100 bins regardless of a given variable's magnitude
  
}

###INSERT DUMMY DATA HERE### (N2O em in Mt is modeled as CO2 in Gt divided by 2; CH4 em (Mt) is 10*CO2 (Gt))
data2[[8]] <- data2[[3]]
data2[[8]]$var %*=% .5

data2[[9]] <- data2[[3]]
data2[[9]]$var %*=% 10

data2[[10]] <- data2[[7]] # atmo conc: n2o (ppb) = co2 (ppm) times .75; ditto ch4 = co2 times 4
data2[[10]]$var %*=% .75

data2[[11]] <- data2[[7]]
data2[[11]]$var %*=% 4
###INSERT DUMMY DATA HERE###

data3 <- data2 %>%
  lapply(function(x) group_by(x, time, var, socio)) %>%
  lapply(function(x) summarise(x, n = n())) %>%
  lapply(function(x) mutate(x, str = paste0('["',
                                            var,
                                            '","',
                                            n,
                                            '"]')))

data4 <- lapply(data3, function(x) {
  
  x %>%
    group_by(time, socio) %>%
    summarise(str = paste(str, collapse = ','))
  
})

for (i in 1:length(colnames)) {
  
  data4[[i]][paste0('PRO_',colnames[i])] <- data4[[i]]$str
  data4[[i]] <- select(data4[[i]], -str)
  
}

###PROCESS DUMMY DATA HERE###
for (i in 1:length(extra_colnames)) {
  
  data4[[i+7]][paste0('PRO_',extra_colnames[i])] <- data4[[i+7]]$str
  data4[[i+7]] <- select(data4[[i+7]], -str)
  
}
###PROCESS DUMMY DATA HERE###

for (i in 1:length(data4)) csv <- left_join(csv, data4[[i]], by = c('YEA' = 'time', 'XSC' = 'socio'))

### DAM-------------------------------------------------------------------------

data <- dam_data %>%
  lapply(function(x) rename(x, 'trial' = 1, 'time' = 2, 'damages' = 3)) %>%
  lapply(function(x) filter(x, time %in% specs$YEA))

for (i in 1:length(data)) data[[i]]$socio <- if_else(i%%2==1, 'RFF', 'SSP')

data2 <- list()

for (i in 1:(length(data)/2)) data2[[i]] <- bind_rows(data[[2*i-1]], data[[2*i]])

for (i in 1:length(data2)) data2[[i]]$XAD <- if_else(i%%2==1, 'DICE', 'None')

data3 <- bind_rows(data2)

data4 <- data3 %>%
  mutate(damages = round(damages)) %>%
  group_by(time, damages, socio, XAD) %>%
  summarise(n = n()) %>%
  mutate(str_c = paste0('["',
                      damages,
                      '","',
                      n,
                      '"]'),
         str_n = paste0('["',
                        damages*80, # DUMMY DATA!!!!!
                        '","',
                        n,
                        '"]'),
         str_ch4 = paste0('["',
                        damages*6, # DUMMY DATA!!!!!
                        '","',
                        n,
                        '"]')) %>%
  group_by(time, socio, XAD) %>%
  summarise(PRO_DAC = paste(str_c, collapse = ','),
            PRO_DAN = paste(str_n, collapse = ','),
            PRO_DAM = paste(str_ch4, collapse = ','))

csv <- left_join(csv, data4, by = c('YEA' = 'time', 'XSC' = 'socio', 'XAD'))        

### SCC-------------------------------------------------------------------------

data <- scc_data %>%
  lapply(function(x) mutate(x, XDR = factor(prtp, labels = c('2% Ramsey',
                                                             '3% Ramsey',
                                                             '2% constant',
                                                             '3% constant')))) %>%
  lapply(function(x) select(x, scc, XDR))

names(data) <- files

year <- str_sub(str_extract(files, '\\d{4}/'),,4)

sectoral <- if_else(str_detect(files, 'sectoral'),'None', 'DICE')

socio <- if_else(str_detect(files, 'RFF'),'RFF', 'SSP')

for (i in 1:length(data)) {
  
  data[[i]]$YEA <- as.numeric(year[i])
  data[[i]]$XAD <- sectoral[i]
  data[[i]]$XSC <- socio[i]
  
}

data2 <- bind_rows(data) %>% 
  mutate(scc = round(scc, -1)) %>%
  group_by(YEA, XSC, XAD, XDR, scc) %>%
  summarise(n = n()) %>%
  mutate(str_c = paste0('["',
                      scc,
                      '","',
                      n,
                      '"]'),
         str_n = paste0('["', # DUMMY DATA!!!!!!!!!!
                        scc*80,
                        '","',
                        n,
                        '"]'),
         str_ch4 = paste0('["', # DUMMY DATA!!!!!!!!!
                        scc*7,
                        '","',
                        n,
                        '"]')) %>%
  group_by(YEA, XSC, XAD, XDR) %>%
  summarise(PRO_CO2 = paste(str_c, collapse = ','),
            PRO_N2O = paste(str_n, collapse = ','),
            PRO_CH4 = paste(str_ch4, collapse = ','))

csv <- left_join(csv, data2, by = c('YEA', 'XSC', 'XAD', 'XDR'))

# FINAL FORMATTING--------------------------------------------------------------

csv_final <- mutate(csv, PRO = paste0('{"CO2":[',
                                      PRO_CO2,
                                      '],"N2O":[',
                                      PRO_N2O, # DUMMY DATA!!!
                                      '],"CH4":[',
                                      PRO_CH4, # DUMMY DATA!!!
                                      '],"POP":[',
                                      PRO_POP,
                                      '],"GDP":[',
                                      PRO_GDP,
                                      '],"EMI":[',
                                      PRO_EMI,
                                      '],"NOE":[',
                                      PRO_NOE, # DUMMY DATA!!!
                                      '],"MEM":[',
                                      PRO_MEM, # DUMMY DATA!!!
                                      '],"CON":[',
                                      PRO_CON,
                                      '],"NOC":[',
                                      PRO_NOC, # DUMMY DATA!!!
                                      '],"MEC":[',
                                      PRO_MEC, # DUMMY DATA!!!
                                      '],"TEM":[',
                                      PRO_TEM,
                                      '],"SEA":[',
                                      PRO_SEA,
                                      '],"OPH":[',
                                      PRO_OPH,
                                      '],"DAC":[',
                                      PRO_DAC,
                                      '],"DAN":[',
                                      PRO_DAN,
                                      '],"DAM":[',
                                      PRO_DAM,
                                      ']}')) %>%
  select(XSC,
         XTE,
         XSL,
         XPH,
         XHE,
         XAG,
         XEN,
         XCI,
         XOT,
         XAD,
         XDR,
         YEA,
         POP,
         GDP,
         EMI,
         NOE,
         MEM,
         TEM,
         SEA,
         OPH,
         CON,
         NOC,
         MEC,
         DAC,
         DAN,
         DAM,
         CO2,
         N2O,
         CH4,
         PRO)

write_csv(csv_final, 'scc_data_tool.csv')
