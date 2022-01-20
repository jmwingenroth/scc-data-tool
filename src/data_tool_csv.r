# Jordan Wingenroth
# 11/15/2021

library(tidyverse)

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
