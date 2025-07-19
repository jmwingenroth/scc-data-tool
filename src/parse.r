library(tidyverse)

web_ready <- read_csv("web_ready.csv")

web_ready %>%
    select(XSC, XAD, XDR, YEA, CO2, CH4, N2O) %>%
    filter(XSC == "RFF-SPs", XAD == "None") %>%
    separate_wider_delim(CO2:N2O, ",", names_sep = "_") %>%
    select(XDR, YEA, matches("[1-3]$")) %>%
    mutate(across(CO2_1:N2O_3, \(x) str_extract(x, "(-*\\d*\\.?\\d+)"))) %>%
    mutate(across(CO2_1:N2O_3, as.numeric)) %>%
    write_csv("rff_defaults.csv")
