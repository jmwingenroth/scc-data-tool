library(tidyverse)

scc_files <- list.files("output/scghg", recursive = TRUE, full.names = TRUE)
years <- c(2020, 2030)
pricelevel_2005_to_2020 = 113.648 / 87.504

scc_raw <- lapply(scc_files, read_csv)

for (i in 1:length(scc_raw)) scc_raw[[i]]$year <- years[i]

scc_tidy <- bind_rows(scc_raw) %>%
    mutate(scghg = scghg*pricelevel_2005_to_2020) %>%
    group_by(region, sector, discount_rate, year) %>%
    summarise(mean(scghg), sd(scghg))

write_csv(scc_tidy, "output/scc_summary_statistics.csv")

# Addressing SLR anomaly

scc_tidy_censored <- bind_rows(scc_raw) %>%
    filter(scghg < 10000) %>%
    mutate(scghg = scghg*pricelevel_2005_to_2020) %>%
    group_by(region, sector, discount_rate, year) %>%
    summarise(mean(scghg), sd(scghg))

write_csv(scc_tidy_censored, "output/scc_summary_statistics_censored.csv")
