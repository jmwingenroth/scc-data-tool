library(tidyverse)

# Inflation constant
inflate_05_to_20 = 113.648 / 87.504

scc_files <- list.files(
    path = "output/scghg",
    pattern = "sc-CO2",
    recursive = TRUE, 
    full.names = TRUE
)

ce_scc_files <- list.files(
    path = "output/scghg",
    pattern = "cert-equiv-sc-CO2",
    recursive = TRUE, 
    full.names = TRUE
)

uncertain_scc_files <- scc_files[!scc_files %in% ce_scc_files]

ce_scc <- lapply(ce_scc_files, read_csv, show_col_types = FALSE)
uncertain_scc <- lapply(uncertain_scc_files, read_csv, show_col_types = FALSE)

for (i in 1:length(ce_scc_files)) ce_scc[[i]]$path <- str_sub(ce_scc_files[i],20,-41)
for (i in 1:length(uncertain_scc_files)) uncertain_scc[[i]]$path <- str_sub(uncertain_scc_files[i],20,-30)

ce_inflated <- lapply(ce_scc, mutate, ce_scghg = ce_scghg*inflate_05_to_20)
uncertain_summary_inflated <- lapply(uncertain_scc, group_by, path, sector, discount_rate) %>%
    lapply(
        summarize, 
        mean = mean(scghg*inflate_05_to_20), 
        fifth_percentile = quantile(scghg*inflate_05_to_20, .05), 
        ninety_fifth_percentile = quantile(scghg*inflate_05_to_20, .95)
    )

tidy <- left_join(
        bind_rows(uncertain_summary_inflated), 
        bind_rows(ce_inflated)
    ) %>%
    # Match what is shown on the website
    filter(
        (str_detect(path, "RFF") & str_detect(discount_rate, "Ramsey"))|
        (str_detect(path, "SSP") & str_detect(discount_rate, "CDR"))
    )

# Filepath contains other grouping variables
tidy %>% 
    separate(path, sep = "-", into = c("socioeconomic source", "damage function", "pulse year")) %>%
    write_csv("SCC_summary_statistics.csv")
