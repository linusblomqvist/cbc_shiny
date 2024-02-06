
source("scripts/1_CBC_data_fcn.R")

sb_cbc_input <- read_csv("data/cbc_sb_historical_data_all_years.csv")

df <- cbc_tidy_data_fcn(sb_cbc_input, start_year = 1961, 241, 42272)

tot_count_by_year <- df %>%
  group_by(year) %>%
  summarize(sum_by_year = sum(count))

tot_count_by_year <- bind_rows(data.frame(year = 1960, sum_by_year = NA),
                                          tot_count_by_year, 
                                          data.frame(year = 2023, sum_by_year = 26440))


saveRDS(tot_count_by_year, "cbc_shiny/tot_count_by_year.rds")
