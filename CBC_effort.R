# Effort

# Load packages
library(tidyverse)
library(stringr)
library(readr)
library(magrittr)

# Load raw dataframe
df_raw <- read_csv("cbc_sb_historical_data_all_years.csv")

# Effort
df_effort <- df_raw %>%
  slice(58:119) %>%
  select(CircleName, LatLong) %>%
  rename(count_no = CircleName, hours = LatLong)

df_effort$count_no <- as.numeric(df_effort$count_no)

df_effort <- df_effort %>%
  arrange(count_no)

df_effort$hours <- str_extract(df_effort$hours, "(?<=,)(.*?)(?=,)")
df_effort$hours <- as.numeric(df_effort$hours)
df_effort$count_no <- df_effort$count_no - 1 + 1900
colnames(df_effort)[1] <- "year"

saveRDS(df_effort, "cbc_shiny/df_effort.rds")

df_effort %>%
  ggplot(aes(x = year, y = hours)) +
  labs(y = "party-hours") +
  geom_line() +
  scale_x_continuous(breaks = seq(1960, 2020, 10),
                     minor_breaks = seq(min(df_effort$year, na.rm = TRUE), 
                                        max(df_effort$year, na.rm = TRUE), 1))
