library(tidyverse)

df <- readRDS("cbc_shiny/sb_cbc_df.rds")

df <- df %>%
  filter(!(common_name %in% c("Green-winged Teal (Eurasian)",
                              "Western/Clark's Grebe",
                              "Sooty/Short-tailed Shearwater",
                              "American/Pacific Golden-Plover",
                              "Short-billed/Long-billed Dowitcher",
                              "jaeger sp.",
                              "Scripps's/Guadalupe Murrelet",
                              "Rufous/Allen's Hummingbird",
                              "Yellow-bellied/Red-naped Sapsucker",
                              "Northern Flicker (Yellow-shafted)",
                              "Dark-eyed Junco (Slate-colored)",
                              "Dark-eyed Junco (Gray-headed)",
                              "Rose-breasted/Black-headed Grosbeak")))

tot_sp_by_year <- df %>%
  filter(!is.na(count), count != 0) %>%
  group_by(year) %>%
  summarize(tot_sp_by_year = n())

tot_sp_df <- data.frame(year = 1960:2022)

tot_sp_df <- tot_sp_df %>%
  left_join(tot_sp_by_year, by = "year")

tot_sp_2023 <- data.frame(year = 2023, tot_sp_by_year = 194)

tot_sp_df <- bind_rows(tot_sp_df, tot_sp_2023)

saveRDS(tot_sp_df, "cbc_shiny/tot_sp_df.rds")
