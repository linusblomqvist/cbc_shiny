library(tidyverse)

df <- readRDS("cbc_shiny/sb_cbc_df.rds")
tot_count_by_year <- readRDS("cbc_shiny/tot_count_by_year.rds")
tot_count_by_year <- bind_rows(tot_count_by_year, data.frame(year = 2023, sum_by_year = 26440))

make_species_graph <- function(species) {
  df %>%
    filter(common_name == species,
           year %in% 1977:2023) %>%
    ggplot(aes(x = year, y = count, group = 1)) +
    labs(title = species) +
    geom_line() +
    scale_x_continuous(breaks = seq(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), 10),
                       minor_breaks = seq(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), 1)) +
    scale_y_continuous(breaks = pretty_breaks(),
                       limits = c(0, NA)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size=15))
}

make_species_graph("Chestnut-backed Chickadee")

### Total count
tot_count_by_year %>%
  filter(year %in% 1977:2023) %>%
  ggplot(aes(x = year, y = sum_by_year)) +
  geom_line() +
  ylim(0, NA) +
  labs(y = "",
       caption = "Black line is the actual count and blue line is a smoothed trendline") +
  scale_x_continuous(breaks = seq(1960, 2020, 10),
                     minor_breaks = seq(min(tot_count_by_year$year, na.rm = TRUE),
                                        max(tot_count_by_year$year, na.rm = TRUE), 1)) +
  theme_bw() +
  theme(text = element_text(size=15),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Total number of individuals") +
  geom_smooth(se = FALSE)
