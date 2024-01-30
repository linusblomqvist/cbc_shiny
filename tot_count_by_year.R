# Total count by year
library(tidyverse)

df <- readRDS("cbc_shiny/sb_cbc_df.rds")

tot_count_by_year <- df %>% 
  group_by(year) %>% 
  summarize(sum_by_year = sum(count, na.rm = TRUE))

tot_count_by_year$sum_by_year[tot_count_by_year$year == 1960] <- NA

saveRDS(tot_count_by_year, "cbc_shiny/tot_count_by_year.rds")

tot_count_by_year %>%
  ggplot(aes(x = year, y = sum_by_year)) +
  geom_line() +
  ylim(0, NA) +
  geom_smooth(method = "lm") +
  labs(y = "total # individuals")
