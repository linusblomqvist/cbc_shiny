# Santa Barbara CBC

source("CBC_historical_data.R")

sb_cbc_input <- read_csv("cbc_sb_historical_data_all_years.csv")

df <- clean_cbc_data(sb_cbc_input, 241, 42272)

# Manually fix apparent errors
df$count[df$common_name == "American Crow" & df$year == 1967] <- df$count[df$common_name == "Common Raven" & df$year == 1967]
df$count[df$common_name == "Common Raven" & df$year == 1967] <- NA
df$count[df$common_name == "Northern Flicker (Yellow-shafted)" & df$year == 1967] <- NA
df$count[df$common_name == "Orange-crowned Warbler" & df$year == 2020] <- 288
df$count[df$common_name == "California Thrasher" & df$year == 1979] <- NA # was 900
df$count[df$common_name == "White-crowned Sparrow" & df$year == 2020] <- 1728
df$count[df$common_name == "Cackling Goose" & df$year %in% 1961:2003] <- NA
df$count[df$common_name == "Spotted Sandpiper" & df$year == 1978] <- NA # Lehman says probably incorrect
df$count[df$common_name == "Lesser Yellowlegs" & df$year == 1971] <- NA # Lehman says probably incorrect
df$count[df$common_name == "Glaucous Gull" & df$year == 1967] <- NA # Lehman doesn't mention this
df$count[df$common_name == "Vaux's Swift" & df$year == 1976] <- NA # Lehman doesn't mention this
df$count[df$common_name == "Yellow-bellied/Red-naped Sapsucker" & df$year %in% 1961:1985] <- df$count[df$common_name == "Yellow-bellied Sapsucker" & df$year %in% 1961:1985]
df$count[df$common_name == "Yellow-bellied Sapsucker" & df$year %in% 1961:1985] <- NA # Lehman only mentions 12 records 1978-1994

df <- df %>%
  filter(common_name != "Manx Shearwater") # No confirmed December records in Lehman

saveRDS(df, "cbc_shiny/sb_cbc_df.rds")
