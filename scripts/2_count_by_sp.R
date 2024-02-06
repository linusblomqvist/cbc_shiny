# Santa Barbara CBC

source("scripts/1_CBC_data_fcn.R")

sb_cbc_input <- read_csv("data/cbc_sb_historical_data_all_years.csv")
sb_cbc_2023 <- read_csv("data/sb_cbc_data_2023.csv")

df <- cbc_tidy_data_fcn(sb_cbc_input, start_year = 1961, 241, 42272)
df <- cbc_ssp_spuh_etc_fcn(df)
df <- cbc_create_final_dataframe_fcn(df, even_base_year = 1960)

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

# Add data for 2023
colnames(sb_cbc_2023) <- c("common_name", "count")

sb_cbc_2023 <- sb_cbc_2023 %>%
  mutate(year = 2023)

taxa_to_exclude_2023 <- c("Mallard (Domestic type)",
                          "teal sp.",
                          "pigeon/dove sp.",
                          "Selasphorus sp.",
                          "hummingbird sp.",
                          "finch sp.",
                          "Larus sp.",
                          "gull sp.",
                          "loon sp.",
                          "Accipiter sp.",
                          "swallow sp.",
                          "wren sp.",
                          "bird sp.")


sb_cbc_2023 <- sb_cbc_2023 %>%
  filter(!(common_name %in% taxa_to_exclude_2023))

# Run the sum_ssp in the previous script
sb_cbc_2023 <- sum_ssp(sb_cbc_2023, "Herring Gull", "Herring Gull (American)")
sb_cbc_2023 <- sum_ssp(sb_cbc_2023, "Iceland Gull", "Iceland Gull (Thayer's)")
sb_cbc_2023 <- sum_ssp(sb_cbc_2023, "Great Blue Heron", "Great Blue Heron (Great Blue)")
sb_cbc_2023 <- sum_ssp(sb_cbc_2023, "Northern Flicker", "Northern Flicker (Red-shafted)")
sb_cbc_2023 <- sum_ssp(sb_cbc_2023, "Bushtit", "Bushtit (Pacific)")
sb_cbc_2023 <- sum_ssp(sb_cbc_2023, "White-breasted Nuthatch", "White-breasted Nuthatch (Pacific)")
sb_cbc_2023 <- sum_ssp(sb_cbc_2023, "House Wren", "House Wren (Northern)")
sb_cbc_2023 <- sum_ssp(sb_cbc_2023, "Fox Sparrow", "Fox Sparrow (Sooty)")
sb_cbc_2023 <- sum_ssp(sb_cbc_2023, "Dark-eyed Junco", "Dark-eyed Junco (Oregon)")
sb_cbc_2023 <- sum_ssp(sb_cbc_2023, "White-crowned Sparrow", c("White-crowned Sparrow (Gambel's)", "White-crowned Sparrow (pugetensis)"))
sb_cbc_2023 <- sum_ssp(sb_cbc_2023, "Yellow-rumped Warbler", c("Yellow-rumped Warbler (Myrtle)", "Yellow-rumped Warbler (Audubon's)"))

# New df
new_df <- expand_grid(unique(df$common_name), 1960:2023)

colnames(new_df) <- c("common_name", "year")

combine_df <- bind_rows(df, sb_cbc_2023)

new_df <- left_join(new_df, combine_df, by = c("common_name", "year"))

new_df$count <- ifelse(new_df$year == 2023 & is.na(new_df$count), 0, new_df$count)

# Save
saveRDS(new_df, "cbc_shiny/sb_cbc_df.rds")
