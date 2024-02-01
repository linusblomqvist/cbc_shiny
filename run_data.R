# Santa Barbara CBC

source("CBC_historical_data.R")

sb_cbc_input <- read_csv("cbc_sb_historical_data_all_years.csv")
sb_cbc_2023 <- read_csv("sb_cbc_data_2023.csv")

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
sum_ssp <- function(sp, ssp) {
  count_by_year <- sb_cbc_2023 %>%
    filter(common_name %in% c(sp, ssp)) %>%
    group_by(year) %>%
    summarize(count = sum(count))
  
  sb_cbc_2023$count[sb_cbc_2023$common_name == sp] <- count_by_year$count
  
  sb_cbc_2023 <- sb_cbc_2023 %>%
    filter(!(common_name %in% ssp))
}

# "Herring Gull (American)", "Iceland Gull (Thayer's)", "Great Blue Heron (Great Blue)"
# "Northern Flicker (Red-shafted)", "Bushtit (Pacific)", "White-breasted Nuthatch (Pacific)"
# "House Wren (Northern)", "Fox Sparrow (Sooty)", "Dark-eyed Junco (Oregon)"
# "White-crowned Sparrow (Gambel's)", "White-crowned Sparrow (pugetensis)"
# "Yellow-rumped Warbler (Myrtle)", "Yellow-rumped Warbler (Audubon's)"
# "bird sp.", 

sb_cbc_2023 <- sum_ssp("Herring Gull", "Herring Gull (American)")
sb_cbc_2023 <- sum_ssp("Iceland Gull", "Iceland Gull (Thayer's)")
sb_cbc_2023 <- sum_ssp("Great Blue Heron", "Great Blue Heron (Great Blue)")
sb_cbc_2023 <- sum_ssp("Northern Flicker", "Northern Flicker (Red-shafted)")
sb_cbc_2023 <- sum_ssp("Bushtit", "Bushtit (Pacific)")
sb_cbc_2023 <- sum_ssp("White-breasted Nuthatch", "White-breasted Nuthatch (Pacific)")
sb_cbc_2023 <- sum_ssp("House Wren", "House Wren (Northern)")
sb_cbc_2023 <- sum_ssp("Fox Sparrow", "Fox Sparrow (Sooty)")
sb_cbc_2023 <- sum_ssp("Dark-eyed Junco", "Dark-eyed Junco (Oregon)")
sb_cbc_2023 <- sum_ssp("White-crowned Sparrow", c("White-crowned Sparrow (Gambel's)", "White-crowned Sparrow (pugetensis)"))
sb_cbc_2023 <- sum_ssp("Yellow-rumped Warbler", c("Yellow-rumped Warbler (Myrtle)", "Yellow-rumped Warbler (Audubon's)"))


new_df <- expand_grid(unique(df$common_name), 1960:2023)
colnames(new_df) <- c("common_name", "year")

combine_df <- bind_rows(df, sb_cbc_2023)

new_df <- left_join(new_df, combine_df, by = c("common_name", "year"))

new_df$count <- ifelse(new_df$year == 2023 & is.na(new_df$count), 0, new_df$count)



# Save
saveRDS(new_df, "cbc_shiny/sb_cbc_df.rds")
