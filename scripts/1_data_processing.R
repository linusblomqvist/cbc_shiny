# packages ---------------------------------------------------------------------
library(tidyverse)
library(janitor)

# read data files --------------------------------------------------------------
species_df_raw <- read_csv("data/20251201_LinusBlomqvis_CBC_Data_Request/20251201_LinusBlomqvis_CBC_Circle_Species_Report.csv")
aba_checklist_raw <- read_csv("data/ABA_Checklist-8.18.csv")

# clean aba_checklist  ---------------------------------------------------------

aba_checklist <- aba_checklist_raw %>%
  slice(4:1295) %>%
  clean_names() %>%
  filter(!is.na(names(.)[2])) %>%
  select(common_name = names(.)[2])

# clean species_df  ------------------------------------------------------------


species_df <- species_df_raw %>%
  type_convert() %>%
  mutate(year = 1900 - 1 + count_yr) %>%
  select(common_name = com_name, year, count = how_many) %>%
  filter(year > 1959)

species_df <- species_df %>%
  filter(!str_detect(common_name, "sp\\."),
         !str_detect(common_name, "/"),
         !str_detect(common_name, "hybrid"))

species_df <- species_df %>%
  mutate(
    species_base = str_replace(common_name, " \\(.*\\)$", "")
    # e.g. "Bushtit (Pacific)" -> "Bushtit"
  ) %>%
  group_by(year, species_base) %>%   # add other grouping vars here if needed
  summarise(
    count = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(common_name = species_base)

vec <- setdiff(unique(species_df$common_name), aba_checklist$common_name)

name_fixes <- tibble(
  old_name = vec,
  new_name = c(
    "Black-crowned Night Heron",
    "American Herring Gull",
    "Northern House Wren",
    "Short-billed Gull",
    "American Barn Owl",
    "African Collared-Dove (1969-1994)",
    "Western Warbling-Vireo",
    "Western Cattle-Egret",
    "American Goshawk",
    "Iceland Gull",
    "Common Ground Dove",
    "Ruddy Ground Dove",
    "Western Flycatcher",
    "California Scrub-Jay",
    "Yellow-crowned Night Heron"
  )
)

species_df <- species_df %>%
  left_join(name_fixes, by = c("common_name" = "old_name")) %>%
  mutate(common_name = coalesce(new_name, common_name)) %>%
  select(-new_name)


# species_df in taxonomic order  -----------------------------------------------

species_df <- aba_checklist %>%
  left_join(species_df, 
            by = "common_name",
            relationship = "many-to-many") %>%
  filter(!is.na(count)) %>%
  mutate(common_name = factor(common_name, levels = unique(common_name)))

species_df <- data.frame(year = 1960:2024) %>%
  left_join(species_df, by = "year")

df <- species_df %>%
  expand(common_name, year) %>%
  left_join(species_df, 
            by = c("year", "common_name"),
            relationship = "many-to-many") %>%
  mutate(count = ifelse(year != 1960 & is.na(count), 0, count)) %>%
  mutate(common_name = str_replace(common_name, " \\(.*\\)$", ""))

# Manually fix apparent errors -------------------------------------------------

## A1. Copy Common Raven 1967 to American Crow 1967
raven_to_crow <- df %>%
  filter(common_name == "Common Raven", year == 1967) %>%
  transmute(
    common_name = "American Crow",   # target species
    year,
    new_count  = count               # value to overwrite with
  )

## A2. Copy Yellow-bellied Sapsucker (1961–1985) to Yellow-bellied/Red-naped Sapsucker
yb_to_ybrn <- df %>%
  filter(common_name == "Yellow-bellied Sapsucker",
         dplyr::between(year, 1961, 1985)) %>%
  transmute(
    common_name = "Yellow-bellied/Red-naped Sapsucker",
    year,
    new_count  = count
  )

## A3. Clear Yellow-bellied Sapsucker for those years
yb_clear <- df %>%
  filter(common_name == "Yellow-bellied Sapsucker",
         dplyr::between(year, 1961, 1985)) %>%
  transmute(
    common_name,
    year,
    new_count = NA_real_
  )

## A4. Clear Cackling Goose 1961–2003
cackling_clear <- df %>%
  filter(common_name == "Cackling Goose",
         dplyr::between(year, 1961, 2003)) %>%
  transmute(
    common_name,
    year,
    new_count = NA_real_
  )

## B. All the “set to literal value / NA” corrections
static_fixes <- tibble(
  common_name = c(
    "American Crow",                     # gets overwritten by Raven value above too
    "Common Raven",
    "Northern Flicker (Yellow-shafted)",
    "Orange-crowned Warbler",
    "California Thrasher",
    "White-crowned Sparrow",
    "Spotted Sandpiper",
    "Lesser Yellowlegs",
    "Glaucous Gull",
    "Vaux's Swift"
  ),
  year = c(
    1967,
    1967,
    1967,
    2020,
    1979,
    2020,
    1978,
    1971,
    1967,
    1976
  ),
  new_count = c(
    NA_real_,  # this one will be overwritten by raven_to_crow when we bind_rows
    NA_real_,
    NA_real_,
    288,
    NA_real_,  # was 900
    1728,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_
  )
)

## Combine all corrections
corrections <- bind_rows(
  static_fixes,
  raven_to_crow,
  yb_to_ybrn,
  yb_clear,
  cackling_clear
)

df <- df %>%
  filter(common_name != "Manx Shearwater") # No confirmed December records in Lehman

# Total species by year --------------------------------------------------------
tot_sp_by_year <- df %>%
  filter(!is.na(count), count != 0) %>%
  group_by(year) %>%
  summarize(tot_sp_by_year = n())

tot_sp_df <- data.frame(year = 1960:2024)

tot_sp_df <- tot_sp_df %>%
  left_join(tot_sp_by_year, by = "year")

# Total individuals by year ----------------------------------------------------

tot_count_by_year <- df %>%
  group_by(year) %>%
  summarize(sum_by_year = sum(count))

# Save -------------------------------------------------------------------------
saveRDS(df, "cbc_shiny/sb_cbc_df.rds")
saveRDS(tot_sp_df, "cbc_shiny/tot_sp_df.rds")
saveRDS(tot_count_by_year, "cbc_shiny/tot_count_by_year.rds")
