# CBC Historical Data
# Linus Blomqvist

# Load packages
library(tidyverse)
library(stringr)
library(readr)
library(magrittr)

cbc_tidy_data_fcn <- function(input_file, start_year, row_span_cut_end_1, row_span_cut_start_2) {
  
  # Cut out extraneous rows
  df <- input_file %>%
    slice(-(1:row_span_cut_end_1), -(row_span_cut_start_2:nrow(.)))
  
  # Change column names
  colnames(df) <- c("common_name", "year", "count")
  
  # Extract common name from string
  df$common_name <- str_extract(df$common_name, "^.*(?=())")
  
  # Extract year from string
  df$year <- substr(df$year, start = 1, stop = 4)
  
  # Extract count from string
  df$count <- str_extract(df$count, "[^,]+")
  df$count <- extract_numeric(df$count) # eliminates "cw"
  
  # Make numeric
  df$count <- as.numeric(df$count)
  df$year <- as.numeric(df$year)
  
  # Replace NA with 0
  df$count <- ifelse(is.na(df$count), 0, df$count)
  
  # Filter time period
  df <- df %>%
    filter(year >= start_year)
  
  # Eliminate species with zero count all years
  sp_0 <- df %>%
    group_by(common_name) %>%
    summarize(tot_count = sum(count, na.rm = TRUE)) %>%
    filter(tot_count == 0) %$%
    unique(common_name)
  
  df <- df %>%
    filter(!(common_name %in% sp_0))
  
  return(df)
}

# Dealing with subspecies and slashes
# Write function
sum_ssp <- function(input_file, sp, ssp) {
  count_by_year <- input_file %>%
    filter(common_name %in% c(sp, ssp)) %>%
    group_by(year) %>%
    summarize(count = sum(count))
  
  input_file$count[input_file$common_name == sp] <- count_by_year$count
  
  input_file <- input_file %>%
    filter(!(common_name %in% ssp))
  
  return(input_file)
}

cbc_ssp_spuh_etc_fcn <- function(input_file) {
  
  df <- input_file
  
  # Run function
  df <- sum_ssp(df, "Brant", "Brant (Black)")
  df <- sum_ssp(df, "Tundra Swan", "Tundra Swan (Whistling)")
  df <- sum_ssp(df, "Green-winged Teal", "Green-winged Teal (American)")
  df <- sum_ssp(df, "Northern Fulmar", "Northern Fulmar (Pacific)")
  df <- sum_ssp(df, "Double-crested Cormorant", "Double-crested Cormorant (White-crested)")
  df <- sum_ssp(df, "Brown Pelican", "Brown Pelican (California)")
  df <- sum_ssp(df, "Great Blue Heron", "Great Blue Heron (Blue form)")
  df <- sum_ssp(df, "Green Heron", "Green Heron (anthonyi)")
  df <- sum_ssp(df, "White-faced Ibis", "Glossy/White-faced Ibis")
  df <- sum_ssp(df, "American/Pacific Golden-Plover (Lesser Golden-Plover)", "golden-plover sp.")
  df <- sum_ssp(df, "Willet", "Willet (Western)")
  df <- sum_ssp(df, "Whimbrel", "Whimbrel (American)")
  df <- sum_ssp(df, "Wilson's Snipe", "Wilson's/Common Snipe")
  df <- sum_ssp(df, "Mew Gull", "Mew Gull (American)")
  df <- sum_ssp(df, "Barn Owl", "Barn Owl (American)")
  df <- sum_ssp(df, "Northern Flicker", c("Northern Flicker (Red-shafted)", "Northern Flicker (intergrade)"))
  df <- sum_ssp(df, "Merlin", "Merlin (Prairie)")
  df <- sum_ssp(df, "Pacific-slope Flycatcher", "Pacific-slope/Cordilleran Flycatcher (Western Flycatcher)")
  df <- sum_ssp(df, "Tropical Kingbird", "Tropical/Couch's Kingbird")
  df <- sum_ssp(df, "Oak Titmouse", "Oak/Juniper Titmouse (Plain Titmouse)")
  df <- sum_ssp(df, "Bushtit", "Bushtit (Pacific)")
  df <- sum_ssp(df, "White-breasted Nuthatch", "White-breasted Nuthatch (Pacific)")
  df <- sum_ssp(df, "Bewick's Wren", "Bewick's Wren (spilurus Group)")
  df <- sum_ssp(df, "Common Yellowthroat", c("Common Yellowthroat (arizela Group)", "Common Yellowthroat (occidentalis Group)"))
  df <- sum_ssp(df, "Yellow-rumped Warbler", c("Yellow-rumped Warbler (Myrtle)", "Yellow-rumped Warbler (Audubon's)"))
  df <- sum_ssp(df, "Wilson's Warbler", "Wilson's Warbler (pileolata)")
  df <- sum_ssp(df, "Orange-crowned Warbler", "Orange-crowned Warbler (lutescens)")
  df <- sum_ssp(df, "Fox Sparrow", c("Fox Sparrow (Sooty)", "Fox Sparrow (Red)", "Fox Sparrow (Thick-billed)"))
  df <- sum_ssp(df, "Dark-eyed Junco", "Dark-eyed Junco (Oregon)")
  df <- sum_ssp(df, "White-crowned Sparrow", c("White-crowned Sparrow (Gambel's)", "White-crowned Sparrow (pugetensis)", "White-crowned Sparrow (leucophrys)", "White-crowned Sparrow (nuttalli)"))
  df <- sum_ssp(df, "Bell's Sparrow", "Sagebrush/Bell's Sparrow (Sage Sparrow)")
  df <- sum_ssp(df, "Savanna Sparrow", "Savannah Sparrow (Belding's)")
  df <- sum_ssp(df, "Song Sparrow", c("Song Sparrow (montana/merrilli)", "Song Sparrow (heermanni Group)"))
  df <- sum_ssp(df, "Spotted Towhee", "Spotted/Eastern Towhee (Rufous-sided Towhee)")
  df <- sum_ssp(df, "Purple Finch", "Purple Finch (Western)")
  df <- sum_ssp(df, "Blue-gray Gnatcatcher", "Blue-gray Gnatcatcher (obscura Group)")
  df <- sum_ssp(df, "Western Scrub-Jay", "jay sp.")
  df <- sum_ssp(df, "Pacific Wren", "Pacific/Winter Wren")
  df <- sum_ssp(df, "California Towhee", "towhee sp.")
  df <- sum_ssp(df, "Short-billed/Long-billed Dowitcher", "Short-billed Dowitcher")
  
  # Change common names
  df$common_name[df$common_name == "Mew Gull"] <- "Short-billed Gull"
  df$common_name[df$common_name == "Thayer's Gull"] <- "Iceland Gull"
  df$common_name[df$common_name == "Pacific-slope Flycatcher"] <- "Western Flycatcher"
  df$common_name[df$common_name == "Western Scrub-Jay"] <- "California Scrub-Jay"
  df$common_name[df$common_name == "Cattle Egret"] <- "Western Cattle Egret"
  df$common_name[df$common_name == "Black Rail (Northern)"] <- "Black Rail"
  df$common_name[df$common_name == "Ridgway's Rail (Light-footed)"] <- "Ridgway's Rail"
  df$common_name[df$common_name == "American/Pacific Golden-Plover (Lesser Golden-Plover)"] <- "American/Pacific Golden-Plover"
  df$common_name[df$common_name == "Scripps's/Guadalupe Murrelet (Xantus's Murrelet)"] <- "Scripps's/Guadalupe Murrelet"
  
  # Remove spuhs and the like
  sp_remove <- c("Eurasian x American Wigeon (hybrid)",
                 "Blue-winged/Cinnamon Teal",
                 "Surf/Black Scoter",
                 "loon sp.",
                 "scoter sp.",
                 "shearwater sp.",
                 "cormorant sp.",
                 "Scolopacidae sp.",
                 "murrelet sp.",
                 "white egret sp.",
                 "Western x Glaucous-winged Gull (hybrid)",
                 "gull sp.",
                 "heron sp.",
                 "screech-owl sp.",
                 "nighthawk sp.",
                 "Chaetura sp.",
                 "small swift sp.",
                 "American/Pacific Golden-Plover (Lesser Golden-Plover)",
                 "golden-plover sp.",
                 "Selasphorus sp.",
                 "large falcon sp.",
                 "pewee sp.",
                 "solitary vireo sp.",
                 "swallow sp.",
                 "Townsend's x Hermit Warbler (hybrid)",
                 "passerine sp.",
                 "phalarope sp.",
                 "kingfisher sp.",
                 "Olomao",
                 "Mimidae sp.",
                 "plover sp.",
                 "crow sp.",
                 "pipit sp.",
                 "yellowthroat sp.")
  
  df <- df %>%
    filter(!(common_name %in% sp_remove))
  
  return(df)
}

cbc_create_final_dataframe_fcn <- function(input_file, even_base_year) {
  
  df <- input_file

  df_new <- expand_grid(unique(df$common_name), c(even_base_year, unique(df$year)))
  
  colnames(df_new) <- colnames(df)
  
  df <- left_join(df_new, df, by = c("year", "common_name"))
  
  return(df)
}

