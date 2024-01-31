library(shiny)

# load dataset
df <- readRDS("sb_cbc_df.rds")

# Use a fluid Bootstrap layout
ui <- fluidPage(
  
  # Give the page a title
  titlePanel("Total count by species, Santa Barbara Christmas Bird Count"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("species", "Species:", 
                  choices=unique(df$common_name)),
      hr()#,
      #helpText("Data from the National Audubon Society, with adjustments to account for changing taxonomy, erroneous records, and the like. Web app by Linus Blomqvist. ")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("trendline"),
      p("Data from National Audubon Society (https://netapp.audubon.org/cbcobservation). Web app by Linus Blomqvist. If you see anything odd or have suggestions, contact me at linus [dot] blomqvist [at] gmail [dot] com."),
      p("The raw data have been adjusted in several ways. First, expected subspecies (or subspecies groups) have been merged with the species. For example, Northern Flicker (Red-shafted) has been folded into Northern Flicker."),
      p("In some cases, spuhs and slashes have been folded into species. For example, 'jay sp.' appears to refer to Western (now California) Scrub-Jay in some years, and large numbers of 'Glossy/White-faced Ibis' can be assumed to be White-faced Ibis."),
      p("Species have been renamed following taxonomic changes, so what is referred to as 'Cattle Egret' in the raw data has been renamed 'Western Cattle Egret.' In similar fashion, 'Pacific-slope/Cordilleran Flycatcher (Western Flycatcher)' has been merged with 'Pacific-slope Flycatcher' into the current 'Western Flycatcher.'"),
      p("For the most part, spuhs (such as 'scoter sp.') have been removed."),
      p("Finally, some corrections have been made. In some cases, there are obvious errors, such as over 300 Yellow-shafted Northern Flickers reported in one year. For other outliers and suspicious records, I have cross-checked with Paul Lehman's 'Birds of Santa Barbara County', and replaced some records deemed incorrect in this publication with NA.
        In some cases where misidentification was deemed likely by Lehman, I have replaced the species with slashes. For example, high numbers of Short-billed Dowitcher in the 70s and 80s have been replaced by 'Short-billed/Long-billed Dowitcher.'"),
      p("A note on interpretation of trends. The number of individuals detected in any given year may be influenced by participant effort, measured as total participant-hours. Effort increased in the 1960s, but has remained stable (with some fluctuations) since then. The number of hours in the last year (2022) is very similar to the number of hours in 1970. Hence, any secular trends in the number of individuals counted cannot be attributed to changes in effort, and are likely the result of actual changes in bird populations."),
      p("While some species have increased in numbers and some have decreased, overall numbers are down over time. In the early 1970s, an average of about 50,000 individuals were counted every year. By 2020, this number was down to about 30,000, a drop by about 40%."),
      HTML("<p>Code available <a href = 'https://github.com/linusblomqvist/cbc_shiny'>here.</a><p>")
    )
    
  )
)
