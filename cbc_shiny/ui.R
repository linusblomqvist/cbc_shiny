library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)

# load dataset
df <- readRDS("sb_cbc_df.rds")
df_effort <- readRDS("df_effort.rds")
tot_count_by_year <- readRDS("tot_count_by_year.rds")
tot_sp <- readRDS("tot_sp_df.rds")

ui <- navbarPage("Santa Barbara CBC", theme = shinytheme("flatly"),
                 tags$head(includeHTML("google-analytics.Rhtml")),
                 
                 tabPanel(title = "Count by species",
                          sidebarLayout(      
                            # Define the sidebar with one input
                            sidebarPanel(
                              selectInput("species", "Species:", 
                                          choices = unique(df$common_name),
                                          selected = sample(unique(df$common_name), 1))
                            ),
                            
                            # Create a spot for the barplot
                            mainPanel(
                              plotlyOutput("trendline")
                            )
                          )
                 ),
                 
                 tabPanel("Effort",
                          plotlyOutput("effort")),
                 
                 tabPanel("Total species",
                          plotlyOutput("tot_sp")),
                 
                 tabPanel("Total # individuals",
                          plotlyOutput("tot_count")),
                 
                 tabPanel("Data processing",
                          HTML("<p>Data from National Audubon Society (<a href = https://netapp.audubon.org/CBCObservation/Historical/ResultsByCount.aspx>here</a>). Web app by Linus Blomqvist (code <a href = https://github.com/linusblomqvist/cbc_shiny>here</a>).<br><br>
                         The raw data have been adjusted in several ways:<br>
<li>Expected subspecies (or subspecies groups) have been merged with the species. For example, Northern Flicker (Red-shafted) has been folded into Northern Flicker.</li>
<li>In some cases, spuhs and slashes have been folded into species. For example, 'jay sp.' appears to refer to Western (now California) Scrub-Jay in some years, and large numbers of 'Glossy/White-faced Ibis' can be assumed to be White-faced Ibis.</li>
<li>Species have been renamed following taxonomic changes, so what is referred to as 'Cattle Egret' in the raw data has been renamed 'Western Cattle Egret.' In similar fashion, 'Pacific-slope/Cordilleran Flycatcher (Western Flycatcher)' has been merged with 'Pacific-slope Flycatcher' into the current 'Western Flycatcher.'</li>
<li>Spuhs (such as 'scoter sp.') have been removed.</li>
<li>Some corrections have been made. In some cases, there are obvious errors, such as over 300 Yellow-shafted Northern Flickers reported in one year. For other outliers and suspicious records, I have cross-checked with Paul Lehman's <a href = 'http://sbcobirding.com/lehmanbosbc.html'>Birds of Santa Barbara County</a>, and replaced some records deemed incorrect in this publication with NA. In some cases where misidentification was deemed likely by Lehman, I have replaced the species with slashes. For example, high numbers of Short-billed Dowitcher in the 70s and 80s have been replaced by 'Short-billed/Long-billed Dowitcher.'</li><p>"))
)
