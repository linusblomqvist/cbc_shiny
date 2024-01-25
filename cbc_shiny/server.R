library(shiny)
library(tidyverse)

# load dataset
df <- readRDS("sb_cbc_df.rds")

# Define a server for the Shiny app
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$trendline <- renderPlot({
    
    # Render a line plot
    df %>%
      filter(common_name == input$species) %>%
      ggplot(aes(x = year, y = count, group = 1)) +
      labs(title = input$species) +
      geom_line() +
      scale_x_continuous(breaks = seq(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), 5),
                         minor_breaks = seq(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), 1)) +
      ylim(0, NA) +
      theme(plot.title = element_text(hjust = 0.5))
  })
}
