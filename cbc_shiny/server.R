library(shiny)
library(tidyverse)
library(scales)

# load dataset
df <- readRDS("sb_cbc_df.rds")
df_effort <- readRDS("df_effort.rds")
tot_count_by_year <- readRDS("tot_count_by_year.rds")
tot_sp <- readRDS("tot_sp_df.rds")

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
      scale_x_continuous(breaks = seq(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), 10),
                         minor_breaks = seq(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), 1)) +
      ylim(0, NA) +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size=15)) +
      scale_y_continuous(breaks = pretty_breaks())
  })
  
  output$effort <- renderPlot({
    df_effort %>%
      ggplot(aes(x = year, y = hours)) +
      labs(y = "party-hours") +
      geom_line() +
      scale_x_continuous(breaks = seq(min(df_effort$year, na.rm = TRUE), max(df_effort$year, na.rm = TRUE), 10),
                         minor_breaks = seq(min(df_effort$year, na.rm = TRUE),
                                            max(df_effort$year, na.rm = TRUE), 1)) +
      theme(text = element_text(size=15))
  })

  output$tot_count <- renderPlot({
    tot_count_by_year %>%
      ggplot(aes(x = year, y = sum_by_year)) +
      geom_line() +
      ylim(0, NA) +
      labs(y = "total # individuals") +
      scale_x_continuous(breaks = seq(1960, 2020, 10),
                         minor_breaks = seq(min(tot_count_by_year$year, na.rm = TRUE),
                                            max(tot_count_by_year$year, na.rm = TRUE), 1)) +
      theme(text = element_text(size=15))
  })

  output$tot_sp <- renderPlot({
    tot_sp %>%
      ggplot(aes(x = year, y = tot_sp_by_year)) +
      geom_line() +
      ylim(0, NA) +
      labs(y = "total # species") +
      scale_x_continuous(breaks = seq(1960, 2020, 10),
                         minor_breaks = seq(min(tot_sp$year, na.rm = TRUE),
                                            max(tot_sp$year, na.rm = TRUE), 1)) +
      theme(text = element_text(size=15))
  })
}
