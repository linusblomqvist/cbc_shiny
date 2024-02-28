library(shiny)
library(tidyverse)
library(scales)
library(plotly)

# load dataset
df <- readRDS("sb_cbc_df.rds")
df_effort <- readRDS("df_effort.rds")
tot_count_by_year <- readRDS("tot_count_by_year.rds")
tot_sp <- readRDS("tot_sp_df.rds")

# Define a server for the Shiny app
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$trendline <- renderPlotly({
    ggplotly(
      df %>%
        filter(common_name == input$species) %>%
        ggplot(aes(x = year, y = count, group = 1)) +
        labs(title = input$species) +
        geom_line(size = 0.5) +
        theme_bw() +
        scale_x_continuous(breaks = seq(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), 10),
                           minor_breaks = seq(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), 1)) +
        ylim(0, NA) +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(size = 12)) +
        scale_y_continuous(breaks = pretty_breaks())
    )
    # Render a line plot

  })
  
  output$effort <- renderPlotly({
    ggplotly(
      df_effort %>%
        ggplot(aes(x = year, y = hours)) +
        labs(y = "party-hours") +
        geom_line(size = 0.5) +
        scale_x_continuous(breaks = seq(min(df_effort$year, na.rm = TRUE), max(df_effort$year, na.rm = TRUE), 10),
                           minor_breaks = seq(min(df_effort$year, na.rm = TRUE),
                                              max(df_effort$year, na.rm = TRUE), 5)) +
        theme_bw() +
        theme(text = element_text(size=12))
    )

  })

  output$tot_count <- renderPlotly({
    ggplotly(
      tot_count_by_year %>%
        rename(individuals = sum_by_year) %>%
        ggplot(aes(x = year, y = individuals)) +
        geom_line(size = 0.5) +
        ylim(0, NA) +
        labs(y = "total # individuals") +
        scale_x_continuous(breaks = seq(1960, 2020, 10),
                           minor_breaks = seq(min(tot_count_by_year$year, na.rm = TRUE),
                                              max(tot_count_by_year$year, na.rm = TRUE), 5)) +
        theme_bw() +
        theme(text = element_text(size=12))
    )

  })

  output$tot_sp <- renderPlotly({
    ggplotly(
      tot_sp %>%
        rename(species = tot_sp_by_year) %>%
        ggplot(aes(x = year, y = species)) +
        geom_line(size = 0.5) +
        ylim(0, NA) +
        labs(y = "total # species") +
        scale_x_continuous(breaks = seq(1960, 2020, 10),
                           minor_breaks = seq(min(tot_sp$year, na.rm = TRUE),
                                              max(tot_sp$year, na.rm = TRUE), 5)) +
        theme_bw() +
        theme(text = element_text(size=12))
    )

  })
}
