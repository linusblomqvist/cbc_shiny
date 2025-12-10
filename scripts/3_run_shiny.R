# Load shiny package
library(shiny)
setwd("~/Documents/cbc_shiny/cbc_shiny")


# Run app
source("server.R")
source("ui.R")
shinyApp(ui, server)

# Publish app
library(rsconnect)
deployApp()

