# Load shiny package
library(shiny)

# Run app
source("server.R")
source("ui.R")
shinyApp(ui, server)

# Publish app
setwd("/Users/linusblomqvist/Library/CloudStorage/Dropbox/Birding/ebird_R/cbc_shiny/cbc_shiny")
library(rsconnect)
deployApp()

