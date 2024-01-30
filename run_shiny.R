# Load shiny package
library(shiny)
setwd("/Users/linusblomqvist/Library/CloudStorage/Dropbox/Birding/ebird_R/cbc_shiny/cbc_shiny")


# Run app
source("server.R")
source("ui.R")
shinyApp(ui, server)

# Publish app
library(rsconnect)
deployApp()

