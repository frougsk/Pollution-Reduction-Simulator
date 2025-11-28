# load required libraries
library(shiny)
library(shinyWidgets)
library(dplyr)
library(DT)

# source other files
source("Solve.R")
source("UI.R")
source("Server.R")

# run the app
options(shiny.launch.browser = TRUE) # makes it so automatically launches on chrome
shinyApp(ui = ui, server = server)
