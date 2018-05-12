# initialize
library(shiny)    # used for implementation of GUI functionality  
library(shinythemes)    # for GUI aesthetics
source("setup.R")
source("ui.R")
source("server.R")

# run the app
shinyApp(ui = ui, server = server)