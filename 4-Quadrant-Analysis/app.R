# initialize
library(shiny)    # used for implementation of GUI functionality  
source("setup.R")
source("ui.R")
source("server.R")

# run the app
shinyApp(ui = ui, server = server)