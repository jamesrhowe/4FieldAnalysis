#initialize
rm(list = ls())
library(shiny)    # used for implementation of GUI functionality
library(evaluate)     # for debugging
source("analysisfunctions.R")     # contains all the analytic functions and graphics calls
ui <- navbarPage("4-Quadrant Explorer",
  #control panel
  tabPanel("Upload",
  sidebarLayout(
    sidebarPanel(
      h3("Data Control Panel"),
      fileInput("upload", "Raw file upload", multiple = TRUE, buttonLabel = "Select"),
      actionButton("analyze", "Analyze"),
      actionButton("reset", "Reset"),
      textInput("baselineID", h4("Baseline Condition"), value = "Baseline"),
      textInput("treatmentID", h4("Treatment Applied"), value = "Baseline"),
      sliderInput("baselinePeriod", h5("Baseline Analysis Period (min)"), 
                  min = 0, max = 30, value = c(0,10)),
      sliderInput("treatmentPeriod", h5("Treatment Analysis Period (min)"), 
                  min = 0, max = 30, value = c(12,22)),
      downloadButton("downloadResults", label = "Download Results Summary"),
      selectInput("selectedfile2", label = "Choose analysis file", choices = filelist),
      downloadButton("downloadIA", label = "Download Individual Analysis")
    ),
    mainPanel(
      h2("Uploaded Data"), 
      tableOutput("summarytable"),
      h2("Summarized Results"),
      tableOutput("resultstable")
      )
    )
  ),
  tabPanel("Trial Summary",
    sidebarPanel(
      selectInput("selectedfile1", label = "Choose trial", choices = filelist),
      selectInput("infotypeind", label = "Choose time series metric", choices = c("Quadrant Occupancy", "Freezing", "Center Occupancy")),
      radioButtons("typeoftime", h4("Time Series Format"), choices = list("Proportion" = 1, "Cumulative" = 2), selected = 1),
      downloadButton("downloadPath", label = "Download Path Plot"),
      downloadButton("downloadQInd", label = "Download Quadrant Stripe"),
      downloadButton("downloadOFInd", label = "Download Open Field Stripe"),
      downloadButton("downloadVelInd", label = "Download Velocity Plot"),
      downloadButton("downloadCDFInd", label = "Download Time Series")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Path", plotOutput("plottedpath")),
        tabPanel("Time Series",
                 plotOutput("quadstripeind", height = 60),
                 plotOutput("ofstripeind", height = 60),
                 plotOutput("velplotind", height = 180),
                 plotOutput("indCDF"))
        )
    )
  ),
  tabPanel("Quadrants",
    sidebarLayout(
      sidebarPanel(
        h3("Quadrant Occupancy"),
        selectInput("quadrant", label = "Choose quadrant", choices = c("Lower Right", "Lower Left", "Upper Right", "Upper Left")),
        selectInput("conditionIDquad", label = "Choose time series condition", choices = conditionlist),
        radioButtons("typeoftimequad", h4("Time Series Format"), choices = list("Proportion" = 1, "Cumulative" = 2), selected = 1),
        downloadButton("downloadQuadGraph", label = "Download Quadrant Graph"),
        downloadButton("downloadQuadStripe", label = "Download Quadrant Stripe"),
        downloadButton("downloadQuadStats", label = "Download Quadrant Stats")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Graph", plotOutput("QuadPlot")),
          tabPanel("Time Series", 
                   plotOutput("QuadStripeAll", height = 240),
                   plotOutput("QuadCDF")),
          tabPanel("Paired T-Test/2-factor Mixed-design ANOVA", verbatimTextOutput("QuadAnalysis"))
        )
      )
    )
  ),
  tabPanel("Performance Index",
    sidebarLayout(
      sidebarPanel(
        h3("Performance Index"),
        downloadButton("downloadPIGraph", label = "Download PI Graph"),
        downloadButton("downloadPIStats", label = "Download PI Stats")
      ),
      mainPanel(
        tabsetPanel(
         tabPanel("Graph", plotOutput("PIPlot")),
         tabPanel("Paired T-Test/2-factor Mixed-design ANOVA", verbatimTextOutput("PIAnalysis"))
        )
      )
    )
  ),
  tabPanel("Freezing",
    sidebarLayout(
      sidebarPanel(
        h3("Mobility"),
        selectInput("conditionIDfreeze", label = "Choose time series condition", choices = conditionlist),
        radioButtons("typeoftimeFreeze", h4("Time Series Format"), choices = list("Proportion" = 1, "Cumulative" = 2), selected = 1),
        downloadButton("downloadFreezeGraph", label = "Download Freezing Graph"),
        downloadButton("downloadFreezeStripe", label = "Download Freezing Stripe"),
        downloadButton("downloadFreezeCDF", label = "Download Freezing Time Series"),
        downloadButton("downloadFreezeStats", label = "Download Freezing Stats")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Graph", plotOutput("FreezePlot")),
          tabPanel("Time Series", 
                   plotOutput("FreezeStripeAll", height = 240), 
                   plotOutput("FreezeCDF")),
          tabPanel("Paired T-Test/2-factor Mixed-design ANOVA", verbatimTextOutput("FreezeAnalysis"))
        )
      )
    )
  ),
  tabPanel("Open Field",
    sidebarLayout(
      sidebarPanel(
        h3("Center Occupancy"),
        selectInput("conditionIDOF", label = "Choose time series condition", choices = conditionlist),
        radioButtons("typeoftimeOF", h4("Time Series Format"), choices = list("Proportion" = 1, "Cumulative" = 2), selected = 1),
        downloadButton("downloadOFGraph", label = "Download Open Field Graph"),
        downloadButton("downloadOFStripe", label = "Download Open Field Stripe"),
        downloadButton("downloadOFCDF", label = "Download Open Field Time Series"),
        downloadButton("downloadOFStats", label = "Download Open Field Stats")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Graph", plotOutput("OFPlot")),
          tabPanel("Time Series", 
                   plotOutput("OFStripeAll", height = 240),
                   plotOutput("OFCDF")),
          tabPanel("Paired T-Test/2-factor Mixed-design ANOVA", verbatimTextOutput("OFAnalysis"))
        )
      )
    )
  ),
  tags$script("$(document).on('shiny:connected', function(event) {    # this is needed to match the size of the downloaded graph with the size of the graph being displayed
var myWidth = $(window).width();
              Shiny.onInputChange('shiny_width',myWidth)
              
              });"),

  tags$script("$(document).on('shiny:connected', function(event) {
              var myHeight = $(window).height();
              Shiny.onInputChange('shiny_height',myHeight)
              
              });")
)
server <- function(input, output, session) {
   output$summarytable <- renderTable(show_summary(), na = "--")    # displays the summary table, and changes NA values to something more aesthetically pleasing
   output$resultstable <- renderTable(analyze_data(), na = "--")    # displays the results summary table, and then changes the NA values as well
   updateplot <- reactive(input$analyze)
   # display stripe plots
   output$plottedpath <- renderPlot(PlotPath(get(input$selectedfile1)), 
                                    width = 720, height = 720, res = 72)    # locks dimensions to optimal resolution and to their most representative dimensions
   output$quadstripeind <- renderPlot(IndStripePlot(get(input$selectedfile1), 8, c("blue4", "red", "blue", "cornflowerblue")),
                                    width = 720, height = 60, res = 72)     # locks dimensions to match the velocity plot
   output$ofstripeind <- renderPlot(IndStripePlot(get(input$selectedfile1), 17, c("grey", "purple4")), 
                                      width = 720, height = 60, res = 72)
   output$velplotind <- renderPlot(VelocityPlot(get(input$selectedfile1)), 
                                    width = 720, height = 180, res = 72)    # gives a defined length to align the stripes to, allowing correspondence between different plots based on mutual information
   # display graphs
   output$PIPlot <- renderPlot({
     updateplot()
     PerfInd(results)})
   output$FreezePlot <- renderPlot({
     updateplot()
     OFFComp(results2, 8, label = "Time Immobile (%)")})
   output$OFPlot <- renderPlot({
     updateplot()
     OFFComp(results2, 9, label = "Center Occupancy (%)")})
   output$QuadPlot <- renderPlot({
     updateplot()
     QuadComp(results2)})
   output$indCDF <- renderPlot(IndCDF(get(input$selectedfile1), input$typeoftime, input$infotypeind), 
                                width = 720, height = 360, res = 72)
   output$QuadCDF <- renderPlot({
     updateplot()
     if (input$quadrant == "Lower Right") {
       quadlabel <<- "LR"
     }
     else {
       if (input$quadrant == "Lower Left") {
         quadlabel <<- "LL"
       }
       else {
         if (input$quadrant == "Upper Right") {
           quadlabel <<- "UR"
         }
         else {
           quadlabel <<- "UL"
         }
       }
     }
     GroupCDF(input$conditionIDquad, quadlabel, input$typeoftimequad)
   }, width = 720, height = 400, res = 72)
   output$FreezeCDF <- renderPlot({
     updateplot()
     GroupCDF(input$conditionIDfreeze, "Freeze", input$typeoftimeFreeze)
   }, width = 720, height = 400, res = 72)
   output$OFCDF <- renderPlot({
     updateplot()
     GroupCDF(input$conditionIDOF, "OF", input$typeoftimeOF)
   }, width = 720, height = 400, res = 72)
   output$QuadStripeAll <- renderPlot({
     updateplot()
     GroupStripe(input$conditionIDquad, "Quad", c("blue4", "red", "blue", "cornflowerblue"))
   }, width = 720, height = 240, res = 72)
   output$OFStripeAll <- renderPlot({
     updateplot()
     GroupStripe(input$conditionIDOF, "OF", c("grey", "purple4"))
   }, width = 720, height = 240, res = 72)
   output$FreezeStripeAll <- renderPlot({
     updateplot()
     GroupStripe(input$conditionIDfreeze, "Freeze", c("red", "white"))
   }, width = 720, height = 240, res = 72)
   # display stats
   output$QuadAnalysis <- renderPrint({
     updateplot()
     if (input$quadrant == "Lower Right") {
       quad <- 4
     }
     else {
       if (input$quadrant == "Lower Left") {
         quad <- 3
       }
       else {
         if (input$quadrant == "Upper Right") {
           quad <- 6
         }
         else {
           quad <- 5
         }
       }
     }
     MEANOVA(results2, quad)
   })
   output$PIAnalysis <- renderPrint({
     updateplot()
     MEANOVA(results2, 7)
   })
   output$FreezeAnalysis <- renderPrint({
     updateplot()
     MEANOVA(results2, 8)
   })
   output$OFAnalysis <- renderPrint({
     updateplot()
     MEANOVA(results2, 9)
   })
   #download data
   output$downloadResults <- downloadHandler(
     filename = function() {
       paste0("results_", Sys.Date(), ".csv")
     },
     content = function(file) {
       results <- na.omit(results)
       write.csv(results, file)
     }
   )
   output$downloadIA <- downloadHandler(
     filename = function() {
       paste(input$selectedfile2, "analysis.csv")
     },
     content = function(file) {
       write.csv(get(input$selectedfile2), file)
     }
   )
   output$downloadPath <- downloadHandler(
     filename = function() {
       paste(input$selectedfile1, "path.png")
     },
     content = function(file) {
       image <- PlotPath(get(input$selectedfile1))
       png(file, width = 3000, height = 3000, res = 300, bg = "transparent")
       print(image)
       dev.off()
     }
   )
   output$downloadQInd <- downloadHandler(
     filename = function() {
       paste(input$selectedfile1, "quadrant stripe.png")
     },
     content = function(file) {
       image <- QuadrantStripePlot(get(input$selectedfile1))
       png(file, width = 3000, height = 250, res = 300)
       print(image)
       dev.off()
     }
   )
   output$downloadOFInd <- downloadHandler(
     filename = function() {
       paste(input$selectedfile1, "open field stripe.png")
     },
     content = function(file) {
       image <- OpenFieldStripePlot(get(input$selectedfile1))
       png(file, width = 3000, height = 250, res = 300)
       print(image)
       dev.off()
     }
   )
   output$downloadVelInd <- downloadHandler(
     filename = function() {
       paste(input$selectedfile1, "velocity plot.png")
     },
     content = function(file) {
       image <- VelocityPlot(get(input$selectedfile1))
       png(file, width = 3000, height = 750, res = 300)
       print(image)
       dev.off()
     }
   )
   output$downloadQuadStripe <- downloadHandler(
     filename = function() {
       paste0("QuadrantStripe_",input$conditionIDquad, "_", Sys.Date(), ".png")
     },
     content = function(file) {
       image <- QuadrantStripeAll(input$conditionIDquad)
       png(file, width = 3000, height = 1500, res = 300)
       print(image)
       dev.off()
     })
   output$downloadOFStripe <- downloadHandler(
     filename = function() {
       paste0("OFStripe_",input$conditionIDOF, "_", Sys.Date(), ".png")
     },
     content = function(file) {
       image <- OpenFieldStripe(input$conditionIDOF)
       png(file, width = 3000, height = 1500, res = 300)
       print(image)
       dev.off()
     })
   output$downloadQuadGraph <- downloadHandler(
     filename = function() {
       paste0("QuadrantPlot_", Sys.Date(), ".png")
     },
     content = function(file) {
       image <- QuadComp(results2)
       png(file, height = 1667, width = (input$shiny_width * 4.16667), res = 300, bg = "transparent")
       print(image)
       dev.off()
     }
   )
   output$downloadPIGraph <- downloadHandler(
     filename = function() {
       paste0("PerformanceIndexPlot_", Sys.Date(), ".png")
     },
     content = function(file) {
       image <- PerfInd(results2)
       png(file, height = 1667, width = (input$shiny_width * 4.16667), res = 300, bg = "transparent")
       print(image)
       dev.off()
     }
   )
   output$downloadOFGraph <- downloadHandler(
     filename = function() {
       paste0("OpenFieldPlot_", Sys.Date(), ".png")
     },
     content = function(file) {
       image <- OFFComp(results2, 9, label = "Center Occupancy (%)")
       png(file, height = 1667, width = (input$shiny_width * 4.16667), res = 300, bg = "transparent")
       print(image)
       dev.off()
     }
   )
   output$downloadFreezeGraph <- downloadHandler(
     filename = function() {
       paste0("FreezingPlot_", Sys.Date(), ".png")
     },
     content = function(file) {
       image <- OFFComp(results2, 8, label = "Time Immobile (%)")
       png(file, height = 1667, width = (input$shiny_width * 4.16667), res = 300, bg = "transparent")
       print(image)
       dev.off()
     }
   )
   output$downloadFreezeStripe <- downloadHandler(
     filename = function() {
       paste0("Freeze_GroupStripe_", input$conditionIDfreeze, "_", Sys.Date(), ".png") 
     },
     content = function(file) {
       image <- GroupStripe(input$conditionIDfreeze, "Freeze", c("red", "white"))
       png(file, height = 1000, width = 3000, res = 300, bg = "transparent")
       print(image)
       dev.off()
     }
   )
   output$downloadOFStripe <- downloadHandler(
     filename = function() {
        paste0("OpenField_GroupStripe_", input$conditionIDOF, "_", Sys.Date(), ".png") 
     },
     content = function(file) {
       image <- GroupStripe(input$conditionIDOF, "OF", c("grey", "purple4"))
       png(file, height = 1000, width = 3000, res = 300, bg = "transparent")
       print(image)
       dev.off()
     }
   )
   output$downloadQuadStripe <- downloadHandler(
     filename = function() {
       paste0("Quadrant_GroupStripe_", input$conditionIDquad, "_", Sys.Date(), ".png") 
     },
     content = function(file) {
       image <- GroupStripe(input$conditionIDquad, "Quad", c("blue4", "red", "blue", "cornflowerblue"))
       png(file, height = 1000, width = 3000, res = 300, bg = "transparent")
       print(image)
       dev.off()
     }
   )
   output$downloadFreezeCDF <- downloadHandler(
     filename = function() {
       if (input$typeoftimeFreeze == 1) {
         paste0("Freeze_Mean_TimeSeries_", input$conditionIDfreeze, "_", Sys.Date(), ".png")  
       }
       else {
         paste0("Freeze_Cumulative_TimeSeries_", input$conditionIDfreeze, "_", Sys.Date(), ".png")
       }
     },
     content = function(file) {
       image <- GroupCDF(input$conditionIDfreeze, "Freeze", input$typeoftimeFreeze)
       png(file, height = 1667, width = 3000, res = 300, bg = "transparent")
       print(image)
       dev.off()
     }
   )
   output$downloadOFCDF <- downloadHandler(
     filename = function() {
       if (input$typeoftimeFreeze == 1) {
         paste0("OpenField_Mean_TimeSeries_", input$conditionIDOF, "_", Sys.Date(), ".png")  
       }
       else {
         paste0("OpenField_Cumulative_TimeSeries_", input$conditionIDOF, "_", Sys.Date(), ".png")
       }
     },
     content = function(file) {
       image <- GroupCDF(input$conditionIDOF, "OF", input$typeoftimeOF)
       png(file, height = 1667, width = 3000, res = 300, bg = "transparent")
       print(image)
       dev.off()
     }
   )
   output$downloadCDFInd <- downloadHandler(
     filename = function() {
       if (input$typeoftime == 1){
         if (input$infotypeind == "Quadrant Occupancy") {
           paste(input$selectedfile1,"Mean Quadrant Time Series.png")
         }
         else {
           if (input$infotypeind == "Freezing") {
             paste(input$selectedfile1,"Mean Freezing Time Series.png")
           }
           else {
             paste(input$selectedfile1,"Mean Open Field Time Series.png")
           }
         }
       }
       else {
         if (input$infotypeind == "Quadrant Occupancy") {
           paste(input$selectedfile1,"Total Quadrant Time Series.png")
         }
         else {
           if (input$infotypeind == "Freezing") {
             paste(input$selectedfile1,"Total Freezing Time Series.png")
           }
           else {
             paste(input$selectedfile1,"Total Open Field Time Series.png")
           }
         }
       }
     },
     content = function(file) {
       image <- IndCDF(get(input$selectedfile1), input$typeoftime, input$infotypeind)
       png(file, height = 1500, width = 3000, res = 300, bg = "transparent")
       print(image)
       dev.off()
     }
   )
   #download stats
   output$downloadQuadStats <- downloadHandler(
     filename = function() {
       if (input$quadrant == "Lower Right") {
         quadn <- "Lower_Right"
       }
       else {
         if (input$quadrant == "Lower Left") {
           quadn <- "Lower_Left"
         }
         else {
           if (input$quadrant == "Upper Right") {
             quadn <- "Upper_Right"
           }
           else {
             quadn <- "Upper_Left"
           }
         }
       }
       paste0("QuadrantStats_",quadn,"_",Sys.Date(),".txt")
     },
     content = function(file) {
       if (input$quadrant == "Lower Right") {
         quad <- 4
       }
       else {
         if (input$quadrant == "Lower Left") {
           quad <- 3
         }
         else {
           if (input$quadrant == "Upper Right") {
             quad <- 6
           }
           else {
             quad <- 5
           }
         }
       }
       statout <- MEANOVA(results2, quad)
       write(statout, file)
     }
   )
   output$downloadPIStats <- downloadHandler(
     filename = function() {
       paste0("PIStats_",Sys.Date(),".txt")
     },
     content = function(file) {
       statout <- MEANOVA(results2, 7)
       write(statout, file)
     }
   )
   output$downloadFreezeStats <- downloadHandler(
     filename = function() {
       paste0("FreezingStats_",Sys.Date(),".txt")
     },
     content = function(file) {
       statout <- MEANOVA(results2, 8)
       write(statout, file)
     }
   )
   output$downloadOFStats <- downloadHandler(
     filename = function() {
       paste0("OpenFieldStats_",Sys.Date(),".txt")
     },
     content = function(file) {
       statout <- MEANOVA(results2, 9)
       write(statout, file)
     }
   )
   # transform data
   show_summary <- eventReactive(input$analyze, {
      file <- input$upload
      namedfile <<- file$name
      baseline_period <<- input$baselinePeriod
      treatment_period <<- input$treatmentPeriod
      size3 <<- as.numeric(dim(summarized)[1])
      summarized[size3,] <<- c(namedfile, input$treatmentID, min(baseline_period), max(baseline_period), min(treatment_period), max(treatment_period))
      summarized <<- rbind.data.frame(summarized, c(NA, NA, NA, NA, NA, NA))
      return(summarized)
   })
   analyze_data <- eventReactive(input$analyze, {
     withProgress(message = "Uploading data", detail = "Reading settings...", expr = {
      # run initial analysis on whole data file
      file <- input$upload
      analyzed <<- read.table(file$datapath)
      namedfile <<- file$name     # needed to output the name of the file into the individual file selector menu
      baseline_period <<- input$baselinePeriod
      treatment_period <<- input$treatmentPeriod
      setProgress(value = .3, message = "Analyzing time series data", detail = "Calculating velocity and freezing...")
      Velocity(analyzed)
      setProgress(value = .5, message = "Analyzing time series data", detail = "Calculating quadrant occupancy...")
      QuadrantOccupancy(analyzed)
      setProgress(value = .6, message = "Analyzing time series data", detail = "Calculating center occupancy...")
      OpenField(analyzed)
      setProgress(value = .7, message = "Analyzing time series data", detail = "Separating into baseline and treatment groups...")
      #place full analysis into collated bins
      OFFullStripe <<- cbind.data.frame(OFFullStripe, analyzed[,17])    # unwieldy, but all these required because there is no full period that can be taken similar to how baseline and treatment can
      QuadFullStripe <<- cbind.data.frame(QuadFullStripe, analyzed[,8])
      FreezeFullStripe <<- cbind.data.frame(FreezeFullStripe, analyzed[,5])
      OFFullCumulative <<- cbind.data.frame(OFFullCumulative, analyzed[,18])
      OFFullProportion <<- cbind.data.frame(OFFullProportion, analyzed[,19])
      FreezeFullCumulative <<- cbind.data.frame(FreezeFullCumulative, analyzed[,6])
      FreezeFullProportion <<- cbind.data.frame(FreezeFullProportion, analyzed[,7])
      ULFullCumulative <<- cbind.data.frame(ULFullCumulative, analyzed[,11])
      ULFullProportion <<- cbind.data.frame(ULFullProportion, analyzed[,15])
      URFullCumulative <<- cbind.data.frame(URFullCumulative, analyzed[,9])
      URFullProportion <<- cbind.data.frame(URFullProportion, analyzed[,13])
      LLFullCumulative <<- cbind.data.frame(LLFullCumulative, analyzed[,12])
      LLFullProportion <<- cbind.data.frame(LLFullProportion, analyzed[,16])
      LRFullCumulative <<- cbind.data.frame(LRFullCumulative, analyzed[,10])
      LRFullProportion <<- cbind.data.frame(LRFullProportion, analyzed[,14])
      # divide into baseline and treatments
      baseline_data <- analyzed[((min(baseline_period)*240)+1):((max(baseline_period)*240)+1),]
      colnames(baseline_data)[1:3] <- c("Time", "XCoordinate", "YCoordinate")
      treatment_data <- analyzed[((min(treatment_period)*240)+1):((max(treatment_period)*240)+1),]
      colnames(treatment_data)[1:3] <- c("Time", "XCoordinate", "YCoordinate")
      setProgress(value = .8, message = "Analyzing collated data", detail = "Calculating summary statistics...")
      assign(paste(namedfile,"||",input$baselineID), baseline_data, .GlobalEnv)
      filelist <<- c(filelist, namedfile, paste(namedfile,"||",input$baselineID), paste(namedfile,"||",input$treatmentID))
      filelist <<- na.omit(filelist)
      COF(baseline_data, input$baselineID, "Freeze", 5, 6, 7, 8, "Immobile", "Mobile")
      COF(baseline_data, input$baselineID, "OF", 17, 18, 19, 9, "Center", "Surround")
      PerformanceIndex(baseline_data,input$baselineID)
      results2[size1,] <<- c(results[size1,1:9], input$baselineID, "Baseline")
      results <<- rbind.data.frame(results, c(NA, NA, NA, NA, NA, NA, NA))
      COF(treatment_data, input$treatmentID, "Freeze", 5, 6, 7, 8, "Immobile", "Mobile")
      COF(treatment_data, input$treatmentID, "OF", 17, 18, 19, 9, "Center", "Surround")
      PerformanceIndex(treatment_data, input$treatmentID)
      results2[size1,] <<- c(results[size1,1:9], input$treatmentID, "Treatment")
      setProgress(value = .9, message = "Analyzing collated data", detail = "Normalizing treatment time series...")
      for (i in 2:as.numeric(dim(treatment_data)[1])) {     # required to normalize each cumulative measure back to zero after having a total sum from earlier time points
        treatment_data[i,1] <- (treatment_data[i,1] - treatment_data[1,1])
        treatment_data[i,6] <- (treatment_data[i,6] - treatment_data[1,6])
        treatment_data[i,7] <- treatment_data[i,6] / i
        treatment_data[i,9:12] <- c((treatment_data[i,9] - treatment_data[1,9]), (treatment_data[i,10] - treatment_data[1,10]), (treatment_data[i,11] - treatment_data[1,11]), (treatment_data[i,12] - treatment_data[1,12]))
        treatment_data[i,13:16] <- c((treatment_data[i,9] / i), (treatment_data[i,10] / i), (treatment_data[i,11] / i), (treatment_data[i,12] / i))
        treatment_data[i,18] <- (treatment_data[i,18] - treatment_data[1,18])
        treatment_data[i,19] <- treatment_data[i,18] / i
      }
      treatment_data[1,1] <- 0    # needed to set starting values to zero afterwards, instead of leaving starting values intact
      treatment_data[1,6:7] <- 0
      treatment_data[1,9:16] <- 0
      treatment_data[1,18:19] <- 0
      assign(paste(namedfile,"||",input$treatmentID), treatment_data, .GlobalEnv)
      results <<- rbind.data.frame(results, c(NA, NA, NA, NA, NA, NA, NA))
      return(results)
     })
   })
   observeEvent(analyze_data(), {     # makes the selection menus update after each new upload
     updateSelectInput(session, "selectedfile1", label = "Choose trial", choices = filelist)
     updateSelectInput(session, "selectedfile2", label = "Choose analysis file", choices = filelist)
     updateSelectInput(session, "conditionIDOF", label = "Choose time series condition", choices = conditionlist)
     updateSelectInput(session, "conditionIDquad", label = "Choose time series condition", choices = conditionlist)
     updateSelectInput(session, "conditionIDfreeze", label = "Choose time series condition", choices = conditionlist)
   })
}
shinyApp(ui = ui, server = server)