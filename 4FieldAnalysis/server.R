source("setup.R")     # needed to initialize the app

server <- function(input, output, session) {
  
  output$summarytable <- renderTable(show_summary(), na = "--")    # displays the summary table, and changes NA values to something more aesthetically pleasing
  output$resultstable <- renderTable(analyze_data(), na = "--")    # displays the results summary table, and then changes the NA values as well
  updateplot <- reactive(input$analyze)
  
  # display stripe plots
  output$plottedpath <- renderPlot(PlotPath(get(input$selectedfile1)), 
                                   width = 720, height = 720, res = 72)    # locks dimensions to optimal resolution and to their most representative dimensions
  output$quadstripeind <- renderPlot(IndStripePlot(get(input$selectedfile1), 8, c("blue4", "red", "blue", "cornflowerblue")),
                                     width = 720, height = 60, res = 72)     # locks dimensions to match the velocity plot
  output$ofstripeind <- renderPlot(IndStripePlot(get(input$selectedfile1), 17, c("grey", "forestgreen")), 
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
    input_list <- c("Lower Right", "Lower Left", "Upper Right", "Upper Left")
    quadlabel_list <- c("LR", "LL", "UR", "UL")
    quadlabel <- quadlabel_list[grep(input$quadrant, input_list)]
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
    GroupStripe(input$conditionIDOF, "OF", c("grey", "forestgreen"))
  }, width = 720, height = 240, res = 72)
  output$FreezeStripeAll <- renderPlot({
    updateplot()
    GroupStripe(input$conditionIDfreeze, "Freeze", c("red", "white"))
  }, width = 720, height = 240, res = 72)
  output$ComparePlot <- renderPlot({
    updateplot()
    AxisCompare(results2, input$compare_X_axis, input$compare_Y_axis, input$conditionIDcompare)
  })
  output$CompareHeatmap <- renderPlot({
    updateplot()
    Compare_Heatmap(results2, input$conditionIDcompare, input$heatmap_type)
  }, width = 720, height = 720, res = 72)
  
  # display stats
  output$QuadAnalysis <- renderPrint({
    updateplot()
    input_list <- c("Lower Right", "Lower Left", "Upper Right", "Upper Left")
    quadlabel_list <- c(4, 3, 6, 5)
    quad <- quadlabel_list[grep(input$quadrant, input_list)]
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
  output$CompareStats <- renderPrint({
    updateplot()
    LinReg_Display(results2, input$compare_X_axis, input$compare_Y_axis, input$conditionIDcompare)
  })
  output$CompareTable <- renderTable({
    updateplot()
    Total_Compare(results2, input$conditionIDcompare)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
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
      image <- IndStripePlot(get(input$selectedfile1), 8, c("blue4", "red", "blue", "cornflowerblue"))
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
      image <- IndStripePlot(get(input$selectedfile1), 17, c("grey", "forestgreen"))
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
      image <- GroupStripe(input$conditionIDOF, "OF", c("grey", "forestgreen"))
      png(file, width = 3000, height = 1000, res = 300)
      print(image)
      dev.off()
    })
  output$downloadQuadGraph <- downloadHandler(
    filename = function() {
      paste0("QuadrantPlot_", Sys.Date(), ".png")
    },
    content = function(file) {
      image <- QuadComp(results2)
      wide <- input$shiny_width     # will not give output if width is direct input
      png(file, height = 1667, width = (wide * 4.16667), res = 300, bg = "transparent")
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
      wide <- input$shiny_width     # will not give output if width is direct input
      png(file, height = 1667, width = (wide * 4.16667), res = 300, bg = "transparent")
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
      wide <- input$shiny_width     # will not give output if width is direct input
      png(file, height = 1667, width = (wide * 4.16667), res = 300, bg = "transparent")
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
      wide <- input$shiny_width     # will not give output if width is direct input
      png(file, height = 1667, width = (wide * 4.16667), res = 300, bg = "transparent")
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
      type_list <- c("Quadrant Occupancy", "Freezing", "Center Occupancy")
      name_list <- c("Quadrant", "Freezing", "Open Field")
      name_num <- grep(input$infotypeind, type_list)
      if (input$typeoftime == 1){
        time_type <- "Mean"
      }
      else {
        time_type <- "Total"
      }
      paste(input$selectedfile1, time_type, name_list[name_num], "Time Series.png")
    },
    content = function(file) {
      image <- IndCDF(get(input$selectedfile1), input$typeoftime, input$infotypeind)
      png(file, height = 1500, width = 3000, res = 300, bg = "transparent")
      print(image)
      dev.off()
    }
  )
  output$downloadCompareGraph <- downloadHandler(
    filename = function() {
      paste(input$compare_X_axis, "+", input$compare_Y_axis, Sys.Date(), input$conditionIDcompare, "Comparison Graph.png")
    },
    content = function(file) {
      image <- AxisCompare(results2, input$compare_X_axis, input$compare_Y_axis, input$conditionIDcompare)
      wide <- input$shiny_width     # will not give output if width is direct input
      png(file, height = 1667, width = (wide * 4.16667), res = 300, bg = "transparent")
      print(image)
      dev.off()
    }
  )
  output$downloadCompareHeatmap <- downloadHandler(
    filename = function() {
      if (input$heatmap_type == 1) {
        paste0("ComparisonHeatmap_", input$conditionIDcompare, "_R-squared_", Sys.Date(), ".png") 
      }
      if (input$heatmap_type == 2) {
        paste0("ComparisonHeatmap_", input$conditionIDcompare, "_P-value_", Sys.Date(), ".png")
      }
    },
    content = function(file) {
      image <- Compare_Heatmap(results2, input$conditionIDcompare, input$heatmap_type)
      png(file, height = 3000, width = 3000, res = 300, bg = "transparent")
      print(image)
      dev.off()
    }
  )
  
  #download stats
  output$downloadQuadStats <- downloadHandler(
    filename = function() {
      info_list <- c("Lower Right", "Lower Left", "Upper Right", "Upper Left")
      quad_list <- c("Lower_Right", "Lower_Left", "Upper_Right", "Upper_Left")
      quadn <- quad_list[grep(input$quadrant, info_list)]
      paste0("QuadrantStats_",quadn,"_",Sys.Date(),".txt")
    },
    content = function(file) {
      info_list <- c("Lower Right", "Lower Left", "Upper Right", "Upper Left")
      quad_list <- c(4, 3, 6, 5)
      quad <- quad_list[grep(input$quadrant, info_list)]
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
  output$downloadCompareStats <- downloadHandler(
    filename = function() {
      paste(input$compare_X_axis, input$compare_Y_axis, Sys.Date(), input$conditionIDcompare, "Comparison Stats.txt")
    },
    content = function(file) {
      statout <- LinReg_Display(results2, input$compare_X_axis, input$compare_Y_axis, input$conditionIDcompare)
      write(statout, file)
    }
  )
  output$downloadCompareTable <- downloadHandler(
    filename = function() {
      paste(input$conditionIDcompare, Sys.Date(), "Total Comparison Fit Summaries.csv")
    },
    content = function(file) {
      Total_Compare(results2, input$conditionIDcompare) 
      write.csv(total_comparison, file)
    }
  )
  
  # transform data
  show_summary <- eventReactive(input$analyze, {
    for (i in 1:length(input$upload[,1])) {
    namedfile <<- input$upload[[i, "name"]]
    baseline_period <<- input$baselinePeriod
    treatment_period <<- input$treatmentPeriod
    size3 <<- as.numeric(dim(summarized)[1])
    summarized[size3,] <<- c(namedfile, input$treatmentID, min(baseline_period), max(baseline_period), min(treatment_period), max(treatment_period))
    summarized <<- rbind.data.frame(summarized, c(NA, NA, NA, NA, NA, NA))}
    return(summarized)
  })
  analyze_data <- eventReactive(input$analyze, {
    withProgress(message = "Uploading data", detail = "Reading settings...", expr = {
      # run initial analysis on whole data file
    for (i in 1:length(input$upload[,1])) {
      analyzed <<- read.table(input$upload[[i, "datapath"]])
      analyzed <<- analyzed[1:5990,]
      namedfile <<- input$upload[[i, "name"]]     # needed to output the name of the file into the individual file selector menu
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
      # need to offset treatment period by 5 seconds to accommodate data formatting
      treatment_data <- analyzed[(min(treatment_period)*240-5):(max(treatment_period)*240-5),]
      colnames(treatment_data)[1:3] <- c("Time", "XCoordinate", "YCoordinate")
      setProgress(value = .8, message = "Analyzing collated data", detail = "Calculating summary statistics...")
      assign(paste(namedfile,"||",input$baselineID), baseline_data, .GlobalEnv)
      filelist <<- c(filelist, namedfile, paste(namedfile,"||",input$baselineID), paste(namedfile,"||",input$treatmentID))
      filelist <<- na.omit(filelist)
      COF(baseline_data, input$baselineID, "Freeze", 5, 6, 7, 8, "Immobile", "Mobile")
      COF(baseline_data, input$baselineID, "OF", 17, 18, 19, 9, "Center", "Surround")
      PerformanceIndex(baseline_data,input$baselineID)
      results2[size1,] <<- c(results[size1,1:9], input$treatmentID, "Baseline")
      results <<- rbind.data.frame(results, c(NA, NA, NA, NA, NA, NA, NA))
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
      COF(treatment_data, input$treatmentID, "Freeze", 5, 6, 7, 8, "Immobile", "Mobile")
      COF(treatment_data, input$treatmentID, "OF", 17, 18, 19, 9, "Center", "Surround")
      PerformanceIndex(treatment_data, input$treatmentID)
      results2[size1,] <<- c(results[size1,1:9], input$treatmentID, "Treatment")
      setProgress(value = .9, message = "Analyzing collated data", detail = "Normalizing treatment time series...")
      assign(paste(namedfile,"||",input$treatmentID), treatment_data, .GlobalEnv)
      results <<- rbind.data.frame(results, c(NA, NA, NA, NA, NA, NA, NA))}
      return(results)
    })
  })
  observeEvent(analyze_data(), {     # makes the selection menus update after each new upload
    updateSelectInput(session, "selectedfile1", label = "Choose trial", choices = filelist)
    updateSelectInput(session, "selectedfile2", label = "Choose analysis file", choices = filelist)
    updateSelectInput(session, "conditionIDOF", label = "Choose time series condition", choices = conditionlist)
    updateSelectInput(session, "conditionIDquad", label = "Choose time series condition", choices = conditionlist)
    updateSelectInput(session, "conditionIDfreeze", label = "Choose time series condition", choices = conditionlist)
    updateSelectInput(session, "conditionIDcompare", label = "Choose comparison condition", choices = conditionlist)
    updateSelectInput(session, "compare_X_axis", label = "Choose comparison X-axis metric", choices = comparelist)
    updateSelectInput(session, "compare_Y_axis", label = "Choose comparison Y-axis metric", choices = comparelist)
  })
}