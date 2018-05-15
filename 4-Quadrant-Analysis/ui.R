source("setup.R")     # needed to initialize the app
ui <- navbarPage("4-Quadrant-Analysis", theme = shinytheme("cosmo"),
                 # control panel page
                 tabPanel("Upload",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Data Control Panel"),
                              fileInput("upload", "Raw file upload", multiple = TRUE, buttonLabel = "Select"),
                              actionButton("analyze", "Analyze"),
                              downloadButton("downloadResults", label = "Download Results"),
                              textInput("baselineID", h4("Baseline Condition"), value = "Baseline"),
                              textInput("treatmentID", h4("Treatment Applied"), value = "Baseline"),
                              sliderInput("baselinePeriod", h5("Baseline Analysis Period (min)"), 
                                          min = 0, max = 30, value = c(0,10)),
                              sliderInput("treatmentPeriod", h5("Treatment Analysis Period (min)"), 
                                          min = 0, max = 30, value = c(12,22)),
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
                 
                 # page for summaries of individual trials
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
                 tabPanel("Compare",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Comparison Control Panel"),
                              selectInput("conditionIDcompare", label = "Choose comparison condition", choices = conditionlist),
                              selectInput("compare_X_axis", label = "Choose comparison X-axis metric", choices = results2[,1]),
                              selectInput("compare_Y_axis", label = "Choose comparison Y-axis metric", choices = results2[,1]),
                              downloadButton("downloadCompareGraph", label = "Download Comparison Graph"),
                              downloadButton("downloadCompareHeatmap", label = "Download Comparison Heatmap"),
                              downloadButton("downloadCompareTable", label = "Download Comparison Table")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Graph", plotOutput("ComparePlot")),
                                tabPanel("Heatmap", plotOutput("CompareHeatmap")),
                                tabPanel("Table", plotOutput("CompareTable"))
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