#initializes the app each time it starts

rm(list = ls())     #reset the environment, remove all old values

# initialize
# import dependencies
library(shiny)    # used for implementation of GUI functionality  
library(shinythemes)    # for GUI aesthetics
library(ggplot2)    #library for plotting
library(reshape2)     #needed for melting data into long format
library(dplyr)    #needed for grouping data by nested subgroups using %>%
library(nlme)     #needed for making mixed linear models
library(car)    #needed for good anova function that can take a mixed linear model
library(multcomp)     #needed for doing multiple comparisons for the mixed two-factor ANOVA
library(scales)     #contains pretty_breaks() which allows for publication-quality x axes
library(evaluate)    # for debugging

# create initial summary dataframes
results <<- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
results2 <<- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
summarized <<- c(0, 0, 0, 0, 0, 0)
results <<- as.data.frame(t(data.frame(results, row.names = c("Filename","Condition", "Lower Left", "Lower Right", "Upper Left", "Upper Right", "Performance Index", "Freezing", "Open Field"))))    #creates empty matrix for displaying summary results on first page
results2 <<- as.data.frame(t(data.frame(results2, row.names= c("Filename","Condition", "Lower Left", "Lower Right", "Upper Left", "Upper Right", "Performance Index", "Freezing", "Open Field", "Group", "Sequence"))))   #creates empty matrix for storing extended results for manipulation and export
summarized <<- as.data.frame(t(data.frame(summarized, col.names = c("Filename", "Treatment", "Baseline On", "Baseline Off", "Treatment On", "Treatment Off"))))     #creates empty matrix for displaying metadata on first page
conditionlist <<- "Full"     #creates list of conditions, enters "Full" as first input, because there is no input to define the condition and it will always be present
filelist <<- NA   #creates the initial list of files, gets removed later once files uploaded, will not display

# create full-length dataframes independent of baseline or treatment ID

Time <- 0     #creates blank numeric column to allocate time data to later, reserves first column
OFFullStripe <- as.data.frame(Time)     #these create series of full-length arrays for data display that cannot be created and named later based on inputs
QuadFullStripe <- as.data.frame(Time) 
FreezeFullStripe <- as.data.frame(Time)     
OFFullCumulative <- as.data.frame(Time)      
OFFullProportion <- as.data.frame(Time)
FreezeFullCumulative <- as.data.frame(Time)
FreezeFullProportion <- as.data.frame(Time)
ULFullCumulative <- as.data.frame(Time)
ULFullProportion <- as.data.frame(Time)
URFullCumulative <- as.data.frame(Time)
URFullProportion <- as.data.frame(Time)
LLFullCumulative <- as.data.frame(Time)
LLFullProportion <- as.data.frame(Time)
LRFullCumulative <- as.data.frame(Time)
LRFullProportion <- as.data.frame(Time)

source("analysisfunctions.R")     # contains all the analytic functions
source("graphicsfunctions.R")     # contains all the graphics calls

