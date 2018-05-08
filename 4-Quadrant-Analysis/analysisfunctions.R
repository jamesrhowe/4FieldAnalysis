# import dependencies
rm(list = ls())     # reset the environment and remove all old values, ensures all old files are cleared when analyzing new ones
library(ggplot2)    # library for plotting
library(reshape2)     # needed for melting data into long format
library(dplyr)    # needed for grouping data by nested subgroups using %>%
library(nlme)     # needed for making mixed linear models
library(car)    # needed for good anova function that can take a mixed linear model
library(multcomp)     # needed for doing multiple comparisons for the mixed two-factor ANOVA
library(scales)     # contains pretty_breaks() which allows for publication-quality x axes
# defining functions for analysis
results <- as.data.frame(matrix(NA, nrow = 1, ncol = 9))    # creates empty matrix for displaying summary results on first page
results2 <- as.data.frame(matrix(NA, nrow = 1, ncol = 11))    # creates empty matrix for storing extended results for manipulation and export
colnames(results) <- c("Filename","Condition", "Lower Left", "Lower Right", "Upper Left", "Upper Right", "Performance Index", "Freezing", "Open Field")     
colnames(results2) <- c("Filename","Condition", "Lower Left", "Lower Right", "Upper Left", "Upper Right", "Performance Index", "Freezing", "Open Field", "Group", "Sequence")
summarized <- as.data.frame(matrix(NA, nrow = 1, ncol = 6))     #creates empty matrix for displaying metadata on first page
colnames(summarized) <- c("Filename", "Treatment", "Baseline On", "Baseline Off", "Treatment On", "Treatment Off")
conditionlist <- "Full"     # creates list of conditions, enters "Full" as first input, because there is no input to define the condition and it will always be present
#define cumulative groupings
Time <- 0     # creates blank numeric column to allocate time data to later, reserves first column
OFFullStripe <- as.data.frame(Time)     # these create series of full-length arrays for data display that cannot be created and named later based on inputs
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
filelist <- NA    # creates the initial list of files, gets removed later once files uploaded, will not display
# Time-variable stats
# Finding quadrant occupancy at each time point
QuadrantOccupancy <- function(x) {
  size1 <<- dim(t(x))[1] + 1    # need this to dynamically add to the array independent of file number
  for (i in 1:as.numeric(dim(x)[1])) {
    if (x[i,2] >= 0) {
      if (x[i,3] >= 0) {
        x[i,size1] <- "Upper Right"     # these allow tracking in the time series stripe
        x[i,(size1+1)] <- 1     # these create a tally that allows tracking of cumulative sums for each quadrant
        x[i,(size1+2)] <- 0
        x[i,(size1+3)] <- 0
        x[i,(size1+4)] <- 0
      }
      else {
        x[i,size1] <- "Lower Right"
        x[i,(size1+1)] <- 0
        x[i,(size1+2)] <- 1
        x[i,(size1+3)] <- 0
        x[i,(size1+4)] <- 0
      }
    }
    else {
      if (x[i,3] >= 0) {
        x[i,size1] <- "Upper Left"
        x[i,(size1+1)] <- 0
        x[i,(size1+2)] <- 0
        x[i,(size1+3)] <- 1
        x[i,(size1+4)] <- 0
      }
      else {
        x[i,size1] <- "Lower Left"
        x[i,(size1+1)] <- 0
        x[i,(size1+2)] <- 0
        x[i,(size1+3)] <- 0
        x[i,(size1+4)] <- 1
      }}
  }
  x[,(size1+5):(size1+8)] <- c(cumsum(x[,(size1+1)]), cumsum(x[,(size1+2)]), cumsum(x[,(size1+3)]), cumsum(x[,(size1+4)]))
  x[,(size1+9):(size1+12)] <- c(cummean(x[,(size1+1)]), cummean(x[,(size1+2)]), cummean(x[,(size1+3)]), cummean(x[,(size1+4)]))
  x <- x[,-(size1+1)]     # removes the running tally from the results sheet, do not want it to display due to redundancy
  x <- x[,-(size1+1)]
  x <- x[,-(size1+1)]
  x <- x[,-(size1+1)]
  colnames(x)[1:3] <- c("Time", "XCoordinate", "YCoordinate")
  colnames(x)[size1:(size1+8)] <- c("Quadrant", "Upper Right Sum", "Lower Right Sum", "Upper Left Sum", "Lower Left Sum", "Upper Right Mean", "Lower Right Mean", "Upper Left Mean", "Lower Left Mean")
  assign(namedfile, x, .GlobalEnv)
  analyzed <<- x
}
#Finding velocity and freezing at each time point
Velocity <- function(x) {
  size1 <<- dim(t(x))[1] + 1
  size2 <<- dim(t(x))[1] + 2    # this one is required to simultaneously post whether or not it is freezing, along with velocity
  for (i in 1:as.numeric(dim(x)[1])) {
    if (i == 1) {
      x[i,size1] <- 0
      x[i,size2] <- "Mobile"
      x[i,(size2+1)] <- 0
    }
    else {
      dist <- sqrt((x[i,2] - x[(i-1),2])^2 + (x[i,3] - x[(i-1),3])^2) / (x[i,1] - x[(i-1),1])
      x[i,size1] <- dist
      if (dist < 1) {
        x[i,size2] <- "Immobile"
        x[i,(size2+1)] <- 1
      }
      else {
        x[i,size2] <- "Mobile"
        x[i,(size2+1)] <- 0
      }
    }
  }
  x[,(size2+2)] <- cumsum(x[,(size2+1)])
  x[,(size2+3)] <- cummean(x[,(size2+1)])
  x <- x[,-(size2+1)]
  colnames(x)[1:3] <- c("Time", "XCoordinate", "YCoordinate")
  colnames(x)[size1:(size2+2)] <- c("Velocity", "Freezing", "Freezing Sum", "Freezing Mean")
  assign(namedfile, x, .GlobalEnv)
  analyzed <<- x
}
OpenField <- function(x) {
  size1 <<- dim(t(x))[1] + 1
  for (i in 1:as.numeric(dim(x)[1])) {
    if (x[i,2] > -13) {
      if (x[i,2] < 13) {
        if (x[i,3] > -13) {
          if (x[i,3] < 13) {
            x[i,size1] <- "Center"
            x[i,(size1+1)] <- 1
          }
          else {
            x[i,size1] <- "Surround"
            x[i,(size1+1)] <- 0
          }
        }
        else {
          x[i,size1] <- "Surround"
          x[i,(size1+1)] <- 0
        }
      }
      else {
        x[i,size1] <- "Surround"
        x[i,(size1+1)] <- 0
      }
    }
    else {
      x[i,size1] <- "Surround"
      x[i,(size1+1)] <- 0
    }
  }
  x[,(size1+2)] <- cumsum(x[,(size1+1)])
  x[,(size1+3)] <- cummean(x[,(size1+1)])
  x <- x[,-(size1+1)]
  colnames(x)[1:3] <- c("Time", "XCoordinate", "YCoordinate")
  colnames(x)[size1:(size1+2)] <- c("OpenField", "OpenField Sum", "OpenField Mean")
  assign(namedfile, x, .GlobalEnv)
  analyzed <<- x
}
# Summary stats
# Quadrant occupancy tallies and performance index calculation
PerformanceIndex <- function(x, condition) {
  if (exists(paste0("UL",condition,"Cumulative")) == FALSE) {
    assign(paste0("UL",condition,"Cumulative"), as.data.frame(Time), .GlobalEnv)    # this is pretty unwieldy, but it is required for grouped estimates of quadrant occupancy for each one
    assign(paste0("UL",condition,"Proportion"), as.data.frame(Time), .GlobalEnv)
    assign(paste0("LL",condition,"Cumulative"), as.data.frame(Time), .GlobalEnv)
    assign(paste0("LL",condition,"Proportion"), as.data.frame(Time), .GlobalEnv)
    assign(paste0("LR",condition,"Cumulative"), as.data.frame(Time), .GlobalEnv)
    assign(paste0("LR",condition,"Proportion"), as.data.frame(Time), .GlobalEnv)
    assign(paste0("UR",condition,"Cumulative"), as.data.frame(Time), .GlobalEnv)
    assign(paste0("UR",condition,"Proportion"), as.data.frame(Time), .GlobalEnv)
    assign(paste0("Quad",condition,"Stripe"), as.data.frame(Time), .GlobalEnv)
  }
  if (condition %in% conditionlist == FALSE) {    # used to identify if the condition here is new and adds non-redundantly to the list of conditions
    conditionlist <<- c(conditionlist, condition)
  }
  ulcum <- get(paste0("UL",condition,"Cumulative"))     # needed to find the condition of interest and analyze it
  ulprop <- get(paste0("UL",condition,"Proportion"))
  urcum <- get(paste0("UR",condition,"Cumulative"))
  urprop <- get(paste0("UR",condition,"Proportion"))
  llcum <- get(paste0("LL",condition,"Cumulative"))
  llprop <- get(paste0("LL",condition,"Proportion"))
  lrcum <- get(paste0("LR",condition,"Cumulative"))
  lrprop <- get(paste0("LR",condition,"Proportion"))
  stripe <- get(paste0("Quad",condition,"Stripe"))
  ulcum <- cbind.data.frame(get(paste0("UL",condition,"Cumulative")), x$`Upper Left Sum`)     # needed to iteratively add new data to the group based on its condition
  ulprop <- cbind.data.frame(get(paste0("UL",condition,"Proportion")), x$`Upper Left Mean`)
  urcum <- cbind.data.frame(get(paste0("UR",condition,"Cumulative")), x$`Upper Right Sum`)
  urprop <- cbind.data.frame(get(paste0("UR",condition,"Proportion")), x$`Upper Right Mean`)
  llcum <- cbind.data.frame(get(paste0("LL",condition,"Cumulative")), x$`Lower Left Sum`)
  llprop <- cbind.data.frame(get(paste0("LL",condition,"Proportion")), x$`Lower Left Mean`)
  lrcum <- cbind.data.frame(get(paste0("LR",condition,"Cumulative")), x$`Lower Right Sum`)
  lrprop <- cbind.data.frame(get(paste0("LR",condition,"Proportion")), x$`Lower Right Mean`)
  stripe <- cbind.data.frame(get(paste0("Quad",condition,"Stripe")), x$`Quadrant`)
  name <- as.numeric(dim(t(get(paste0("Quad",condition,"Stripe"))))[1])     # this is required to make the stripe count by mouse and stack atop each other
  colnames(stripe)[name] <- name - 1
  assign(paste0("UL",condition,"Cumulative"), ulcum, .GlobalEnv)    # replaces the old version of the file with the new one containing the most recent data
  assign(paste0("UL",condition,"Proportion"), ulprop, .GlobalEnv)
  assign(paste0("LL",condition,"Cumulative"), llcum, .GlobalEnv)
  assign(paste0("LL",condition,"Proportion"), llprop, .GlobalEnv)
  assign(paste0("LR",condition,"Cumulative"), lrcum, .GlobalEnv)
  assign(paste0("LR",condition,"Proportion"), lrprop, .GlobalEnv)
  assign(paste0("UR",condition,"Cumulative"), urcum, .GlobalEnv)
  assign(paste0("UR",condition,"Proportion"), urprop, .GlobalEnv)
  assign(paste0("Quad",condition,"Stripe"), stripe, .GlobalEnv)
  size1 <<- as.numeric(dim(results)[1])
  tally <- as.data.frame(table(x$Quadrant))
  results[size1,1] <<- namedfile
  results[size1,2] <<- condition
  results[size1,3] <<- tally[1,2] / sum(tally[,2])    # finds average proportion of time spent in this state as a fraction of time
  results[size1,4] <<- tally[2,2] / sum(tally[,2])
  results[size1,5] <<- tally[3,2] / sum(tally[,2])
  results[size1,6] <<- tally[4,2] / sum(tally[,2])
  results[size1,7] <<- ((results[size1,4] * 100) - 25) / 0.25     # calculates the performance index based on lower right occupancy calculation
}
# Center Occupancy and Freezing
COF <- function(x, condition, label, coln, sumcol, meancol, resultcol, wantedval, unwantedval) {
  if (exists(paste0(label,condition,"Cumulative")) == FALSE) {    # needed to identify condition of interest
    assign(paste0(label,condition,"Cumulative"), as.data.frame(Time), .GlobalEnv)
    assign(paste0(label,condition,"Proportion"), as.data.frame(Time), .GlobalEnv)
    assign(paste0(label,condition,"Stripe"), as.data.frame(Time), .GlobalEnv)
  }
  if (condition %in% conditionlist == FALSE) {
    conditionlist <<- c(conditionlist, condition)
  }
  cum <- get(paste0(label,condition,"Cumulative"))    # uses the condition and label to identify the exact type of measure and the conditions of interest 
  prop <- get(paste0(label,condition,"Proportion"))
  stripe <- get(paste0(label,condition,"Stripe"))
  cum <- cbind.data.frame(get(paste0(label,condition,"Cumulative")), x[,sumcol])
  prop <- cbind.data.frame(get(paste0(label,condition,"Proportion")), x[,meancol])
  x[,coln] <- gsub(wantedval, 1, x[,coln])    # finds the column with the data needed, then takes it and places into a new column for transformation and analysis
  x[,coln] <- gsub(unwantedval, 2, x[,coln])
  stripe <- cbind.data.frame(get(paste0(label,condition,"Stripe")), x[,coln])
  assign(paste0(label,condition,"Cumulative"), cum, .GlobalEnv)
  assign(paste0(label,condition,"Proportion"), prop, .GlobalEnv)
  assign(paste0(label,condition,"Stripe"), stripe, .GlobalEnv)
  size1 <<- as.numeric(dim(results)[1])
  tally <- as.data.frame(table(x[,coln]))
  results[size1,resultcol] <<- tally[1,2] / sum(tally[,2])
}
# Creating plots for these
# Single plots
# Path plot
PlotPath <- function(x) {
  ggplot(x, aes(x = XCoordinate, y = YCoordinate)) + geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0), fill = "lightcoral", alpha = 0.03) +
    geom_rect(aes(xmin = -13, xmax = 13, ymin = -13, ymax = 13), fill = "grey", alpha = 0.01) +
    geom_path(colour = "blue") + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2) +
    theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
}
#Individual stripe plot
IndStripePlot <- function(x, coln, colorlist) {
  ggplot(x, aes(Time, 60)) + geom_tile(aes(fill = x[,coln])) + 
    scale_fill_manual(values = colorlist) + ylab(" ") +
    theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_text(size=30,face="bold"), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.position = "none") + scale_x_continuous(expand = c(0,0))
}
#Velocity and freezing plot
VelocityPlot <- function(x) {
  x$Time <- (x$Time / 60)
  ggplot(x, aes(Time, Velocity)) + geom_segment(x = x$Time, xend = x$Time, y = 0, yend = max(x$Velocity), aes(colour = x$Freezing)) + geom_path() + 
  scale_colour_manual(values = c('lightcoral', 'white')) + scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0), breaks = pretty_breaks()) +
  theme(legend.position="none", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) + xlab("Time (min)") + ylab("Velocity (cm/s)")
}
#Summary plots
#Performance Index
PerfInd <- function(x) {
  x2 <- na.omit(x)
  PI_summary <- x2 %>%
    group_by(Condition) %>%
    summarize(sem = sd(`Performance Index`)/sqrt(n()), `Performance Index` = mean(`Performance Index`))
  ggplot(x2, aes(Condition, `Performance Index`)) +  geom_col(aes(colour = Condition), data = PI_summary, fill = NA, position = "dodge", na.rm = TRUE, show.legend = FALSE) + 
    geom_jitter(aes(colour = Condition), na.rm = TRUE, size = 5, height = 0) + scale_colour_manual(values = c("grey", "red", "blue", "forestgreen", "purple4", "darkorange")) + 
    ylab("Performance Index") + geom_hline(yintercept = 0) + geom_errorbar(aes(ymin = `Performance Index` - sem, ymax = `Performance Index` + sem), data = PI_summary, width = .5) +
    theme(legend.position="top", axis.title.x = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.text=element_text(size=20), legend.title=element_blank(),
          axis.title.y = element_text(size=20), axis.text.y = element_text(size=15), axis.text.x = element_text(size=20), 
          legend.key = element_rect(fill = NA, color = NA), legend.direction = "horizontal")
  }
# Freezing and Open Field Comparisons
OFFComp <- function(x, coln, label) {
  x2 <- na.omit(x)
  colnames(x2)[coln] <- "type"
  x2$type <- x2$type * 100
  summary <- x2 %>%
    group_by(Group, Condition) %>%
    summarize(sem = sd(type)/sqrt(n()), type = mean(type))
  ggplot(x2, aes(Condition, type)) + geom_col(aes(colour = Condition), data = summary, fill = NA, position = position_dodge(width=0.9), na.rm = TRUE, show.legend = FALSE) +
    geom_errorbar(aes(ymin = type - sem, ymax = type + sem), data = summary, width = .5, position = position_dodge(width = 0.9)) + scale_y_continuous(expand = c(0,0), limits = c(0, max(x2$type)*1.1)) +
    scale_colour_manual(values = c("grey", "red", "blue", "forestgreen", "purple4", "darkorange")) + scale_fill_manual(values = c("grey", "red", "blue", "forestgreen", "purple4", "darkorange")) +
    ylab(label) + geom_point(aes(fill = Condition), shape = 21, stroke = 1, colour = "black", na.rm = TRUE, size = 6, position=position_dodge(width=0.9)) + geom_line(aes(group = Filename)) +
    theme(legend.position="top", axis.title.x = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.text=element_text(size=20), legend.title=element_blank(),
          axis.title.y = element_text(size=20), axis.text.y = element_text(size=15), axis.text.x = element_text(size=20), strip.background = element_blank(),
          legend.key = element_rect(fill = NA, color = NA), legend.direction = "horizontal", strip.text.x = element_blank()) + facet_wrap(~ Group, scales="free")
}
# Quadrant Occupancy
QuadComp <- function(x) {
  x2 <- na.omit(x)
  x2 <- x2[,1:6]
  x2 <- melt(x2)
  colnames(x2) <- c("Filename", "Condition", "Quadrant", "QTime") 
  number <- as.numeric(dim(as.data.frame(table(x2$Condition)))[1]) - 1
  x2$QTime <- x2$QTime * 100
  Q_summary <- x2 %>%
    group_by(Condition, Quadrant) %>%
    summarize(sem = sd(QTime)/sqrt(n()), QTime = mean(QTime))
  ggplot(x2, aes(Quadrant, QTime, group = Condition)) +  geom_col(aes(colour = Condition), data = Q_summary, fill = NA, position = position_dodge(width=0.9), na.rm = TRUE, show.legend = FALSE) + 
    geom_errorbar(aes(ymin = QTime - sem, ymax = QTime + sem), data = Q_summary, width = .5, position = position_dodge(width = 0.9)) + scale_colour_manual(values = c("grey", "red", "blue", "forestgreen", "purple4", "darkorange")) + 
    ylab("Quadrant Occupancy (%)") + geom_point(aes(colour = Condition), na.rm = TRUE, size = 3, position= position_jitterdodge(dodge.width=0.9, jitter.width = .1)) + scale_y_continuous(expand = c(0,0), limits = c(0, max(x2$QTime)*1.1)) +
    theme(legend.position="top", axis.title.x = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.text=element_text(size=20), legend.title=element_blank(),
          axis.title.y = element_text(size=20), axis.text.y = element_text(size=15), axis.text.x = element_text(size=20), 
          legend.key = element_rect(fill = NA, color = NA), legend.direction = "horizontal")
}
# CDF functions
# Individual CDFs in summary info
IndCDF <- function(x, format, infotype) {
  if (format == 1) {
    if (infotype == "Quadrant Occupancy") {
      timex <- x[,1] / 60
      x <- x[,-1:-12]
      x <- x[,-5:-8]
      colnames(x) <- c("Upper Right", "Lower Right", "Upper Left", "Lower Left")
      x <- melt(x)
      x <- cbind.data.frame(timex, x)
      colnames(x) <- c("Time", "Quadrant", "value")
      x$value <- x$value * 100
      ggplot(x, aes(Time, value, colour = Quadrant)) + geom_line() + scale_colour_manual(values = c("cornflowerblue", "red", "blue", "blue4")) +
        scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0), breaks = pretty_breaks()) +
        theme(legend.position="top", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"), legend.key = element_rect(fill = NA, color = NA)) + xlab("Time (min)") + ylab("Quadrant Occupancy (%)")
    }
    else {
      x$Time <- (x$Time / 60)
      if (infotype == "Freezing") {
        x$`Freezing Mean` <- x$`Freezing Mean` * 100
        ggplot(x, aes(Time, `Freezing Mean`)) + geom_line() +
          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0), breaks = pretty_breaks()) +
          theme(legend.position="none", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black")) + xlab("Time (min)") + ylab("Time Immobile (%)")
      }
      else {
        x$`OpenField Mean` <- x$`OpenField Mean` * 100
        ggplot(x, aes(Time, `OpenField Mean`)) + geom_line() +
          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0), breaks = pretty_breaks()) +
          theme(legend.position="none", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black")) + xlab("Time (min)") + ylab("Center Occupancy (%)")
      }
    }
  }
  else {
    if (infotype == "Quadrant Occupancy") {
      timex <- x[,1] / 60
      x <- x[,-1:-8]
      x <- x[,-5:-11]
      colnames(x) <- c("Upper Right", "Lower Right", "Upper Left", "Lower Left")
      x <- melt(x)
      x <- cbind.data.frame(timex, x)
      colnames(x) <- c("Time", "Quadrant", "value")
      x$value <- x$value / 4
      ggplot(x, aes(Time, value, colour = Quadrant)) + geom_line() + scale_colour_manual(values = c("cornflowerblue", "red", "blue", "blue4")) +
        scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0), breaks = pretty_breaks()) +
        theme(legend.position="top", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"), legend.key = element_rect(fill = NA, color = NA)) + xlab("Time (min)") + ylab("Cumulative Quadrant Occupancy (sec)")
    }
    else {
      x$Time <- (x$Time / 60)
      if (infotype == "Freezing") {
        x$`Freezing Sum` <- x$`Freezing Sum` / 4
        ggplot(x, aes(Time, `Freezing Sum`)) + geom_line() +
          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0), breaks = pretty_breaks()) +
          theme(legend.position="none", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black")) + xlab("Time (min)") + ylab("Cumulative Time Immobile (sec)")
      }
      else {
        x$`OpenField Sum` <- x$`OpenField Sum` / 4
        ggplot(x, aes(Time, `OpenField Sum`)) + geom_line() +
          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0), breaks = pretty_breaks()) +
          theme(legend.position="none", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black")) + xlab("Time (min)") + ylab("Cumulative Center Occupancy (sec)")
      }
    }
  }
}
# Freezing CDF
GroupCDF <- function(condition, label, type) {
  if (type == 1) {
    x <- data.frame(get(paste0(label,condition,"Proportion")), stringsAsFactors = FALSE)
    if (label == "Freeze") {
      axislab <<- "Time Immobile (%)"
    }
    if (label == "LR") {
      axislab <<- "Lower Right Occupancy (%)"
    }
    if (label == "UR") {
      axislab <<- "Upper Right Occupancy (%)"
    }
    if (label == "LL") {
      axislab <<- "Lower Left Occupancy (%)"
    }
    if (label == "UL") {
      axislab <<- "Upper Left Occupancy (%)"
    }
    else {
      axislab <<- "Center Occupancy (%)"
    }
  }
  else {
    x <- data.frame(get(paste0(label,condition,"Cumulative")), stringsAsFactors = FALSE)
    if (label == "Freeze") {
      axislab <<- "Cumulative Time Immobile (sec)"
    }
    if (label == "LR") {
      axislab <<- "Cumulative Lower Right Occupancy (sec)"
    }
    if (label == "UR") {
      axislab <<- "Cumulative Upper Right Occupancy (sec)"
    }
    if (label == "LL") {
      axislab <<- "Cumulative Lower Left Occupancy (sec)"
    }
    if (label == "UL") {
      axislab <<- "Cumulative Upper Left Occupancy (sec)"
    }
    else {
      axislab <<- "Cumulative Center Occupancy (sec)"
    }
  }
  colnames(x) <- 0:as.numeric(dim(t(x))[1] - 1)
  x[] <- lapply(x, as.numeric)
  x[,1] <- "placeholder"
  vec <- x[,-1]
  vec <- rowMeans(vec)
  x <- melt(x)
  x[,1] <- rep(seq(from = 0, to = ((dim(x)[1]/length(levels(x[,2]))-1)/240), by = (1/240)), length.out = dim(x)[1])
  x <- cbind.data.frame(x, vec)
  ggplot(x, aes(`0`, value)) + geom_line(aes(colour = variable)) + geom_line(aes(y = vec), linetype = 2) +
    scale_colour_manual(values = c("red", "blue", "forestgreen", "purple4", "darkorange", "cornflowerblue", "indianred", "chartreuse3", "blue4", "maroon", "olivedrab", "darkmagenta", "sandybrown", "seagreen", "lightgreen", "lightcoral", "slategrey", "steelblue")) + 
    xlab("Time (min)") + ylab(axislab) + scale_y_continuous(expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0), breaks = pretty_breaks()) +
    theme(legend.position="none", panel.background = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
}
# Combined stripes
GroupStripe <- function(condition, type, colorlist) {
  x <- data.frame(get(paste0(type,condition,"Stripe")), stringsAsFactors = FALSE)
  colnames(x) <- 0:as.numeric(dim(t(x))[1] - 1)
  x[] <- lapply(x, as.numeric)
  x[,1] <- "placeholder"
  x <- melt(x)
  x[,3] <- as.character(x[,3])
  x[,1] <- rep(seq(from = 0, to = ((dim(x)[1]/length(levels(x[,2]))-1)/240), by = (1/240)), length.out = dim(x)[1])
  ggplot(x, aes(`0`, variable)) + geom_tile(aes(fill = value)) + 
    scale_fill_manual(values = colorlist) + ylab("Mouse") + scale_x_continuous(expand = c(0,0), breaks = pretty_breaks()) +
    scale_y_discrete(expand = c(0,0)) + xlab("Time (min)") +
    theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.title.y=element_text(size=20), axis.title.x=element_text(size=20), legend.position = "none", 
          axis.line = element_line(colour = "black"), axis.text.y = element_text(size=10), axis.text.x = element_text(size=10))
  
}
# ANOVA
MEANOVA <- function(x, coln) {
  anovamat <<- na.omit(x)
  anovamat <<- data.frame(anovamat[,1], anovamat[,10], anovamat[,11], anovamat[,coln])    # needed to import metadata into the ANOVA and paired t-tests along with the metric of interest
  colnames(anovamat) <<- c("Filename", "Group", "Sequence", "Response")
  anovamat <<- melt(anovamat)     # data needs to be in long format to work properly in the lme() and aov() functions
  anovamat <<- anovamat[,-4]
  if (nlevels(anovamat$Group) == 2) {
    return(t.test(value ~ Sequence, data = anovamat, paired = TRUE, conf.level = .95))
  }
  else {
    aovx <<- lme(value ~ Group + Sequence + Group*Sequence, random = ~ Sequence|Filename, data = anovamat, control = list(opt = "optim"))
    GroupSequence <<- interaction(anovamat$Group, anovamat$Sequence)
    anovax <<- aov(lme(value ~ GroupSequence, random = ~Sequence|Filename, data = anovamat), data = anovamat, control = list(opt = "optim"))
    return(capture.output(summary(aovx), Anova(aovx, type = "III"), summary(anovax), TukeyHSD(anovax))) 
  }
}
