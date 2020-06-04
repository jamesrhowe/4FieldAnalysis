# Analysis functions and statistical transformations used by the app

# Time-series statistics

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

# Finding velocity and freezing at each time point
Velocity <- function(x) {
  size1 <<- dim(t(x))[1] + 1
  size2 <<- dim(t(x))[1] + 2    # this one is required to simultaneously post whether or not it is freezing, along with velocity
  for (i in 1:as.numeric(dim(x)[1])) {
    if (i == 1) {
      x[i,size1] <- 0
      x[i,size2] <- runif(1, max = .99)     # need to assign random numbers below 1 to ensure the only long strings of repeated numbers are the immobile time points
    }
    else {
      dist <- sqrt((x[i,2] - x[(i-1),2])^2 + (x[i,3] - x[(i-1),3])^2) / (x[i,1] - x[(i-1),1])
      x[i,size1] <- dist
      if (dist < 1) {
        x[i,size2] <- 1     # need to assign 1 to this to ensure a long string of unique but repeated values
      }
      else {
        x[i,size2] <- runif(1, max = .99)
      }
    }
  }
  mintime <- rle(x[,size2])     # this portion needed to ensure we only count freezing that lasts one second or more
  x[,size2] <- ifelse(rep(mintime$lengths >= 4, times = mintime$lengths), "Immobile", "Mobile")
  x[,size2+1] <- ifelse(rep(mintime$lengths >= 4, times = mintime$lengths), 1, 0)
  x[,(size2+2)] <- cumsum(x[,(size2+1)])
  x[,(size2+3)] <- cummean(x[,(size2+1)])
  x <- x[,-(size2+1)]
  colnames(x)[1:3] <- c("Time", "XCoordinate", "YCoordinate")
  colnames(x)[size1:(size2+2)] <- c("Velocity", "Freezing", "Freezing Sum", "Freezing Mean")
  assign(namedfile, x, .GlobalEnv)
  analyzed <<- x
}

# Finding center and surround occupancy at each time point
OpenField <- function(x) {
  size1 <<- dim(t(x))[1] + 1
  for (i in 1:as.numeric(dim(x)[1])) {
    if (x[i,2] > -15) {
      if (x[i,2] < 15) {
        if (x[i,3] > -15) {
          if (x[i,3] < 15) {
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

# Summary statistics

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

# Center occupancy and freezing calculations
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

# ANOVA and Paired t-tests for comparisons within and between groups
MEANOVA <- function(x, coln) {
  anovamat <<- na.omit(x)
  anovamat <<- data.frame(anovamat[,1], anovamat[,10], anovamat[,11], anovamat[,coln])    # needed to import metadata into the ANOVA and paired t-tests along with the metric of interest
  colnames(anovamat) <<- c("Filename", "Group", "Sequence", "Response")
  anovamat <<- melt(anovamat)     # data needs to be in long format to work properly in the lme() and aov() functions
  anovamat <<- anovamat[,-4]
  if (nlevels(anovamat$Group) == 1) {
    return(capture.output(t.test(value ~ Sequence, data = anovamat, paired = TRUE, conf.level = .95)))
  }
  else {
    aovx <<- lme(value ~ Group + Sequence + Group*Sequence, random = ~ Sequence|Filename, data = anovamat, control = list(opt = "optim"))
    GroupSequence <<- interaction(anovamat$Group, anovamat$Sequence)
    anovax <<- aov(lme(value ~ GroupSequence, random = ~Sequence|Filename, data = anovamat), data = anovamat, control = list(opt = "optim"))
    return(capture.output(summary(aovx), Anova(aovx, type = "III"), summary(anovax), TukeyHSD(anovax))) 
  }
}

# Linear regression statistics for metric comparisons
LinReg_Compare <- function(x, x_cond, y_cond, cond) {
  x2 <- na.omit(x)
  x2[,7] <- x2[,7] / 100      # needed to prevent performance index values from being inflated
  num_x <<- grep(x_cond, comparelist)      # makes much more efficient than many nested if() statements
  num_y <<- grep(y_cond, comparelist)
  colnames(x2)[num_x + 2] <- "x_condition"
  if (x_cond == y_cond) {     # need this to ensure that no errors occur if the same metric is used on both axes
    x2$y_condition <- x2$x_condition
  }
  else {
    colnames(x2)[num_y + 2] <- "y_condition" 
  }
  x2$x_condition <- x2$x_condition * 100      # to turn decimal into percentage for graph
  x2$y_condition <- x2$y_condition * 100
  current_comparison <<- x2
}

# Creating table of both R-squared and P-values of all comparisons
Total_Compare <- function(x, cond) {
  x2 <- na.omit(x)
  x2[,7] <- x2[,7] / 100      # needed to prevent performance index values from being inflated
  if (cond != "Full") {
    total_comparison <- x2[grep(cond, x2$Condition),]
  }
  if (cond == "Full") {
    total_comparison <- x2
  }
  total_comparison <- total_comparison[,3:9]
  total_comparison <- total_comparison * 100    # not strictly necessary, but most likely best for a uniform transformation compared with the more detailed summaries
  total_rsquared <- as.data.frame(matrix(0, nrow = 7, ncol = 7), row.names = colnames(total_comparison))
  total_pvalues <- as.data.frame(matrix(0, nrow = 7, ncol = 7), row.names = colnames(total_comparison))
  colnames(total_rsquared) <- colnames(total_comparison)
  colnames(total_pvalues) <- colnames(total_comparison)
  for (i in 1:7) {
    for (j in 1:7) {
      total_regression <- cbind.data.frame(total_comparison[,i], total_comparison[,j])
      colnames(total_regression) <- c('x', 'y')
      total_regression <- lm(y ~ x, total_regression)
      total_rsquared[i,j] <- summary(total_regression)$r.squared    # gets the r-squared value that is normally returned for the linear model
      f <- summary(total_regression)$fstatistic     # gets the p value of this comparison
      p <- pf(f[1],f[2],f[3],lower.tail=F)
      attributes(p) <- NULL     # ensures this is only the value and not the metadata as well
      total_pvalues[i,j] <- p
    }
  }
  total_comparison <- cbind.data.frame(melt(total_rsquared)[,1], colnames(total_rsquared), melt(total_rsquared)[,2], melt(total_pvalues)[,2])     # need this to order the columns correctly
  if (cond == "Full") {
    colnames(total_comparison) <- c("Measure 1: Full", "Measure 2: Full", "R-squared", "P-value")
  }
  if (cond != "Full") {
    colnames(total_comparison) <- c(paste("Measure 1:", cond), paste("Measure 2:", cond), "R-squared", "P-value")
  }
  total_comparison <<- total_comparison     # cannot finalize until final step or else will return original input
  return(total_comparison)
}