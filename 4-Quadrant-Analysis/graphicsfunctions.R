# Graphics calls used by the app

source("analysisfunctions.R")     # contains all the analytic functions, makes plotting more efficient

# Individual trial plots

# Path plot to summarize total mouse motion
PlotPath <- function(x) {
  ggplot(x, aes(x = XCoordinate, y = YCoordinate)) + geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0), fill = "lightcoral", alpha = 0.03) +
    geom_rect(aes(xmin = -13, xmax = 13, ymin = -13, ymax = 13), fill = "grey", alpha = 0.01) +
    geom_path(colour = "blue") + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2) +
    theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
}

#Single stripe plot for center and quadrant occupancy in each individual trial
IndStripePlot <- function(x, coln, colorlist) {
  ggplot(x, aes(Time, 60)) + geom_tile(aes(fill = x[,coln])) + 
    scale_fill_manual(values = colorlist) + ylab(" ") +
    theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_text(size=30,face="bold"), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.position = "none") + scale_x_continuous(expand = c(0,0))
}

#Velocity plot to visualize general locomotion and proportional freezing
VelocityPlot <- function(x) {
  x$Time <- (x$Time / 60)
  ggplot(x, aes(Time, Velocity)) + geom_segment(x = x$Time, xend = x$Time, y = 0, yend = max(x$Velocity), aes(colour = x$Freezing)) + geom_path() + 
    scale_colour_manual(values = c('lightcoral', 'white')) + scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0), breaks = pretty_breaks()) +
    theme(legend.position="none", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) + xlab("Time (min)") + ylab("Velocity (cm/s)")
}

# Individual CDF plots
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

# Grouped trial plots

# Combined, stacked stripes
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

# Grouped CDF plots
GroupCDF <- function(condition, label, type) {
  label_list <- c("Freeze", "LR", "UR", "LL", "UL", "OF")
  label_num <- grep(label, label_list)
  if (type == 1) {
    x <- data.frame(get(paste0(label,condition,"Proportion")), stringsAsFactors = FALSE)
    axislablist <- c("Time Immobile (%)", "Lower Right Occupancy (%)", "Upper Right Occupancy (%)", "Lower Left Occupancy (%)", "Upper Left Occupancy (%)", "Center Occupancy (%)")
  }
  else {
    x <- data.frame(get(paste0(label,condition,"Cumulative")), stringsAsFactors = FALSE)
    axislablist <- c("Cumulative Time Immobile (sec)", "Cumulative Lower Right Occupancy (sec)", "Cumulative Upper Right Occupancy (sec)", "Cumulative Lower Left Occupancy (sec)", "Cumulative Upper Left Occupancy (sec)", "Cumulative Center Occupancy (sec)")
  }
  colnames(x) <- 0:as.numeric(dim(t(x))[1] - 1)
  x[] <- lapply(x, as.numeric)
  x[,1] <- "placeholder"
  x <- melt(x)
  x[,1] <- rep(seq(from = 0, to = ((dim(x)[1]/length(levels(x[,2]))-1)/240), by = (1/240)), length.out = dim(x)[1])
  x$value <- x$value * 100
  ggplot(x, aes(`0`, value)) + geom_line(aes(colour = variable)) + geom_smooth(method = "loess", linetype = 2, colour = "black", span = 0.25, se = FALSE) +
    scale_colour_manual(values = c("red", "blue", "forestgreen", "purple4", "darkorange", "cornflowerblue", "indianred", "chartreuse3", "blue4", "maroon", "olivedrab", "darkmagenta", "sandybrown", "seagreen", "lightgreen", "lightcoral", "slategrey", "steelblue")) + 
    xlab("Time (min)") + ylab(axislablist[label_num]) + scale_y_continuous(expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0), breaks = pretty_breaks()) +
    theme(legend.position="none", panel.background = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
}

# Graphs for statistical comparisons

# Quadrant occupancy
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

# Performance Index
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

# Freezing and Center Occupancy
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

# Comparison line plot
AxisCompare <- function(x, x_cond, y_cond, cond) {
  LinReg_Compare(x, x_cond, y_cond, cond)
  axislablist <- c("Upper Left Occupancy (%)", "Upper Right Occupancy (%)", "Lower Left Occupancy (%)", "Lower Right Occupancy (%)", "Performance Index", "Time Immobile (%)", "Center Occupancy (%)")
  if (cond == "Full") {
    plot <- ggplot(current_comparison, aes(x_condition, x_condition)) + geom_smooth(method = lm, linetype = 2, colour = "black", fullrange = TRUE) + xlab(axislablist[num_x]) + ylab(axislablist[num_y]) + 
    geom_point(aes(fill = Condition), shape = 21, stroke = 1, colour = "black", na.rm = TRUE, size = 4) +
    scale_fill_manual(values = c("red", "blue", "forestgreen", "purple4", "darkorange")) + 
    scale_x_continuous(expand = c(0,0), breaks = pretty_breaks(), limits = c(min(current_comparison$x_condition) - (5+(abs(mean(current_comparison$x_condition)*.15))), max(current_comparison$x_condition) + (5+abs(mean(current_comparison$x_condition)*.15)))) + 
    coord_cartesian(xlim = c(min(current_comparison$x_condition) - abs(mean(current_comparison$x_condition)*.1), max(current_comparison$x_condition) + abs(mean(current_comparison$x_condition)*.1))) +
    theme(legend.position="top", panel.background = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.text=element_text(size=20), legend.title=element_blank(),
          axis.title.y = element_text(size=20), axis.text.y = element_text(size=15), axis.title.x = element_text(size=20), axis.text.x = element_text(size=15),
          strip.background = element_blank(), legend.key = element_rect(fill = NA, color = NA), legend.direction = "horizontal", strip.text.x = element_blank())
  }
  if (cond != "Full") {
    current_comparison <- current_comparison[grep(cond, current_comparison$Condition),]
    plot <- ggplot(current_comparison, aes(x_condition, y_condition)) + geom_smooth(method = lm, linetype = 2, colour = "black", fullrange = TRUE) + xlab(axislablist[num_x]) + ylab(axislablist[num_y]) + 
      geom_point(fill = "white", shape = 21, stroke = 1, colour = "black", na.rm = TRUE, size = 4) +
      scale_x_continuous(expand = c(0,0), breaks = pretty_breaks(), limits = c(min(current_comparison$x_condition) - (5+(abs(mean(current_comparison$x_condition)*.15))), max(current_comparison$x_condition) + (5+abs(mean(current_comparison$x_condition)*.15)))) + 
      coord_cartesian(xlim = c(min(current_comparison$x_condition) - abs(mean(current_comparison$x_condition)*.1), max(current_comparison$x_condition) + abs(mean(current_comparison$x_condition)*.1))) +
      theme(legend.position="none", panel.background = element_blank(), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.text=element_text(size=20), legend.title=element_blank(),
            axis.title.y = element_text(size=20), axis.text.y = element_text(size=15), axis.title.x = element_text(size=20), axis.text.x = element_text(size=15),
            strip.background = element_blank(), legend.key = element_rect(fill = NA, color = NA), legend.direction = "horizontal", strip.text.x = element_blank())
  }
  return(plot)
}

LinReg_Display <- function(x, x_cond, y_cond, cond) {
  LinReg_Compare(x, x_cond, y_cond, cond)
  rownames(current_comparison) <- paste(current_comparison$Filename, current_comparison$Condition)
  if (cond == "Full") {
    all_inputs <- current_comparison
    model <- lm(y_condition ~ x_condition, all_inputs)
    return(capture.output(summary(model)))
  }
  if (cond != "Full") {
    selected_inputs <- current_comparison[grep(cond, current_comparison$Condition),]
    model <- lm(y_condition ~ x_condition, selected_inputs)
    return(capture.output(summary(model)))
  }
}

# Comparison Heatmap

Compare_Heatmap <- function(x, cond, display_type) {
  Total_Compare(x, cond)
  if (display_type == 1) {
    image <- ggplot(total_comparison, aes(x = total_comparison[,1], y = factor(total_comparison[,2], 
      level = c('Lower Left', 'Lower Right', 'Upper Left', 'Upper Right', 'Performance Index', 'Freezing', 'Open Field')))) + 
      geom_tile(aes(fill = total_comparison[,3])) + scale_x_discrete(expand = c(0,0)) + 
      scale_y_discrete(expand = c(0,0)) + scale_fill_gradientn(colours = matlab.like(100), name = paste("R-squared:", cond)) + 
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.x=element_blank(), 
            axis.ticks.y=element_blank(), legend.position="bottom", legend.direction = "horizontal")
    return(image)     # need this for the graph to show up
  }
  if (display_type == 2) {
    image <- ggplot(total_comparison, aes(x = total_comparison[,1], y = factor(total_comparison[,2], 
      level = c('Lower Left', 'Lower Right', 'Upper Left', 'Upper Right', 'Performance Index', 'Freezing', 'Open Field')))) + 
      geom_tile(aes(fill = total_comparison[,4])) + scale_x_discrete(expand = c(0,0)) + 
      scale_y_discrete(expand = c(0,0)) + scale_fill_gradientn(colours = rev(matlab.like(100)), name = paste("P-value:", cond)) + 
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.x=element_blank(), 
            axis.ticks.y=element_blank(), legend.position="bottom", legend.direction = "horizontal")
    return(image)
  }
}