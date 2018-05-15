4-Quadrant-Analysis
===================

 [![Build Status](https://travis-ci.org/jamesrhowe/4-Quadrant-Analysis.svg?branch=master)](https://travis-ci.org/jamesrhowe/4-Quadrant-Analysis)
 [![Coverage Status](https://coveralls.io/repos/github/jamesrhowe/4-Quadrant-Analysis/badge.svg?branch=master)](https://coveralls.io/github/jamesrhowe/4-Quadrant-Analysis?branch=master)
 [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

4-Quadrant-Analysis is a dynamic dashboard application for data analysis, written in R and implemented using the Shiny framework. This application is designed to analyze behavioral data gathered using the 4 Quadrant Chamber assay to find the innate valence and fear/anxiety response associated with a given olfactory or optogenetic stimulus.

Initialization
--------------
#### Implementations
This tool has 2 current implementations.

*Online/Remote*: Hosted at jamesrhowe.shinyapps.io/4-quadrant-analysis. It requires no user installation or initialization. (Currently undeployed due to server issues)

*Local*: Hosted on a local repository and accessible from the command line. Requires user installation and setup.

#### Local Installation

Navigate to the directory containing your local repository in the terminal by inputting `cd insert/path/here` 

Initialize R in the terminal by simply inputting the letter `R`, then input `install.packages(c("shiny", "shinythemes", "ggplot2", "reshape2", "dplyr", "nlme", "car", "multcomp", "scales", "evaluate")` and follow the subsequent directions to install the following packages required for the application to function:

  * shiny
  * shinythemes
  * ggplot2
  * reshape2
  * dplyr
  * nlme
  * car
  * multcomp
  * scales
  * evaluate
  
#### Local Initialization
  
Initialize the application in R using the command `shiny::runApp('4-quadrant-analysis')` and the application should open itself in your browser automatically. If the application does not open in your browser automatically, copy the server port identity into your browser.

Usage
-----

#### Upload and Analyze Data

On the control panel, click the "Select" button and and choose a text file output from the 4 Quadrant Chamber. Replace the placeholder text for the baseline and stimulus identities with the trial's relevant conditions, and choose the periods of time for analysis before and during the stimulus. To perform all analyses, simply click the "Analyze" button. This application includes four 30-minute sample trials for testing in the `test-data` directory.  

#### Select Measures of Interest

This application can analyze both individual and grouped trial performance in the 4 Quadrant Chamber, in full and subdivided into control and stimulus conditions. 

For each individual trial, the application can display the entire path traveled by the mouse in the chamber over the time series of interest. It can also track the instantaneous velocity at each time point, and summarize it over the course of the time series in a single graph. 

This application can also estimate means and variances for each group of trials, graph them, and calculate the resulting statistics, and then make comparisons between each group.  

The application also displays time series information for both grouped and individual trials in two different formats. The first is as a temporal raster plot, coloring based on the mouse's location or velocity at each time point. The second is a density function, showing the mean or cumulative time spent either freezing or in a location of interest. 

#### Download Results and Graphics

All outputs can be downloaded individually. The total summarized results output can be downloaded from the control output page, as well as all individual time series analyses for the full length of the trial, its baseline period, and its stimulus period. Statistical comparisons between and within groups can be downloaded on the pages for their respective metrics. The application can automatically detect whether a paired t-test or a mixed design 2-way ANOVA is most appropriate. 

All displayed graphics can be downloaded on the pages where they appear, and the downloaded version will appear exactly the same as the version displayed. Most graphics' proportions can be manipulated just by changing the width of the screen. All graphics are print quality (300 dpi).

License
-------

4-Quadrant-Analysis may be used, copied, or modified without restrictions under the terms of the MIT License.
