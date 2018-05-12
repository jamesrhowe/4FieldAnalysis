4-Quadrant-Analysis (v0.1)
==========================

4-Quadrant-Analysis is a dynamic dashboard application for data analysis, written in R and implemented using the Shiny framework. This application is designed to analyze behavioral data gathered using the 4 Quadrant Chamber assay to find the innate valence and fear/anxiety response associated with a given olfactory or optogenetic stimulus.

Initialization
--------------
#### Implementations
This tool has 2 current implementations.

*Online/Remote*: Hosted at jamesrhowe.shinyapps.io/4-quadrant-analysis. It requires no user installation or setup. (Currently undeployed due to server issues)

*Local*: Hosted on a local repository and accessible from the command line. Requires user installation and setup.

#### Local Initialization

Navigate to the directory containing your local repository in the terminal by inputting `cd insert/path/here` 

Initialize R in the terminal by simply inputting the letter `R`, then input `install.packages("INSERT PACKAGE NAME HERE")` and follow the subsequent directions to install the following packages:

  * shiny
  * ggplot2
  * reshape2
  * dplyr
  * nlme
  * car
  * multcomp
  * scales
  * evaluate
  
Initialize the application using the command `shiny::runApp('4-quadrant-analysis')` and then copy the server port identity into your browser if the application does not open in your browser automatically.

Usage
-----

#### Upload and Analyze Data

On the control panel, click the "Select" button and and choose a text file output from the 4 Quadrant Chamber. Replace the placeholder text for the baseline and stimulus identities with the trial's relevant conditions, and choose the periods of time for analysis before and during the stimulus. To perform all analyses, simply click the "Analyze" button. This application includes four 30-minute sample trials for testing in the `test-data` directory.  

#### Select Measures of Interest

This can analyze both individual and group performance in the 4 Quadrant Chamber.

#### Download Results and Graphics

All outputs can be downloaded individually. The total summarized results output can be downloaded from the control output page, as well as all individual time series analyses for the full length of the trial, its baseline period, and its stimulus period. Statistical comparisons between and within groups can be downloaded on the pages for their respective metrics. The application can automatically detect whether a paired t-test or a mixed design 2-way ANOVA is most appropriate. 

All displayed graphics can be downloaded on the pages where they appear, and the downloaded version will appear exactly the same as the version displayed. Most graphics' proportions can be manipulated just by changing the width of the screen. All graphics are print quality (300 dpi).

License
-------

4-Quadrant-Analysis may be used, copied, or modified without restrictions under the terms of the MIT License.
