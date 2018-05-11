4-Quadrant-Analysis (v0.1)
==========================

4-Quadrant-Analysis is a dynamic dashboard application designed to analyze behavioral data gathered using the 4 Quadrant Chamber assay for stimulus valence and fear/anxiety behavior.

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

#### Upload Data



License
-------

4-Quadrant-Analysis may be used, copied, or modified without restrictions under the terms of the MIT License.
