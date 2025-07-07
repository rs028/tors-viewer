tors-viewer ![license](https://img.shields.io/github/license/rs028/tors-viewer)
===========

**tors-viewer** is an interactive interface for the UoB Total Ozone Reactivity System (TORS) instrument. For a detailed description of the TORS instrument see:

Sommariva et al., *Atmos. Meas. Tech.*, 13, 1655-1670, https://doi.org/10.5194/amt-13-1655-2020, 2020.


Requirements
------------

A base installation of [R](https://www.r-project.org), with the following packages:
- [shiny](https://cran.r-project.org/web/packages/shiny)
- [shinythemes](https://cran.r-project.org/web/packages/shinythemes)
- [shinyFiles](https://cran.r-project.org/web/packages/shinyFiles).

In addition, tors-viewer requires the [atmosch-R](https://github.com/rs028/atmosch-R) functions. See the related [README file](https://github.com/rs028/atmosch-R/blob/master/README.md) for instructions on how to download and install atmosch-R.


Installation & Execution
------------------------

Click on the `<> Code` button to download the ZIP archive. Then, unzip it in a directory of choice. Make sure that the required libraries (see above) are installed in R. There are two ways to execute tors-viewer:

1. Launch R, and set the R working directory to the tors-viewer directory. Type `runApp("app.R")` at the command prompt. This will open tors-viewer in the default browser.  
**Note:** it may be necesary to execute `library(shiny)`, before `runApp().

2. Open the `app.R` file in RStudio and click the **Run App** button on the toolbar. This will open tors-viewer in the Rstudio viewer pane.
