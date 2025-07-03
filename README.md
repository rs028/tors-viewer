tors-viewer
===========

tors-viewer is an interactive interface for the Total Ozone Reactivity
System (TORS) instrument.


Requirements
------------

A base installation of [R](https://www.r-project.org/), with the
packages [shiny](https://cran.r-project.org/web/packages/shiny/) and
[shinythemes](https://cran.r-project.org/web/packages/shinythemes/).

In addition, tors-viewer requires the
[atmosch-R](https://github.com/rs028/atmosch-R/) functions.


Execution
---------

Launch R and move to the tors-viewer directory, then type
`runApp("app.R")` at the command prompt. This will open the viewer in
the default browser.

Alternatively, open the `app.R` file in RStudio and click the **Run App**
button on the toolbar.
