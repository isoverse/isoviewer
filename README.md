
<!-- README.md is generated from README.Rmd. Please edit that file -->
Installation
------------

You can install isoviewer and its prerequisite [isoreader](http://www.github.com/KopfLab/isoreader) from github with:

``` r
# install.packages("devtools")
devtools::install_github("KopfLab/isoreader")
devtools::install_github("KopfLab/isoviewer")
```

Using the User Interface
------------------------

Once isoviewer is installed, you can easily launch the user interface on your local machine by using the `run` function and providing the path to the directory where your data is stored (the `data_dir` parameter). Additional optional parameters control other aspects of the GUI and can be set as needed. See `?isoviewer::run` for help. To run it as a local server accessible via your network, make sure to specify the correct `host` IP address and a fixed `port` (see `?shiny::runApp` for details).

``` r
library(isoviewer)
run(data_dir = ".")
```
