
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isoviewer <a href='http://isoviewer.isoverse.org'><img src='man/figures/isoviewer_logo_thumb.png' align="right" height="138.5"/></a>

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/isoviewer)](https://cran.r-project.org/package=isoviewer)
[![Git\_Hub\_Version](https://img.shields.io/badge/GitHub-0.8.4-orange.svg?style=flat-square)](/commits)
[![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://isoviewer.isoverse.org)

## About

The [isoviewer](https://isoviewer.isoverse.org) graphical user interface
(GUI) provided by this package is based on the functionlity implemented
by the [isoreader](https://isoreader.isoverse.org) and
[isoprocessor](https://isoprocessor.isoverse.org) packages The purpose
of the GUI is two fold.

First and foremost, it is a learning tool to illustrate how to build
literate RMarkdown data processing files for IRMS data using the
underlying isoreader package as a unified interface for reading,
converting and quickly visualizing various raw IRMS data file formats.

The second purpose is to provide an example for a web-based IRMS data
repository that is easy to run on any computer/server completely
platform-independent and open-source. The isoviewer package provides
some rudimentary GUI modules that could be used in other more customized
applications.

## Installation

[isoviewer](https://kopflab.github.io/isoviewer/) is still in
development and not yet available on the Comprehensive R Archive Network
(CRAN) but it and its dependencies can be installed from GitHub using
the `devtools` package:

``` r
# installs the development tools package if not yet installed
if(!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("isoverse/isoprocessor")
devtools::install_github("isoverse/isoviewer")
```

## Using the User Interface

Once isoviewer is installed, you can launch the user interface on your
local machine by using the `iso_start_viewer()` function which will
search for all iso file objects in your workspace and make them
accessible through the graphical interface.

``` r
isoviewer::iso_start_viewer()
```

Troubleshooting: if you run into any issues with the viewer crashing
unexpectedly and getting stuck during restart. Try resetting it with
`isoviewer::iso_start_viewer(reset = TRUE)`. Also, please consider
reporting the details of the crash at
<https://github.com/isoverse/isoviewer/issues>.

## Running a local server

To run a local isoviewer server to which other users on your network can
connect, simply use the `iso_start_viewer_server()` server function and
direct others to your IP address, port 3838 (for example
192.168.0.42:3838). Note that this won’t work if your firewall or local
network block port 3838. Keep the R session open as long as you want to
run the server and stop by it by pressing `Esc` or closing your
R/RStudio session.

``` r
isoviewer::iso_start_viewer_server()
```

## Running a web server

To run a web server with your isofiles (e.g. on
[www.shinyapps.io](https://www.shinyapps.io/)), create a new R project
folder with a script file called `app.R` at the top level (= working
directory) of the project. In this file, add the following code and
adjust which iso objects to make available in the viewer GUI. Note that
for your app to launch quickly when a user connects, it is higly
recommended to load only `.rds` collections of iso files (i.e. save your
iso objects via [isoreader’s
`iso_save()`](https://isoreader.isoverse.org/reference/iso_save.html)
function and then load them from there for your isoviewer server). To
test the server, launch it on your own computer by running
`shiny::runApp()` from the R console in your project. To deploy this app
e.g. to [www.shinyapps.io](https://www.shinyapps.io/), follow [their
instructions](https://shiny.rstudio.com/articles/shinyapps.html) to get
your `rsconnect` credentials set up and then simply run
`rsconnect::deployApp()` from the R console in your project. Note that
the [bioconductor](https://www.bioconductor.org/) packages used by
isoreader sometimes cause trouble during the upload, in which case you
may have to explicitly re-install them before deploying your app
(e.g. by using `BiocManager::install()`) and setting the bioconductor
repositories using `options(repos = BiocManager::repositories())`.

``` r
# start isoviewer server
library(isoreader)
isoviewer::iso_start_viewer_server(
  # provide a list of objects to make accessible
  iso_objects = list(
    di_example = iso_read_dual_inlet("di_example.di.rds"),
    cf_example = iso_read_continuous_flow("cf_example.cf.rds"),
    scan_example = iso_read_scan("scan_example.scan.rds")
  ),
  # make sure it doesn't launch on its own
  launch = FALSE
)
```

This code example is actually the one available as the online demo
below.

# Online Demo

An online demo of the isoviewer GUI is [available on our
server](https://kopflab.shinyapps.io/isoviewer_demo/) but has limited
computational resources (i.e. could be slow).

## Open Source

[isoviewer](http://isoviewer.isoverse.org/) is and will always be fully
open-source (i.e. free as in ‘freedom’ and free as in ‘free beer’) and
is provided as is. The source code is released under GPL-2.

## isoverse <a href='http://www.isoverse.org'><img src='man/figures/isoverse_logo_thumb.png' align="right" height="138.5"/></a>

This package is part of the isoverse suite of data tools for stable
isotopes.
