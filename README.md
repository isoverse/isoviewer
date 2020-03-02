
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isoviewer <a href='http://isoviewer.isoverse.org'><img src='man/figures/isoviewer_logo_thumb.png' align="right" height="138.5"/></a>

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/isoviewer)](https://cran.r-project.org/package=isoviewer)
[![Git\_Hub\_Version](https://img.shields.io/badge/GitHub-0.8.1-orange.svg?style=flat-square)](/commits)
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
(CRAN) but it and its dependencies can be installed easily from GitHub
using the `devtools`
package:

``` r
install.packages("devtools") # only needed if you don't have this installed yet
devtools::install_github("isoverse/isoreader")
devtools::install_github("isoverse/isoprocessor")
devtools::install_github("isoverse/isoviewer")
```

## Using the User Interface

Once isoviewer is installed, you can easily launch the user interface on
your local machine by using the `iso_start_viewer()` function which will
search for all iso file objects in your workspace and make them
accessible through the graphical interface.

``` r
library(isoviewer)
iso_start_viewer()
```

# Online Demo

An online demo of the isoviewer GUI is [available on our
server](https://kopflab.shinyapps.io/isoviewer_demo/) but has limited
computational resources (i.e. could be slow).

## Open Source

[isoviewer](http://isoviewer.isoverse.org/) is and will always be fully
open-source (i.e. free as in ‘freedom’ and free as in ‘free beer’) and
is provided as is. The source code is released under
GPL-2.

## isoverse <a href='http://www.isoverse.org'><img src='man/figures/isoverse_logo_thumb.png' align="right" height="138.5"/></a>

This package is part of the isoverse suite of data tools for stable
isotopes.
