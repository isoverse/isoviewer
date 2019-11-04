
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isoviewer <a href='http://isoviewer.isoverse.org'><img src='man/figures/isoviewer_logo_thumb.png' align="right" height="138.5"/></a>

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/isoviewer)](https://cran.r-project.org/package=isoviewer)
[![Git\_Hub\_Version](https://img.shields.io/badge/GitHub-0.7.1.9999-orange.svg?style=flat-square)](/commits)
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
applications. However, while this example app is relatively light weight
and does not use a database back-end, one could consider more elaborate
use cases of the isoreader and similar packages as a programmatic
interface to IRMS data that allows large scale parallel processing,
reproducible aggregation and potentially sharing of years of accumulated
data.

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
your local machine by using the `run` function and providing the path to
the directory where your data is stored (the `data_dir` parameter).
Additional optional parameters control other aspects of the GUI and can
be set as needed. See `?isoviewer::run` for help. To run it as a local
server accessible via your network, make sure to specify the correct
`host` IP address and a fixed `port` (see `?shiny::runApp` for details).

``` r
library(isoviewer)
run(data_dir = ".")
```

## Open Source

[isoviewer](http://isoviewer.isoverse.org/) is and will always be fully
open-source (i.e. free as in ‘freedom’ and free as in ‘free beer’) and
is provided as is. The source code is released under
GPL-2.

## isoverse <a href='http://www.isoverse.org'><img src='man/figures/isoverse_logo_thumb.png' align="right" height="138.5"/></a>

This package is part of the isoverse suite of data tools for stable
isotopes.
