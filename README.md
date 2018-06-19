
<!-- README.md is generated from README.Rmd. Please edit that file -->
isoprocessor
============

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/isoviewer)](https://cran.r-project.org/package=isoviewer) [![Git\_Hub\_Version](https://img.shields.io/badge/GitHub-0.7.0.9999-orange.svg?style=flat-square)](/commits) [![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://kopflab.github.io/isoviewer/)

About
-----

The [isoviewer](https://kopflab.github.io/isoviewer/) graphical user interface (GUI) provided by this package is based on the functionlity implemented by the [isoreader](https://kopflab.github.io/isoreader/) package. The purpose of the GUI is two fold.

First and foremost, it is a learning tool to illustrate how to build literate RMarkdown data processing files for IRMS data using the underlying isoreader package as a unified interface for reading, converting and quickly visualizing various raw IRMS data file formats.

The second purpose is to provide an example for a web-based IRMS data repository that is easy to run on any computer/server completely platform-independent and open-source. The isoviewer package provides some rudimentary GUI modules that could be used in other more customized applications. However, while this example app is relatively light weight and does not use a database back-end, one could consider more elaborate use cases of the isoreader and similar packages as a programmatic interface to IRMS data that allows large scale parallel processing, reproducible aggregation and potentially sharing of years of accumulated data.

Installation
------------

Since [isoreader](http://www.github.com/KopfLab/isoreader) and [isoviewer](https://kopflab.github.io/isoviewer/) are still in development, they are not yet available on the Comprehensive R Archive Network (CRAN) but can be installed easily from GitHub using the `devtools` package:

``` r
install.packages("devtools") # only needed if you don't have this installed yet
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

Online Demo
-----------

An online demo of the isoviewer GUI is [available on our server](http://server.kopflab.com/shiny/apps/isoviewer/) but has limited computational resources (i.e. could be slow).
