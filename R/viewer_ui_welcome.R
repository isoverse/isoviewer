# tab items ====

viewer_ui_welcome <- function() {
  default_box(
    title = "Welcome to isoviewer", width = 12,

    h3("About"),

    h4("This graphical user interface is created by the ", strong(a("isoviewer", href = "http://isoviewer.isoverse.org", target = "_new")), " R package, which is part of the ", strong(a("isoverse", href = "http://www.isoverse.org", target = "_new")), " suite of open-source stable isotope data tools. Its purpose is two fold."),
    h4("First and foremost, ", strong(a("isoviewer", href = "http://isoviewer.isoverse.org", target = "_new")), " is a learning tool to illustrate how to build literate RMarkdown data processing files for IRMS data using the underlying ", strong(a("isoreader", href = "http://isoreader.isoverse.org", target = "_new")), " and ", strong(a("isprocessor", href = "http://isoprocessor.isoverse.org", target = "_new")), " packages. For this purpose, all screens in this GUI feature a live ", strong("Code Preview"), " that shows the corresponding commands for all GUI functionality. The ", icon("commenting"), " icon in each code preview allows switching between the source cody only and the same code embedded in RMarkdown. The ", icon("download"), " icon in the code preview allows download of a full ", a("RMarkdown", href = "http://rmarkdown.rstudio.com/articles_intro.html", target = "_new"), " file that reflects the parameters entered in the GUI with the goal of allowing the user to reproduce the illustrated functionality offline with a simple script that records all steps."),

    h4("The second purpose is to provide an example for a web-based IRMS data interface that is easy to run on any computer/server completely platform-independent and open-source. The isoviewer package provides some rudimentary GUI modules that can be used in other more customized applications. However, while this example app is relatively light weight and does not use a database back-end, one could consider more elaborate use cases as a programmatic interface to IRMS data that allows large scale parallel processing, reproducible aggregation and potentially sharing of years of accumulated data."),

    h3("How to use this GUI"),
    h4("The GUI searches for all iso file objects in the workspace and lists them under the ", strong("Continuous Flow"), ", ", strong("Dual Inlet"), " and ", strong("Scan"), " tabs in the menu, respectively. Simply click on the variable name of interest and it will load the iso files stored in the variable. Select which files to work with and explore/download/visualize the data from there. Make sure to check the different tabs ('Raw data', 'File Info', 'Method Info', etc.) when viewing the data to check what information is currently retrieved from the raw data files and what functionality is available."),

    h3("Disclaimer"),
    h4("The ", strong(a("isoreader", href = "http://isoreader.isoverse.org", target = "_new")), " (version ", as.character(packageVersion("isoreader")),"), ", strong(a("isoprocessor", href = "http://isoprocessor.isoverse.org", target = "_new")), " (version ", as.character(packageVersion("isoprocessor")),") and ", strong(a("isoviewer", href = "http://isoviewer.isoverse.org", target = "_new")), " (version ", as.character(packageVersion("isoviewer")),") packages are fully open-source (i.e. they are free as in 'freedom' and free as in 'free beer') and are provided as is. They are still in active development but release of a stable version to the Comprehensive R Archive Network (CRAN) is in progress. The source code is released under ",
       a(href="http://www.gnu.org/licenses/gpl-2.0.html", target = "_new", "GPL-2")," and is available on ",
       a(href="https://www.github.com/isoverse", target = "_new", "GitHub"), "."),
    h3("Feedback"),
    h4("Feedback is most welcome. These packages are intended as tools for data aggregation and large scale reproducible data processing for the scientific geochemical community but are unlikely to capture all relevant data formats at this point. Please use the repositories' respective issue trackers for any feedback, suggestions and especially bug reports (",
       a(href="https://github.com/isoverse/isoreader/issues", target = "_new", "isoreader issues"), ", ",
       a(href="https://github.com/isoverse/isoprocessor/issues", target = "_new", "isoprocessor issues"), ", ",
       a(href="https://github.com/isoverse/isoviewer/issues", target = "_new", "isoviewer issues"), ").")
  )
}
