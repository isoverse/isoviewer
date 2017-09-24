# tab items ====

app_ui_welcome <- function(id) {
  tabItem(
    tabName = id,
    default_box(
      title = "Welcome to Isoviewer", width = 12,

      h3("About"),

      h4("This graphical user interface is created by the ", strong(a("isoviewer", href = "http://www.github.com/KopfLab/isoviewer", target = "_new")), " R package. Its purpose is two fold."),
      h4("First and foremost, it is a learning tool to illustrate how to build literate RMarkdown data processing files for IRMS data using the underlying ", strong(a("isoreader", href = "http://www.github.com/KopfLab/isoreader", target = "_new")), " package as a unified interface for reading, converting and quickly visualizing various raw IRMS data file formats. For this purpose, all screens in this GUI feature a live ", strong("Code Preview"), " that shows the corresponding isoreader commands for all GUI functionality. The ", icon("commenting"), " icon in each code preview allows switching between the source cody only and the same code embedded in RMarkdown. The ", icon("download"), " icon in the code preview allows download of a full ", a("RMarkdown", href = "http://rmarkdown.rstudio.com/articles_intro.html", target = "_new"), " file that reflects all parameters entered in the GUI with the goal of allowing the user to reproduce all illustrated functionality offline with a simple script that records all steps. The long-term goal for scientists using the isoreader package is to skip the GUI entirely and work directly with their data in R/RMarkdown or Python/Jupyter Notebooks (the latter using the isoreader package via cell magic and/or the feather file format to share data between R and Python)."),

      h4("The second purpose is to provide an example for a web-based IRMS data repository that is easy to run on any computer/server completely platform-independent and open-source. The isoviewer package provides some rudimentary GUI modules that could be used in other more customized applications. However, while this example app is relatively light weight and does not use a database back-end, one could consider more elaborate use cases of the isoreader and similar packages as a programmatic interface to IRMS data that allows large scale parallel processing, reproducible aggregation and potentially sharing of years of accumulated data."),

      h3("How to use this GUI"),
      h4("The easiest way to get started is to upload a few data files (or use existing examples) to put together at data set, either for ", actionLink("di_load", "dual inlet files"), " or for ", actionLink("cf_load", "continuous flow files"), ". The datasets are stored as R Data Archives that then can be quickly loaded, visualized, downloaded or exported to other file formats (currently supported are excel and the Python/R joint feather format) again for either ", actionLink("di_view", "dual inlet datasets"), " or ", actionLink("cf_view", "continous flow datasets"), ". Make sure to check the different tabs ('Raw data', 'File Info', 'Method Info', etc.) when viewing the data to explore what information is currently retrieved from the raw data files and what functionality is available. If you prefer to run the GUI on your own computer or server, check out the ", a(href="http://www.github.com/KopfLab/isoviewer#installation", target = "_new", "instructions on GitHub"), "."),

      h3("Disclaimer"),
      h4("Both ", strong(a("isoreader", href = "http://www.github.com/KopfLab/isoreader", target = "_new")), " (version ", as.character(packageVersion("isoviewer")),") and ", strong(a("isoviewer", href = "http://www.github.com/KopfLab/isoviewer", target = "_new")), " (version ", as.character(packageVersion("isoreader")),") are fully open-source (i.e. they are free as in 'freedom' and free as in 'free beer') and are provided as is. They are still in active development but release of a stable version to the Comprehensive R Archive Network (CRAN) is in progress. The source code is released under ",
             a(href="http://www.gnu.org/licenses/gpl-2.0.html", target = "_new", "GPL-2")," and is available on ",
             a(href="https://www.github.com/KopfLab", target = "_new", "GitHub"), "."),
      h3("Feedback"),
      h4("Feedback is most welcome. These packages are intended as tools for data aggregation and large scale reproducible data processing for the scientific geochemical community but are unlikely to capture all relevant data formats at this point. Please use the repositories' respective issue trackers for any feedback, suggestions and especially bug reports (",
             a(href="https://github.com/KopfLab/isoviewer/issues", target = "_new", "isoviewer issues"), ", ",
             a(href="https://github.com/KopfLab/isoreader/issues", target = "_new", "isoreader issues"), ").")
    )
  )
}
