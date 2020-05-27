
# referred to B.Hetman's EpiQuant app for package dependency setup 
# "install_EpiQuant.R" from "https://github.com/hetmanb/EpiQuant"

getPackage <- function(p) {
  # install required packages into packrat
  if (is.element(p, installed.packages()[,"Package"])) {
    print(paste0(p, " is already installed."))
  }else {
    install.packages(p, repos = "http://cran.us.r-project.org")
  }
}

getPackage("packrat")
library(packrat)
packrat::on()
getPackage("backports")
getPackage("devtools")
devtools::install_github("clauswilke/relayer") # 1 to update all
getPackage("shiny")
getPackage("magrittr")
getPackage("tibble")
getPackage("dplyr")
getPackage("Hmisc")
getPackage("reshape2")
getPackage("data.table")
getPackage("DT")
getPackage("RColorBrewer")
getPackage("tidyr")
getPackage("plotly")
getPackage("purrr")
getPackage("shinyWidgets")
getPackage("shinyjs")
getPackage("varhandle")
getPackage("shinydashboard")
getPackage("scales")

print("All packages installed.")
