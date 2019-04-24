suppressMessages({
  suppressWarnings({
    suppressPackageStartupMessages({
      suppressForeignCheck({
mypcg <- c("lhs", "gridExtra","stringr", "factoextra",
  "shiny", "shinydashboard", "DT", "robustbase", "data.table",
  "htmlwidgets","parallel", "ggplot2", "rstudioapi", "boot", "dplyr",
  "rhandsontable", "shinycssloaders","klaR", "tidyverse","e1071", "xgboost",
  "caret", "mlr", "reshape", "formula.tools", "corrplot", "psych", "scatterD3",
  "shinyTree", "ggcorrplot", "grid","zCompositions", 'cluster', "qpcR", "ggfortify", "car",
  "plotly"
)

check.and.install.Package<-function(package_name){
  if(!package_name %in% installed.packages()){
    install.packages(package_name)
  }
}


sapply (mypcg, check.and.install.Package)
sapply (mypcg, library, character.only = TRUE)
      })
    })
  })
})
