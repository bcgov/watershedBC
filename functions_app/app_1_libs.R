suppressPackageStartupMessages({

  library(ranger)

  # DATABASE
  library(DBI)
  library(RPostgreSQL)
  library(pool)

  # DATA
  library(bcmaps)
  library(bcdata)
  library(elevatr)

  # UTILITIES
  library(tictoc)

  # SPATIAL
  library(sf)
  library(terra)
  library(rmapshaper)

  # PLOTS
  library(ggplot2)
  library(plotly)

  # SHINY
  library(shiny)
  library(shinyjs)
  library(shinycssloaders)
  # library(shinyBS)
  library(callr)

  # LEAFLET
  library(leaflet)
  library(leafem)
  library(leafgl)

  # WRANGLING
  library(tidyr)
  library(dplyr)
  library(stringr)
  library(DT)

  # MODELS
  library(randomForest)
  library(readr)
  library(shinyBS)

  library(webshot)
  # webshot::install_phantomjs()

})
