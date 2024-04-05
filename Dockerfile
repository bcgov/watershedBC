FROM rocker/r-ver:4.3.3 AS base

# dependencies needed for RPostgreSQL and sf
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libudunits2-dev \
    libproj-dev \
    libgdal-dev \
    libgeos-dev

FROM base

RUN R -e "install.packages(c('ranger', \
    'DBI', \
    'RPostgreSQL', \
    'pool', \
    'bcmaps', \
    'bcdata', \
    'elevatr', \
    'tictoc', \
    'sf', \
    'terra', \
    'rmapshaper', \
    'ggplot2', \
    'plotly', \
    'shiny', \
    'shinyjs', \
    'shinycssloaders', \
    'callr', \
    'leaflet', \
    'leafem', \
    'leafgl', \
    'tidyr', \
    'dplyr', \
    'stingr', \
    'DT', \
    'randomForest', \
    'readr', \
    'shinyBS', \
    'webshot', \
    'RPostgres'))"

# App working directory
WORKDIR /watershed-app

# Copying entire project to working directory
COPY . .

# Exposing default rshiny port
EXPOSE 3838

# Running the app using R Shiny
CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=3838, launch.browser=FALSE)"]