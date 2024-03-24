# rocker rshiny image: https://rocker-project.org/images/versioned/shiny.html
FROM rocker/shiny

# dependencies needed for RPostgreSQL and sf
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libudunits2-dev

# R dependencies from ./functions_app/app_1_libs.R
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
    'webshot'))"

# App working directory
WORKDIR /watershed-app

# Copying entire project to working directory
COPY . .

# Exposing default rshiny port
EXPOSE 3838

# running the app
CMD [ "Rscript", "app.R" ]