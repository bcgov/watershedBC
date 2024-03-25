# rocker rshiny image: https://rocker-project.org/images/versioned/shiny.html
# This image is quite large with dependencies ~2.30GB might look for alternatives.
# in the future. rocker/shiny is most used shiny image
FROM rocker/shiny AS base

# dependencies needed for RPostgreSQL and sf
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libudunits2-dev \
    libproj-dev \
    libgdal-dev

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

# Running the app
CMD [ "Rscript", "app.R" ]