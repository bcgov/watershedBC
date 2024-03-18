library(rmapshaper)
library(sf)
library(tidyhydat)
library(dplyr)
library(mapview)
library(DBI)
library(RPostgreSQL)

# DOWNLOAD AND READ WSC BASINS FOR REGIONS 7,8,9,10  ####

  # https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/HydrometricNetworkBasinPolygons/
  # ws <- bind_rows(lapply(list.files("F:/WSC_BASINS", pattern = "_DrainageBasin_BassinDeDrainage.shp$", full.names = T, recursive = T), st_read))
  # ws_stn <- bind_rows(lapply(list.files("F:/WSC_BASINS", pattern = "_Station.shp$", full.names = T, recursive = T), st_read))
  # ws_pp <- bind_rows(lapply(list.files("F:/WSC_BASINS", pattern = "_PourPoint_PointExutoire.shp$", full.names = T, recursive = T), st_read))

  # dir.create("wsc_data")
  # st_write(ws, "wsc_data/wsc_drainagebasins_7_8_9_10.shp")
  # st_write(ws_stn, "wsc_data/wsc_stationlocation_7_8_9_10.shp")
  # st_write(ws_pp, "wsc_data/wsc_pourpoints_7_8_9_10.shp")

# REMOVE ARTIFACTS!! CAUSED BY MULTIPLYGONS (ERROR IS SOURCE DATA) ####

  ws <- ws %>% mutate(type = st_geometry_type(.))
  ws_temp <- ws %>% filter(type != "POLYGON")

  ws_clean <- bind_rows(lapply(1:nrow(ws), function(i=1){
    print(i)
    ws[i,] %>% st_transform(3005) %>% st_make_valid() %>%
      rmapshaper::ms_explode() %>%
      mutate(area = st_area(.)) %>%
      filter(area == max(area)) %>% mutate(type = st_geometry_type(.))
    }))

  st_write(ws_clean, "wsc_data/wsc_drainagebasins_7_8_9_10_clean.shp")

# CONNECT TO DB ####

  refresh <- function(){
    dbConnect(RPostgres::dbDriver("PostgreSQL"),
              dbname = Sys.getenv("aw_dbname"),
              host = Sys.getenv("aw_host"),
              port = Sys.getenv("aw_port"),
              user = Sys.getenv("aw_user"),
              password = Sys.getenv("aw_password"))}
  conn <- refresh()
  st_write(ws_clean, dsn = conn, layer = "wsc_drainagebasin_clean_3005")
  DBI::dbDisconnect(conn)

  # ws_clean <- bind_rows(ws_temp %>% filter(type != "MULTIPOLYGON"),
  #           ws_temp_mp_expl)
  # ws_clean <- ws_clean %>% select(-area, -Etat, -Aire_km2, -type)
  #
  # ws_clean <- ws_clean %>% janitor::clean_names() %>% st_transform(4326)
  #
  # st_write(ws_clean, dsn = conn, layer = "wsc_DrainageBasin", delete_dsn = T)
  # st_write(ws_pp, dsn = conn, layer = "wsc_PourPoint", delete_dsn = T)
  # st_write(ws_stn, dsn = conn, layer = "wsc_StationLoc", delete_dsn = T)
  #
  #
  # st_read(dsn = conn, layer = "wsc_drainagebasin")
  #
  #
  # ws <- .LastValue
  #
