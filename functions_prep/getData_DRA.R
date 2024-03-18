
library(sf)
library(dplyr)
library(pool)


# CONNECT TO DB ####
refresh <- function() {
  dbPool(drv = RPostgres::dbDriver("PostgreSQL"),
         dbname = Sys.getenv("aw_dbname"),
         host = Sys.getenv("aw_host"),
         port = Sys.getenv("aw_port"),
         user = Sys.getenv("aw_user"),
         password = Sys.getenv("aw_password"))
}

conn <- refresh()

st_layers("X:/Research/Data_watershedBC/roads_dra/dgtl_road_atlas.gdb")
dra <- st_read("X:/Research/Data_watershedBC/roads_dra/dgtl_road_atlas.gdb", layer = "TRANSPORT_LINE")
type_code <- st_read("X:/Research/Data_watershedBC/roads_dra/dgtl_road_atlas.gdb", layer = "TRANSPORT_LINE_TYPE_CODE")
surf_code <- st_read("X:/Research/Data_watershedBC/roads_dra/dgtl_road_atlas.gdb", layer = "TRANSPORT_LINE_SURFACE_CODE")
div_code <- st_read("X:/Research/Data_watershedBC/roads_dra/dgtl_road_atlas.gdb", layer = "TRANSPORT_LINE_DIVIDED_CODE")

dra <- dra %>%
  select(TRANSPORT_LINE_TYPE_CODE,
         TRANSPORT_LINE_SURFACE_CODE,
         TRANSPORT_LINE_DIVIDED_CODE,
         SPEED_LIMIT,
         TOTAL_NUMBER_OF_LANES) %>%
  left_join(type_code %>% select(TRANSPORT_LINE_TYPE_CODE,TRANSPORT_LINE_TYPE_CODE_DESC = DESCRIPTION)) %>%
  left_join(surf_code %>% select(TRANSPORT_LINE_SURFACE_CODE,TRANSPORT_LINE_SURFACE_CODE_DESC = DESCRIPTION)) %>%
  left_join(div_code %>% select(TRANSPORT_LINE_DIVIDED_CODE,TRANSPORT_LINE_DIVIDED_CODE_DESC = DESCRIPTION)) %>%
  select(SPEED_LIMIT,TOTAL_NUMBER_OF_LANES,TRANSPORT_LINE_TYPE_CODE_DESC,TRANSPORT_LINE_SURFACE_CODE_DESC,TRANSPORT_LINE_DIVIDED_CODE_DESC, geom = GEOMETRY)

# dra %>% st_write(conn, layer = "dra", delete_dsn = T)
st_write(dra, dsn = conn, layer = "dra", delete_dsn = T)

