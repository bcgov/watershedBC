

# https://catalogue.data.gov.bc.ca/dataset/ce-disturbance-2021/resource/d6b7ed3e-c693-421e-993b-a45c1c3d18c7

# LOAD LIBRARIES ###############################################################

source("app_1_libs.R")

# CONNECT TO DATABASE ##########################################################

source("app_2_db.R")

# st_layers("C:/Users/bevin/Downloads/BC_CEF_Human_Disturbance_2021.gdb")
# cef <- st_read("C:/Users/bevin/Downloads/BC_CEF_Human_Disturbance_2021.gdb", layer = "BC_CEF_Human_Disturb_BTM_2021_merge")
# FIX GEOMETRIES IN QGIS
cef <- st_read("F:/cef_fixed_geom.gpkg")

cef_clean <- cef %>%
  # head() %>%
  janitor::clean_names() %>%
  # st_zm() %>%
  # st_buffer(1) %>%
  # st_make_valid() %>%
  # rename(geom = Shape) %>%
  select(-geometry_length,-geometry_area,-source,-shape_length,-area_ha) %>%
  mutate(area_km2 = shape_area/(1000*1000)) %>%
  select(-shape_area)
st_write(cef_clean, dsn = conn, layer = "bc_cef_2021", delete_dsn = T)

st_read(conn, query = "SELECT * FROM bc_cef_2021 LIMIT 10")
DBI::dbExecute(conn, statement = "CREATE INDEX bc_cef_2021_geom_gist ON bc_cef_2021 USING gist (geom)")
# DBI::dbExecute(conn, statement = "VACUUM ANALYZE bc_cef_2021")
