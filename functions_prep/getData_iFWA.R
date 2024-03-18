library(sf)

iFWA <- st_read("iFWA/iFWA.gpkg")

iFWA_area <- iFWA %>% mutate(area_m2 = as.numeric(st_area(.))) %>% arrange(-area_m2) %>% mutate(id = row_number())

st_write(iFWA_area, dsn = conn, layer = "fwa_rollup", delete_dsn = T)

st_read(conn, query = "SELECT * FROM public.fwa_rollup LIMIT 10 ") %>% mapview::mapview()
