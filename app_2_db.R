conn <- pool::dbPool(
  drv = RPostgres::dbDriver("PostgreSQL"),
  dbname = Sys.getenv("aw_dbname"),
  host = Sys.getenv("aw_host"),
  port = Sys.getenv("aw_port"),
  user = Sys.getenv("aw_user"),
  password = Sys.getenv("aw_password")
)


postgis_get_point <- function(to_clip = "dams", my_wkt = new_ws2_wkt) {
  q <- paste0("SELECT w.*, ST_Intersection(w.geom,ST_GeomFromText('",my_wkt,"', 3005)) AS geom
                FROM ",to_clip," w WHERE ST_Intersects(w.geom,ST_GeomFromText('",my_wkt,"', 3005))")
  o <-  st_read(conn, query = q)
  if (nrow(o) == 0) { o <- st_as_sf(data.frame(lon = -111, lat = 55), coords = c("lon", "lat"), crs = 4326) %>% mutate(type = "test") %>% filter(type != "test")}
  return(o)}

postgis_get_line <- function(to_clip = "dra", my_wkt = new_ws2_wkt) {
  q <- paste0("SELECT w.*, ST_SimplifyPreserveTopology(ST_Intersection(w.geom,ST_GeomFromText('",my_wkt,"', 3005)),5) AS geom
                FROM ",to_clip," w WHERE ST_Intersects(w.geom,ST_GeomFromText('",my_wkt,"', 3005))")
  o <-  st_read(conn, query = q)
  if (nrow(o) > 0) {o <- o %>% filter(st_geometry_type(.) %in% c("LINESTRING")) %>%
    mutate(clipped_length_m = as.numeric(st_length(.)))
  }else{
    o <- st_as_sf(data.frame(lon = -111, lat = 55), coords = c("lon", "lat"), crs = 4326) %>%
      mutate(type = "test") %>% filter(type != "test")}
  return(o)}

postgis_get_pol <- function(to_clip = "fwa_named", to_clip_cols_to_keep = "*", elev = T, my_wkt = new_ws2_wkt, min_area_km2 = 0.01) {

  q <- paste0("SELECT w.*, ST_Intersection(w.geom,ST_GeomFromText('", my_wkt, "', 3005)) AS geom
              FROM ", to_clip, " w WHERE ST_Intersects(w.geom,ST_GeomFromText('", my_wkt, "', 3005))")
  o <-  st_read(conn, query = q)

  if (nrow(o) > 0) {o <- o %>% filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
    ms_explode() %>% mutate(clipped_area_m2 = as.numeric(st_area(.))) %>%
    filter(clipped_area_m2 > min_area_km2 * (1000 * 1000))
  if (nrow(o) > 0) {
    if (elev == T) {o <- o %>% bind_cols(elevatr::get_aws_points(o %>% st_centroid(), verbose = FALSE)[[1]] %>% st_drop_geometry() %>% dplyr::select(elevation))
    }
    o <- o  %>% st_buffer(1) %>% st_cast("POLYGON")
  }
  }
  if (nrow(o) > 0) {o <- o %>% filter(clipped_area_m2 > 10) %>%
    mutate(type = to_clip) %>%
    arrange(-clipped_area_m2)
  return(o)
  } else{ o <- st_as_sf(data.frame(lon = -111, lat = 55), coords = c("lon", "lat"), crs = 4326) %>% mutate(type = "test") %>% filter(type != "test")}

  return(o)
}
