refresh <- function() {
  dbPool(drv = RPostgres::dbDriver("PostgreSQL"),
         dbname = Sys.getenv("aw_dbname"),
         host = Sys.getenv("aw_host"),
         port = Sys.getenv("aw_port"),
         user = Sys.getenv("aw_user"),
         password = Sys.getenv("aw_password"))}

# length(DBI::dbListConnections(RPostgres::dbDriver("PostgreSQL")))
#
# for(i in DBI::dbListConnections(RPostgres::dbDriver("PostgreSQL"))){
#   print(i)
#   DBI::dbDisconnect(i)
# }
# pool::poolClose(conn)


pg_clip <- function(to_clip = "fwa_named", to_clip_cols_to_keep = "gnis_name",
                    clip_to = "fwa_named", watershed_id = 22501){

  q <- paste0("SELECT
                 w.*,
                 ST_Intersection(w.geom,n.geom) AS geom
               FROM ",
              to_clip," w
               JOIN ",
              clip_to, " n
               ON
                 ST_Intersects(w.geom,n.geom)
               WHERE
                 n.gnis_id = '",watershed_id,"'")

  o <-  st_read(conn, query = q)

  if(nrow(o)>0){
    o <- o %>% filter(st_geometry_type(.) %in% c("POLYGON","MULTIPOLYGON")) %>%
      mutate(clipped_area_m2 = as.numeric(st_area(.)))
    o <- o %>% bind_cols(elevatr::get_aws_points(o %>% st_centroid())[[1]] %>% st_drop_geometry() %>% dplyr::select(elevation))
    o <- o  %>% st_buffer(1) %>% st_cast("POLYGON")
  }

  if(nrow(o)>0){
    o <- o %>%
      filter(clipped_area_m2 > 10) %>%
      mutate(type = to_clip) %>%
      arrange(-clipped_area_m2)
    return(o)
  }else{
    o <- st_as_sf(data.frame(lon = -111, lat = 55), coords = c("lon","lat"), crs = 4326) %>% mutate(type = "test") %>%
      filter(type != "test")}

  return(o)
}
