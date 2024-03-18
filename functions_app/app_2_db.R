

# CONNECT DATABASE ####

  conn <- pool::dbPool(
    drv = RPostgres::dbDriver("PostgreSQL"),
    dbname = Sys.getenv("aw_dbname"),
    host = Sys.getenv("aw_host"),
    port = Sys.getenv("aw_port"),
    user = Sys.getenv("aw_user"),
    password = Sys.getenv("aw_password")
  )

# POINT ####

  postgis_get_point <-
    function(to_clip = "dams",
             my_wkt = new_ws2_wkt) {
      q <-
        paste0(
          "SELECT w.*, ST_Intersection(w.geom,ST_GeomFromText('",
          my_wkt,
          "', 3005)) AS geom
                    FROM ",
          to_clip,
          " w WHERE ST_Intersects(w.geom,ST_GeomFromText('",
          my_wkt,
          "', 3005))"
        )
      o <-  st_read(conn, query = q, quiet = T)
      if (nrow(o) == 0) {
        o <-
          st_as_sf(data.frame(lon = -111, lat = 55),
                   coords = c("lon", "lat"),
                   crs = 4326) %>% mutate(type = "test") %>% filter(type != "test")
      }
      return(o)
    }

# LINE ####

  postgis_get_line <-
    function(to_clip = "dra", my_wkt = new_ws2_wkt) {
      q <-
        paste0(
          "SELECT w.*, ST_SimplifyPreserveTopology(ST_Intersection(w.geom,ST_GeomFromText('",
          my_wkt,
          "', 3005)),5) AS geom
                    FROM ",
          to_clip,
          " w WHERE ST_Intersects(w.geom,ST_GeomFromText('",
          my_wkt,
          "', 3005))"
        )
      o <-  st_read(conn, query = q, quiet = T)
      if (nrow(o) > 0) {
        o <- o %>% filter(st_geometry_type(.) %in% c("LINESTRING")) %>%
          mutate(clipped_length_m = as.numeric(st_length(.)))
      } else{
        o <-
          st_as_sf(data.frame(lon = -111, lat = 55),
                   coords = c("lon", "lat"),
                   crs = 4326) %>%
          mutate(type = "test") %>% filter(type != "test")
      }
      return(o)
    }

# POL ####

  postgis_get_pol <- function(to_clip = "fire",
                              to_clip_cols_to_keep = "*",
                              elev = "T",
                              my_wkt = new_ws2_wkt,
                              min_area_km2 = 0.01,
                              my_dem = dem) {
    q <-
      paste0(
        "SELECT w.*, ST_Intersection(w.geom,ST_GeomFromText('",
        my_wkt,
        "', 3005)) AS geom
                   FROM ",
        to_clip,
        " w WHERE ST_Intersects(w.geom,ST_GeomFromText('",
        my_wkt,
        "', 3005))"
      )

    o <-  st_read(conn, query = q, quiet = T)

    if (nrow(o) > 0) {
      o <-
        o %>% filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
        ms_explode() %>% mutate(clipped_area_m2 = as.numeric(st_area(.))) %>%
        filter(clipped_area_m2 > min_area_km2 * (1000 * 1000))
      # print(o %>% head())
      if (nrow(o) > 0) {
        if (elev == "T") {
          o <-
            o %>% bind_cols(
              terra::extract(
                my_dem,
                o %>% st_centroid(of_largest_polygon = T) %>% st_transform(4326)
              ) %>%
                dplyr::select(COP_GLO30_FOR_watershedBC_cog) %>%
                rename(elevation = COP_GLO30_FOR_watershedBC_cog)
            )
        }
        o <- o  %>% #st_buffer(1) %>%
          st_cast("POLYGON")
      }
    }
    if (nrow(o) > 0) {
      o <- o %>% filter(clipped_area_m2 > 10) %>%
        mutate(type = to_clip) %>%
        arrange(-clipped_area_m2)
      return(o)
    } else{
      o <-
        st_as_sf(data.frame(lon = -111, lat = 55),
                 coords = c("lon", "lat"),
                 crs = 4326) %>% mutate(type = "test") %>% filter(type != "test")
    }
    return(o)
  }
