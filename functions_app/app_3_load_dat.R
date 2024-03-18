
# NAMES FOR WATERSHED MENU ####

  names <- dbReadTable(conn, "fwa_named_names")

# BC BOUND ####

  bc_buffer <-
    st_read(conn, layer = "bc_bound_buffer_100") %>% st_geometry() %>% st_as_text()

# WSC STATIONS ####

  q <-
    paste0(
      "SELECT stationnum, status, name, geom FROM wsc_stationloc WHERE ST_Intersects(geom,ST_GeomFromText('",
      bc_buffer,
      "', 4326))"
    )
  wsc_pp <- st_read(conn, query = q)
  wsc_pp <-
    wsc_pp %>% mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

# WSC TRAINING STATION IDS ####

  stn_name_reg <-
    dbGetQuery(conn,
               "SELECT station_number FROM wsc_monthly_with_attri_dams_reg_20240204") %>%
    mutate(regulated = "Regulated Flow")
  stn_name_nat <-
    dbGetQuery(conn,
               "SELECT station_number FROM wsc_monthly_with_attri_dams_nat_20240204") %>% mutate(regulated = "Natural Flow")

  stn_training <- bind_rows(
    stn_name_reg %>%
      left_join(
        wsc_pp %>% rename(station_number = stationnum) %>% select(station_number, name),
        by = "station_number"
      ) %>%
      mutate(total_name = paste0(station_number, " - ", name, " - ", regulated)),
    stn_name_nat %>%
      left_join(
        wsc_pp %>% rename(station_number = stationnum) %>% select(station_number, name),
        by = "station_number"
      ) %>%
      mutate(total_name = paste0(station_number, " - ", name, " - ", regulated))
  )
