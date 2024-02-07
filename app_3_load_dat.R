
# NAMES FOR WATERSHED MENU ####

  names <- dbReadTable(conn, "fwa_named_names")

# BC BOUND ####

  # bcmaps::bc_bound() %>% st_buffer(100000) %>% summarize() %>% st_transform(4326) %>% st_write(dsn = conn, layer = "bc_bound_buffer_100")
  bc_buffer <- st_read(conn, layer = "bc_bound_buffer_100") %>% st_geometry() %>% st_as_text()

# MODEL PARDE ####

  # wsc_parde <- read_csv("model_functions/bc_seasonal_regimes_alex_analysis_jan18_21clust.csv")

  # wsc_parde_lon <-  "bc_seasonal_regimes_alex_analysis_jan18_21clust_wsc_parde_lon"
  # wsc_parde_lon <- wsc_parde %>% pivot_longer(cols = -c(STATION_NUMBER, DRAINAGE_AREA_GROSS, LTMAD,
  #                                                       Cluster,clust_intercept,clust_slope,clust_r.squared,clust_adj.r.squared),
  #                                             names_to = "yday",
  #                                             values_to = "parde") %>%
  #   mutate(parde_perc = (parde / 12),
  #          yday = as.numeric(yday))
  # dbWriteTable(conn, wsc_parde_lon, wsc_parde_lon, overwrite = F)

  # wsc_parde_lon_stat <-  "bc_seasonal_regimes_alex_analysis_jan18_21clust_wsc_parde_lon_stat"
  # wsc_parde_lon_stat <- wsc_parde_lon %>%
  #   group_by(Cluster, yday) %>%
  #   summarize(p000 = quantile(parde_perc, probs = c(0.00), na.rm = T),
  #             p001 = quantile(parde_perc, probs = c(0.01), na.rm = T),
  #             p005 = quantile(parde_perc, probs = c(0.05), na.rm = T),
  #             p025 = quantile(parde_perc, probs = c(0.25), na.rm = T),
  #             p050 = quantile(parde_perc, probs = c(0.50), na.rm = T),
  #             p075 = quantile(parde_perc, probs = c(0.75), na.rm = T),
  #             p095 = quantile(parde_perc, probs = c(0.95), na.rm = T),
  #             p099 = quantile(parde_perc, probs = c(0.99), na.rm = T),
  #             p100 = quantile(parde_perc, probs = c(1.00), na.rm = T))
  # dbWriteTable(conn, wsc_parde_lon_stat, wsc_parde_lon_stat, overwrite = F)

# RANDOM FOREST MODEL ####

  # wsc_rf <- readRDS("model_functions/rf.RDS")

# WSC STATIONS ####

  q <- paste0( "SELECT stationnum, status, name, geom FROM wsc_stationloc WHERE ST_Intersects(geom,ST_GeomFromText('", bc_buffer, "', 4326))")
  wsc_pp <- st_read(conn, query = q)
  wsc_pp <- wsc_pp %>% mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

# WSC TRAINING STATION IDS ####

  stn_name_reg <- dbGetQuery(conn, "SELECT station_number FROM wsc_monthly_with_attri_dams_reg_20240204") %>%mutate(regulated = "Regulated Flow")
  stn_name_nat <- dbGetQuery(conn, "SELECT station_number FROM wsc_monthly_with_attri_dams_nat_20240204") %>% mutate(regulated = "Natural Flow")

  stn_training <- bind_rows(
    stn_name_reg %>%
      left_join(wsc_pp %>% rename(station_number = stationnum) %>% select(station_number, name), by = "station_number") %>%
      mutate(total_name = paste0(station_number," - ", name, " - ", regulated)),
    stn_name_nat %>%
      left_join(wsc_pp %>% rename(station_number = stationnum) %>% select(station_number, name), by = "station_number") %>%
      mutate(total_name = paste0(station_number," - ", name, " - ", regulated)))

