

pred_Q_prepWS <- function(w = new_ws2, my_wetlands = my_wl, my_glaciers = my_gl, my_lakes = my_lk) {

  w$wetland_sum_perc <- 100*((sum(my_wetlands$clipped_area_m2)/(1000*1000))/w$area_km2)
  w$lake_sum_perc <- 100*((sum(my_lakes$clipped_area_m2)/(1000*1000))/w$area_km2)
  w$glacier_sum_perc <- 100*((sum(my_glaciers$clipped_area_m2)/(1000*1000))/w$area_km2)

  pts <- terra::spatSample(vect(w %>% st_transform(4326)), size = 30)
  # mapview(pts)

  # TERRAIN
  dem_pts <- elevatr::get_aws_points(pts %>% st_as_sf())[[1]]

  # CLIMATE BC
  climateBC <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/climateBC.tif", win = terra::ext(vect(w %>% st_transform(4326))))

  w$MAT <- terra::extract(climateBC["Normal_1991_2020_MAT_cog.tif"], pts) %>% pull(Normal_1991_2020_MAT_cog.tif) %>% mean(na.rm = T)
  w$MAP <- terra::extract(climateBC["Normal_1991_2020_MAP_cog.tif"], pts) %>% pull(Normal_1991_2020_MAP_cog.tif) %>% mean(na.rm = T)

  # STATISTICS
  w$dem_p025 <- dem_pts %>% pull(elevation) %>% quantile(probs = 0.25, na.rm = T)
  w$dem_p050 <- dem_pts %>% pull(elevation) %>% quantile(probs = 0.50, na.rm = T)
  w$dem_p075 <- dem_pts %>% pull(elevation) %>% quantile(probs = 0.75, na.rm = T)

  return(w)
  }

pred_Q_findRef <- function(w = new_ws2_forRF(), force_station = NULL){

  #print(force_station)
    if(is.null(force_station)){

      # PRED
      if(w$regulated == "regulated"){model <- readRDS("rf_wsc_pred_station_reg_20240206_LL_A_PROB.RDS")}
      if(w$regulated == "unregulated"){model <- readRDS("rf_wsc_pred_station_nat_20240206_LL_A_PROB.RDS")}

      set.seed(500)

      pred_stn <- predict(model, w %>% st_drop_geometry() %>% select(wetland_sum_perc,
                                                                                lake_sum_perc,
                                                                                glacier_sum_perc,
                                                                                area_km2,
                                                                                MAT,
                                                                                MAP,
                                                                                dem_p025,
                                                                                dem_p050,
                                                                                dem_p075,
                                                                                LONGITUDE,
                                                                                LATITUDE))$predictions
      pred_stn <- data.frame(prob = t(pred_stn)) %>% mutate(id = row_number()) %>% arrange(-prob)

      pred_stn <- pred_stn[1,]$id
      #print(pred_stn)

      if(w$regulated == "regulated"){wsc_monthly_with_attri_dams <- as_tibble(st_read(conn, query = paste0("SELECT * FROM wsc_monthly_with_attri_dams_reg_20240204 WHERE station_cluster = '", pred_stn, "'")))}
      if(w$regulated == "unregulated"){wsc_monthly_with_attri_dams <- as_tibble(st_read(conn, query = paste0("SELECT * FROM wsc_monthly_with_attri_dams_nat_20240204 WHERE station_cluster = '", pred_stn, "'")))}
      #print(wsc_monthly_with_attri_dams)

      force_station <- wsc_monthly_with_attri_dams$station_number
    }
  return(force_station)
}

pred_Q_rf <- function(w = new_ws2_forRF(), force_station = ref_stn, wsc_STATION_NUMBER = NULL){

  #print(paste("force_station",force_station))
  print("w")
  print(w)
  # t <- st_read(conn, query = paste0("SELECT * FROM wsc_dayly_longterm_20240204"))
  # t %>% as_tibble() %>% pull(station_number) %>% unique()
  # ref_wsc_daily <- st_read(conn, query = paste0("SELECT * FROM daily_wsc_clean_20240205 WHERE station_number = '07EF001'"))
  # ref_wsc_area <- st_read(conn, query = paste0("SELECT area FROM wsc_drainagebasin_clean_3005 WHERE stationnum = '07EF001'"))

  ref_wsc_daily <- st_read(conn, query = paste0("SELECT * FROM daily_wsc_clean_20240205 WHERE station_number = '",force_station,"'"))

  ref_wsc_area <- st_read(conn, query = paste0("SELECT area FROM wsc_drainagebasin_clean_3005 WHERE stationnum = '",force_station,"'"))

  #(paste("ref_wsc_daily",ref_wsc_daily))
  ref_mad_perkm2 <- mean(ref_wsc_daily$mean, na.rm = T)/(ref_wsc_area$area/(1000*1000))
  #print(paste("ref_mad_perkm2",ref_mad_perkm2))
  target_mad <- ref_mad_perkm2*w$area_km2
  #print(paste("target_mad",target_mad))

  target_mad_ts <- ref_wsc_daily %>%
    mutate(mean_mad = mean_mad_perc*target_mad,
           p95_mad = p95_mad_perc*target_mad,
           p5_mad = p5_mad_perc*target_mad) %>%
    select(dayof_year, mean_mad, p5_mad, p95_mad)

  print("plotting")
  pppp <- target_mad_ts %>%
    pivot_longer(cols = -dayof_year) %>%
    ggplot() +
    geom_line(aes(dayof_year, value, color = name)) +
    scale_color_manual(values = c("#000000","#D53E4F","blue")) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "Day",
         y = "Estimated Mean Daily Q (cms)",
         color = "Estimate Percentiles",
         title = paste0("Model v3 [WSC reference station: ", force_station,", ", w$regulated,"]")) +
    theme_bw()

  if(is.null(wsc_STATION_NUMBER)){
    # style(ggplotly(pppp, dynamicTicks = T, width = 900), visible="legendonly", traces = c(1,7))
    print("plotting null")

    ggplotly(pppp, dynamicTicks = T, width = 800)
  }else{
    # print("plotting wsc")
    #(wsc_STATION_NUMBER)
    # wsc_STATION_NUMBER = "08KB00"
    # in_train <- st_read(conn, query = paste0("SELECT * FROM ", wsc_parde_lon, " WHERE station_number = '",wsc_STATION_NUMBER,"' LIMIT 1"))
    wsc_lt <- st_read(conn, query = paste0("SELECT * FROM wsc_pp_fasstr_calc_daily_stats WHERE STATION_NUMBER = '",wsc_STATION_NUMBER,"'"))
    #print(nrow(wsc_lt))
    if(nrow(wsc_lt) == 0){
      #print("no wsc data")
      # style(ggplotly(pppp, dynamicTicks = T, width = 900), visible="legendonly", traces = c(1,7))
      ggplotly(pppp, dynamicTicks = T, width = 800)
    }
    if(nrow(wsc_lt) > 0){
      #print("wsc data")
      ggplotly(pppp +
                 geom_line(data = wsc_lt,
                           aes(yday, median,
                               linetype = paste0(wsc_STATION_NUMBER, " [WSC Actual]"),
                               group = paste0(wsc_STATION_NUMBER, " [WSC Actual]")), size = 1) +
                 labs(linetype = "Station Data")
               , dynamicTicks = T, width = 800)
    }

  }
  }

