# wsc_q_basin_reg <- wsc_q_basin %>% filter(regulated == "regulated")
# wsc_q_basin_reg_rf <- wsc_q_basin_reg %>%
#   select(STATION_NUMBER,
#          # LATITUDE, LONGITUDE,
#          wetland_sum_perc, lake_sum_perc, glacier_sum_perc,
#          MAT, MAP, dem_p025, dem_p050, dem_p075) %>%
#   mutate(STATION_CLUSTER = factor(row_number()))
#
# set.seed(500)
# rf_station_reg <- randomForest(STATION_CLUSTER~.,data = wsc_q_basin_reg_rf %>% select(-STATION_NUMBER), ntree = 10, proximity = F, oob.prox = TRUE)
# data.frame(round(importance(rf_station_reg), 2)) %>% arrange(-MeanDecreaseGini)



wsc_estimate_cluster <- function(w = new_ws2,
                                 my_wetlands = my_wl,
                                 my_glaciers = my_gl,
                                 my_lakes = my_lk,
                                 my_STATION_NUMBER = NULL) {

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

  # PRED
  if(w$regulated == "regulated"){model <- readRDS("rf_wsc_pred_station_reg_20240204.RDS")}
  if(w$regulated == "unregulated"){model <- readRDS("rf_wsc_pred_station_nat_20240204.RDS")}

  pred_stn <- as.numeric(predict(model, w %>% select(wetland_sum_perc,lake_sum_perc,glacier_sum_perc,MAT,MAP,dem_p025,dem_p050,dem_p075)))

  if(w$regulated == "regulated"){wsc_monthly_with_attri_dams <- as_tibble(st_read(conn, query = paste0("SELECT * FROM wsc_monthly_with_attri_dams_reg_20240204 WHERE station_cluster = '", pred_stn, "'")))}
  if(w$regulated == "unregulated"){wsc_monthly_with_attri_dams <- as_tibble(st_read(conn, query = paste0("SELECT * FROM wsc_monthly_with_attri_dams_nat_20240204 WHERE station_cluster = '", pred_stn, "'")))}

  ref_wsc_daily <- st_read(conn, query = paste0("SELECT * FROM wsc_dayly_longterm_20240204 WHERE station_number = '",wsc_monthly_with_attri_dams$station_number,"'"))
  ref_mad_perkm2 <- mean(ref_wsc_daily$mean)/wsc_monthly_with_attri_dams$area_km2

  target_mad <- ref_mad_perkm2*w$area_km2

  target_mad_ts <- ref_wsc_daily %>%
    mutate(mean_mad = mean_mad_perc*target_mad,
           p95_mad = p95_mad_perc*target_mad,
           p5_mad = p5_mad_perc*target_mad) %>%
    select(dayof_year, mean_mad, p5_mad, p95_mad)

  pppp <- target_mad_ts %>%
    pivot_longer(cols = -dayof_year) %>%
    ggplot() +
    geom_line(aes(dayof_year, value, color = name)) +
    scale_color_manual(values = c("#D53E4F","#000000","blue")) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "Day",
         y = "Estimated Mean Daily Q (cms)",
         color = "Estimate Percentiles",
         title = paste0("Model v2 [Reference: ", wsc_monthly_with_attri_dams$station_number," Basin: ", w$regulated,"]")) +
    theme_bw()

  if(is.null(my_STATION_NUMBER)){
    # style(ggplotly(pppp, dynamicTicks = T, width = 900), visible="legendonly", traces = c(1,7))
    ggplotly(pppp, dynamicTicks = T, width = 900)
  }else{
    print(my_STATION_NUMBER)
    # my_STATION_NUMBER = "08KB00"
    # in_train <- st_read(conn, query = paste0("SELECT * FROM ", wsc_parde_lon, " WHERE station_number = '",my_STATION_NUMBER,"' LIMIT 1"))
    wsc_lt <- st_read(conn, query = paste0("SELECT * FROM wsc_pp_fasstr_calc_daily_stats WHERE STATION_NUMBER = '",my_STATION_NUMBER,"'"))
    if(nrow(wsc_lt) == 0){
      # style(ggplotly(pppp, dynamicTicks = T, width = 900), visible="legendonly", traces = c(1,7))
      ggplotly(pppp, dynamicTicks = T, width = 900)
    }
    if(nrow(wsc_lt) > 0){
      ggplotly(pppp +
                 geom_line(data = wsc_lt,
                           aes(yday, median,
                               linetype = paste0(my_STATION_NUMBER, " [WSC Actual]"),
                               group = paste0(my_STATION_NUMBER, " [WSC Actual]")), size = 1) +
                 labs(linetype = "Station Data")
               , dynamicTicks = T, width = 900)
    }
  }
}


# %>%
#   ggplot() +
#   geom_line(aes(dayof_year, p5_mad)) +
#   geom_line(aes(dayof_year, mean_mad)) +
#   geom_line(aes(dayof_year, p95_mad))
#   (pred_station_id <- as.numeric(predict(pred_stn, input_ws %>% select(-area_km2))))
#     (pred_station <- wsc_attr[pred_station_id,])
#     (pred_station_group <- as.numeric(pred_station$cluster_group))
#
#     (int <- as.numeric(coef(lm(Annual~area_km2, data = wsc_attr %>% filter(cluster_group == pred_station_group)))[1]))
#     (slp <- as.numeric(coef(lm(Annual~area_km2, data = wsc_attr %>% filter(cluster_group == pred_station_group)))[2]))
#
#     wsc_daily <- st_read(conn, query = paste0("SELECT * FROM wsc_dayly_longterm_20240204 WHERE station_number = '", pred_station$STATION_NUMBER,"'"))
#
#     (pred_mad <- slp*input_ws$area_km2+0)
#     (pred_doy_mad_perc <- daily_wsc_clean %>% filter(STATION_NUMBER == pred_station$STATION_NUMBER))
#
#     pred_doy_mad_perc %>%
#       ggplot() +
#       geom_line(aes(DayofYear, mean_mad_perc*pred_mad)) +
#       geom_line(data = pred_doy_mad_perc, aes(DayofYear, mad*mean_mad_perc), color = "red")
#   }
#
#   i = 3
#   (test_num <- wsc_attr[i,] %>% select(STATION_NUMBER))
#   (test <- wsc_attr[i,] %>% select(area_km2,
#                                    wetland_sum_perc, lake_sum_perc, glacier_sum_perc,
#                                    MAT, MAP, dem_p025, dem_p050, dem_p075))
#
#   pred_flow()
#
#
#
#   w
#
#
#   new_ws_area_km2 = ws$area_km2
#
#   ws %>% st_drop_geometry()
#
#
#   new_ws_area_cluster = as.numeric(predict(wsc_rf, oooo)[[1]])
#   new_ws_area_parde <- st_read(conn, query = paste0("SELECT * FROM ", wsc_parde_lon_stat, " WHERE cluster = ",new_ws_area_cluster))
#   new_ws_area_clust_intercept <- st_read(conn, query = paste0("SELECT clust_intercept FROM ", wsc_parde_lon, " WHERE cluster = ",new_ws_area_cluster, " LIMIT 1"))[[1]]
#   new_ws_area_slope <- st_read(conn, query = paste0("SELECT clust_slope FROM ", wsc_parde_lon, " WHERE cluster = ",new_ws_area_cluster, " LIMIT 1"))[[1]]
#   new_ws_ltmad <- new_ws_area_slope * new_ws_area_km2 + new_ws_area_clust_intercept
#   new_ws_ltmad_daily <- new_ws_area_parde %>%
#     mutate(estimate_p000_Q = p000 * new_ws_ltmad * 12,
#            # p001_Q = p001 * new_ws_ltmad * 12,
#            estimate_p005_Q = p005 * new_ws_ltmad * 12,
#            estimate_p025_Q = p025 * new_ws_ltmad * 12,
#            estimate_p050_Q = p050 * new_ws_ltmad * 12,
#            estimate_p075_Q = p075 * new_ws_ltmad * 12,
#            estimate_p095_Q = p095 * new_ws_ltmad * 12,
#            # p099_Q = p099 * new_ws_ltmad * 12,
#            estimate_p100_Q = p100 * new_ws_ltmad * 12)
#
#   new_ws_ltmad_daily$LTMAD_Q = new_ws_ltmad
#

# }
#
#
# format(as.Date("2021-01-01"),"%b-%d")
