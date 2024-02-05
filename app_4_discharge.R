wsc_estimate_cluster <- function(w = new_ws2,
                                 wetlands = my_wl,
                                 glaciers = my_gl,
                                 lakes = my_lk,
                                 my_STATION_NUMBER = NULL) {

  ws <- w %>% st_transform(4326) %>% select(geom)
  if(nrow(wetlands) > 0) {wetlands = sum((wetlands$area_m2 / (1000 * 1000))) / w$area_km2
  }else{wetlands = 0}
  if(nrow(glaciers) > 0) {glaciers = sum((glaciers$area_m2 / (1000 * 1000))) / w$area_km2
  }else{glaciers = 0}
  if(nrow(lakes) > 0) {lakes = sum((lakes$area_m2 / (1000 * 1000))) / w$area_km2
  }else{lakes = 0}
  ws <- ws %>%
    mutate(
      area_km2 = w$area_km2,
      wtlnd_2 = wetlands,
      lake_m2 = lakes,
      glcr_m2 = glaciers
    )

  pts <- terra::spatSample(vect(ws), size = 100) # mapview(pts)

  # TERRAIN

  print("terrain")
  dem_pts <- elevatr::get_aws_points(pts %>% st_as_sf())[[1]]
  ws$dm_p001 <- dem_pts %>% pull(elevation) %>% quantile(probs = 0.01, na.rm = T)
  ws$dm_p005 <- dem_pts %>% pull(elevation) %>% quantile(probs = 0.05, na.rm = T)
  ws$dm_p025 <- dem_pts %>% pull(elevation) %>% quantile(probs = 0.25, na.rm = T)
  ws$dm_p050 <- dem_pts %>% pull(elevation) %>% quantile(probs = 0.50, na.rm = T)
  ws$dm_p075 <- dem_pts %>% pull(elevation) %>% quantile(probs = 0.75, na.rm = T)
  ws$dm_p095 <- dem_pts %>% pull(elevation) %>% quantile(probs = 0.95, na.rm = T)
  ws$dm_p099 <- dem_pts %>% pull(elevation) %>% quantile(probs = 0.99, na.rm = T)

  # CLIMATE BC

  print("climate")

  climateBC <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/climateBC.tif", win = terra::ext(vect(ws)))
  climateBC_pts <- terra::extract(climateBC, pts)
  climateBC_pts_dat <- climateBC_pts %>% as_tibble() %>%
    pivot_longer(-ID) %>%
    mutate(
      period = case_when(
        str_detect(name, "Normal_1961_1990") ~ "1961-1990",
        str_detect(name, "Normal_1971_2000") ~ "1971-2000",
        str_detect(name, "Normal_1981_2010") ~ "1981-2010",
        str_detect(name, "Normal_1991_2020") ~ "1991-2020",
        str_detect(name, "_2011-2040_") ~ "2011-2040",
        str_detect(name, "_2041-2070_") ~ "2041-2070",
        str_detect(name, "_2071-2100_") ~ "2071-2100",
        TRUE ~ "Error"
      ),
      parameter = case_when(
        str_detect(name, "_MAP_") ~ "Mean Annual Precipitation",
        str_detect(name, "MAT") ~ "Mean Annual Temperature",
        str_detect(name, "CMD") ~ "Cumulative Moisture Deficit",
        TRUE ~ "Error"
      ),
      model = case_when(
        str_detect(name, "Normal_") ~ "Historic",
        str_detect(name, "13GCMs_") ~ "13_GCMs",
        str_detect(name, "8GCMs_") ~ "8_GCMs",
        TRUE ~ "Error"
      ),
      ssp = case_when(
        str_detect(name, "_ssp126_") ~ "SSP126",
        str_detect(name, "_ssp245_") ~ "SSP245",
        str_detect(name, "_ssp370_") ~ "SSP370",
        str_detect(name, "_ssp585_") ~ "SSP585",
        TRUE ~ "Historic"
      )
    ) %>%
    mutate(
      year = case_when(
        period == "1961-1990" ~ 1975,
        period == "1971-2000" ~ 1985,
        period == "1981-2010" ~ 1995,
        period == "1991-2020" ~ 2005,
        period == "2011-2040" ~ 2025,
        period == "2041-2070" ~ 2055,
        period == "2071-2100" ~ 2085,
        TRUE ~ 0
      )
    )

  oooo <-
    bind_cols(
      ws %>% st_transform(3005),
      full_join(
        full_join(
          climateBC_pts_dat %>% filter(
            model == "Historic",
            year == 2005,
            parameter == "Mean Annual Temperature"
          ) %>%
            group_by(year) %>%
            summarize(
              MAT_025 = quantile(value, probs = 0.25, na.rm = T),
              MAT_055 = quantile(value, probs = 0.50, na.rm = T),
              MAT_075 = quantile(value, probs = 0.75, na.rm = T)
            ),
          climateBC_pts_dat %>% filter(
            model == "Historic",
            year == 2005,
            parameter == "Mean Annual Precipitation"
          ) %>%
            group_by(year) %>%
            summarize(
              MAP_025 = quantile(value, probs = 0.25, na.rm = T),
              MAP_055 = quantile(value, probs = 0.50, na.rm = T),
              MAP_075 = quantile(value, probs = 0.75, na.rm = T)
            ),
          by = "year"
        ),
        climateBC_pts_dat %>% filter(
          model == "Historic",
          year == 2005,
          parameter == "Cumulative Moisture Deficit"
        ) %>%
          group_by(year) %>%
          summarize(
            CMD_025 = quantile(value, probs = 0.25, na.rm = T),
            CMD_055 = quantile(value, probs = 0.50, na.rm = T),
            CMD_075 = quantile(value, probs = 0.75, na.rm = T)
          ),
        by = "year"
      ) %>% select(-year)
    ) %>% st_drop_geometry() %>%
    as_tibble()




  new_ws_area_km2 = ws$area_km2
  new_ws_area_cluster = as.numeric(predict(wsc_rf, oooo)[[1]])
  new_ws_area_parde <- st_read(conn, query = paste0("SELECT * FROM ", wsc_parde_lon_stat, " WHERE cluster = ",new_ws_area_cluster))
  new_ws_area_clust_intercept <- st_read(conn, query = paste0("SELECT clust_intercept FROM ", wsc_parde_lon, " WHERE cluster = ",new_ws_area_cluster, " LIMIT 1"))[[1]]
  new_ws_area_slope <- st_read(conn, query = paste0("SELECT clust_slope FROM ", wsc_parde_lon, " WHERE cluster = ",new_ws_area_cluster, " LIMIT 1"))[[1]]
  new_ws_ltmad <- new_ws_area_slope * new_ws_area_km2 + new_ws_area_clust_intercept
  new_ws_ltmad_daily <- new_ws_area_parde %>%
    mutate(estimate_p000_Q = p000 * new_ws_ltmad * 12,
           # p001_Q = p001 * new_ws_ltmad * 12,
           estimate_p005_Q = p005 * new_ws_ltmad * 12,
           estimate_p025_Q = p025 * new_ws_ltmad * 12,
           estimate_p050_Q = p050 * new_ws_ltmad * 12,
           estimate_p075_Q = p075 * new_ws_ltmad * 12,
           estimate_p095_Q = p095 * new_ws_ltmad * 12,
           # p099_Q = p099 * new_ws_ltmad * 12,
           estimate_p100_Q = p100 * new_ws_ltmad * 12)

  new_ws_ltmad_daily$LTMAD_Q = new_ws_ltmad

  pppp <- new_ws_ltmad_daily %>%
    pivot_longer(cols = ends_with("_Q")) %>%
    ggplot() +
    geom_line(aes(yday, value, color = name)) +
    scale_color_manual(values = c("#D53E4F","#F46D43","#FEE08B","#000000", "#E6F598", "#66C2A5", "#3288BD","blue")) +
    labs(x = "Day", y = "Estimated Mean Daily Q (cms)",
         color = "Estimate Percentiles", title = paste0("*DRAFT* [Cluster: ", new_ws_area_cluster,"]")) +
    theme_bw()

  if(is.null(my_STATION_NUMBER)){
    style(ggplotly(pppp, dynamicTicks = T, width = 900), visible="legendonly", traces = c(1,7))
  }else{
    # my_STATION_NUMBER = "08KB003"
    in_train <- st_read(conn, query = paste0("SELECT * FROM ", wsc_parde_lon, " WHERE station_number = '",my_STATION_NUMBER,"' LIMIT 1"))
    wsc_lt <- st_read(conn, query = paste0("SELECT * FROM wsc_pp_fasstr_calc_daily_stats WHERE STATION_NUMBER = '",my_STATION_NUMBER,"'"))
    if(nrow(wsc_lt) == 0){
      style(ggplotly(pppp, dynamicTicks = T, width = 900), visible="legendonly", traces = c(1,7))
    }
    if(nrow(wsc_lt) > 0){
      ggplotly(pppp +
                 geom_line(data = wsc_lt,
                           aes(yday, median,
                               linetype = paste0(my_STATION_NUMBER, " [Cluster: ", in_train[1,]$cluster, "]"),
                               group = paste0(my_STATION_NUMBER, " [Cluster: ", in_train[1,]$cluster, "]")), size = 1) +
                 labs(linetype = "Station Data")
               , dynamicTicks = T, width = 900)
    }
  }
}


format(as.Date("2021-01-01"),"%b-%d")
