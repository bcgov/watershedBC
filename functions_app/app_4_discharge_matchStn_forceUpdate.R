

pred_Q_prepWS <-
  function(w = new_ws2,
           my_wetlands = my_wl,
           my_glaciers = my_gl,
           my_lakes = my_lk) {
    w$wetland_sum_perc <-
      100 * ((sum(my_wetlands$clipped_area_m2) / (1000 * 1000)) / w$area_km2)
    w$lake_sum_perc <-
      100 * ((sum(my_lakes$clipped_area_m2) / (1000 * 1000)) / w$area_km2)
    w$glacier_sum_perc <-
      100 * ((sum(my_glaciers$clipped_area_m2) / (1000 * 1000)) / w$area_km2)

    pts <-
      terra::spatSample(vect(w %>% st_transform(4326)), size = 30)

    # TERRAIN
    dem_pts <- elevatr::get_aws_points(pts %>% st_as_sf())[[1]]

    # CLIMATE BC
    climateBC <-
      terra::rast(
        "/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/climateBC.tif",
        win = terra::ext(vect(w %>% st_transform(4326)))
      )

    w$MAT <-
      terra::extract(climateBC["Normal_1991_2020_MAT_cog.tif"], pts) %>% pull(Normal_1991_2020_MAT_cog.tif) %>% mean(na.rm = T)
    w$MAP <-
      terra::extract(climateBC["Normal_1991_2020_MAP_cog.tif"], pts) %>% pull(Normal_1991_2020_MAP_cog.tif) %>% mean(na.rm = T)

    # STATISTICS
    w$dem_p025 <-
      dem_pts %>% pull(elevation) %>% quantile(probs = 0.25, na.rm = T)
    w$dem_p050 <-
      dem_pts %>% pull(elevation) %>% quantile(probs = 0.50, na.rm = T)
    w$dem_p075 <-
      dem_pts %>% pull(elevation) %>% quantile(probs = 0.75, na.rm = T)

    return(w)
  }

pred_Q_findRef <-
  function(w = new_ws2_forRF(),
           force_station = NULL) {
    if (is.null(force_station)) {
      # PRED
      if (w$regulated == "regulated") {
        model <-
          readRDS("functions_model_app/rf_wsc_pred_station_reg_20240206_LL_A_PROB.RDS")
      }
      if (w$regulated == "unregulated") {
        model <-
          readRDS("functions_model_app/rf_wsc_pred_station_nat_20240206_LL_A_PROB.RDS")
      }

      set.seed(500)

      pred_stn <-
        predict(
          model,
          w %>% st_drop_geometry() %>% select(
            wetland_sum_perc,
            lake_sum_perc,
            glacier_sum_perc,
            area_km2,
            MAT,
            MAP,
            dem_p025,
            dem_p050,
            dem_p075,
            LONGITUDE,
            LATITUDE
          )
        )$predictions
      pred_stn <-
        data.frame(prob = t(pred_stn)) %>% mutate(id = row_number()) %>% arrange(-prob)

      # print(pred_stn[1:10,] %>% left_join(stn_training %>% mutate(id = row_number())))

      pred_stn <- pred_stn[1, ]$id
      #print(pred_stn)

      if (w$regulated == "regulated") {
        wsc_monthly_with_attri_dams <-
          as_tibble(st_read(
            conn,
            query = paste0(
              "SELECT * FROM wsc_monthly_with_attri_dams_reg_20240204 WHERE station_cluster = '",
              pred_stn,
              "'"
            )
          ))
      }
      if (w$regulated == "unregulated") {
        wsc_monthly_with_attri_dams <-
          as_tibble(st_read(
            conn,
            query = paste0(
              "SELECT * FROM wsc_monthly_with_attri_dams_nat_20240204 WHERE station_cluster = '",
              pred_stn,
              "'"
            )
          ))
      }
      #print(wsc_monthly_with_attri_dams)

      force_station <- wsc_monthly_with_attri_dams$station_number
    }
    return(force_station)
  }

pred_Q_rf <-
  function(w = new_ws2_forRF(),
           force_station = ref_stn,
           wsc_STATION_NUMBER = NULL) {
    ref_wsc_daily <-
      st_read(
        conn,
        query = paste0(
          "SELECT * FROM daily_wsc_clean_20240205 WHERE station_number = '",
          force_station,
          "'"
        )
      )

    ref_wsc_area <-
      st_read(
        conn,
        query = paste0(
          "SELECT area FROM wsc_drainagebasin_clean_3005 WHERE stationnum = '",
          force_station,
          "'"
        )
      )

    ref_mad_perkm2 <-
      mean(ref_wsc_daily$mean, na.rm = T) / (ref_wsc_area$area / (1000 * 1000))
    target_mad <- ref_mad_perkm2 * w$area_km2

    target_mad_ts <- ref_wsc_daily %>%
      mutate(
        `LT Mean` = mean_mad_perc * target_mad,
        `LT 95th` = p95_mad_perc * target_mad,
        `LT 5th` = p5_mad_perc * target_mad
      ) %>%
      select(dayof_year, `LT Mean`, `LT 95th`, `LT 5th`)

    (
      pppp <- target_mad_ts %>%
        pivot_longer(cols = -dayof_year) %>%
        ggplot() +
        geom_line(aes(
          dayof_year, value, color = name, linetype = name
        )) +
        geom_hline(aes(
          yintercept = target_mad,
          linetype = "MAD",
          color = "MAD"
        )) +
        geom_text(aes(
          x = 50,
          y = target_mad,
          label = paste0("MAD: ", signif(target_mad, 4))
        )) +
        scale_linetype_manual(values = c(1, 1, 1, 2)) +
        scale_color_manual(values = c("blue", "#D53E4F", "black", "green","yellow")) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        labs(
          x = "Day",
          y = "Estimated Mean Daily Q (cms)",
          color = "Estimate Percentiles",
          linetype = "Estimate Percentiles",
          title = paste0(
            "Model v3 [WSC reference station: ",
            force_station,
            ", ",
            w$regulated,
            "]"
          )
        ) +
        theme_bw()
    )

    if (is.null(wsc_STATION_NUMBER)) {
      pppp

    } else{
      wsc_lt <-
        st_read(
          conn,
          query = paste0(
            "SELECT * FROM wsc_pp_fasstr_calc_daily_stats WHERE STATION_NUMBER = '",
            wsc_STATION_NUMBER,
            "'"
          )
        )
      if (nrow(wsc_lt) == 0) {
        pppp
      }
      if (nrow(wsc_lt) > 0) {
        pppp +
          geom_line(
            data = wsc_lt,
            aes(
              yday,
              median,
              linetype = paste0(wsc_STATION_NUMBER, " [WSC Actual]"),
              group = paste0(wsc_STATION_NUMBER, " [WSC Actual]")
            ),
            size = 1
          ) +
          labs(linetype = "Station Data")
      }
    }
  }
