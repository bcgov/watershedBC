# LOAD LIBRARIES ###############################################################

  source("app_1_libs.R")

# CONNECT TO DATABASE ##########################################################

  source("app_2_db.R")

# LOAD BASELINE DATSETS ########################################################

names <- readRDS("named.RDS")
bc_buffer <- bcmaps::bc_bound() %>% st_buffer(100000) %>% summarize() %>% st_transform(4326) %>% st_geometry() %>% st_as_text()
wsc_parde <- read_csv("model_functions/bc_seasonal_regimes_alex_analysis_jan18_21clust.csv")
wsc_rf <- readRDS("model_functions/rf.RDS")
wsc_parde_lon <- wsc_parde %>% pivot_longer(
    cols = -c(
      STATION_NUMBER,
      DRAINAGE_AREA_GROSS,
      LTMAD,
      Cluster,
      clust_intercept,
      clust_slope,
      clust_r.squared,
      clust_adj.r.squared
    ),
    names_to = "yday",
    values_to = "parde"
  ) %>%
  mutate(parde_perc = (parde / 12)) %>%
  mutate(yday = as.numeric(yday))
wsc_parde_lon_stat <- wsc_parde_lon %>%
  group_by(Cluster, yday) %>%
  summarize(
    p000 = quantile(parde_perc, probs = c(0.00), na.rm = T),
    p001 = quantile(parde_perc, probs = c(0.01), na.rm = T),
    p005 = quantile(parde_perc, probs = c(0.05), na.rm = T),
    p025 = quantile(parde_perc, probs = c(0.25), na.rm = T),
    p050 = quantile(parde_perc, probs = c(0.50), na.rm = T),
    p075 = quantile(parde_perc, probs = c(0.75), na.rm = T),
    p095 = quantile(parde_perc, probs = c(0.95), na.rm = T),
    p099 = quantile(parde_perc, probs = c(0.99), na.rm = T),
    p100 = quantile(parde_perc, probs = c(1.00), na.rm = T)
  )

q <- paste0( "SELECT stationnum, status, name, geom FROM wsc_stationloc WHERE ST_Intersects(geom,ST_GeomFromText('", bc_buffer, "', 4326))")
wsc_pp <- st_read(conn, query = q)

wsc_pp <- wsc_pp %>% mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

# FUNCTIONS ####################################################################

wsc_estimate_cluster <- function(w = new_ws2,
                                 wetlands = my_wl,
                                 glaciers = my_gl,
                                 lakes = my_lk) {
  ws <- w %>% st_transform(4326) %>% select(geom)
  if (nrow(wetlands) > 0) {
    wetlands = sum((wetlands$area_m2 / (1000 * 1000))) / w$area_km2
  } else{
    wetlands = 0
  }
  if (nrow(glaciers) > 0) {
    glaciers = sum((glaciers$area_m2 / (1000 * 1000))) / w$area_km2
  } else{
    glaciers = 0
  }
  if (nrow(lakes) > 0) {
    lakes = sum((lakes$area_m2 / (1000 * 1000))) / w$area_km2
  } else{
    lakes = 0
  }
  ws <- ws %>%
    mutate(
      area_km2 = w$area_km2,
      wtlnd_2 = wetlands,
      lake_m2 = lakes,
      glcr_m2 = glaciers
    )

  pts <-
    terra::spatSample(vect(ws), size = 100) # mapview(pts)

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
  climateBC <-
    terra::rast(
      "/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/climateBC.tif",
      win = terra::ext(vect(ws))
    )
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
  new_ws_area_parde <- wsc_parde_lon_stat %>% filter(Cluster == new_ws_area_cluster)
  new_ws_area_clust_intercept <- wsc_parde_lon %>%
    filter(Cluster == new_ws_area_cluster) %>%
    select(clust_intercept) %>% pull() %>% unique()
  new_ws_area_slope <- wsc_parde_lon %>%
    filter(Cluster == new_ws_area_cluster) %>%
    select(clust_slope) %>% pull() %>% unique()
  new_ws_ltmad <- new_ws_area_slope * new_ws_area_km2 + new_ws_area_clust_intercept

  ggplotly(
    new_ws_area_parde %>%
      mutate(
        p000_Q = p000 * new_ws_ltmad * 12,
        p001_Q = p001 * new_ws_ltmad * 12,
        p005_Q = p005 * new_ws_ltmad * 12,
        p025_Q = p025 * new_ws_ltmad * 12,
        p050_Q = p050 * new_ws_ltmad * 12,
        p075_Q = p075 * new_ws_ltmad * 12,
        p095_Q = p095 * new_ws_ltmad * 12,
        p099_Q = p099 * new_ws_ltmad * 12,
        p100_Q = p100 * new_ws_ltmad * 12) %>%
      ggplot() +
      geom_ribbon(aes(as.numeric(yday),ymin = p000_Q,ymax = p100_Q,fill = "Q000-Q100")) +
      # geom_ribbon(aes(as.numeric(yday),ymin = p001_Q,ymax = p099_Q,fill = "Q1-Q99")) +
      geom_ribbon(aes(as.numeric(yday),ymin = p005_Q,ymax = p095_Q,fill = "Q005-Q095")) +
      geom_ribbon(aes(as.numeric(yday),ymin = p025_Q,ymax = p075_Q,fill = "Q025-Q075")) +
      geom_line(aes(as.numeric(yday), p050_Q, color = "Cluster estimate median")) +
      # geom_point(data = wsc_parde_lon %>% filter(STATION_NUMBER == sample$STATION_NUMBER), aes(as.numeric(yday), parde_perc*LTMAD*12, color = "Measurement"), size = 1) +
      # geom_line(data = fasstr::calc_daily_stats(station_number = sample$STATION_NUMBER, complete_years = T), aes(DayofYear, Mean, color = "Measurement"), size = 1) +
      geom_hline(yintercept = new_ws_ltmad, color = "blue", size = 1) +
      # geom_hline(yintercept = sample$LTMAD, color = "black") +
      labs(x = "Day", y = "Q", title = "Estimated Long Term Mean Daily Dischage for Selected Basin") +
      scale_color_manual(values = c("steelblue", "black")) +
      scale_fill_manual(values = c("grey90","grey70","grey50")) +
      # scale_y_log10() +
      theme_bw(),
    dynamicTicks = T,
    width = 800
  )
}

postgis_get_line <-
  function(to_clip = "dra", my_wkt = new_ws2_wkt) {
    # conn <- refresh()
    q <- paste0(
      "SELECT w.*,
                ST_SimplifyPreserveTopology(ST_Intersection(w.geom,ST_GeomFromText('",
      my_wkt,
      "', 3005)),5) AS geom
                FROM ",
      to_clip,
      " w
                WHERE ST_Intersects(w.geom,ST_GeomFromText('",
      my_wkt,
      "', 3005))"
    )
    o <-  st_read(conn, query = q)
    if (nrow(o) > 0) {
      o <- o %>% filter(st_geometry_type(.) %in% c("LINESTRING")) %>%
        mutate(clipped_length_m = as.numeric(st_length(.)))
    } else{
      o <-
        st_as_sf(data.frame(lon = -111, lat = 55),
                 coords = c("lon", "lat"),
                 crs = 4326) %>% mutate(type = "test") %>% filter(type != "test")
    }

    return(o)
  }

postgis_get_pol <-
  function(to_clip = "fwa_named",
           to_clip_cols_to_keep = "*",
           elev = T,
           my_wkt = new_ws2_wkt,
           min_area_km2 = 0.01) {
    # conn <- refresh()

    q <- paste0(
      "SELECT w.*,
                ST_Intersection(w.geom,ST_GeomFromText('",
      my_wkt,
      "', 3005)) AS geom
        FROM ",
      to_clip,
      " w
        WHERE ST_Intersects(w.geom,ST_GeomFromText('",
      my_wkt,
      "', 3005))"
    )

    o <-  st_read(conn, query = q)

    if (nrow(o) > 0) {
      o <-
        o %>% filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
        ms_explode() %>%
        mutate(clipped_area_m2 = as.numeric(st_area(.))) %>%
        filter(clipped_area_m2 > min_area_km2 * (1000 * 1000))
      if (nrow(o) > 0) {
        if (elev == T) {
          o <-
            o %>% bind_cols(
              elevatr::get_aws_points(o %>% st_centroid(), verbose = FALSE)[[1]] %>% st_drop_geometry() %>% dplyr::select(elevation)
            )
        }
        o <- o  %>% st_buffer(1) %>% st_cast("POLYGON")
      }
    }

    if (nrow(o) > 0) {
      o <- o %>%
        filter(clipped_area_m2 > 10) %>%
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

# BASEMAP ######################################################################

res <- 300

initial_map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery") %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "WorldTopoMap") %>%
  addWMSTiles(
    "http://maps.gov.bc.ca/arcserver/rest/services/province/roads_wm/MapServer/tile/{z}/{y}/{x}",
    layers = "GRB_BSK",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    group = "BC Basemap"
  ) %>%
  addMeasure(
    primaryLengthUnit = "kilometers",
    secondaryLengthUnit = "meters",
    primaryAreaUnit = "hectares",
    secondaryAreaUnit = "sqmeters",
    position = "topleft"
  ) %>%
  leafem:::addCOG(
    url = "https://bcbasin.s3.ca-central-1.amazonaws.com/BC_2023v2_4326_v2_bigTiff_JPEG.tif",
    group = "Sentinel 2023 (slow)",
    resolution = res,
    opacity = 1,
    autozoom = F
  ) %>%
  leafem:::addCOG(
    url = "https://bcbasin.s3.ca-central-1.amazonaws.com/1985_1990v3_COG_AV_JP_BIG.tif",
    group = "Landsat 1985-1990 (slow)",
    resolution = res,
    opacity = 1,
    autozoom = F
  ) %>%
  leafem:::addCOG(
    url = "https://bcbasin.s3.ca-central-1.amazonaws.com/2020_2023v3_COG_AV_JP_BIG.tif",
    group = "Landsat 2020-2023 (slow)",
    resolution = res,
    opacity = 1,
    autozoom = F
  ) %>%
  addLayersControl(
    baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
    overlayGroups = c(
      "Sentinel 2023 (slow)",
      "Landsat 2020-2023 (slow)",
      "Landsat 1985-1990 (slow)"
    ),
    options = layersControlOptions(collapsed = F)
  ) %>%
  hideGroup(c(
    "Sentinel 2023 (slow)",
    "Landsat 1985-1990 (slow)",
    "Landsat 2020-2023 (slow)"
  )) %>%
  addMouseCoordinates()

# UI ####

ui <- navbarPage(
  theme = "css/bcgov.css",
  title = "WatershedBC (v0.1 Beta Testing)",
  useShinyjs(),


  tabPanel(
    "WatershedBC (v0.1 Beta Testing)",

    modalDialog(
      h3("watershedBC in draft mode"),
      HTML("Welcome to WatershedBC, a research tool that is in <u>active development</u> designed to aggregate watershed data and estimate streamflow. Before you proceed, please read and accept the following disclaimer:"),
      HTML("<ul><br>
        <li><b><u>Prototype:</u></b> watershedBC is a working prototype, there are known issues and innacuracies that we are actively fixing.</li><br>
        <li><b><u>Not validated:</u></b> At this time, do not use any information from watershedBC for decision making.</li><br>
        <li><b><u>Frequent outages:</u></b> This tool is a proof of concept and will periodically be offline, freeze, or crash.</li><br>
        <li><b><u>User responsibility:</u></b> The User is responsible for the safe interpretation of the datasets presented.</li><br>
        <li><b><u>Open source:</u></b> The goal of this project is to be openly transparent via <a href='https://github.com/bcgov/watershedBC/', target='_blank'>https://github.com/bcgov/watershedBC/</a></li><br>
        <li><b><u>Speed:</u></b> This tool slows down with more concurent users. This will be fixed in future versions.</li><br>
        <li><b><u>Feedback:</u></b> Users can provide feedback here: <a href='https://github.com/bcgov/watershedBC/issues/', target='_blank'>https://github.com/bcgov/watershedBC/issues/</a></li><br>
        </ul> "),
      title = "Disclaimer",
      size = "l",
      easyClose = FALSE,
      footer = modalButton("I understand the disclaimer"),

    ),

    fluidRow(
      column(
        width = 2,
        h3("Get started"),
        HTML("1 - Click anywhere in BC to get started<br>
              2 - Click 'Run Preliminary Report'<br>
              3 - Be patient!"), br(), br(),
        shiny::radioButtons(
          inputId = "watershed_source",
          label = "Watershed data source",
          choices = c(
            "Freshwater Atlas Named Watersheds",
            "Freshwater Atlas by Stream Order",
            "Custom Basin at Point of Interst",
            "Water Survey of Canada Basins"
          ),
          selected = "Freshwater Atlas Named Watersheds"
        ),
        shiny::selectizeInput(
          inputId = "psql_zoom_to_name",
          label = "Or search watersheds by name",
          choices = c("", names$name),
          selected = "",
          multiple = F
        ),
        actionButton(inputId = "zoom_to_button", label = "Zoom to.."),
        br(),
        br(),
        # HTML("<b>Map Options</b>"),
        # checkboxInput(inputId = "active_mouse", label = "Watershed Delineation ON", value = T, ),
        shiny::checkboxGroupInput(inputId = "run_modules",
                                  label = "Include in Watershed Report",
          choices = c("Streamflow and Freshwater", "Forest Disturbance", "Stream Profile", "Water Allocations", "Climate BC", "Satellite Imagery"),
          selected = c("Streamflow and Freshwater", "Forest Disturbance"))),

      column(
        width = 10,
        leafletOutput("mymap", height = '600px') %>% withSpinner(color = "steelblue"),
        checkboxInput(inputId = "active_mouse", label = "Watershed Delineation ON", value = T, ),
        h3(textOutput(outputId = "ws_selection")),
        actionButton(inputId = "run_button", label = "Run Report"),
        textOutput(outputId = "ws_run"),
        textOutput(outputId = "ws_selection_pred_time"),
        tableOutput('table_named'),
        plotlyOutput("plot_discharge"),
        htmlOutput("text_plot_discharge"),
        plotlyOutput("plot_profile"),
        htmlOutput("text_plot_profile"),
        plotlyOutput("plot_timeseries"),
        htmlOutput("text_plot_timeseries"),
        plotlyOutput("plot_timeseries_cumsum"),
        htmlOutput("text_plot_timeseries_cumsum"),
        plotlyOutput("plot_eca"),
        htmlOutput("text_plot_eca"),
        plotlyOutput("plot_elevbins"),
        htmlOutput("text_plot_elevbins"),
        plotlyOutput("plot_fwa"),
        htmlOutput("text_plot_fwa"),
        plotlyOutput("plot_auth"),
        htmlOutput("text_plot_auth"),
        plotlyOutput("plot_gl"),
        htmlOutput("text_plot_gl"),
        plotlyOutput("plot_dra"),
        htmlOutput("text_plot_dra"),
        plotlyOutput("plot_mat"),
        htmlOutput("text_plot_mat"),
        plotlyOutput("plot_map"),
        htmlOutput("text_plot_map"),
        plotlyOutput("plot_cmd"),
        htmlOutput("text_plot_cmd"),
        plotOutput("plot_landsat_1985", width = 800, height = 800),
        plotOutput("plot_landsat_2020", width = 800, height = 800),
        plotOutput("plot_sentinel_2023", width = 800, height = 800),
        downloadButton("downloadWatershed", "Watershed"),
        downloadButton("downloadCutblocks", "Cutblocks"),
        downloadButton("downloadWildfires", "Wildfire"),
        downloadButton("downloadLakes", "FWA - Lakes"),
        downloadButton("downloadWetlands", "FWA - Wetlands"),
        downloadButton("downloadGlaciers", "FWA - Glaciers"),
        # downloadButton("downloadGlaciers85", "Glaciers 1985"),
        # downloadButton("downloadGlaciers21", "Glaciers 2021"),
        # downloadButton("downloadRoads", "Roads"),
        br(),
        br(),
        br()
      )
    ),

    fluidRow(
      column(
        width = 12,
        HTML("<b>Data sources:</b> Freshwater Atlas of BC, Consolidated Cutblocks of BC, BC Wildfire Service Fire Perimeters, Landsat, and Sentinel-2"), br(),
        HTML("<b>Known issues:</b> Data is not accurate across provincial, territorial, national borders."), br(),
        HTML("This tool is provided with <b>no guarantees of reliability or accuracy</b>, please scrutinize the results."), br(),
        HTML("Please contact <i>alexandre.bevington@gov.bc.ca</i> with any questions or comments about this tool."), br(),
        HTML("More info at: <a href='https://github.com/bcgov/watershedBC/'>https://github.com/bcgov/watershedBC/"), br())),

    fluidRow(
      column(
        width = 12,
        style = "background-color:#003366; border-top:2px solid #fcba19;",
        tags$footer(
          class = "footer",
          tags$div(
            class = "container",
            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
            tags$ul(
              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
              tags$li(a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
              tags$li(a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style ="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
              tags$li(a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style ="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
              tags$li(a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style ="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
              tags$li(a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style ="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
              tags$li(a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style ="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              )
            )
          )
        )
      )
    )
  )
)

# SERVER #######################################################################

server <- function(input, output, session) {




  hide("downloadWatershed")
  hide("downloadCutblocks")
  hide("downloadWildfires")
  hide("downloadWetlands")
  hide("downloadLakes")
  hide("downloadGlaciers")

  hide('table_named')
  hide("plot_discharge")
  hide("text_plot_discharge")
  hide("plot_profile")
  hide("text_plot_profile")
  hide("plot_timeseries")
  hide("text_plot_timeseries")
  hide("plot_timeseries_cumsum")
  hide("text_plot_timeseries_cumsum")
  hide("plot_eca")
  hide("text_plot_eca")
  hide("plot_elevbins")
  hide("text_plot_elevbins")
  hide("plot_fwa")
  hide("text_plot_fwa")
  hide("plot_auth")
  hide("text_plot_auth")
  hide("plot_gl")
  hide("text_plot_gl")
  hide("plot_dra")
  hide("text_plot_dra")
  hide("plot_mat")
  hide("text_plot_mat")
  hide("plot_map")
  hide("text_plot_map")
  hide("plot_cmd")
  hide("text_plot_cmd")
  hide("plot_landsat_1985")
  hide("plot_landsat_2020")
  hide("plot_sentinel_2023")

  # SESSION INFO

  session_start <- Sys.time()
  session_token <- session$token

  onStop(function() {
    cat("Closing Database  Connections")

  })

  # BASEMAP

  output$mymap <-
    renderLeaflet({
      initial_map %>% fitBounds(-139.01451, 47.68300, -110.48408, 59.99974)
    })

  observeEvent(input$watershed_source, {
    if (input$watershed_source == "Water Survey of Canada Basins") {
      # output$mymap <- renderLeaflet({
      wsc_pp_ac <-
        wsc_pp %>% filter(status == "active") %>% mutate(id = stationnum)
      wsc_pp_dc <- wsc_pp %>% filter(status == "discontinued")

      leafletProxy("mymap") %>%
        addCircleMarkers(
          data = wsc_pp_ac,
          lng = wsc_pp_ac$lon,
          lat = wsc_pp_ac$lat,
          color = "steelblue",
          radius = 3,
          group = "WSC Active",
          label = paste0(wsc_pp_ac$name, " - ", wsc_pp_ac$stationnum, " [active]")
        ) %>%
        addCircleMarkers(
          data = wsc_pp_dc,
          lng = wsc_pp_dc$lon,
          lat = wsc_pp_dc$lat,
          color = "grey",
          radius = 3,
          group = "WSC Discontinued",
          label = paste0(wsc_pp_dc$name, " - ", wsc_pp_dc$stationnum, " [discontinued]")) %>%
        addLayersControl(
          baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
          overlayGroups = c(
            "Sentinel 2023 (slow)",
            "Landsat 2020-2023 (slow)",
            "Landsat 1985-1990 (slow)",
            "WSC Active",
            "WSC Discontinued"
          ),
          options = layersControlOptions(collapsed = F)
        )
      # })


    } else{
      leafletProxy("mymap") %>%
        leaflet::clearGroup("WSC Active") %>%
        leaflet::clearGroup("WSC Discontinued")
    }
  })

  # CREATE REACTIVE VAL FOR WATERSHED

  new_ws <- reactiveVal()
  basin_source <- reactiveVal()

# OPTION 1: CLICK MAP TO SELECT WATERSHED ####################################

  # ACTION ON CLICK

  observeEvent(input$mymap_click, {
    # START TIMER

    tic()
    withProgress(message = 'Finding watershed...', max = 1,  {
      incProgress(1, detail = "...")

      point <-
        input$mymap_click # point <- data.frame(lat=52.2398896365648, lng=-127.468363996359)
      point_df <- data.frame(lat = point$lat, lng = point$lng)

      if (input$active_mouse == T) {
        if (input$watershed_source == "Freshwater Atlas Named Watersheds") {
          basin_source("FWA")
          print("FWA")
          # conn <- refresh()
          bas <- st_read(
            conn,
            query = paste0(
              "SELECT * FROM fwa_named
                 WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",
              point$lng,
              ",",
              point$lat,
              "), 4326),3005))
                 ORDER BY area_m2 ASC LIMIT 1"
            )
          )

          if (nrow(bas) > 0) {
            new_ws(bas %>% mutate(area_km2 = area_m2 / (1000 * 1000)))
          }
        }

        if (input$watershed_source == "Freshwater Atlas by Stream Order") {
          basin_source("FWA Order")
          print("FWA Order")
          # conn <- refresh()
          bas <-
            st_read(
              conn,
              query = paste0(
                "SELECT * FROM fwa_rollup
            WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",
                point$lng,
                ",",
                point$lat,
                "), 4326),3005))
            ORDER BY area_m2 ASC LIMIT 1"
              )
            )

          bas <- bas %>% st_cast("POLYGON", warn = F)
          bas$overl <-
            bas %>% st_intersects(
              as.data.frame(point) %>% st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% st_transform(st_crs(bas)),
              sparse = F
            )
          bas <-
            bas %>% filter(overl == T) %>% mutate(area_m2 = as.numeric(st_area(.)))

          if (nrow(bas) > 0) {
            new_ws(
              bas %>% mutate(area_km2 = area_m2 / (1000 * 1000)) %>%
                rename(gnis_name = id, gnis_id = iFWA)
            )
          }
        }

        if (input$watershed_source == "Custom Basin at Point of Interst") {
          basin_source("basinsv4")
          print("basinv4")
          # conn <- refresh()
          bas <- st_read(
            conn,
            query = paste0(
              "SELECT * FROM basinsv4
                 WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",
              point$lng,
              ",",
              point$lat,
              "), 4326),3005))
                 ORDER BY area_m2 ASC LIMIT 1"
            )
          )

          if (nrow(bas) > 0) {
            new_ws(
              bas %>%
                mutate(area_km2 = area_m2 / (1000 * 1000)) %>%
                rename(gnis_name = id, gnis_id = basin) %>%
                ms_simplify(keep = 0.5)
            )
          }
        }

        if (input$watershed_source == "Water Survey of Canada Basins") {
          basin_source("WSC")
          print("WSC")

          point_sf <-
            st_as_sf(point_df,
                     coords = c("lng", "lat"),
                     crs = 4326)

          wsc_pp_id <- wsc_pp %>%
            mutate(dist = as.numeric(st_distance(., point_sf))) %>%
            filter(dist == min(dist)) %>% pull(stationnum)

          # conn <- refresh()
          bas <- st_read(
            conn,
            query = paste0(
              "SELECT stationnum,name,area,geometry FROM wsc_drainagebasin_clean_3005
                WHERE stationnum = '",
              wsc_pp_id,
              "'"
            )
          )


          bas <- bas %>% rmapshaper::ms_simplify(keep = 0.8)

          if (nrow(bas) > 0) {
            new_ws(
              bas %>%
                mutate(
                  gnis_name = paste0(bas$stationnum, " ", bas$name),
                  gnis_id = paste0(bas$stationnum, " ", bas$name),
                  area_km2 = area / (1000 * 1000)
                ) %>%
                ms_simplify(keep = 0.5) %>% st_transform(crs = 3005)
            )
          }
        }

        if (nrow(bas) > 0) {
          output$ws_selection <-
            renderText({
              paste0(
                "You selected ",
                new_ws()$gnis_name,
                " (",
                format(round(
                  as.numeric(new_ws()$area_km2), 0
                ), big.mark = ",") ,
                " sq.km)"
              )
            })
          output$ws_selection_pred_time <-
            renderText({
              paste0("Estimated time to run report ~ ",
                     0.5 + round((new_ws()$area_km2 * 0.03) / 60, 1),
                     " min")
            })

          print("map")
          bbbb <- st_bbox(bas %>% st_transform(4326))
          # output$mymap <- renderLeaflet({

          leafletProxy("mymap") %>%
            leaflet::clearGroup("Watershed")

          # initial_map %>%
          leafletProxy("mymap") %>%
            leaflet::clearGroup("FWA Wetland") %>%
            leaflet::clearGroup("FWA Lake") %>%
            leaflet::clearGroup("FWA Glacier") %>%
            leaflet::clearGroup("Glacier 1985") %>%
            leaflet::clearGroup("Glacier 2021") %>%
            leaflet::clearGroup("Fire") %>%
            leaflet::clearGroup("Cutblock") %>%
            leaflet::clearGroup("Roads") %>%
            leaflet::clearGroup("Water Rights") %>%
            leaflet::clearGroup("Approvals") %>%
            addPolygons(
              data = bas %>% st_transform(4326),
              fillOpacity = 0,
              weight = 2,
              color = "blue",
              group = "Watershed"
            )
          #     addLayersControl(baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
          #                      overlayGroups = c("Sentinel 2023 (slow)","Landsat 2020-2023 (slow)","Landsat 1985-1990 (slow)","Watershed"), options = layersControlOptions(collapsed = T)) %>%
          #     hideGroup(c("Sentinel 2023 (slow)","Landsat 1985-1990 (slow)","Landsat 2020-2023 (slow)")) %>%
          # fitBounds(bbbb$xmin[[1]], bbbb$ymin[[1]], bbbb$xmax[[1]], bbbb$ymax[[1]])
          # })
        }


        a <- toc()$callback_msg

        if (nrow(bas) > 0) {
          output$ws_run <- renderText({
            a
          })

          # conn <- refresh()
          dbWriteTable(
            conn,
            "usage",
            data.frame(
              date_time = as.character(session_start),
              session_token = session_token,
              gnis_name = new_ws()$gnis_name,
              gnis_id = new_ws()$gnis_id,
              processing_time = a,
              action = "select watershed",
              area_km2 = round(new_ws()$area_km2, 1),
              basin_source = basin_source()
            ),
            append = TRUE
          )


        }

      }
    })
  })

  # OPTION 2: ZOOM TO NAMED WATERSHED ##########################################

  observeEvent(input$zoom_to_button, {
    if (input$psql_zoom_to_name != "") {
      tic(quiet = T)
      basin_source("FWA")
      withProgress(message = 'Finding watershed...', max = 1,  {
        split_name <-
          strsplit(strsplit(input$psql_zoom_to_name, "id:")[[1]][2], ")")[[1]][1]
        # conn <- refresh()
        n <-
          st_read(conn,
                  query = paste0("SELECT * FROM fwa_named WHERE gnis_id = ", split_name))

        new_ws(n %>% mutate(area_km2 = area_m2 / (1000 * 1000)))
        output$ws_selection <- renderText({paste0("You selected ", new_ws()$gnis_name, " (", format(round(as.numeric(new_ws()$area_km2), 0), big.mark = ",") , " sq.km)")})
        output$ws_selection_pred_time <- renderText({paste0("Estimated time to run report ~ ", 0.5 + round((new_ws()$area_km2 * 0.03) / 60, 1), " min")})

        bbbb <- st_bbox(n %>% st_transform(4326))
        output$mymap <- renderLeaflet({
          initial_map %>% addPolygons(data = n %>% st_transform(4326), fillOpacity = 0, weight = 2, color = "blue", group = "Watershed") %>%
            addLayersControl(baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
                             overlayGroups = c("Sentinel 2023 (slow)", "Landsat 2020-2023 (slow)", "Landsat 1985-1990 (slow)", "Watershed"),
                             options = layersControlOptions(collapsed = F)) %>%
            hideGroup(c("Sentinel 2023 (slow)", "Landsat 1985-1990 (slow)", "Landsat 2020-2023 (slow)")) %>%
            fitBounds(bbbb$xmin[[1]], bbbb$ymin[[1]], bbbb$xmax[[1]], bbbb$ymax[[1]])
        })
      })

      a <- toc()$callback_msg
      output$ws_run <- renderText({
        a
      })

      # conn <- refresh()
      dbWriteTable(
        conn,
        "usage",
        data.frame(
          date_time = as.character(session_start),
          session_token = session_token,
          gnis_name = new_ws()$gnis_name,
          gnis_id = new_ws()$gnis_id,
          processing_time = a,
          action = "select watershed",
          area_km2 = round(new_ws()$area_km2, 1),
          basin_source = basin_source()
        ),
        append = TRUE
      )



    }
  })

  # RUN REPORT #################################################################

  observeEvent(input$run_button, {



    hide('table_named')
    hide("plot_discharge")
    hide("text_plot_discharge")
    hide("plot_profile")
    hide("text_plot_profile")
    hide("plot_timeseries")
    hide("text_plot_timeseries")
    hide("plot_timeseries_cumsum")
    hide("text_plot_timeseries_cumsum")
    hide("plot_eca")
    hide("text_plot_eca")
    hide("plot_elevbins")
    hide("text_plot_elevbins")
    hide("plot_fwa")
    hide("text_plot_fwa")
    hide("plot_auth")
    hide("text_plot_auth")
    hide("plot_gl")
    hide("text_plot_gl")
    hide("plot_dra")
    hide("text_plot_dra")
    hide("plot_mat")
    hide("text_plot_mat")
    hide("plot_map")
    hide("text_plot_map")
    hide("plot_cmd")
    hide("text_plot_cmd")
    hide("plot_landsat_1985")
    hide("plot_landsat_2020")
    hide("plot_sentinel_2023")



    if (!is.null(new_ws())) {
      shiny::updateCheckboxInput(inputId = "active_mouse", value = F)

      new_ws2 <- new_ws()
      # new_ws2 <- bas
      # new_ws2 <- st_read(refresh(), query = "SELECT * FROM fwa_named WHERE gnis_name = 'Bowron River'") %>% mutate(area_km2 = area_m2/(1000*1000))
      # new_ws2 <- st_read(conn, query = "SELECT * FROM fwa_named WHERE gnis_name = 'McMillan Creek'") %>% mutate(area_km2 = area_m2/(1000*1000))
      # new_ws2 <- st_read(conn, query = "SELECT * FROM fwa_named WHERE gnis_name = 'Joe Smith Creek'") %>% mutate(area_km2 = area_m2/(1000*1000))
      # new_ws2 <- st_read(refresh(), query = "SELECT * FROM fwa_named WHERE gnis_id = 26413") %>% mutate(area_km2 = area_m2/(1000*1000))
      # new_ws2 <- st_read(conn, query = paste0("SELECT * FROM basinsv4 WHERE id = 874586")) %>% rename(gnis_name = basin, gnis_id = id) %>% mutate(area_km2 = area_m2/(1000*1000))

      if (new_ws2$area_km2 > 20000) {
        output$ws_run <-
          renderText({
            "Watershed is too large... please select a smaller watershed smaller than 20,000 sq. km."
          })
      }

      if (new_ws2$area_km2 < 200000) {
        tic()
        new_ws2_wkt <-
          st_as_text(st_geometry(new_ws2 %>% ms_explode()))

        withProgress(message = 'Processing...', max = 15,  {

          # NAMED WATERSHEDS ####
          incProgress(1, detail = paste0("Get watersheds (", round(new_ws2$area_km2, 0), ")"))
          print("getting watershed")
          my_named <-
            postgis_get_pol(
              "fwa_named",
              "*",
              elev = F,
              my_wkt = new_ws2_wkt,
              min_area_km2 = new_ws2$area_km2 * 0.1
            )
          if (nrow(my_named) > 0) {
            my_named = my_named %>%
              mutate(area_km2 = area_m2 / (1000 * 1000)) %>%
              dplyr::select(gnis_name, area_km2) %>%
              arrange(-area_km2) %>%
              mutate(
                Location = case_when(
                  gnis_name == new_ws2$gnis_name ~ "Watershed of Interest",
                  area_km2 > new_ws2$area_km2 ~ "Downstream",
                  area_km2 < new_ws2$area_km2 ~ "Upstream",
                  TRUE ~ ""
                )
              )

            output$table_named <- renderTable(
              my_named %>% st_drop_geometry() %>%
                mutate(area_km2 = round(area_km2, 1)) %>%
                rename(Name = gnis_name,
                       Area_km2 = area_km2)
              ,
              digits = 2
            )
          }


# FRESHWATER ATLAS ####

  if("Streamflow and Freshwater" %in% input$run_modules) {

    show("plot_fwa")
    show("plot_gl")

          incProgress(1, detail = "Get Wetlands")
          print("getting wetlands")
          my_wl <-
            postgis_get_pol("fwa_wetlands", "waterbody_type", my_wkt = new_ws2_wkt)
          if (nrow(my_wl) == 0) {
            my_wl <-
              st_as_sf(
                data.frame(
                  clipped_area_m2 = 0,
                  waterbody_type = "",
                  elevation = 0,
                  area_m2 = 0,
                  lat = 0,
                  long = 0,
                  type = "fwa_wetlands"
                ),
                coords = c("long", "lat"),
                crs = 3005
              )
          }

          incProgress(1, detail = "Get Lakes")
          print("getting lakes")
          my_lk <-
            postgis_get_pol("fwa_lakes", "waterbody_type", my_wkt = new_ws2_wkt)
          if (nrow(my_lk) == 0) {
            my_lk <-
              st_as_sf(
                data.frame(
                  clipped_area_m2 = 0,
                  waterbody_type = "",
                  elevation = 0,
                  area_m2 = 0,
                  lat = 0,
                  long = 0,
                  type = "fwa_lakes"
                ),
                coords = c("long", "lat"),
                crs = 3005
              )
          }

          incProgress(1, detail = "Get Glaciers")
          print("getting glaciers")
          my_gl <-
            postgis_get_pol("fwa_glaciers", "waterbody_type", my_wkt = new_ws2_wkt)
          if (nrow(my_gl) == 0) {
            my_gl <-
              st_as_sf(
                data.frame(
                  clipped_area_m2 = 0,
                  waterbody_type = "",
                  elevation = 0,
                  area_m2 = 0,
                  lat = 0,
                  long = 0,
                  type = "fwa_glaciers"
                ),
                coords = c("long", "lat"),
                crs = 3005
              )
          }
          my_gl_1985 <-
            bcdata::bcdc_query_geodata("historical-glaciers") %>% select(GBA_GLHIST_SYSID,
                                                                         GLACIER_ID,
                                                                         SOURCE_YEAR,
                                                                         FEATURE_AREA_SQM) %>% filter(INTERSECTS(new_ws2)) %>% collect()
          if (nrow(my_gl_1985) > 0) {
            my_gl_1985 <- my_gl_1985 %>% st_intersection(new_ws2) %>%
              mutate(FEATURE_AREA_SQM = as.numeric(st_area(.)))
          } else{
            my_gl_1985 <-
              st_as_sf(
                data.frame(
                  GLACIER_ID = 0,
                  FEATURE_AREA_SQM = 0,
                  lat = 0,
                  long = 0,
                  SOURCE_YEAR = 1985
                ),
                coords = c("long", "lat"),
                crs = 3005
              )
          }
          my_gl_2021 <-
            bcdata::bcdc_query_geodata("glaciers") %>% select(GLACIER_ID, SOURCE_YEAR, FEATURE_AREA_SQM) %>% filter(INTERSECTS(new_ws2)) %>% collect()
          if (nrow(my_gl_2021) > 0) {
            my_gl_2021 <- my_gl_2021 %>% st_intersection(new_ws2) %>%
              mutate(FEATURE_AREA_SQM = as.numeric(st_area(.)))
          } else{
            my_gl_2021 <-
              st_as_sf(
                data.frame(
                  GLACIER_ID = 0,
                  FEATURE_AREA_SQM = 0,
                  lat = 0,
                  long = 0,
                  SOURCE_YEAR = 2021
                ),
                coords = c("long", "lat"),
                crs = 3005
              )
          }

          output$plot_gl <- renderPlotly({
            ggplotly(
              bind_rows(
                my_gl_1985 %>% st_drop_geometry(),
                my_gl_2021 %>% st_drop_geometry()
              ) %>%
                pivot_wider(
                  id_cols = GLACIER_ID,
                  names_from = SOURCE_YEAR,
                  values_from = FEATURE_AREA_SQM,
                  names_prefix = "gl_"
                ) %>%
                mutate(diff = gl_2021 - gl_1985) %>%
                mutate(
                  `Glacier Area Percent Change` = round(100 * (diff / (
                    1000 * 1000
                  )) / (gl_1985 / (
                    1000 * 1000
                  )), 1),
                  `Glacier Area Change (km2)` = round(diff / (1000 *
                                                                1000), 1),
                  `Glacier Area in 1985 (km2)` = round(gl_1985 /
                                                         (1000 * 1000), 1)
                ) %>%
                ggplot() +
                theme_bw() +
                geom_point(
                  aes(
                    `Glacier Area in 1985 (km2)`,
                    `Glacier Area Percent Change`,
                    group = `Glacier Area Change (km2)`
                  )
                ) +
                labs(
                  x = "Glacier Area in 1985 (km2)",
                  y = "Glacier Area Percent \nChange from 1985-2021",
                  title = paste0(
                    "Glacier Area Change",
                    " [1985=",
                    round(sum(
                      my_gl_1985$FEATURE_AREA_SQM
                    ) / (1000 * 1000), 1),
                    "km2, 2021=",
                    round(sum(
                      my_gl_2021$FEATURE_AREA_SQM
                    ) / (1000 * 1000), 1),
                    "km2]"
                  )
                ),
              dynamicTicks = T,
              width = 800,
              tooltip = c("GLACIER_ID", "x", "Glacier Area Change (km2)", "y")
            )
          })


          print("merge fwa")
          my_fwa <-
            bind_rows(
              data.frame(
                waterbody_type  = c("W", "L", "G"),
                area_m2 = c(0, 0, 0)
              ),
              my_wl %>% st_drop_geometry(),
              my_lk %>% st_drop_geometry(),
              my_gl %>% st_drop_geometry()
            ) %>%
            st_drop_geometry() %>%
            mutate(
              type = case_when(
                type == "fwa_wetlands" ~ "Wetland",
                type == "fwa_glaciers" ~ "Glacier",
                type == "fwa_lakes" ~ "Lake"
              )
            )

          print("plot fwa")
          output$plot_fwa <- renderPlotly({
            ggplotly(
              my_fwa %>%
                group_by(type) %>%
                dplyr::summarize(area_km2 = sum(clipped_area_m2, na.rm = T) /
                                   (1000 * 1000)) %>%
                filter(!is.na(type)) %>%
                ggplot() +
                geom_col(aes(type, area_km2, fill = type), color = "black") +
                theme_bw() +
                labs(
                  x = "",
                  y = "Area sq.km",
                  title = "Freshwater Atlas"
                ) +
                scale_fill_manual(values = c(
                  "grey", "steelblue", "yellow"
                )) +
                scale_y_continuous(n.breaks = 10),
              dynamicTicks = T,
              width = 800
            )
          })
  }else{
    hide("plot_fwa")
    hide("plot_gl")
  }

# FOREST DISTURBANCE ####

  if("Forest Disturbance" %in% input$run_modules) {

    show("plot_dra")
    show("text_plot_dra")
    show("plot_eca")
    show("text_plot_eca")
    show("plot_timeseries")
    show("text_plot_timeseries")
    show("plot_timeseries_cumsum")
    show("text_plot_timeseries_cumsum")
    show("plot_elevbins")
    show("text_plot_elevbins")

    # ROADS ####

    incProgress(1, detail = "Get Roads")
    print("getting dra")

    dra <- postgis_get_line(to_clip = "dra", my_wkt = new_ws2_wkt)
    if (nrow(dra) > 0) {
      dra <- dra  %>% st_make_valid() %>% mutate(length_km = as.numeric(st_length(.)) / 1000) %>% mutate(type = "dra", group = transport_line_surface_code_desc)
        }else{dra <- st_as_sf(data.frame(lon = -111, lat = 55, length_km = 0), coords = c("lon", "lat"), crs = 4326) %>% mutate(type = "test", group = "test") %>% filter(type != "test")}

    output$plot_dra <- renderPlotly({
            ggplotly(
              dra %>% st_drop_geometry() %>%
                select(group, length_km) %>%
                group_by(group) %>%
                dplyr::summarize(length_km = sum(length_km)) %>%
                ggplot() +
                geom_col(aes(group, length_km))    +
                theme_bw() +
                labs(
                  x = "",
                  y = "Length km",
                  title = "Total Road Length",
                  fill = ""
                ) +
                scale_y_continuous(n.breaks = 10)
              ,
              dynamicTicks = T,
              width = 800
            )
          })

          output$text_plot_dra <- renderText({
            "<br><b>Total Road Length</b> is the total distance of roads in the <a href='https://www2.gov.bc.ca/gov/content/data/geographic-data-services/topographic-data/roads'>
          Digital Road Atlas</a> of BC.<br><br><br>"
          })

          # FORESTS ####
          incProgress(1, detail = "Get Wildfires")
          print("getting fires")
          my_wf <-
            postgis_get_pol("fire", "fire_year", my_wkt = new_ws2_wkt)

          incProgress(1, detail = "Get Cutblocks")
          print("getting blocks")
          my_cb <-
            postgis_get_pol("cutblocks", "harvest_year", my_wkt = new_ws2_wkt)

          if (nrow(my_wf) == 0) {
            my_wf <-
              st_as_sf(
                data.frame(
                  clipped_area_m2 = 0,
                  fire_year = 1900,
                  elevation = 0,
                  area_m2 = 0,
                  lat = 0,
                  long = 0,
                  type = "fire"
                ),
                coords = c("long", "lat"),
                crs = 3005
              )
          }
          if (nrow(my_cb) == 0) {
            my_cb <-
              st_as_sf(
                data.frame(
                  clipped_area_m2 = 0,
                  harvest_year = 1900,
                  elevation = 0,
                  area_m2 = 0,
                  lat = 0,
                  long = 0,
                  type = "cutblock"
                ),
                coords = c("long", "lat"),
                crs = 3005
              )
          }

          eca <-
            bind_rows(
              my_wf %>% mutate(type = "WILDFIRE") %>% rename(year = fire_year) %>% st_drop_geometry(),
              my_cb %>% mutate(type = "CUTBLOCK") %>% rename(year = harvest_year) %>% st_drop_geometry()
            ) %>%
            as_tibble() %>%
            mutate(
              ws_area = as.numeric(st_area(new_ws2[1, ])) / (1000 * 1000),
              eca = case_when(type == "WILDFIRE" ~ 100,
                              type == "CUTBLOCK" ~ 100)
            )

          e <-
            do.call(bind_rows, lapply(1:nrow(eca), function(i = 1) {
              p <- eca[i, ]
              if (p$type == "WILDFIRE") {
                slope = 100 / 40
              }
              if (p$type == "CUTBLOCK") {
                slope = 100 / 40
              }
              years <-
                (p$year + 1):(lubridate::year(lubridate::now()))
              bind_rows(
                p,
                data.frame(year = years) %>% mutate(
                  type = p$type,
                  eca = p$eca -
                    ((year - p$year) * slope),
                  area = p$clipped_area_m2 /
                    (1000 * 1000),
                  ws_area = p$ws_area
                )
              ) %>%
                mutate(eca = case_when(eca < 0 ~ 0, TRUE ~ eca))
            }))

          output$plot_eca <- renderPlotly({
            ggplotly(
              e %>%
                mutate(
                  eca_area = (eca / 100) * area,
                  eca_area_ws_perc = 100 * (eca_area / ws_area)
                ) %>%
                group_by(type, year) %>%
                summarize(eca = sum(eca_area_ws_perc, na.rm = T)) %>%
                mutate(eca = signif(eca, 3)) %>%
                ggplot() +
                geom_col(aes(year, eca, fill = type)) +
                theme_bw() +
                scale_y_continuous(n.breaks = 10) +
                scale_x_continuous(n.breaks = 10) +
                labs(
                  x = "Year",
                  y = "ECA (%)",
                  title = "Simplified ECA"
                ) +
                scale_fill_manual(values = c("darkgreen", "orange")),
              dynamicTicks = T,
              width = 800
            )
          })

          output$text_plot_eca <- renderText({
            "<br><b> Equivalent Clearcut Area (ECA)</b> is a basin-wide statistic that is the sum of the total area of all forest disturbance
          with a reduction factor that account for forest regeneration. ECA is expressed as a percentage of forest disturbance relative to
          the total forested area of the watershed. The <b>Simplified ECA</b> is a quick calculation that assumes that all wildfires and cutblocks take 40 years to recover. The
          Simplified ECA does not account for non-forested areas (e.g. alpine, lakes, ..), permanent forest disturbances (e.g. roads, pipelines, ..),
          forest health factors (e.g. mountain pine beetle, etc.), or variable recovery rates based on site specific factors. This Simplified ECA
          is not the same as ECA, and should not be misinterpreted as such. Read more about ECA: Winkler and Boone 2017 <a href='https://www.for.gov.bc.ca/hfd/pubs/Docs/En/En118.htm'>
          Equivalent Clearcut Area as an Indicator of Hydrologic Change in Snow-dominated Watersheds of Southern British Columbia</a><br><br><br>"
          })

          output$plot_timeseries <- renderPlotly({
            ggplotly(
              bind_rows(
                my_wf %>% st_drop_geometry() %>% rename(year = fire_year) %>% dplyr::select(clipped_area_m2, year) %>% mutate(type = "wildfire"),
                my_cb %>% st_drop_geometry() %>% rename(year = harvest_year) %>% dplyr::select(clipped_area_m2, year) %>% mutate(type = "cutblock"),
                data.frame(
                  year = seq(1900, lubridate::year(Sys.Date()), 1),
                  clipped_area_m2 = 0,
                  type = "cutblock"
                ),
                data.frame(
                  year = seq(1900, lubridate::year(Sys.Date()), 1),
                  clipped_area_m2 = 0,
                  type = "wildfire"
                )
              ) %>%
                group_by(type, year) %>%
                dplyr::summarize(area_km2 = sum(clipped_area_m2) / (1000 * 1000)) %>%
                ggplot() +
                geom_col(aes(year, area_km2, fill = type)) +
                theme_bw() +
                labs(
                  x = "",
                  y = "Area sq.km",
                  title = "Forest Disturbance History",
                  fill = ""
                ) +
                scale_fill_manual(values = c("darkgreen", "orange")) +
                scale_y_continuous(n.breaks = 10) +
                scale_x_continuous(n.breaks = 10),
              dynamicTicks = T,
              width = 800
            )
          })

          output$text_plot_timeseries <- renderText({
            "<br>The <b>Forest Disturbance History</b> is the total forest disturbance by year. Data is from the <a href='https://catalogue.data.gov.bc.ca/dataset/harvested-areas-of-bc-consolidated-cutblocks-'>
          Consolidated Cutblocks</a> and <a href ='https://catalogue.data.gov.bc.ca/dataset/fire-perimeters-historical'>Wildfire Perimeter</a>. These datasets are frequently
          updated. Some limitations of this data include: cutblocks do not include forest removal on private land, cutblocks are incosistently mapped before 1950, older wildfire polygons
          are generally poorly mapped, and wildfire polygons generally do not account for unburned timber inside of the polygon.<br><br><br>"
          })

          output$plot_timeseries_cumsum <- renderPlotly({
            a <- bind_rows(
              my_wf %>% st_drop_geometry() %>% rename(year = fire_year) %>% dplyr::select(clipped_area_m2, year) %>% mutate(type = "wildfire"),
              my_cb %>% st_drop_geometry() %>% rename(year = harvest_year) %>% dplyr::select(clipped_area_m2, year) %>% mutate(type = "cutblock")
            )
            ggplotly(
              a %>%
                filter(!is.na(year)) %>%
                bind_rows(
                  data.frame(
                    year = seq(1900, lubridate::year(Sys.Date()), 1),
                    clipped_area_m2 = 0,
                    type = "cutblock"
                  ),
                  data.frame(
                    year = seq(1900, lubridate::year(Sys.Date()), 1),
                    clipped_area_m2 = 0,
                    type = "wildfire"
                  )
                ) %>%
                group_by(type, year) %>%
                dplyr::summarize(area_km2 = sum(clipped_area_m2) / (1000 * 1000)) %>%
                arrange(year) %>%
                mutate(cumsum = cumsum(area_km2)) %>%
                ggplot() +
                geom_col(aes(
                  year, 100 * (cumsum / new_ws2$area_km2), fill = type
                )) +
                theme_bw() +
                labs(
                  x = "",
                  y = "Percent of Watershed (%)",
                  title = "Cumulative Forest Disturbance History",
                  fill = ""
                ) +
                scale_fill_manual(values = c("darkgreen", "orange")) +
                scale_y_continuous(n.breaks = 10) +
                scale_x_continuous(n.breaks = 10),
              dynamicTicks = T,
              width = 800
            )
          })

          output$text_plot_timeseries_cumsum <- renderText({
            "<br>The <b>Cumulative Forest Disturbance History</b> is the total forest disturbance by year, accumulated over time. As such, it is possible to have a cumulative forest disturbance
          that is greater than the total area of the watershed (i.e. > 100%). of Data is from the <a href='https://catalogue.data.gov.bc.ca/dataset/harvested-areas-of-bc-consolidated-cutblocks-'>
          Consolidated Cutblocks</a> and <a href ='https://catalogue.data.gov.bc.ca/dataset/fire-perimeters-historical'>Wildfire Perimeter</a>. These datasets are frequently
          updated. Some limitations of this data include: cutblocks do not include forest removal on private land, cutblocks are incosistently mapped before 1950, older wildfire polygons
          are generally poorly mapped, and wildfire polygons generally do not account for unburned timber inside of the polygon.<br><br><br>"
          })


          # LAND COVER BY ELEVATION ####
          incProgress(1, detail = paste0("Getting Polygon Elevation"))
          output$plot_elevbins <- renderPlotly({
            plotly::ggplotly(
              bind_rows(
                my_wl %>% st_drop_geometry() %>% dplyr::select(type, clipped_area_m2, elevation),
                my_lk %>% st_drop_geometry() %>% dplyr::select(type, clipped_area_m2, elevation),
                my_gl %>% st_drop_geometry() %>% dplyr::select(type, clipped_area_m2, elevation),
                my_wf %>% st_drop_geometry() %>% dplyr::select(type, clipped_area_m2, elevation),
                my_cb %>% st_drop_geometry() %>% dplyr::select(type, clipped_area_m2, elevation)
              ) %>%
                as_tibble() %>%
                mutate(bin = cut(
                  elevation,
                  breaks = seq(0, 5000, 100),
                  labels = seq(0, 4900, 100)
                )) %>%
                group_by(type, bin) %>%
                dplyr::summarize(area_km2 = sum(
                  clipped_area_m2 / (1000 * 1000)
                )) %>%
                mutate(bin = as.numeric(as.character(bin))) %>%
                mutate(bin = case_when(
                  is.na(bin) ~ median(.$bin, na.rm = T),
                  TRUE ~ bin
                )) %>%
                ggplot() + geom_col(aes(bin, area_km2, fill = type)) +
                labs(
                  x = "Centroid elevation",
                  y = "Area sq.km",
                  title = "Centroid Elevation"
                ) +
                theme_bw() +
                coord_flip() +
                scale_fill_manual(
                  values = c(
                    "darkgreen",
                    "orange",
                    "grey",
                    "steelblue",
                    "yellow"
                  )
                ),
              dynamicTicks = T,
              width = 800
            )
          })

          output$text_plot_elevbins <- renderText({
            "<br>The <b>Centroid Elevation</b> is the elevation of the centroid of each polygon by type. The elevation source is the SRTM digital elevation model (circa 2000).
          This only shows the centroid elevation and is not an accurate depiction of the rnage of elevations present in polygons over complex terrain. <br><br><br>"
          })

  }else{
    hide("plot_dra")
    hide("text_plot_dra")
    hide("plot_eca")
    hide("text_plot_eca")
    hide("plot_timeseries")
    hide("text_plot_timeseries")
    hide("plot_timeseries_cumsum")
    hide("text_plot_timeseries_cumsum")
    hide("plot_elevbins")
    hide("text_plot_elevbins")
  }


# STREAM PROFILE ####

  if("Stream Profile" %in% input$run_modules) {

    show("plot_profile")
    show("text_plot_profile")

    incProgress(1, detail = paste0("Getting Stream Profile"))
    print("getting profile")

    # GET NETWORK
    if (new_ws2$area_km2 < 1000){SO=1} else{SO=2}

    my_stream_network <-
      bcdc_query_geodata("freshwater-atlas-stream-network") %>%
      filter(INTERSECTS(new_ws2)) %>%
      filter(STREAM_ORDER >= SO) %>%
      filter(!is.na(GNIS_NAME)) %>%
      collect()

    if(nrow(my_stream_network) == 0) {
      my_stream_network <-
        bcdc_query_geodata("freshwater-atlas-stream-network") %>%
        filter(INTERSECTS(new_ws2)) %>%
        filter(STREAM_ORDER >= SO) %>%
        collect() %>%
        mutate(GNIS_NAME = new_ws2$gnis_name)}

    my_stream_network_l <- my_stream_network %>%
      st_intersection(new_ws2)

    my_stream_network_l <- my_stream_network_l %>%
      group_by(GNIS_NAME) %>% summarize() %>%
      mutate(length_km = round(as.numeric(st_length(.) / (1000)), 1)) %>% st_drop_geometry()

    if (nrow(my_stream_network_l) > 0) {

      # CAST TO XYZ POINTS
      my_stream_network_pt <- my_stream_network %>%
        st_cast(to = "POINT") %>%
        dplyr::select(geometry, BLUE_LINE_KEY, GNIS_NAME) %>%
        mutate(coords = st_coordinates(.)) %>%
        mutate(X = coords[, "X"],
               Y = coords[, "Y"],
               Z = coords[, "Z"]) %>%
        dplyr::select(-coords) %>%
        arrange(Z)

      my_stream_network_pt <- my_stream_network_pt %>%
        st_intersection(new_ws2)

      my_stream_network_pt <- my_stream_network_pt[seq(1, nrow(my_stream_network_pt), 10), ]

  # TOP 5, and all named
            keys <- my_stream_network %>%
              st_drop_geometry() %>%
              filter(!is.na(GNIS_NAME)) %>%
              group_by(BLUE_LINE_KEY, GNIS_NAME) %>%
              dplyr::summarize(n = n()) %>%
              arrange(-n) %>%
              ungroup() %>%
              filter(row_number() <= 5) %>%
              dplyr::select(BLUE_LINE_KEY) %>%
              pull()

            # MAX MAG
            max_magnitude <- my_stream_network %>%
              st_drop_geometry() %>%
              filter(!is.na(GNIS_NAME)) %>%
              filter(BLUE_LINE_KEY %in% keys) %>%
              group_by(BLUE_LINE_KEY, GNIS_NAME) %>%
              dplyr::summarize(STREAM_MAGNITUDE_MAX = max(STREAM_MAGNITUDE)) %>%
              arrange(-STREAM_MAGNITUDE_MAX)

            my_stream_network_main_pt <-
              my_stream_network_pt %>%
              filter(BLUE_LINE_KEY == max_magnitude[1, ]$BLUE_LINE_KEY) %>%
              mutate(
                dist_seg_m = replace_na(as.numeric(
                  st_distance(geometry, lag(geometry), by_element = TRUE)
                ), 0),
                dist_tot_m = cumsum(dist_seg_m)
              )

            if (nrow(max_magnitude) > 1) {
              my_stream_network_tribs = do.call(bind_rows,
                                                lapply(2:nrow(max_magnitude), function(i = 2) {
                                                  # GET TRIB AND CALCULATE DISTANCE
                                                  my_stream_network_trib_pt <-
                                                    my_stream_network_pt %>%
                                                    filter(BLUE_LINE_KEY == max_magnitude[i, ]$BLUE_LINE_KEY) %>%
                                                    mutate(
                                                      dist_seg_m = replace_na(as.numeric(
                                                        st_distance(geometry, lag(geometry), by_element = TRUE)
                                                      ), 0),
                                                      dist_tot_m = cumsum(dist_seg_m)
                                                    )

                                                  # BUFFER LOWEST POINT
                                                  my_stream_network_trib_pt_low_buf <-
                                                    my_stream_network_trib_pt %>%
                                                    filter(dist_tot_m == 0) %>%
                                                    st_buffer(2000)

                                                  # INTERSECT BUFFER WITH MAIN
                                                  candidates <-
                                                    my_stream_network_main_pt %>%
                                                    st_intersection(my_stream_network_trib_pt_low_buf %>% dplyr::select(geometry))

                                                  # FIND CLOSEST
                                                  candidates <-
                                                    candidates %>%
                                                    mutate(distt = as.numeric(
                                                      st_distance(
                                                        candidates,
                                                        my_stream_network_trib_pt %>% filter(dist_tot_m == 0)
                                                      )
                                                    )) %>%
                                                    arrange(distt) %>%
                                                    filter(distt == min(.$distt)) %>%
                                                    filter(dist_seg_m == min(.$dist_seg_m))

                                                  candidates <-
                                                    candidates %>%
                                                    filter(
                                                      BLUE_LINE_KEY == filter(
                                                        max_magnitude,
                                                        BLUE_LINE_KEY %in% candidates$BLUE_LINE_KEY
                                                      )[1, "BLUE_LINE_KEY"] %>% pull()
                                                    )

                                                  offset = candidates[1, ]$dist_tot_m

                                                  my_stream_network_trib_pt <-
                                                    my_stream_network_trib_pt %>%
                                                    mutate(dist_tot_m = dist_tot_m + offset)

                                                  my_stream_network_trib_pt
                                                }))

              my_stream_network_main_pt <-
                bind_rows(my_stream_network_main_pt,
                          my_stream_network_tribs)

            }


            my_stream_network_main_pt <-
              my_stream_network_main_pt %>%
              full_join(max_magnitude) %>%
              mutate(name = paste0(GNIS_NAME))#," \n(",BLUE_LINE_KEY,")"))

            output$plot_profile <- renderPlotly({
              t <- my_stream_network_main_pt %>%
                left_join(my_stream_network_l %>% st_drop_geometry()) %>%
                mutate(label = paste0(GNIS_NAME, " (", round(length_km, 1), " km)"))

              t$label  <- with(t, reorder(label,-length_km))

              ggplotly(
                ggplot(t) +
                  geom_line(aes(dist_tot_m / 1000, Z, color = label)) +
                  theme_bw() +
                  labs(
                    x = "Distance along stream km",
                    y = "Elevation m a.s.l.",
                    color = "Name",
                    title = "Stream Profile"
                  ) ,
                dynamicTicks = T,
                width = 800
              )
            })
          }

          output$text_plot_profile <- renderText({
            "<br>The <b>Stream Profile</b> shows the elevation profile of the stream. The stream network is taken from the <a href='https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-stream-network'>
          Freshwater Atlas</a>. The points that make up the stream network have elevation data built in. <br><br><br>"
          })

  }else{
    hide("plot_profile")
    hide("text_plot_profile")
  }

# climate ####

  if ("Climate" %in% input$run_modules) {

    show("plot_mad")
    show("text_plot_mad")
    show("plot_map")
    show("text_plot_map")
    show("plot_cmd")
    show("text_plot_cmd")

    incProgress(1, detail = "Get climateBC")

    climateBC <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/climateBC.tif", win = terra::ext(new_ws2 %>% st_transform(4326)))
    climateBC_pts <- terra::spatSample(vect(new_ws2 %>% st_transform(4326)), method = "regular", size = 1000)
    climateBC_pts <- terra::extract(climateBC, climateBC_pts)
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
                TRUE ~ "Error"),
              parameter = case_when(
                str_detect(name, "_MAP_") ~ "Mean Annual Precipitation",
                str_detect(name, "MAT") ~ "Mean Annual Temperature",
                str_detect(name, "CMD") ~ "Cumulative Moisture Deficit",
                TRUE ~ "Error"),
              model = case_when(
                str_detect(name, "Normal_") ~ "Historic",
                str_detect(name, "13GCMs_") ~ "13_GCMs",
                str_detect(name, "8GCMs_") ~ "8_GCMs",
                TRUE ~ "Error"),
              ssp = case_when(
                str_detect(name, "_ssp126_") ~ "SSP126",
                str_detect(name, "_ssp245_") ~ "SSP245",
                str_detect(name, "_ssp370_") ~ "SSP370",
                str_detect(name, "_ssp585_") ~ "SSP585",
                TRUE ~ "Historic")) %>%
            mutate(
              year = case_when(
                period == "1961-1990" ~ 1975,
                period == "1971-2000" ~ 1985,
                period == "1981-2010" ~ 1995,
                period == "1991-2020" ~ 2005,
                period == "2011-2040" ~ 2025,
                period == "2041-2070" ~ 2055,
                period == "2071-2100" ~ 2085,
                TRUE ~ 0))

          output$plot_mat <- renderPlotly({
            ggplotly(
              climateBC_pts_dat %>%
                filter(parameter == "Mean Annual Temperature") %>%
                filter(model %in% c("Historic", "13_GCMs")) %>%
                group_by(period, ssp, year) %>%
                dplyr::summarize(
                  min = quantile(value, probs = 0.25, na.rm = T),
                  mean = mean(value, na.rm = T),
                  max = quantile(value, probs = 0.75, na.rm = T)
                ) %>%
                ggplot() +
                geom_rect(
                  aes(
                    xmin = year - 14,
                    xmax = year + 15,
                    ymin = min,
                    ymax = max,
                    color = ssp,
                    fill = ssp,
                    group = period
                  ),
                  alpha = 0.2
                ) +
                labs(x = "Climate Normal Period", y = "Mean Annual Temperature") +
                theme_bw()
              ,
              dynamicTicks = T,
              width = 800
            )
          })

          output$text_plot_mat <- renderText({
            "<br>The <b>Mean Annual Temperature</b> shows the range of values from past climate normals and shows future projections under different scenarios. The
          data source is <a href='https://climatebc.ca/'>ClimateBC</a>. <br><br><br>"
          })

          output$plot_map <- renderPlotly({
            ggplotly(
              climateBC_pts_dat %>%
                filter(parameter == "Mean Annual Precipitation") %>%
                filter(model %in% c("Historic", "13_GCMs")) %>%
                group_by(period, ssp, year) %>%
                dplyr::summarize(
                  min = quantile(value, probs = 0.25, na.rm = T),
                  mean = mean(value, na.rm = T),
                  max = quantile(value, probs = 0.75, na.rm = T)
                ) %>%
                ggplot() +
                geom_rect(
                  aes(
                    xmin = year - 14,
                    xmax = year + 15,
                    ymin = min,
                    ymax = max,
                    color = ssp,
                    fill = ssp,
                    group = period
                  ),
                  alpha = 0.2
                ) +
                labs(x = "Climate Normal Period", y = "Mean Annual Precipitation") +
                theme_bw(),
              dynamicTicks = T,
              width = 800
            )
          })
          output$text_plot_map <- renderText({
            "<br>The <b>Mean Annual Precipitation</b> shows the range of values from past climate normals and shows future projections under different scenarios. The
          data source is <a href='https://climatebc.ca/'>ClimateBC</a>. <br><br><br>"
          })

          output$plot_cmd <-  renderPlotly({
            ggplotly(
              climateBC_pts_dat %>%
                filter(parameter == "Cumulative Moisture Deficit") %>%
                filter(model %in% c("Historic", "13_GCMs")) %>%
                group_by(period, ssp, year) %>%
                dplyr::summarize(
                  min = quantile(value, probs = 0.25, na.rm = T),
                  mean = mean(value, na.rm = T),
                  max = quantile(value, probs = 0.75, na.rm = T)
                ) %>%
                ggplot() +
                geom_rect(
                  aes(
                    xmin = year - 14,
                    xmax = year + 15,
                    ymin = min,
                    ymax = max,
                    color = ssp,
                    fill = ssp,
                    group = period
                  ),
                  alpha = 0.2
                ) +
                labs(x = "Climate Normal Period", y = "Cumulative Moisture Deficit") +
                theme_bw(),
              dynamicTicks = T,
              width = 800
            )
          })

          output$text_plot_cmd <- renderText({
            "<br>The <b>Cumulative Moisture Deficit</b> shows the range of values from past climate normals and shows future projections under different scenarios. The
          data source is <a href='https://climatebc.ca/'>ClimateBC</a>. <br><br><br>"
          })
  }else{
    hide("plot_mad")
    hide("text_plot_mad")
    hide("plot_map")
    hide("text_plot_map")
    hide("plot_cmd")
    hide("text_plot_cmd")

  }

# DISCHAGE ####

  if ("Streamflow and Freshwater" %in% input$run_modules) {

    show("plot_discharge")
    show("text_plot_discharge")

    incProgress(1, detail = "Estimating discharge...")
    print("getting discharge")

    output$plot_discharge <- renderPlotly({ wsc_estimate_cluster(w = new_ws2, wetlands = my_wl, glaciers = my_gl, lakes = my_lk)})
    output$text_plot_discharge <- renderText({"<br>The <b>Estimated Discharge</b> is a first working prototype that has <b>NOT BEEN VALIDATED</b>. Please do not use this information for decision making at this time. The model classifies existing Water Survey of Canada stations into 21 groups based on the similarity of their annual flow distributions, then a linear Basin Area / LTMAD relationship is built for each group. Then a second model uses basin statistics (ClimateBC, Terrain and Freshwater Atlas) to determin which group an ungauged basin should belong to, and scales the flow distribution to the size of the basin. This dataset has <b>KNOWN PROBLEMS</b> that we are actively fixing. <br><br><br>"})

    }else{
      hide("plot_discharge")
      hide("textplot_discharge")}




# WATER ALLOCATIONS ####

  if ("Water Allocations" %in% input$run_modules) {

    show("plot_auth")
    show("text_plot_auth")

    incProgress(1, detail = "Getting water allocations...")
    print("getting water alloc wap")

    # WATER WORKS
    wap <- bcdc_query_geodata("water-approval-points") %>%
            filter(INTERSECTS(new_ws2)) %>%
            select(
              WATER_APPROVAL_ID,
              APPROVAL_TYPE,
              WORKS_DESCRIPTION,
              QUANTITY,
              QUANTITY_UNITS,
              QTY_DIVERSION_MAX_RATE,
              QTY_UNITS_DIVERSION_MAX_RATE,
              APPROVAL_STATUS,
              APPROVAL_START_DATE,
              APPROVAL_EXPIRY_DATE
            ) %>% collect()

          if (nrow(wap) > 0) {
            wap <- wap %>%
              filter(APPROVAL_STATUS %in% c("Current")) %>%
              st_transform(st_crs(new_ws2)) %>%
              st_intersection(new_ws2) %>%
              st_transform(4326) %>% mutate(lon = st_coordinates(.)[, 1],
                                            lat = st_coordinates(.)[, 2]) %>% st_drop_geometry()
          }
          if (nrow(wap) == 0) {
            wap <- data.frame(lon = -111, lat = 55) %>% mutate(
              type = "test",
              QUANTITY = "",
              QUANTITY_UNITS = "",
              APPROVAL_STATUS = "",
              APPROVAL_TYPE = ""
            ) %>% filter(type != "test")
          }

          # WATER QUANTITY
          print("getting water alloc wrl")
          wrl <-
            bcdc_query_geodata("water-rights-licences-public") %>%
            filter(INTERSECTS(new_ws2)) %>% select(
              POD_NUMBER,
              POD_SUBTYPE,
              POD_DIVERSION_TYPE,
              POD_STATUS,
              LICENCE_NUMBER,
              LICENCE_STATUS,
              LICENCE_STATUS_DATE,
              PRIORITY_DATE,
              PURPOSE_USE_CODE,
              PURPOSE_USE,
              QUANTITY,
              QUANTITY_UNITS,
              QUANTITY_FLAG,
              QUANTITY_FLAG_DESCRIPTION,
              QTY_DIVERSION_MAX_RATE,
              QTY_UNITS_DIVERSION_MAX_RATE,
              PRIMARY_LICENSEE_NAME
            ) %>% collect()
          if (nrow(wrl) > 0) {
            wrl <- wrl %>%
              st_transform(st_crs(new_ws2)) %>%
              filter(LICENCE_STATUS %in% "Current") %>%
              st_intersection(new_ws2) %>%
              st_transform(4326) %>% mutate(lon = st_coordinates(.)[, 1],
                                            lat = st_coordinates(.)[, 2]) %>% st_drop_geometry()
          }
          if (nrow(wrl) == 0) {
            wrl <- data.frame(lon = -111, lat = 55) %>% mutate(
              type = "test",
              QUANTITY = "",
              QUANTITY_UNITS = "",
              LICENCE_STATUS = "",
              POD_SUBTYPE = ""
            ) %>% filter(type != "test")
          }

          water_auth <-
            bind_rows(
              wap %>% mutate(
                vol_units = str_split_fixed(QUANTITY_UNITS, "/", 2)[, 1],
                time_units = str_split_fixed(QUANTITY_UNITS, "/", 2)[, 2],
                total_vol_year = case_when(
                  sub(" ", "", time_units) == "sec" ~ as.numeric(QUANTITY) * 60 * 60 * 24 * 365,
                  sub(" ", "", time_units) == "day" ~ as.numeric(QUANTITY) * 365,
                  sub(" ", "", time_units) == "year" ~ as.numeric(QUANTITY),
                  TRUE ~ 0
                )
              ) %>%
                group_by(TYPE = APPROVAL_TYPE, STATUS = APPROVAL_STATUS) %>%
                st_drop_geometry() %>%
                summarise(SUM = sum(total_vol_year, na.rm = T)),
              wrl %>% mutate(
                vol_units = str_split_fixed(QUANTITY_UNITS, "/", 2)[, 1],
                time_units = str_split_fixed(QUANTITY_UNITS, "/", 2)[, 2],
                total_vol_year = case_when(
                  sub(" ", "", time_units) == "sec" ~ as.numeric(QUANTITY) * 60 * 60 * 24 * 365,
                  sub(" ", "", time_units) == "day" ~ as.numeric(QUANTITY) * 365,
                  sub(" ", "", time_units) == "year" ~ as.numeric(QUANTITY),
                  TRUE ~ 0
                )
              ) %>%
                group_by(TYPE = POD_SUBTYPE, STATUS = LICENCE_STATUS) %>%
                st_drop_geometry() %>%
                summarise(SUM = sum(total_vol_year, na.rm = T))
            )

          output$plot_auth <-  renderPlotly({
            ggplotly(
              water_auth %>% ggplot() +
                geom_col(aes(TYPE, SUM)) +
                scale_y_continuous(
                  labels = function(x)
                    format(x, big.mark = ",", scientific = F)
                ) +
                labs(y = "Total Annual Water Allocation (cubic metres)", title = "Water Allocations") +
                theme_bw(),
              dynamicTicks = T,
              width = 800
            )
          })

          output$text_plot_auth <- renderText({
            "<br>The <b>Water Allocations</b> shows the total annual water that has been allocated in the water-approval-points, and water-rights layers.
              licenses have limited information on the timing of withdrawls. As such we assume water licenses are used to their full extent.
              Allocation quantites in m^3/s are annualized."
          })
  }else{
    hide("plot_auth")
    hide("text_plot_auth")}

# SATELLITE IMAGERY ####

  if ("Satellite Imagery" %in% input$run_modules) {

    show("plot_landsat_1985")
    show("plot_landsat_2020")
    show("plot_sentinel_2023")

    incProgress(1, detail = "Update Satellite Imagery")

    v <- vect(new_ws2 %>% st_transform(4326))

    output$plot_landsat_1985 <- renderPlot({
      r1985 <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/1985_1990v3_COG_AV_JP_BIG.tif", win = terra::ext(new_ws2 %>% st_transform(4326)))
            plot(r1985, main = "Landsat 1985-1990", mar = 2)
            plot(v, add = T, border = "red", lwd = 2)}, height = 800, width = 800)

    output$plot_landsat_2020 <- renderPlot({
      r2020 <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/2020_2023v3_COG_AV_JP_BIG.tif", win = terra::ext(new_ws2 %>% st_transform(4326)))
            plot(r2020, main = "Landsat 2020-2023", mar = 2)
            plot(v, add = T, border = "red", lwd = 2)}, height = 800, width = 800)

    output$plot_sentinel_2023 <- renderPlot({
      r2023 <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/BC_2023v2_4326_v2_bigTiff_JPEG.tif", win = terra::ext(new_ws2 %>% st_transform(4326)))
            plot(r2023, main = "Sentinel Mosaic 2023", mar = 2)
            plot(v, add = T, border = "red", lwd = 2)}, height = 800, width = 800)

    }else{
      hide("plot_landsat_1985")
      hide("plot_landsat_2020")
      hide("plot_sentinel_2023")}

# UPDATE LEAFLET ####

    incProgress(1, detail = "Update map")
    print("map")
    bbbb <- st_bbox(new_ws2 %>% st_transform(4326))


    new_leaflet <- initial_map
    if("Forest Disturbance" %in% input$run_modules){
      new_leaflet <- new_leaflet %>%
        addPolylines(data = dra %>% st_transform(4326), group = "Roads", fillColor = "black", color = "black", weight = 1, fillOpacity = 1, label = dra$transport_line_surface_code_desc) %>%
        addPolygons(data = my_wf %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "Fire", fillColor = "red", color = "red", weight = 2, opacity = 1, fillOpacity = 0.3, label = paste0("Fire year:", my_wf$fire_year)) %>%
        addPolygons(data = my_cb %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "Cutblock", fillColor = "darkgreen", color = "darkgreen", weight = 1, fillOpacity = 0.3, label = paste("Harvest year:", my_cb$harvest_year))}

    if("Water Allocations" %in% input$run_modules){
      new_leaflet <- new_leaflet %>%
        addCircles(data = wrl, lng = wrl$lon, lat = wrl$lat, color = "red", fillColor = "red", group = "Water Rights", label = paste0(wrl$POD_STATUS, " - ", wrl$PRIMARY_LICENSEE_NAME),
                   popup =  ~ paste("<h3> Water Rights Status: ", wrl$POD_STATUS, "</h3>",
                                    "<b>Licensee Name:</b><br>", wrl$PRIMARY_LICENSEE_NAME, "<br><br>",
                                    "<b>POD_NUMBER:</b>", wrl$POD_NUMBER, "<br>",
                                    "<b>POD_SUBTYPE:</b>", wrl$POD_SUBTYPE, "<br>",
                                    "<b>POD_DIVERSION_TYPE:</b>", wrl$POD_DIVERSION_TYPE, "<br>",
                                    "<b>PURPOSE_USE:</b>", wrl$PURPOSE_USE, "<br>",
                                    "<b>QUANTITY:</b>", wrl$QUANTITY, " ", wrl$QUANTITY_UNITS, "<br>",
                                    "<b>QUANTITY_FLAG:</b>", wrl$QUANTITY_FLAG_DESCRIPTION, "<br>",
                                    "<b>QTY_DIVERSION_MAX_RATE:</b>", wrl$QTY_DIVERSION_MAX_RATE, " ", wrl$QTY_UNITS_DIVERSION_MAX_RATE, "<br>", sep = " ")) %>%
        addCircles(data = wap, lng = wap$lon, lat = wap$lat, color = "blue", fillColor = "blue", group = "Water Approvals", label = paste0(wap$APPROVAL_STATUS, " - ", wap$APPROVAL_TYPE),
                   popup =  ~ paste("<h3> Water Approvals: ", wap$APPROVAL_STATUS, "</h3>",
                                    "<b>WATER_APPROVAL_ID:</b>", wap$WATER_APPROVAL_ID, "<br>",
                                    "<b>APPROVAL_TYPE:</b>", wap$APPROVAL_TYPE, "<br>",
                                    "<b>WORKS_DESCRIPTION:</b>", wap$WORKS_DESCRIPTION, "<br>",
                                    "<b>DATE:</b>", wap$APPROVAL_START_DATE, " ", wap$APPROVAL_EXPIRY_DATE, "<br>",
                                    "<b>QUANTITY:</b>", wap$QUANTITY, " ", wap$QUANTITY_UNITS, "<br>",
                                    "<b>QTY_DIVERSION_MAX_RATE:</b>", wap$QTY_DIVERSION_MAX_RATE, " ", wap$QTY_UNITS_DIVERSION_MAX_RATE, "<br>", sep = " "))
    }

    if("Streamflow and Freshwater" %in% input$run_modules){
      new_leaflet <- new_leaflet %>%
        addPolygons(data = my_wl %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "FWA Wetland", fillColor = "yellow", color = "pink", weight = 1, fillOpacity = 0.3, label = my_wl$waterbody_type) %>%
        addPolygons(data = my_lk %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "FWA Lake", fillColor = "steelblue", color = "steelblue", weight = 1, fillOpacity = 1, label = my_lk$waterbody_type) %>%
        addPolygons(data = my_gl %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "FWA Glacier", fillColor = "grey", color = "grey", weight = 1, fillOpacity = 0.3, label = my_gl$waterbody_type) %>%
        addPolygons(data = my_gl_1985 %>% filter(FEATURE_AREA_SQM > 0) %>% st_transform(4326), group = "Glacier 1985", fillColor = "brown", color = "brown", weight = 1, fillOpacity = 0.3, label = my_gl_1985$SOURCE_YEAR) %>%
        addPolygons(data = my_gl_2021 %>% filter(FEATURE_AREA_SQM > 0) %>% st_transform(4326), group = "Glacier 2021", fillColor = "blue", color = "blue", weight = 1, fillOpacity = 0.3, label = my_gl_1985$SOURCE_YEAR)
    }


    output$mymap <- renderLeaflet({
      new_leaflet %>%
        addPolygons(data = new_ws2 %>% st_transform(4326), fillOpacity = 0, weight = 2, color = "blue") %>%
        addLayersControl(baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
                         overlayGroups = c("Sentinel 2023 (slow)", "Landsat 2020-2023 (slow)", "Landsat 1985-1990 (slow)",
                                           "FWA Wetland", "FWA Lake", "FWA Glacier", "Glacier 1985", "Glacier 2021",
                                           "Fire", "Cutblock", "Roads", "Water Rights", "Water Approvals"),
                         options = layersControlOptions(collapsed = F)) %>%
        hideGroup(c("Roads")) %>%
        hideGroup(c("Sentinel 2023 (slow)", "Landsat 1985-1990 (slow)", "Landsat 2020-2023 (slow)")) %>%
        fitBounds(bbbb$xmin[[1]], bbbb$ymin[[1]], bbbb$xmax[[1]], bbbb$ymax[[1]]) %>%
        addLegend("bottomleft",
                  colors = c("blue", "red"),
                  labels = c("Water Approvals", "Water Rights"), title = "Points", opacity = 1) %>%
        addLegend("bottomright",
                  colors = c("yellow","steelblue","grey","brown","blue","red","darkgreen"),
                  labels = c("FWA Wetland","FWA Lake","FWA Glacier","Glacier 1985","Glacier 2021","Wildfire","Cutblock"),
                  title = "Polygons", opacity = 1)})

# DOWNLOAD BUTTONS ####

  output$downloadWatershed <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_watershed.sqlite")}, content = function(file) { st_write(new_ws2, file)})
  output$downloadCutblocks <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_cutblocks.sqlite")}, content = function(file) { st_write(my_cb, file)})
  output$downloadWildfires <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_wildfires.sqlite")}, content = function(file) { st_write(my_wf, file)})
  output$downloadWetlands <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_wetlands.sqlite")}, content = function(file) { st_write(my_wl, file)})
  output$downloadLakes <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_lakes.sqlite")}, content = function(file) { st_write(my_lk, file)})
  output$downloadGlaciers <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_glaciers.sqlite")}, content = function(file) { st_write(my_gl, file)})

# TIME ####

  time <- paste0(round(as.numeric(as.numeric(strsplit(toc()$callback_msg, " ")[[1]][1]) / 60), 1), " minutes elapsed")
  output$ws_run <- renderText({time})
  output$ws_selection_pred_time <- renderText({"Processing complete!"})

  # conn <- refresh()
  dbWriteTable(conn, "usage",
               data.frame(date_time = as.character(session_start),
                          session_token = session_token,
                          gnis_name = new_ws()$gnis_name,
                          gnis_id = new_ws()$gnis_id,
                          processing_time = time,
                          action = "processing",
                          area_km2 = round(new_ws()$area_km2, 1),
                          basin_source = basin_source()), append = TRUE)


        })
      }
    }
  })
}

shinyApp(ui, server)


# lapply(DBI::dbListConnections(RPostgres::dbDriver("PostgreSQL")), function(i){
#   DBI::dbDisconnect(i)})
