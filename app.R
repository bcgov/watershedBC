# CLEAR ENVIRONMENT ############################################################

  rm(list=ls())

# LOAD LIBRARIES ###############################################################

  source("app_1_libs.R")

# CONNECT TO DATABASE ##########################################################

  source("app_2_db.R")

# LOAD BASELINE DATSETS ########################################################

  source("app_3_load_dat.R")

# FUNCTIONS ####################################################################

  # source("app_4_discharge.R")
  # source("app_4_discharge_matchStn.R")
  source("app_4_discharge_matchStn_forceUpdate.R")

# BASEMAP ######################################################################

  source("app_5_leaflet.R")

# UI ###########################################################################

ui <- navbarPage(theme = "css/bcgov.css",
                 title = "watershedBC (v0.1 Beta Testing)",
                 useShinyjs(),

                 tabPanel("watershedBC (v0.1 Beta Testing)",

                          modalDialog(
                            HTML("Welcome to watershedBC, an experimental research tool that is in <u>active development</u>.
            The tool is designed to summarize watershed data and estimate streamflow. Before you proceed,
            please read the following disclaimer:<br><br><ul>
            <li><b><u>Prototype:</u></b> watershedBC is a prototype. There are multiple known issues and innacuracies that are actively being worked on.</li><br>
            <li><b><u>Not validated:</u></b> At this time, do not use any information from watershedBC for decision making.</li><br>
            <li><b><u>Frequent outages:</u></b> This tool is a proof of concept and will periodically be offline, freeze, or crash.</li><br>
            <li><b><u>User responsibility:</u></b> The User is responsible for the safe interpretation of the datasets presented.</li><br>
            <li><b><u>Open source:</u></b> The goal of this project is to be openly transparent via <a href='https://github.com/bcgov/watershedBC/', target='_blank'>https://github.com/bcgov/watershedBC/</a></li><br>
            <li><b><u>Speed:</u></b> This tool slows down with more concurent users. This will be fixed in future versions.</li><br>
            <li><b><u>Feedback:</u></b> Users can provide feedback here: <a href='https://github.com/bcgov/watershedBC/issues/', target='_blank'>https://github.com/bcgov/watershedBC/issues/</a></li><br>
            </ul>"),
                            title = "Disclaimer: watershedBC is a prototype",
                            size = "m",
                            easyClose = FALSE,
                            footer = modalButton("I accept the disclaimer")),

                          shiny::fluidRow(
                            shiny::column(
                              width = 2,
                              h3("Get started"),

                              htmltools::HTML("1 - Click anywhere in BC to get started<br>
                         2 - Click 'Run Report'<br>
                         3 - Be patient!"), br(), br(),

                              shiny::radioButtons(inputId = "watershed_source",
                                                  label = "Watershed Data Source",
                                                  choices = c("Freshwater Atlas Named Watersheds",
                                                              "Freshwater Atlas by Stream Order",
                                                              "Custom Basin at Point of Interst",
                                                              "Water Survey of Canada Basins"),
                                                  selected = "Freshwater Atlas Named Watersheds"),

                              shiny::selectizeInput(inputId = "psql_zoom_to_name",
                                                    label = "Or search watersheds by name",
                                                    choices = c("", names$name),
                                                    selected = "",
                                                    multiple = F),

                              shiny::actionButton(inputId = "zoom_to_button",
                                                  label = "Zoom to.."),br(),br(),

                              shiny::checkboxGroupInput(inputId = "run_modules",
                                                        label = "Include in Watershed Report",
                                                        choices = c("Streamflow and Freshwater",
                                                                    "Forest Disturbance",
                                                                    "CEF Human Disturbance - 2021",
                                                                    "Stream Profile",
                                                                    "Water Allocations",
                                                                    "ClimateBC",
                                                                    "Satellite Imagery"),
                                                        selected = c("Streamflow and Freshwater"))),

                            shiny::column(
                              width = 10,
                              leafletOutput("mymap", height = '600px') %>% withSpinner(color = "steelblue"),
                              checkboxInput(inputId = "active_mouse", label = "Watershed Delineation on Map Click", value = T, ),
                              h3(textOutput(outputId = "ws_selection")),
                              actionButton(inputId = "run_button", label = "Run Report"),
                              textOutput(outputId = "ws_run"),
                              textOutput(outputId = "ws_selection_pred_time"),
                              tableOutput('table_named'),
                              plotlyOutput("plot_discharge"),
                              shiny::selectInput(inputId = "plot_discharge_site_sel",
                                                        label = "Override Reference Station for Streamflow Estimation",
                                                        choices = stn_training$total_name,
                                                        width = "500px"),
                              htmlOutput("text_plot_discharge"),
                              plotlyOutput("plot_profile"), htmlOutput("text_plot_profile"),
                              plotlyOutput("plot_cef_group"),
                              plotlyOutput("plot_cef_group_flag"), htmlOutput("text_plot_cef_group_flag"),
                              plotlyOutput("plot_timeseries"), htmlOutput("text_plot_timeseries"),
                              plotlyOutput("plot_timeseries_cumsum"), htmlOutput("text_plot_timeseries_cumsum"),
                              plotlyOutput("plot_eca"), htmlOutput("text_plot_eca"),
                              plotlyOutput("plot_elevbins"), htmlOutput("text_plot_elevbins"),
                              plotlyOutput("plot_fwa"), htmlOutput("text_plot_fwa"),
                              plotlyOutput("plot_auth"), htmlOutput("text_plot_auth"),
                              plotlyOutput("plot_dra"), htmlOutput("text_plot_dra"),
                              plotlyOutput("plot_mat"), htmlOutput("text_plot_mat"),
                              plotlyOutput("plot_map"), htmlOutput("text_plot_map"),
                              plotlyOutput("plot_cmd"), htmlOutput("text_plot_cmd"),
                              plotOutput("plot_landsat_1985", width = 800, height = 800),
                              plotOutput("plot_landsat_2020", width = 800, height = 800),
                              plotOutput("plot_sentinel_2023", width = 800, height = 800),
                              downloadButton("export_pdf", "Export PDF"),br(),br(),br(),
                              downloadButton("downloadWatershed", "Watershed"),br(),
                              downloadButton("downloadCutblocks", "Cutblocks"),br(),
                              downloadButton("downloadWildfires", "Wildfire"),br(),
                              downloadButton("downloadLakes", "FWA - Lakes"),br(),
                              downloadButton("downloadWetlands", "FWA - Wetlands"),br(),
                              downloadButton("downloadGlaciers", "FWA - Glaciers"),br(),
                              downloadButton("downloadGlaciers85", "Glaciers 1985"),br(),
                              downloadButton("downloadGlaciers21", "Glaciers 2021"),br(),
                              downloadButton("downloadCEF", "Human Disturbance"),br(),
                              downloadButton("downloadRoads", "Roads"),br(),br(),br())),

                          shiny::fluidRow(
                            shiny::column(
                              width = 12,
                              HTML("<b>More info:</b> <a href='https://github.com/bcgov/watershedBC/', target='_blank'>https://github.com/bcgov/watershedBC/</a><br>
              <b>Data sources:</b> Freshwater Atlas of BC, Consolidated Cutblocks of BC, BC Wildfire Service Fire Perimeters, Landsat, and Sentinel-2<br>
              <b>Known issues:</b> Data is not accurate across provincial, territorial, national borders.<br>
              This tool is provided with <b>no guarantees of reliability or accuracy</b>, please scrutinize the results.<br>
              Please contact <i>alexandre.bevington@gov.bc.ca</i> with any questions or comments about this tool."), br())),

                          shiny::fluidRow(
                            shiny::column(
                              width = 12,
                              style = "background-color:#003366; border-top:2px solid #fcba19;",
                              tags$footer(class = "footer", tags$div(class = "container", style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;", tags$ul(
                                style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                tags$li(a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                tags$li(a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style ="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                tags$li(a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style ="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                tags$li(a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style ="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                tags$li(a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style ="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                tags$li(a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style ="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))))))))
)

# SERVER #######################################################################

server <- function(input, output, session) {

  ## HIDE PLOTS ON START
  shinyjs::hide("export_pdf")
  shinyjs::hide("downloadWatershed")
  shinyjs::hide("downloadCutblocks")
  shinyjs::hide("downloadWildfires")
  shinyjs::hide("downloadWetlands")
  shinyjs::hide("downloadLakes")
  shinyjs::hide("downloadGlaciers")
  shinyjs::hide("downloadGlaciers")
  shinyjs::hide("downloadGlaciers85")
  shinyjs::hide("downloadGlaciers21")
  shinyjs::hide("downloadCEF")
  shinyjs::hide("downloadRoads")
  shinyjs::hide('table_named')
  shinyjs::hide("plot_discharge")
  shinyjs::hide("plot_discharge_site_sel")
  shinyjs::hide("text_plot_discharge")
  shinyjs::hide("plot_profile")
  shinyjs::hide("text_plot_profile")
  shinyjs::hide("plot_cef_group")
  shinyjs::hide("text_plot_cef_group")
  shinyjs::hide("plot_cef_group_flag")
  shinyjs::hide("text_plot_cef_group_flag")
  shinyjs::hide("plot_timeseries")
  shinyjs::hide("text_plot_timeseries")
  shinyjs::hide("plot_timeseries_cumsum")
  shinyjs::hide("text_plot_timeseries_cumsum")
  shinyjs::hide("plot_eca")
  shinyjs::hide("text_plot_eca")
  shinyjs::hide("plot_elevbins")
  shinyjs::hide("text_plot_elevbins")
  shinyjs::hide("plot_fwa")
  shinyjs::hide("text_plot_fwa")
  shinyjs::hide("plot_auth")
  shinyjs::hide("text_plot_auth")
  shinyjs::hide("plot_dra")
  shinyjs::hide("text_plot_dra")
  shinyjs::hide("plot_mat")
  shinyjs::hide("text_plot_mat")
  shinyjs::hide("plot_map")
  shinyjs::hide("text_plot_map")
  shinyjs::hide("plot_cmd")
  shinyjs::hide("text_plot_cmd")
  shinyjs::hide("plot_landsat_1985")
  shinyjs::hide("plot_landsat_2020")
  shinyjs::hide("plot_sentinel_2023")

  ## RECORD SESSION INFO
  session_start <- Sys.time() %>% format(tz="UTC")
  session_token <- session$token

  ## CREATE REACTIVE VAL FOR WATERSHED
  new_ws <- reactiveVal()
  basin_source <- reactiveVal()
  new_ws2_forRF <- reactiveVal()

  ## ADD INITAL BASEMAP
  output$mymap <- renderLeaflet({initial_map %>% fitBounds(-140, 48, -113, 60)})

  ## IF WSC BASIN OPTION, ADD WSC LOCATIONS TO MAP
  observeEvent(input$watershed_source, {

    if(input$watershed_source == "Water Survey of Canada Basins") {

      wsc_pp_ac <- wsc_pp %>% filter(status == "active") %>% mutate(id = stationnum)
      wsc_pp_dc <- wsc_pp %>% filter(status == "discontinued") %>% mutate(id = stationnum)

      leafletProxy("mymap") %>%
        addCircleMarkers(data = wsc_pp_ac, lng = wsc_pp_ac$lon, lat = wsc_pp_ac$lat, color = "steelblue", radius = 3, group = "WSC Active",
                         label = paste0(wsc_pp_ac$name, " - ", wsc_pp_ac$stationnum, " [active]")) %>%
        addCircleMarkers(data = wsc_pp_dc, lng = wsc_pp_dc$lon, lat = wsc_pp_dc$lat, color = "grey", radius = 3, group = "WSC Discontinued",
                         label = paste0(wsc_pp_dc$name, " - ", wsc_pp_dc$stationnum, " [discontinued]")) %>%
        addLayersControl(baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
                         overlayGroups = c("Sentinel 2023 (slow)", "Landsat 2020-2023 (slow)", "Landsat 1985-1990 (slow)", "WSC Active", "WSC Discontinued"),
                         options = layersControlOptions(collapsed = F))
    }
    # else{
    #   leafletProxy("mymap") %>%
    #     # leaflet::clearGroup("WSC Active") %>%
    #     # leaflet::clearGroup("WSC Discontinued")
    #   }
    })

  # OPTION 1: CLICK MAP TO SELECT WATERSHED ####

  # ACTION ON CLICK
  observeEvent(input$mymap_click, {

    if (input$active_mouse == T) {

      withProgress(message = 'Finding watershed...', max = 1,  {

        # START TIMER
        tic()

        # GET POINT
        incProgress(1, detail = "...")
        point <- input$mymap_click
        point_df <- data.frame(lat = point$lat, lng = point$lng)
        print(paste0("point_df <- data.frame(lat = ",point$lat,", lng = ",point$lng,")"))
        # point_df <- data.frame(lat = 53.1204052831066, lng = -124.937831245044)

        # SELECT WATERSHED FROM ONE OF THESE OPTIONS

        if(input$watershed_source == "Freshwater Atlas Named Watersheds") {
          basin_source("FWA")
          print("FWA")
          bas <- st_read(conn, query = paste0(
            "SELECT * FROM fwa_named
             WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",point_df$lng,",",point_df$lat,"), 4326),3005))
             ORDER BY area_m2 ASC LIMIT 1"))
          bas <- bas %>% st_cast("POLYGON", warn = F)
          if(nrow(bas) > 0) {
            bas <- bas %>% mutate(area_km2 = area_m2 / (1000 * 1000))
            new_ws(bas)}}

        if(input$watershed_source == "Freshwater Atlas by Stream Order") {
          basin_source("FWA Order")
          print("FWA Order")
          bas <- st_read(conn, query = paste0(
            "SELECT * FROM fwa_rollup
             WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",point_df$lng,",",point_df$lat,"), 4326),3005))
             ORDER BY area_m2 ASC LIMIT 1"))
          bas <- bas %>% st_cast("POLYGON", warn = F)
          bas$overl <- bas %>% st_intersects(as.data.frame(point) %>% st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% st_transform(st_crs(bas)), sparse = F)
          bas <- bas %>% filter(overl == T) %>% mutate(area_m2 = as.numeric(st_area(.)))
          if (nrow(bas) > 0) {
            new_ws(bas %>% mutate(area_km2 = area_m2 / (1000 * 1000)) %>%
                     rename(gnis_name = id, gnis_id = iFWA))}}

        if(input$watershed_source == "Custom Basin at Point of Interst") {
          basin_source("basinsv4")
          print("basinv4")
          bas <- st_read(conn,query = paste0(
            "SELECT * FROM basinsv4
              WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",point_df$lng,",",point_df$lat,"), 4326),3005))
              ORDER BY area_m2 ASC LIMIT 1"))
          if(nrow(bas) > 0) {
            bas <- bas %>%
              mutate(area_km2 = area_m2 / (1000 * 1000)) %>%
              rename(gnis_name = id, gnis_id = basin) %>%
              ms_simplify(keep = 0.5)
            new_ws(bas)}}

        if(input$watershed_source == "Water Survey of Canada Basins") {
          basin_source("WSC")
          print("WSC")
          point_sf <- st_as_sf(point_df, coords = c("lng", "lat"), crs = 4326)
          wsc_pp_id <- wsc_pp %>%
            mutate(dist = as.numeric(st_distance(., point_sf))) %>%
            filter(dist == min(dist)) %>% pull(stationnum)
          bas <- st_read(conn, query = paste0(
            "SELECT stationnum,name,area,geometry FROM wsc_drainagebasin_clean_3005
              WHERE stationnum = '",wsc_pp_id,"'")) %>% rename(geom = geometry)
          bas <- bas %>% rmapshaper::ms_simplify(keep = 0.8)
          if (nrow(bas) > 0) {
            bas <- bas %>% mutate(
              gnis_name = paste0(bas$stationnum, " ", bas$name),
              gnis_id = bas$stationnum,
              area_km2 = area / (1000 * 1000)) %>%
              ms_simplify(keep = 0.5) %>%
              st_transform(crs = 3005)
            new_ws(bas)}}

        # UPDATE MAP WITH SELECTED WATERSHED
        if(nrow(bas) > 0) {
          output$ws_selection <- renderText({paste0("You selected ", new_ws()$gnis_name, " (", format(round(as.numeric(new_ws()$area_km2), 0), big.mark = ",") ," sq.km)")})
          output$ws_selection_pred_time <- renderText({paste0("Estimated time to run a full report ~ ", 0.5 + round((new_ws()$area_km2 * 0.03) / 60, 1)," min")})

          print("map")
          bbbb <- st_bbox(bas %>% st_transform(4326))

          leafletProxy("mymap") %>%
            leaflet::clearGroup("Watershed")

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
            addPolygons(data = bas %>% st_transform(4326),
                        fillOpacity = 0,
                        weight = 2,
                        color = "blue",
                        group = "Watershed")}
        a <- toc()$callback_msg
        if (nrow(bas) > 0) {
          output$ws_run <- renderText({a})
          dbWriteTable(conn, "usage", data.frame(date_time = as.character(session_start),
                                                 session_token = session_token,
                                                 gnis_name = new_ws()$gnis_name,
                                                 gnis_id = new_ws()$gnis_id,
                                                 processing_time = a,
                                                 action = "select watershed",
                                                 area_km2 = round(new_ws()$area_km2, 1),
                                                 basin_source = basin_source()), append = TRUE)}
      })
    }
  })

  # OPTION 2: ZOOM TO NAMED WATERSHED ####

  observeEvent(input$zoom_to_button, {
    if (input$psql_zoom_to_name != "") {
      tic(quiet = T)
      basin_source("FWA")
      withProgress(message = 'Finding watershed...', max = 1,  {
        split_name <- strsplit(strsplit(input$psql_zoom_to_name, "id:")[[1]][2], ")")[[1]][1]
        bas <- st_read(conn, query = paste0("SELECT * FROM fwa_named WHERE gnis_id = ", split_name))
        new_ws(bas %>% mutate(area_km2 = area_m2 / (1000 * 1000)))

        if(nrow(bas) > 0) {
          output$ws_selection <- renderText({paste0("You selected ", new_ws()$gnis_name, " (", format(round(as.numeric(new_ws()$area_km2), 0), big.mark = ",") ," sq.km)")})
          output$ws_selection_pred_time <- renderText({paste0("Estimated time to run a full report ~ ", 0.5 + round((new_ws()$area_km2 * 0.03) / 60, 1)," min")})

          print("map")
          bbbb <- st_bbox(bas %>% st_transform(4326))

          leafletProxy("mymap") %>%
            leaflet::clearGroup("Watershed")

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
            addPolygons(data = bas %>% st_transform(4326),
                        fillOpacity = 0,
                        weight = 2,
                        color = "blue",
                        group = "Watershed") %>%
            fitBounds(bbbb$xmin[[1]], bbbb$ymin[[1]], bbbb$xmax[[1]], bbbb$ymax[[1]])


          a <- toc()$callback_msg
          output$ws_run <- renderText({a})
          dbWriteTable(conn,"usage",data.frame(date_time = as.character(session_start),
                                               session_token = session_token,
                                               gnis_name = new_ws()$gnis_name,
                                               gnis_id = new_ws()$gnis_id,
                                               processing_time = a,
                                               action = "select watershed",
                                               area_km2 = round(new_ws()$area_km2, 1),
                                               basin_source = basin_source()), append = TRUE)
        }
      })
    }})


  # RUN REPORT #################################################################
  observeEvent(input$plot_discharge_site_sel,
               {
                 withProgress(message = 'Update Reference Station...', max = 2,  {
                   my_force_station <- str_split_fixed(input$plot_discharge_site_sel, " - ", 3)[,1]
                   # new_ws2_forRF(pred_Q_prepWS(w = new_ws2, my_wetlands = my_wl, my_glaciers = my_gl, my_lakes = my_lk))

                   if(input$watershed_source == "Water Survey of Canada Basins"){
                     print(new_ws2$gnis_id)
                     output$plot_discharge <- renderPlotly({
                       # ref_stn <- pred_Q_findRef()
                       pred_Q_rf(w = new_ws2_forRF(), force_station = my_force_station, wsc_STATION_NUMBER = new_ws2$gnis_id)
                     })
                   }else{
                     output$plot_discharge <- renderPlotly({
                       print("update2")
                       print(new_ws2_forRF())
                       # ref_stn <- pred_Q_findRef()
                       pred_Q_rf(w = new_ws2_forRF(), force_station = my_force_station)
                     })
                   }
                 })
               })
  observeEvent(input$run_button, {

    shinyjs::hide('table_named')
    shinyjs::hide("plot_discharge")
    shinyjs::hide("text_plot_discharge")
    shinyjs::hide("plot_profile")
    shinyjs::hide("text_plot_profile")
    shinyjs::hide("plot_cef_group")
    shinyjs::hide("text_plot_cef_group")
    shinyjs::hide("plot_cef_group_flag")
    shinyjs::hide("text_plot_cef_group_flag")
    shinyjs::hide("plot_timeseries")
    shinyjs::hide("text_plot_timeseries")
    shinyjs::hide("plot_timeseries_cumsum")
    shinyjs::hide("text_plot_timeseries_cumsum")
    shinyjs::hide("plot_eca")
    shinyjs::hide("text_plot_eca")
    shinyjs::hide("plot_elevbins")
    shinyjs::hide("text_plot_elevbins")
    shinyjs::hide("plot_fwa")
    shinyjs::hide("text_plot_fwa")
    shinyjs::hide("plot_auth")
    shinyjs::hide("text_plot_auth")
    shinyjs::hide("plot_dra")
    shinyjs::hide("text_plot_dra")
    shinyjs::hide("plot_mat")
    shinyjs::hide("text_plot_mat")
    shinyjs::hide("plot_map")
    shinyjs::hide("text_plot_map")
    shinyjs::hide("plot_cmd")
    shinyjs::hide("text_plot_cmd")
    shinyjs::hide("plot_landsat_1985")
    shinyjs::hide("plot_landsat_2020")
    shinyjs::hide("plot_sentinel_2023")

    if(!is.null(new_ws())){

      # shiny::updateCheckboxInput(inputId = "active_mouse", value = F)

      new_ws2 <- new_ws()
      print(new_ws2)
      # new_ws2 <- bas
      # new_ws2 <- st_read(conn, query = "SELECT * FROM fwa_named WHERE gnis_name = 'Tezzeron Creek'") %>% mutate(area_km2 = area_m2/(1000*1000))
      # new_ws2 <- st_read(conn, query = "SELECT * FROM fwa_named WHERE gnis_name = 'Bowron River'") %>% mutate(area_km2 = area_m2/(1000*1000))
      # new_ws2 <- st_read(conn, query = "SELECT * FROM fwa_named WHERE gnis_name = 'Joe Smith Creek'") %>% mutate(area_km2 = area_m2/(1000*1000))
      # new_ws2 <- st_read(refresh(), query = "SELECT * FROM fwa_named WHERE gnis_id = 26413") %>% mutate(area_km2 = area_m2/(1000*1000))
      # new_ws2 <- st_read(conn, query = paste0("SELECT * FROM basinsv4 WHERE id = 874586")) %>% rename(gnis_name = basin, gnis_id = id) %>% mutate(area_km2 = area_m2/(1000*1000))

      if(new_ws2$area_km2 > 20000){
        output$ws_run <-renderText({"Watershed is too large... please select a smaller watershed smaller than 20,000 sq. km."})
      }

      if(new_ws2$area_km2 < 200000){

        tic()
        new_ws2_wkt <- st_as_text(st_geometry(new_ws2 %>% ms_explode()))

        withProgress(message = 'Processing...', max = 15,  {

          # FRESHWATER ####

          if("Streamflow and Freshwater" %in% input$run_modules) {

            ## NAMED WATERSHEDS ####

            shinyjs::show('table_named')
            incProgress(1, detail = paste0("Get watersheds (", round(new_ws2$area_km2, 0), ")"))
            print("getting watershed")
            my_named <- postgis_get_pol("fwa_named","*",elev = F,my_wkt = new_ws2_wkt,min_area_km2 = new_ws2$area_km2 * 0.1)
            if(nrow(my_named) > 0){
              my_named = my_named %>%
                mutate(area_km2 = area_m2 / (1000 * 1000)) %>%
                dplyr::select(gnis_name, area_km2) %>%
                arrange(-area_km2) %>%
                mutate(Location = case_when(
                  gnis_name == new_ws2$gnis_name ~ "Watershed of Interest",
                  area_km2 > new_ws2$area_km2 ~ "Downstream",
                  area_km2 < new_ws2$area_km2 ~ "Upstream",
                  TRUE ~ ""))
              output$table_named <- renderTable(
                my_named %>% st_drop_geometry() %>%
                  mutate(area_km2 = round(area_km2, 1)) %>%
                  rename(Name = gnis_name,
                         Area_km2 = area_km2)
                ,digits = 2)}

            # FRESHWATER RESOURCES ####

            shinyjs::show("plot_fwa")

            incProgress(1, detail = "Get Wetlands")
            print("getting wetlands")
            my_wl <- postgis_get_pol("fwa_wetlands", "waterbody_type", my_wkt = new_ws2_wkt)
            if(nrow(my_wl) == 0){my_wl <- st_as_sf(data.frame(clipped_area_m2 = 0, waterbody_type = "", elevation = 0, area_m2 = 0, lat = 0, long = 0, type = "fwa_wetlands"), coords = c("long", "lat"), crs = 3005)}

            incProgress(1, detail = "Get Lakes")
            print("getting lakes")
            my_lk <- postgis_get_pol("fwa_lakes", "waterbody_type", my_wkt = new_ws2_wkt)
            if(nrow(my_lk) == 0){my_lk <- st_as_sf(data.frame(clipped_area_m2 = 0,waterbody_type = "",elevation = 0,area_m2 = 0,lat = 0,long = 0,type = "fwa_lakes"),coords = c("long", "lat"),crs = 3005)}

            incProgress(1, detail = "Get Glaciers")
            print("getting glaciers")
            my_gl <- postgis_get_pol("fwa_glaciers", "waterbody_type", my_wkt = new_ws2_wkt)
            if(nrow(my_gl) == 0) {my_gl <- st_as_sf(data.frame(clipped_area_m2 = 0,waterbody_type = "",elevation = 0,area_m2 = 0,lat = 0,long = 0,type = "fwa_glaciers"),coords = c("long", "lat"),crs = 3005)}

            my_gl_1985 <- bcdata::bcdc_query_geodata("historical-glaciers") %>% select(GBA_GLHIST_SYSID, GLACIER_ID, SOURCE_YEAR, FEATURE_AREA_SQM) %>% filter(INTERSECTS(new_ws2)) %>% collect()
            if(nrow(my_gl_1985) > 0) {my_gl_1985 <- my_gl_1985 %>% st_intersection(new_ws2) %>% mutate(FEATURE_AREA_SQM = as.numeric(st_area(.)))
            }else{my_gl_1985 <- st_as_sf(data.frame(GLACIER_ID = 0,FEATURE_AREA_SQM = 0,lat = 0,long = 0,SOURCE_YEAR = 1985),coords = c("long", "lat"),crs = 3005)}

            my_gl_2021 <- bcdata::bcdc_query_geodata("glaciers") %>% select(GLACIER_ID, SOURCE_YEAR, FEATURE_AREA_SQM) %>% filter(INTERSECTS(new_ws2)) %>% collect()
            if (nrow(my_gl_2021) > 0) {my_gl_2021 <- my_gl_2021 %>% st_intersection(new_ws2) %>% mutate(FEATURE_AREA_SQM = as.numeric(st_area(.)))
            } else{my_gl_2021 <-st_as_sf(data.frame(GLACIER_ID = 0,FEATURE_AREA_SQM = 0,lat = 0,long = 0,SOURCE_YEAR = 2021),coords = c("long", "lat"),crs = 3005)}

            print("merge fwa")
            my_fwa <- bind_rows(data.frame(type  = c("W", "L", "G","G 1985","G 2021"),
                                           area_m2 = c(0, 0, 0,0,0)),
                                my_wl %>% st_drop_geometry(),
                                my_lk %>% st_drop_geometry(),
                                my_gl %>% st_drop_geometry(),
                                my_gl_1985 %>% st_drop_geometry() %>% rename(clipped_area_m2 = FEATURE_AREA_SQM) %>% mutate(type = "G 1985"),
                                my_gl_2021 %>% st_drop_geometry() %>% rename(clipped_area_m2 = FEATURE_AREA_SQM) %>% mutate(type = "G 2021")) %>%
              st_drop_geometry() %>%
              mutate(type = case_when(type == "fwa_wetlands" ~ "Wetland (FWA)",
                                      type == "fwa_glaciers" ~ "Glacier (FWA)",
                                      type == "fwa_lakes" ~ "Lake (FWA)",
                                      type == "G 1985" ~ "Glacier (1985)",
                                      type == "G 2021" ~ "Glacier (2021)"))

            print("plot fwa")
            output$plot_fwa <- renderPlotly({
              ggplotly(my_fwa %>%
                         group_by(type) %>%
                         dplyr::summarize(area_km2 = sum(clipped_area_m2, na.rm = T) / (1000 * 1000)) %>%
                         filter(!is.na(type)) %>%
                         ggplot() +
                         geom_col(aes(type, area_km2, fill = type), color = "black") +
                         theme_bw() +
                         labs(x = "",y = "Area sq.km", title = "Freshwater Atlas", fill = "") +
                         scale_fill_manual(values = c("grey90", "grey80", "grey70", "steelblue", "yellow")) +
                         scale_y_continuous(n.breaks = 10), dynamicTicks = T, width = 800)
            })
          }else{shinyjs::hide("plot_fwa")}

    # DAMS ####

          dams <- postgis_get_point("dams", new_ws2_wkt)
          if(nrow(dams) == 0) {dams <- st_as_sf(data.frame(total_licence_storage = 0, long = 0, lat = 0),coords = c("long", "lat"),crs = 3005)}

          new_ws2 <- new_ws2 %>%
            mutate(total_licence_storage = sum(dams$total_licence_storage)) %>%
            mutate(regulated_percent = 100*(total_licence_storage/1e9)/area_km2) %>%
            mutate(regulated = case_when(regulated_percent > 0.003 ~ "regulated", TRUE ~ "unregulated"))

# DISCHARGE ####

  if ("Streamflow and Freshwater" %in% input$run_modules) {

    incProgress(1, detail = "Estimating discharge...")
    print("getting discharge")

    new_ws2_forRF(pred_Q_prepWS(w = new_ws2, my_wetlands = my_wl, my_glaciers = my_gl, my_lakes = my_lk))
    print(new_ws2_forRF())
    ref_stn <- pred_Q_findRef(w = new_ws2_forRF())
    print("REFFF")
    print(ref_stn)
            if(input$watershed_source == "Water Survey of Canada Basins"){
              print("WSC")
              print(new_ws2$gnis_id)
              output$plot_discharge <- renderPlotly({
                pred_Q_rf(w = new_ws2_forRF(), force_station = ref_stn, wsc_STATION_NUMBER = new_ws2$gnis_id)
              })
            }else{
              output$plot_discharge <- renderPlotly({
                print("other")
                pred_Q_rf(w = new_ws2_forRF(), force_station = ref_stn)
              })
            }

    updateselection <- stn_training$total_name[str_detect(stn_training$total_name, ref_stn)]
    print(updateselection)
    updateSelectInput(inputId = "plot_discharge_site_sel",
                      selected = updateselection)

            output$text_plot_discharge <- renderText({"<br>The <b>Estimated Discharge</b> is a working <u>prototype</u> that has <b>NOT BEEN VALIDATED</b>. Please do not use this information for decision making at this time. The model finds the most similar Water Survey of Canada station that meets the selection criteria. Similarity is based on basin statistics (ClimateBC, Terrain and Freshwater Atlas). Then the total discharge and seasonal distribution is scaled to the basin of interest.<br><br><br>"})

            shinyjs::show("plot_discharge")
            shinyjs::show("text_plot_discharge")
            shinyjs::show("plot_discharge_site_sel")

          }else{
            shinyjs::hide("plot_discharge")
            shinyjs::hide("textplot_discharge")
          }

          # FOREST DISTURBANCE ####

          if("Forest Disturbance" %in% input$run_modules) {

    shinyjs::show("plot_dra")
    shinyjs::show("text_plot_dra")
    shinyjs::show("plot_eca")
    shinyjs::show("text_plot_eca")
    shinyjs::show("plot_timeseries")
    shinyjs::show("text_plot_timeseries")
    shinyjs::show("plot_timeseries_cumsum")
    shinyjs::show("text_plot_timeseries_cumsum")
    shinyjs::show("plot_elevbins")
    shinyjs::show("text_plot_elevbins")

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
                  title = paste0("Total Road Length ", round(dra$length_km %>% sum(), 1), " km"),
                  fill = ""
                ) +
                scale_y_continuous(n.breaks = 10)
              ,
              dynamicTicks = T,
              width = 800
            )
          })

          output$text_plot_dra <- renderText({
            paste0("<br><b>Total Road Length: ", round(dra$length_km %>% sum(), 1), " km</b>. Roads are calculated from the <a href='https://www2.gov.bc.ca/gov/content/data/geographic-data-services/topographic-data/roads'>
          Digital Road Atlas</a> of BC.<br><br><br>")
          })

          # FORESTS ####
          incProgress(1, detail = "Get Wildfires")
          print("getting fires")
          my_wf <- postgis_get_pol("fire", "fire_year", my_wkt = new_ws2_wkt)

          incProgress(1, detail = "Get Cutblocks")
          print("getting blocks")
          my_cb <- postgis_get_pol("cutblocks", "harvest_year", my_wkt = new_ws2_wkt)

          if (nrow(my_wf) == 0) {my_wf <- st_as_sf(data.frame(clipped_area_m2 = 0,fire_year = 1900,elevation = 0,area_m2 = 0,lat = 0,long = 0,type = "fire"),coords = c("long", "lat"),crs = 3005)}
          if (nrow(my_cb) == 0) {my_cb <- st_as_sf(data.frame(clipped_area_m2 = 0,harvest_year = 1900,elevation = 0,area_m2 = 0,lat = 0,long = 0,type = "cutblock"), coords = c("long", "lat"),crs = 3005)}

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
    shinyjs::hide("plot_dra")
    shinyjs::hide("text_plot_dra")
    shinyjs::hide("plot_eca")
    shinyjs::hide("text_plot_eca")
    shinyjs::hide("plot_timeseries")
    shinyjs::hide("text_plot_timeseries")
    shinyjs::hide("plot_timeseries_cumsum")
    shinyjs::hide("text_plot_timeseries_cumsum")
    shinyjs::hide("plot_elevbins")
    shinyjs::hide("text_plot_elevbins")
  }

  # CUMULATIVE EFFECTS ####

    if("CEF Human Disturbance - 2021" %in% input$run_modules) {

      incProgress(1, detail = paste0("Get Cumulative Effects"))

      shinyjs::show("plot_cef_group")
      shinyjs::show("plot_cef_group_flag")
      shinyjs::show("text_plot_cef_group_flag")

      bc_cef_2021 <- postgis_get_pol("bc_cef_2021", "*", my_wkt = new_ws2_wkt, elev = F)

      bc_cef_2021_sum <- bc_cef_2021 %>% mutate(area_km22 = as.numeric(st_area(.))/(1000*1000)) %>%
        st_drop_geometry() %>%
        group_by(cef_disturb_group, cef_human_disturb_flag) %>%
        summarize(area = sum(area_km22))

      cef_sum <- plot_ly(bc_cef_2021_sum,
                          labels = ~paste0(gsub("_", " ", gsub("BTM - ", "", cef_disturb_group))),
                          values = ~area,
                          type = 'pie',
                          hole = 0.6,
                          width = 800,
                          textinfo = 'percent',
                          hoverinfo = 'text',
                          text = ~paste0(cef_human_disturb_flag, "\n", cef_disturb_group, "\n", round(area,1), ' sq. km'),
                          showlegend = T) %>%
        layout(title = 'Human Disturbance - 2021 - By group - % of total watershed area',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

      bc_cef_2021_sum_flag <- bc_cef_2021 %>% mutate(area_km22 = as.numeric(st_area(.))/(1000*1000)) %>%
        st_drop_geometry() %>%
        group_by(cef_human_disturb_flag) %>%
        summarize(area = sum(area_km22))

      cef_sum_flag <- plot_ly(bc_cef_2021_sum_flag,
                               labels = ~paste0(gsub("_", " ", gsub("BTM - ", "", cef_human_disturb_flag))),
                               values = ~area,
                               type = 'pie',
                               hole = 0.6,
                               width = 800,
                               textinfo = 'percent',
                               hoverinfo = 'text',
                               text = ~paste0(cef_human_disturb_flag, "\n", round(area,1), ' sq. km'),
                               showlegend = T) %>%
          layout(title = 'Human Disturbance - 2021 - Summarized - % of total watershed area',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

      output$plot_cef_group <- renderPlotly({cef_sum})
      output$plot_cef_group_flag <- renderPlotly({cef_sum_flag})


      output$text_plot_cef_group_flag <- renderText({
        "<br>The <b>Cumulative Effects Framework - Human Disturbance - 2021</b> is a consolidated human disturbance footprint data set for
        provincial-scale spatial assessment to support
        <a href = 'https://www2.gov.bc.ca/gov/content/environment/natural-resource-stewardship/cumulative-effects-framework'', target = '_blank'>Cumulative Effects Framework (CEF)</a> analysis. The dataset is publicly available in the
        <a href = 'https://catalogue.data.gov.bc.ca/dataset/ce-disturbance-2021/', target = '_blank'>BC Data Catalogue</a>. <br><br><br>"
      })


    }


# STREAM PROFILE ####

  if("Stream Profile" %in% input$run_modules) {

    shinyjs::show("plot_profile")
    shinyjs::show("text_plot_profile")

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
    shinyjs::hide("plot_profile")
    shinyjs::hide("text_plot_profile")
  }

# climate ####

  if ("ClimateBC" %in% input$run_modules) {

    shinyjs::show("plot_mad")
    shinyjs::show("text_plot_mad")
    shinyjs::show("plot_map")
    shinyjs::show("text_plot_map")
    shinyjs::show("plot_cmd")
    shinyjs::show("text_plot_cmd")

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
    shinyjs::hide("plot_mad")
    shinyjs::hide("text_plot_mad")
    shinyjs::hide("plot_map")
    shinyjs::hide("text_plot_map")
    shinyjs::hide("plot_cmd")
    shinyjs::hide("text_plot_cmd")

  }



# WATER ALLOCATIONS ####

  if ("Water Allocations" %in% input$run_modules) {

    shinyjs::show("plot_auth")
    shinyjs::show("text_plot_auth")

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
    shinyjs::hide("plot_auth")
    shinyjs::hide("text_plot_auth")}

# SATELLITE IMAGERY ####

  if ("Satellite Imagery" %in% input$run_modules) {

    shinyjs::show("plot_landsat_1985")
    shinyjs::show("plot_landsat_2020")
    shinyjs::show("plot_sentinel_2023")

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
      shinyjs::hide("plot_landsat_1985")
      shinyjs::hide("plot_landsat_2020")
      shinyjs::hide("plot_sentinel_2023")}

# UPDATE LEAFLET ####

    incProgress(1, detail = "Update map")
    print("update map")
    bbbb <- st_bbox(new_ws2 %>% st_transform(4326))

    if(input$watershed_source == "Water Survey of Canada Basins") {

      wsc_pp_ac <- wsc_pp %>% filter(status == "active") %>% mutate(id = stationnum)
      wsc_pp_dc <- wsc_pp %>% filter(status == "discontinued") %>% mutate(id = stationnum)

      new_leaflet <- initial_map %>%
        addCircleMarkers(data = wsc_pp_ac, lng = wsc_pp_ac$lon, lat = wsc_pp_ac$lat, color = "steelblue", radius = 3, group = "WSC Active",
                         label = paste0(wsc_pp_ac$name, " - ", wsc_pp_ac$stationnum, " [active]")) %>%
        addCircleMarkers(data = wsc_pp_dc, lng = wsc_pp_dc$lon, lat = wsc_pp_dc$lat, color = "grey", radius = 3, group = "WSC Discontinued",
                         label = paste0(wsc_pp_dc$name, " - ", wsc_pp_dc$stationnum, " [discontinued]")) %>%
        addLayersControl(baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
                         overlayGroups = c("Sentinel 2023 (slow)", "Landsat 2020-2023 (slow)", "Landsat 1985-1990 (slow)", "WSC Active", "WSC Discontinued"),
                         options = layersControlOptions(collapsed = F))
    }
    else{new_leaflet <- initial_map}



    if("Forest Disturbance" %in% input$run_modules){
      new_leaflet <- new_leaflet %>%
        addPolylines(data = dra %>% st_transform(4326), group = "Roads", fillColor = "black", color = "black", weight = 1, fillOpacity = 1, label = dra$transport_line_surface_code_desc) %>%
        addPolygons(data = my_wf %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "Fire", fillColor = "red", color = "red", weight = 2, opacity = 1, fillOpacity = 0.3, label = paste0("Fire year:", my_wf$fire_year)) %>%
        addPolygons(data = my_cb %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "Cutblock", fillColor = "darkgreen", color = "darkgreen", weight = 1, fillOpacity = 0.3, label = paste("Harvest year:", my_cb$harvest_year))}

    # if("CEF Human Disturbance - 2021" %in% input$run_modules){
    #   new_leaflet <- new_leaflet %>%
    #     addPolygons(data = bc_cef_2021 %>% filter(cef_human_disturb_flag == "Natural Landbase") %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "Fire", fillColor = "red", color = "red", weight = 2, opacity = 1, fillOpacity = 0.3, label = paste0("Fire year:", my_wf$fire_year)) %>%
    #     addPolygons(data = my_cb %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "Cutblock", fillColor = "darkgreen", color = "darkgreen", weight = 1, fillOpacity = 0.3, label = paste("Harvest year:", my_cb$harvest_year))}
    #
    # bc_cef_2021

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

  # shinyjs::show('export_pdf')

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

# # EXPORT PDF ####
# output$export_pdf <- downloadHandler(
#   filename = "report.pdf",
#   content = function(file) {
#     tempReport <- file.path(tempdir(), "report.Rmd")
#     file.copy("report.Rmd", tempReport, overwrite = TRUE)
#     params <- list(n = 12)
#     rmarkdown::render(output_yaml = tempReport,
#                       output_file = "report.pdf",
#                       params = params,
#                       envir = new.env(parent = globalenv())
#     )
#   }
# )

