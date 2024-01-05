rm(list = ls())

# LOAD LIBRARIES ###############################################################

# DATABASE
library(DBI)
library(RPostgreSQL)
library(pool)

# DATA
library(bcmaps)
library(bcdata)
library(elevatr)

# UTILITIES
library(tictoc)

# SPATIAL
library(sf)
library(terra)
library(rmapshaper)

# PLOTS
library(ggplot2)
library(plotly)

# SHINY
library(shiny)
library(shinycssloaders)

# LEAFLET
library(leaflet)
library(leafem)
library(leafgl)

#WRANGLING
library(tidyr)
library(dplyr)
library(stringr)

# CONNECT TO DATABASE ##########################################################

refresh <- function() {
  dbPool(
    drv = RPostgres::dbDriver("PostgreSQL"),
    dbname = Sys.getenv("aw_dbname"),
    host = Sys.getenv("aw_host"),
    port = Sys.getenv("aw_port"),
    user = Sys.getenv("aw_user"),
    password = Sys.getenv("aw_password")
  )
}

conn <- refresh()

# LOAD BASELINE DATSETS ########################################################

names <- readRDS("named.RDS")

# FUNCTIONS ####################################################################

postgis_get_pol <- function(to_clip = "fwa_named", to_clip_cols_to_keep = "*", elev = T, my_wkt = new_ws2_wkt, min_area_km2 = 0.01) {
  # to_clip = "fwa_wetlands"
  # to_clip_cols_to_keep = "waterbody_type"
  # my_wkt = new_ws2_wkt

    q <- paste0(
      "SELECT w.*,
              ST_Intersection(w.geom,ST_GeomFromText('",my_wkt,"', 3005)) AS geom
      FROM ", to_clip, " w
      WHERE ST_Intersects(w.geom,ST_GeomFromText('",my_wkt,"', 3005))")

    o <-  st_read(conn, query = q)

    if (nrow(o) > 0) {
      o <-
        o %>% filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
        ms_explode() %>%
        mutate(clipped_area_m2 = as.numeric(st_area(.))) %>%
        filter(clipped_area_m2 > min_area_km2*(1000*1000))
      if (nrow(o) > 0) {
        if (elev == T) {
          o <- o %>% bind_cols( elevatr::get_aws_points(o %>% st_centroid(), verbose = FALSE)[[1]] %>% st_drop_geometry() %>% dplyr::select(elevation))
          }
          o <- o  %>% st_buffer(1) %>% st_cast("POLYGON")}}

    if (nrow(o) > 0) {
      o <- o %>%
        filter(clipped_area_m2 > 10) %>%
        mutate(type = to_clip) %>%
        arrange(-clipped_area_m2)
      return(o)
    } else{
      o <- st_as_sf(data.frame(lon = -111, lat = 55), coords = c("lon", "lat"), crs = 4326) %>% mutate(type = "test") %>% filter(type != "test")
    }
    return(o)
  }

# BASEMAP ######################################################################

res <- 300

initial_map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery") %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "WorldTopoMap") %>%
  addWMSTiles("http://maps.gov.bc.ca/arcserver/rest/services/province/roads_wm/MapServer/tile/{z}/{y}/{x}", layers = "GRB_BSK", options = WMSTileOptions(format = "image/png", transparent = TRUE), group = "BC Basemap") %>%
  addMeasure(primaryLengthUnit = "kilometers", secondaryLengthUnit = "meters", primaryAreaUnit = "hectares", secondaryAreaUnit = "sqmeters", position = "topleft") %>%
  leafem:::addCOG(url = "https://bcbasin.s3.ca-central-1.amazonaws.com/BC_2023v2_4326_v2_bigTiff_JPEG.tif", group = "Sentinel 2023 (slow)", resolution = res, opacity = 1, autozoom = F) %>%
  leafem:::addCOG(url = "https://bcbasin.s3.ca-central-1.amazonaws.com/1985_1990v3_COG_AV_JP_BIG.tif", group = "Landsat 1985-1990 (slow)", resolution = res, opacity = 1, autozoom = F) %>%
  leafem:::addCOG(url = "https://bcbasin.s3.ca-central-1.amazonaws.com/2020_2023v3_COG_AV_JP_BIG.tif", group = "Landsat 2020-2023 (slow)", resolution = res, opacity = 1, autozoom = F) %>%
  addLayersControl(baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
                   overlayGroups = c("Sentinel 2023 (slow)", "Landsat 2020-2023 (slow)", "Landsat 1985-1990 (slow)"), options = layersControlOptions(collapsed = T)) %>%
  hideGroup(c("Sentinel 2023 (slow)","Landsat 1985-1990 (slow)","Landsat 2020-2023 (slow)")) %>%
  addMouseCoordinates()

# UI ###########################################################################

ui <- navbarPage(

  theme = "css/bcgov.css", title = "WatershedBC (v0.1 Beta Testing)",

  tabPanel(
    "WatershedBC (v0.1 Beta Testing)",

    fluidRow(
      column(width = 2,
             HTML("<b>Get started:</b><br>"),
             HTML("1 - Click anywhere in BC to get started<br>
                2 - Click 'Run Report'<br>
                3 - Be patient!"),br(),br(),
             shiny::radioButtons(
               inputId = "watershed_source",
               label = "Watershed data source",
               choices = c("Freshwater Atlas Named Watersheds",
                           "Custom Basin at Point of Interst"),
               selected = "Freshwater Atlas Named Watersheds"),
             shiny::selectizeInput(
               inputId = "psql_zoom_to_name",
               label = "Search by Name",
               choices = c("", names$name),
               selected = "",
               multiple = F),
             actionButton(inputId = "zoom_to_button", label = "Zoom to..")),

      column(width = 10,
             leafletOutput("mymap", height = '600px') %>% withSpinner(color = "steelblue"),
             h3(textOutput(outputId = "ws_selection")),
             actionButton(inputId = "run_button", label = "Run Report"),
             textOutput(outputId = "ws_run"),
             textOutput(outputId = "ws_selection_pred_time"),
             tableOutput('table_named'),
             plotlyOutput("plot_profile"),
             plotlyOutput("plot_timeseries"),
             plotlyOutput("plot_timeseries_cumsum"),
             plotlyOutput("plot_elevbins"),
             plotlyOutput("plot_fwa"),
             plotlyOutput("plot_gl"),
             plotlyOutput("plot_dra"),
             plotlyOutput("plot_mat"),
             plotlyOutput("plot_map"),
             plotlyOutput("plot_cmd"),
             plotOutput("plot_landsat_1985", width = 800, height = 800),
             plotOutput("plot_landsat_2020", width = 800, height = 800),
             plotOutput("plot_sentinel_2023", width = 800, height = 800),
             downloadButton("downloadWatershed", "Watershed"),
             downloadButton("downloadCutblocks", "Cutblocks"),
             downloadButton("downloadWildfires", "Wildfire"),
             downloadButton("downloadLakes", "FWA - Lakes"),
             downloadButton("downloadWetlands", "FWA - Wetlands"),
             downloadButton("downloadGlaciers", "FWA - Glaciers"),
             downloadButton("downloadGlaciers85", "Glaciers 1985"),
             downloadButton("downloadGlaciers21", "Glaciers 2021"),
             downloadButton("downloadRoads", "Roads"), br(), br(), br()
             )),

    fluidRow(
      column(width = 12,
             HTML("<b>Data sources:</b> Freshwater Atlas of BC, Consolidated Cutblocks of BC, BC Wildfire Service Fire Perimeters, Landsat, and Sentinel-2"), br(),
             HTML("<b>Known issues:</b> Data is not accurate across provincial, territorial, national borders."), br(),
             HTML("This tool is provided with <b>no guarantees of reliability or accuracy</b>, please scrutinize the results."), br(),
             HTML("Please contact <i>alexandre.bevington@gov.bc.ca</i> with any questions or comments about this tool."), br(),
             HTML("More info at: <a href='https://github.com/bcgov/watershedBC/'>https://github.com/bcgov/watershedBC/"), br())),

    fluidRow(
      column(width = 12,
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
                   tags$li(a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style ="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                 )))))))

# SERVER #######################################################################
server <- function(input, output, session) {

  # SESSION INFO
  session_start <- Sys.time()
  session_token <- session$token

  # BASEMAP
  output$mymap <- renderLeaflet({initial_map %>% fitBounds(-139.01451,47.68300,-110.48408,59.99974)})

  # CREATE REACTIVE VAL FOR WATERSHED
  new_ws <- reactiveVal()
  basin_source <- reactiveVal()

  # OPTION 1: CLICK MAP TO SELECT WATERSHED ####################################
  observeEvent(input$mymap_click, {

    tic(quiet = T)
    withProgress(message = 'Finding watershed...', max = 1,  {
      point <- input$mymap_click
      # point <- data.frame(lat=51.1888118537981, lng=-121.346066558124)
      # point <- data.frame(lat=52.9536023000285, lng=-125.065826398538)
      # point <- data.frame(lat=54.9220840082985, lng=-128.177715830218)
      print(paste0("point <- data.frame(lat=", point$lat, ", lng=", point$lng,")"))

      if (input$watershed_source == "Freshwater Atlas Named Watersheds") {
        basin_source("FWA")
        new_ws(st_read(conn,query = paste0(
          "SELECT * FROM fwa_named
               WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",point$lng,",",point$lat,"), 4326),3005))
               ORDER BY area_m2 ASC LIMIT 1")) %>%
            mutate(area_km2 = area_m2 /(1000 * 1000)))}

      if (input$watershed_source == "Custom Basin at Point of Interst") {
        basin_source("basinsv4")
        new_ws(st_read(conn, query = paste0(
          "SELECT * FROM basinsv4
               WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",point$lng,",",point$lat,"), 4326),3005))
               ORDER BY area_m2 ASC LIMIT 1")) %>%
            mutate(area_km2 = area_m2 / (1000 * 1000)) %>%
            rename(gnis_name = id, gnis_id = basin) %>%
            ms_simplify(keep = 0.5))
        }

      output$ws_selection <- renderText({paste0("You selected ", new_ws()$gnis_name," (",format(round(as.numeric(new_ws()$area_km2), 0), big.mark = ",") ," sq.km)")})
      output$ws_selection_pred_time <- renderText({paste0("Estimated time to run report ~ ",0.5 + round((new_ws()$area_km2 * 0.03) / 60, 1)," min")})

      bbbb <- st_bbox(new_ws() %>% st_transform(4326))
      output$mymap <- renderLeaflet({
        initial_map %>%
          addPolygons(data = new_ws() %>% st_transform(4326),fillOpacity = 0,weight = 2,color = "blue",group = "Watershed") %>%
          addLayersControl(baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
                           overlayGroups = c("Sentinel 2023 (slow)","Landsat 2020-2023 (slow)","Landsat 1985-1990 (slow)","Watershed"), options = layersControlOptions(collapsed = T)) %>%
          hideGroup(c("Sentinel 2023 (slow)","Landsat 1985-1990 (slow)","Landsat 2020-2023 (slow)")) %>%
          fitBounds(bbbb$xmin[[1]], bbbb$ymin[[1]], bbbb$xmax[[1]], bbbb$ymax[[1]])
        })
        })

    a <- toc()$callback_msg
    output$ws_run <- renderText({a})

    dbWriteTable(conn, "usage", data.frame(date_time = as.character(session_start),
                                           session_token = session_token,
                                           gnis_name = new_ws()$gnis_name,
                                           gnis_id = new_ws()$gnis_id,
                                           processing_time = a,
                                           action = "select watershed",
                                           area_km2 = round(new_ws()$area_km2,1),
                                           basin_source = basin_source()), append = TRUE)})

  # OPTION 2: ZOOM TO NAMED WATERSHED ##########################################

  observeEvent(input$zoom_to_button, {

    if(input$psql_zoom_to_name!=""){
      tic(quiet = T)
      basin_source("FWA")
      withProgress(message = 'Finding watershed...', max = 1,  {
        split_name <- strsplit(strsplit(input$psql_zoom_to_name, "id:")[[1]][2], ")")[[1]][1]
        print(input$psql_zoom_to_name)
        n <- st_read(conn, query = paste0("SELECT * FROM fwa_named WHERE gnis_id = ", split_name))
        new_ws(n %>% mutate(area_km2 = area_m2 /(1000 * 1000)))
        output$ws_selection <- renderText({paste0("You selected ", new_ws()$gnis_name," (",format(round(as.numeric(new_ws()$area_km2), 0), big.mark = ",") ," sq.km)")})
        output$ws_selection_pred_time <- renderText({paste0("Estimated time to run report ~ ",0.5 + round((new_ws()$area_km2 * 0.03) / 60, 1)," min")})

        bbbb <- st_bbox(n %>% st_transform(4326))
        output$mymap <- renderLeaflet({
          initial_map %>%
            addPolygons(data = n %>% st_transform(4326), fillOpacity = 0, weight = 2, color = "blue", group = "Watershed") %>%
            addLayersControl(baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
                             overlayGroups = c("Sentinel 2023 (slow)","Landsat 2020-2023 (slow)","Landsat 1985-1990 (slow)","Watershed"), options = layersControlOptions(collapsed = T) ) %>%
            hideGroup(c("Sentinel 2023 (slow)","Landsat 1985-1990 (slow)","Landsat 2020-2023 (slow)")) %>%
            fitBounds(bbbb$xmin[[1]], bbbb$ymin[[1]], bbbb$xmax[[1]], bbbb$ymax[[1]])
        })
      })

    a <- toc()$callback_msg
    output$ws_run <- renderText({a})

    dbWriteTable(conn, "usage", data.frame(date_time = as.character(session_start),
                                           session_token = session_token,
                                           gnis_name = new_ws()$gnis_name,
                                           gnis_id = new_ws()$gnis_id,
                                           processing_time = a,
                                           action = "select watershed",
                                           area_km2 = round(new_ws()$area_km2,1),
                                           basin_source = basin_source()), append = TRUE)

    }
    })

  # RUN REPORT #################################################################

  observeEvent(input$run_button, {

    # output$all_plots <- renderUI({})

    new_ws2 <- new_ws()
    # new_ws2 <- st_read(conn, query = "SELECT * FROM fwa_named WHERE gnis_name = 'Bowron River'") %>% mutate(area_km2 = area_m2/(1000*1000))
    # new_ws2 <- st_read(conn, query = "SELECT * FROM fwa_named WHERE gnis_name = 'McMillan Creek'") %>% mutate(area_km2 = area_m2/(1000*1000))
    # new_ws2 <- st_read(conn, query = paste0("SELECT * FROM basinsv4 WHERE id = 360981")) %>% rename(gnis_name = basin, gnis_id = id) %>% mutate(area_km2 = area_m2/(1000*1000))

if (new_ws2$area_km2 > 15000) {

  output$ws_run <- renderText({"Watershed is too large... please select a smaller watershed"})}

    if (new_ws2$area_km2 < 15000) {

      tic()
      new_ws2_wkt <- st_as_text(st_geometry(new_ws2 %>% ms_explode()))

      withProgress(message = 'Processing...', max = 15,  {

        ## NAMED WATERSHEDS ####
        incProgress(1, detail = paste0("Get watersheds (", round(new_ws2$area_km2, 0), ")"))
        print("getting watershed")
        my_named <- postgis_get_pol("fwa_named","*",elev = F,my_wkt = new_ws2_wkt, min_area_km2 = 5) %>%
          mutate(area_km2 = area_m2 / (1000 * 1000)) %>%
          dplyr::select(gnis_name, area_km2) %>%
          arrange(-area_km2) %>%
          filter(area_km2 > new_ws2$area_km2 * 0.05) %>%
          mutate(
            Location = case_when(
              gnis_name == new_ws2$gnis_name ~ "Watershed of Interest",
              area_km2 > new_ws2$area_km2 ~ "Downstream",
              area_km2 < new_ws2$area_km2 ~ "Upstream",
              TRUE ~ ""))

        output$table_named <- renderTable(
          my_named %>% st_drop_geometry() %>%
            mutate(area_km2 = round(area_km2, 1)) %>%
            rename(Name = gnis_name,
                   Area_km2 = area_km2)
          , digits = 2)

        # FRESHWATER ATLAS ####

        incProgress(1, detail = "Get Wetlands")
        print("getting wetlands")
        my_wl <- postgis_get_pol("fwa_wetlands","waterbody_type",my_wkt = new_ws2_wkt)
        if(nrow(my_wl) == 0) {my_wl <- st_as_sf(data.frame(clipped_area_m2 = 0,waterbody_type = "",elevation = 0,area_m2 = 0,lat = 0,long = 0,type = "fwa_wetlands"), coords = c("long", "lat"), crs = 3005)}

        incProgress(1, detail = "Get Lakes")
        print("getting lakes")
        my_lk <- postgis_get_pol("fwa_lakes","waterbody_type",my_wkt = new_ws2_wkt)
        if(nrow(my_lk) == 0) {my_lk <- st_as_sf(data.frame(clipped_area_m2 = 0,waterbody_type = "",elevation = 0,area_m2 = 0,lat = 0,long = 0,type = "fwa_lakes"), coords = c("long", "lat"), crs = 3005)}

        incProgress(1, detail = "Get Glaciers")
        print("getting glaciers")
        my_gl <- postgis_get_pol("fwa_glaciers","waterbody_type",my_wkt = new_ws2_wkt)
        if(nrow(my_gl) == 0) {my_gl <- st_as_sf(data.frame(clipped_area_m2 = 0,waterbody_type = "",elevation = 0,area_m2 = 0,lat = 0,long = 0,type = "fwa_glaciers"), coords = c("long", "lat"), crs = 3005)}
        my_gl_1985 <- bcdata::bcdc_query_geodata("historical-glaciers") %>% select(GBA_GLHIST_SYSID, GLACIER_ID, SOURCE_YEAR, FEATURE_AREA_SQM) %>% filter(INTERSECTS(new_ws2)) %>% collect()
        if(nrow(my_gl_1985)>0){my_gl_1985 <- my_gl_1985 %>% st_intersection(new_ws2) %>%
          mutate(FEATURE_AREA_SQM = as.numeric(st_area(.)))}else{my_gl_1985 <- st_as_sf(data.frame(GLACIER_ID = 0, FEATURE_AREA_SQM = 0,lat = 0,long = 0,SOURCE_YEAR = 1985), coords = c("long", "lat"), crs = 3005)}
        my_gl_2021 <- bcdata::bcdc_query_geodata("glaciers") %>% select(GLACIER_ID, SOURCE_YEAR, FEATURE_AREA_SQM) %>% filter(INTERSECTS(new_ws2)) %>% collect()
        if(nrow(my_gl_2021)>0){my_gl_2021 <- my_gl_2021 %>% st_intersection(new_ws2) %>%
          mutate(FEATURE_AREA_SQM = as.numeric(st_area(.)))}else{my_gl_2021 <- st_as_sf(data.frame(GLACIER_ID = 0, FEATURE_AREA_SQM = 0,lat = 0,long = 0,SOURCE_YEAR = 2021), coords = c("long", "lat"), crs = 3005)}

      output$plot_gl <- renderPlotly({
          ggplotly(bind_rows(my_gl_1985 %>% st_drop_geometry(),
                             my_gl_2021 %>% st_drop_geometry()) %>%
                     pivot_wider(id_cols = GLACIER_ID, names_from = SOURCE_YEAR, values_from = FEATURE_AREA_SQM, names_prefix = "gl_") %>%
                     mutate(diff = gl_2021-gl_1985) %>%
                     mutate(`Glacier Area Percent Change` = round(100*(diff/(1000*1000))/(gl_1985/(1000*1000)),1),
                            `Glacier Area Change (km2)` = round(diff/(1000*1000),1),
                            `Glacier Area in 1985 (km2)` = round(gl_1985/(1000*1000),1)) %>%
                     ggplot() +
                     theme_bw() +
                     geom_point(aes(`Glacier Area in 1985 (km2)`,`Glacier Area Percent Change`, group = `Glacier Area Change (km2)`)) +
                     labs(x = "Glacier Area in 1985 (km2)", y = "Glacier Area Percent \nChange from 1985-2021",
                          title = paste0("Glacier Area Change"," [1985=", round(sum(my_gl_1985$FEATURE_AREA_SQM)/(1000*1000),1),
                                                                "km2, 2021=",round(sum(my_gl_2021$FEATURE_AREA_SQM)/(1000*1000),1), "km2]")),
                   dynamicTicks = T, width = 600, height = 300, tooltip = c("GLACIER_ID","x","Glacier Area Change (km2)","y"))
        })


        print("merge fwa")
        my_fwa <- bind_rows(data.frame(waterbody_type  = c("W", "L", "G"), area_m2 = c(0, 0, 0) ),
                            my_wl %>% st_drop_geometry(),
                            my_lk %>% st_drop_geometry(),
                            my_gl %>% st_drop_geometry()) %>%
          st_drop_geometry() %>%
          mutate(type = case_when(type == "fwa_wetlands" ~ "Wetland",
                                  type == "fwa_glaciers" ~ "Glacier",
                                  type == "fwa_lakes" ~ "Lake"))

        print("plot fwa")
        output$plot_fwa <- renderPlotly({
          ggplotly(my_fwa %>%
                     group_by(type) %>%
                     dplyr::summarize(area_km2 = sum(clipped_area_m2, na.rm = T)/(1000*1000)) %>%
                     filter(!is.na(type)) %>%
                     ggplot() +
                      geom_col(aes(type, area_km2, fill = type), color = "black") +
                     theme_bw() +
                     labs(x = "", y = "Area sq.km", title = "Freshwater Atlas") +
                     scale_fill_manual(values = c("grey", "steelblue", "yellow")) +
                     scale_y_continuous(n.breaks = 10),
                   dynamicTicks = T, width = 600, height = 300)
          })

        # ROADS ####
        incProgress(1, detail = "Get Roads")
        print("getting dra")
        dra <- st_read(conn, query = paste0("SELECT w.*,
                                             ST_SimplifyPreserveTopology(ST_Intersection(w.geom,n.geom),5) AS geom
                                             FROM dra w
                                             JOIN fwa_named n
                                             ON ST_Intersects(w.geom,n.geom) WHERE n.gnis_id = '",new_ws2$gnis_id,"'"))

        if(nrow(dra) > 0) {dra <- dra  %>% st_make_valid() %>% mutate(length_km = as.numeric(st_length(.)) / 1000) %>% mutate(type = "dra", group = transport_line_surface_code_desc)
          }else{dra <- st_as_sf( data.frame(lon = -111, lat = 55, length_km = 0), coords = c("lon", "lat"), crs = 4326 ) %>% mutate(type = "test",group = "test") %>% filter(type != "test") }

        output$plot_dra <- renderPlotly({
          ggplotly(
            dra %>% st_drop_geometry() %>%
              select(group, length_km) %>%
              group_by(group) %>%
              dplyr::summarize(length_km = sum(length_km)) %>%
              ggplot() +
              geom_col(aes(group, length_km))    +
              theme_bw() +
              labs(x = "", y = "Length km", title = "Total Road Length by Surface Type", fill = "") +
              scale_y_continuous(n.breaks = 10) +
              coord_flip()
            , dynamicTicks = T, width = 600, height = 300)
        })

        # FORESTS ####
        incProgress(1, detail = "Get Wildfires")
        print("getting fires")
        my_wf <- postgis_get_pol("fire", "fire_year",my_wkt = new_ws2_wkt)
        if (nrow(my_wf) == 0) {my_wf <- st_as_sf(data.frame(clipped_area_m2 = 0,fire_year = NA_integer_,elevation = 0,area_m2 = 0,lat = 0,long = 0,type = "fire"),coords = c("long", "lat"),crs = 3005)}

        incProgress(1, detail = "Get Cutblocks")
        print("getting blocks")
        my_cb <- postgis_get_pol("cutblocks", "harvest_year",my_wkt = new_ws2_wkt)
        if (nrow(my_cb) == 0) {my_cb <- st_as_sf(data.frame(clipped_area_m2 = 0,harvest_year = NA_integer_,elevation = 0,area_m2 = 0,lat = 0,long = 0,type = "cutblock"), coords = c("long", "lat"), crs = 3005)}

        output$plot_timeseries <- renderPlotly({
          ggplotly(
            bind_rows(
              my_wf %>% st_drop_geometry() %>% rename(year = fire_year) %>% dplyr::select(clipped_area_m2, year) %>% mutate(type = "wildfire"),
              my_cb %>% st_drop_geometry() %>% rename(year = harvest_year) %>% dplyr::select(clipped_area_m2, year) %>% mutate(type = "cutblock"),
              data.frame(year = seq(1900, lubridate::year(Sys.Date()), 1),
                         clipped_area_m2 = 0, type = "cutblock"),
              data.frame(year = seq(1900, lubridate::year(Sys.Date()), 1),
                         clipped_area_m2 = 0,
                         type = "wildfire")) %>%
              group_by(type, year) %>%
              dplyr::summarize(area_km2 = sum(clipped_area_m2) / (1000 * 1000)) %>%
              ggplot() +
              geom_col(aes(year, area_km2, fill = type)) +
              theme_bw() +
              labs(x = "", y = "Area sq.km", title = "Forest Disturbance History", fill = "") +
              scale_fill_manual(values = c("darkgreen", "orange")) +
              scale_y_continuous(n.breaks = 10) +
              scale_x_continuous(n.breaks = 10),
            dynamicTicks = T, width = 600, height = 300)
          })

        output$plot_timeseries_cumsum <- renderPlotly({
          a <- bind_rows(
            my_wf %>% st_drop_geometry() %>% rename(year = fire_year) %>% dplyr::select(clipped_area_m2, year) %>% mutate(type = "wildfire"),
            my_cb %>% st_drop_geometry() %>% rename(year = harvest_year) %>% dplyr::select(clipped_area_m2, year) %>% mutate(type = "cutblock"))
          ggplotly(
            a %>%
              filter(!is.na(year)) %>%
              bind_rows(data.frame(year = seq(1900, lubridate::year(Sys.Date()), 1),
                                   clipped_area_m2 = 0,
                                   type = "cutblock"),
                        data.frame(year = seq(1900, lubridate::year(Sys.Date()), 1),
                                   clipped_area_m2 = 0,
                                   type = "wildfire")) %>%
              group_by(type, year) %>%
              dplyr::summarize(area_km2 = sum(clipped_area_m2) / (1000 * 1000)) %>%
              arrange(year) %>%
              mutate(cumsum = cumsum(area_km2)) %>%
              ggplot() +
              geom_col(aes(year, 100 * (cumsum / new_ws2$area_km2), fill = type)) +
              theme_bw() +
              labs(x = "", y = "Percent of Watershed (%)", title = "Cumulative Forest Disturbance History", fill = "") +
              scale_fill_manual(values = c("darkgreen", "orange")) +
              scale_y_continuous(n.breaks = 10) +
              scale_x_continuous(n.breaks = 10),
            dynamicTicks = T, width = 600, height = 300)
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
              my_cb %>% st_drop_geometry() %>% dplyr::select(type, clipped_area_m2, elevation)) %>%
              as_tibble() %>%
              mutate(bin = cut(elevation, breaks = seq(0, 5000, 100), labels = seq(0, 4900, 100))) %>%
              group_by(type, bin) %>%
              dplyr::summarize(area_km2 = sum(clipped_area_m2/(1000*1000))) %>%
              mutate(bin = as.numeric(as.character(bin))) %>%
              mutate(bin = case_when(is.na(bin) ~ median(.$bin, na.rm = T),
                                     TRUE ~ bin)) %>%
              ggplot() + geom_col(aes(bin, area_km2, fill = type)) +
              labs(x = "Centroid elevation", y = "Area sq.km", title = "Centroid Elevation") +
              theme_bw() +
              coord_flip() +
              scale_fill_manual(values = c("darkgreen", "orange", "grey", "steelblue", "yellow")),
            width = 600, height = 400, dynamicTicks = T)
        })

        # STREAM PROFILE ####

        incProgress(1, detail = paste0("Getting Stream Profile"))
        print("getting profile")
        # GET NETWORK

        if(new_ws2$area_km2 < 1000){
          SO <- 1}else{SO <- 2}

        my_stream_network <- bcdc_query_geodata("freshwater-atlas-stream-network") %>%
          filter(INTERSECTS(new_ws2)) %>%
          filter(STREAM_ORDER > SO) %>%
          filter(!is.na(GNIS_NAME)) %>%
          collect()

        my_stream_network_l <- my_stream_network %>% st_intersection(new_ws2)
        my_stream_network_l <- my_stream_network_l %>% group_by(GNIS_NAME) %>% summarize() %>% mutate(length_km = round(as.numeric(st_length(.)/(1000)), 1)) %>% st_drop_geometry()


        if(nrow(my_stream_network) > 0){

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

          my_stream_network_pt <-
            my_stream_network_pt[seq(1, nrow(my_stream_network_pt), 10),]

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
            filter(BLUE_LINE_KEY == max_magnitude[1,]$BLUE_LINE_KEY) %>%
            mutate(dist_seg_m = replace_na(as.numeric(
              st_distance(geometry, lag(geometry), by_element = TRUE)
            ), 0),
            dist_tot_m = cumsum(dist_seg_m))

          if (nrow(max_magnitude) > 1) {
            my_stream_network_tribs = do.call(bind_rows,
                                              lapply(2:nrow(max_magnitude), function(i = 2) {

                                                # GET TRIB AND CALCULATE DISTANCE
                                                my_stream_network_trib_pt <-
                                                  my_stream_network_pt %>%
                                                  filter(BLUE_LINE_KEY == max_magnitude[i,]$BLUE_LINE_KEY) %>%
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

                                                offset = candidates[1,]$dist_tot_m

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
              mutate(label = paste0(GNIS_NAME, " (", round(length_km,1)," km)"))

            t$label  <- with(t, reorder(label, -length_km))

            ggplotly(ggplot(t) +
                       geom_line(aes(dist_tot_m / 1000, Z, color = label)) +
                       theme_bw() +
                       labs(x = "Distance along stream km", y = "Elevation m a.s.l.", color = "Name", title = "Stream Profile") ,
              dynamicTicks = T, width = 600, height = 300)
          })
        }

        # climate ####
        incProgress(1, detail = "Get climateBC")

        climateBC <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/climateBC.tif",
                                 win = terra::ext(new_ws2 %>% st_transform(4326)))
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

        output$plot_mat <- renderPlotly({
          ggplotly(
            climateBC_pts_dat %>%
              filter(parameter == "Mean Annual Temperature") %>%
              filter(model %in% c("Historic", "13_GCMs")) %>%
              group_by(period, ssp, year) %>%
              dplyr::summarize(min = quantile(value, probs = 0.25, na.rm = T),
                        mean = mean(value, na.rm = T),
                        max = quantile(value, probs = 0.75, na.rm = T)) %>%
              ggplot() +
              geom_rect(aes(xmin = year - 14, xmax = year + 15, ymin = min, ymax = max, color = ssp, fill = ssp, group = period), alpha = 0.2) +
              labs(x = "Climate Normal Period", y = "Mean Annual Temperature") +
              theme_bw()
            ,
            dynamicTicks = T, width = 600, height = 300)
        })
        output$plot_map <- renderPlotly({
          ggplotly(
            climateBC_pts_dat %>%
              filter(parameter == "Mean Annual Precipitation") %>%
              filter(model %in% c("Historic", "13_GCMs")) %>%
              group_by(period, ssp, year) %>%
              dplyr::summarize(min = quantile(value, probs = 0.25, na.rm = T),
                        mean = mean(value, na.rm = T),
                        max = quantile(value, probs = 0.75, na.rm = T)) %>%
              ggplot() +
              geom_rect(aes(xmin = year - 14, xmax = year + 15, ymin = min, ymax = max, color = ssp, fill = ssp, group = period), alpha = 0.2) +
              labs(x = "Climate Normal Period", y = "Mean Annual Precipitation") +
              theme_bw(),
            dynamicTicks = T, width = 600, height = 300)
        })
        output$plot_cmd <-  renderPlotly({
          ggplotly(
            climateBC_pts_dat %>%
              filter(parameter == "Cumulative Moisture Deficit") %>%
              filter(model %in% c("Historic", "13_GCMs")) %>%
              group_by(period, ssp, year) %>%
              dplyr::summarize(min = quantile(value, probs = 0.25, na.rm = T),
                        mean = mean(value, na.rm = T),
                        max = quantile(value, probs = 0.75, na.rm = T)) %>%
              ggplot() +
              geom_rect(aes(xmin = year - 14, xmax = year + 15, ymin = min, ymax = max, color = ssp, fill = ssp, group = period), alpha = 0.2) +
              labs(x = "Climate Normal Period", y = "Cumulative Moisture Deficit") +
              theme_bw(),
            dynamicTicks = T, width = 600, height = 300)
        })

        ## SATELLITE IMAGERY ####
        incProgress(1, detail = "Update Satellite Imagery")

        v <- vect(new_ws2 %>% st_transform(4326))

        output$plot_landsat_1985 <- renderPlot({
          r1985 <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/1985_1990v3_COG_AV_JP_BIG.tif", win = terra::ext(new_ws2 %>% st_transform(4326)))
          plot(r1985, main = "Landsat 1985-1990", mar = 2)
          plot(v, add = T, border = "red", lwd = 2)
          }, height = 800, width = 800)

        output$plot_landsat_2020 <- renderPlot({
          r2020 <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/2020_2023v3_COG_AV_JP_BIG.tif", win = terra::ext(new_ws2 %>% st_transform(4326)))
          plot(r2020, main = "Landsat 2020-2023", mar = 2)
          plot(v, add = T, border = "red", lwd = 2)
          }, height = 800, width = 800)

        output$plot_sentinel_2023 <- renderPlot({
          r2023 <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/BC_2023v2_4326_v2_bigTiff_JPEG.tif", win = terra::ext(new_ws2 %>% st_transform(4326)))
          plot(r2023, main = "Sentinel Mosaic 2023", mar = 2)
          plot(v, add = T, border = "red", lwd = 2)
          }, height = 800, width = 800)

        # UPDATE LEAFLET ####
        incProgress(1, detail = "Update map")

        bbbb <- st_bbox(new_ws2 %>% st_transform(4326))
        output$mymap <- renderLeaflet({
          initial_map %>%
            addPolygons(data = new_ws2 %>% st_transform(4326), fillOpacity = 0, weight = 2, color = "blue") %>%
            addPolylines(data = dra %>% st_transform(4326), group = "Roads", fillColor = "black", color = "black", weight = 1, fillOpacity = 1, label = dra$transport_line_surface_code_desc) %>%
            addPolygons(data = my_wl %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "FWA Wetland", fillColor = "yellow", color = "pink", weight = 1, fillOpacity = 0.3, label = my_wl$waterbody_type) %>%
            addPolygons(data = my_lk %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "FWA Lake", fillColor = "steelblue", color = "steelblue", weight = 1, fillOpacity = 1, label = my_lk$waterbody_type) %>%
            addPolygons(data = my_gl %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "FWA Glacier", fillColor = "grey", color = "grey", weight = 1, fillOpacity = 0.3, label = my_gl$waterbody_type) %>%
            addPolygons(data = my_gl_1985 %>% filter(FEATURE_AREA_SQM > 0) %>% st_transform(4326), group = "Glacier 1985", fillColor = "brown", color = "brown", weight = 1, fillOpacity = 0.3, label = my_gl_1985$SOURCE_YEAR) %>%
            addPolygons(data = my_gl_2021 %>% filter(FEATURE_AREA_SQM > 0) %>% st_transform(4326), group = "Glacier 2021", fillColor = "blue", color = "blue", weight = 1, fillOpacity = 0.3, label = my_gl_1985$SOURCE_YEAR) %>%
            addPolygons(data = my_wf %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "Fire", fillColor = "red", color = "red", weight = 2, opacity = 1, fillOpacity = 0.3, label = paste0("Fire year:", my_wf$fire_year)) %>%
            addPolygons(data = my_cb %>% filter(clipped_area_m2 > 0) %>% st_transform(4326), group = "Cutblock", fillColor = "darkgreen", color = "darkgreen", weight = 1, fillOpacity = 0.3, label = paste("Harvest year:", my_cb$harvest_year)) %>%
            addLayersControl(baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
                             overlayGroups = c("Sentinel 2023 (slow)", "Landsat 2020-2023 (slow)", "Landsat 1985-1990 (slow)", "FWA Wetland", "FWA Lake", "FWA Glacier","Glacier 1985","Glacier 2021", "Fire", "Cutblock", "Roads"),
                             options = layersControlOptions(collapsed = T)) %>%
            hideGroup(c("Roads")) %>%
            hideGroup(c("Sentinel 2023 (slow)","Landsat 1985-1990 (slow)","Landsat 2020-2023 (slow)")) %>%
            fitBounds(bbbb$xmin[[1]], bbbb$ymin[[1]], bbbb$xmax[[1]], bbbb$ymax[[1]]) %>%
            addLegend("bottomright",
                      colors = c("yellow","steelblue","grey","brown","blue","red","darkgreen"),
                      labels = c("FWA Wetland", "FWA Lake", "FWA Glacier","Glacier 1985","Glacier 2021","Wildfire","Cutblock"),
                      title = "",
                      opacity = 1)
          })

        ## DOWNLOAD BUTTONS ####
        output$downloadWatershed <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name),"_",new_ws2$gnis_id,"_watershed.sqlite")},content = function(file) {st_write(my_wl, file)})
        output$downloadCutblocks <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name),"_",new_ws2$gnis_id,"_cutblocks.sqlite")},content = function(file) {st_write(my_wl, file)})
        output$downloadWildfires <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name),"_",new_ws2$gnis_id,"_wildfires.sqlite")},content = function(file) {st_write(my_wl, file)})
        output$downloadLakes <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name),"_",new_ws2$gnis_id,"_lakes.sqlite")},content = function(file) {st_write(my_wl, file)})
        output$downloadWetlands <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name),"_",new_ws2$gnis_id,"_wetlands.sqlite")},content = function(file) {st_write(my_wl, file)})
        output$downloadGlaciers <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name),"_",new_ws2$gnis_id,"_glaciers.sqlite")},content = function(file) {st_write(my_gl, file)})
        output$downloadGlaciers85 <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name),"_",new_ws2$gnis_id,"_glaciers_1985.sqlite")},content = function(file) {st_write(my_gl_1985, file)})
        output$downloadGlaciers21 <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name),"_",new_ws2$gnis_id,"_glaciers_2021.sqlite")},content = function(file) {st_write(my_gl_2021, file)})
        output$downloadRoads <- downloadHandler(filename = function() {paste0(gsub(" ", "-", new_ws2$gnis_name),"_",new_ws2$gnis_id,"_roads.sqlite")}, content = function(file) {st_write(dra, file)})

        ## TIME ####
        time <-paste0(round(as.numeric(as.numeric(strsplit(toc()$callback_msg, " ")[[1]][1]) / 60), 1), " minutes elapsed")
        output$ws_run <- renderText({time})
        output$ws_selection_pred_time <- renderText({"Processing complete! "})

        dbWriteTable(conn, "usage",
                     data.frame(date_time = as.character(session_start),
                                session_token = session_token,
                                gnis_name = new_ws()$gnis_name,
                                gnis_id = new_ws()$gnis_id,
                                processing_time = time,
                                action = "processing",
                                area_km2 = round(new_ws()$area_km2,1),
                                basin_source = basin_source()), append = TRUE)
      })
    }

    # output$all_plots <- renderUI({
    #   if(new_ws2$area_km2 > 0)
    #     fluidRow(
    #       tableOutput('table_named'),
    #       plotlyOutput("plot_profile"),
    #       plotlyOutput("plot_timeseries"),
    #       plotlyOutput("plot_timeseries_cumsum"),
    #       plotlyOutput("plot_elevbins"),
    #       plotlyOutput("plot_fwa"),
    #       plotlyOutput("plot_dra"),
    #       plotlyOutput("plot_mat"),
    #       plotlyOutput("plot_map"),
    #       plotlyOutput("plot_cmd"),
    #       plotOutput("plot_landsat_1985", width = 800, height = 800),
    #       plotOutput("plot_landsat_2020", width = 800, height = 800),
    #       plotOutput("plot_sentinel_2023", width = 800, height = 800)),
    #       downloadButton("downloadWatershed", "Watershed"),
    #       downloadButton("downloadCutblocks", "Cutblocks"),
    #       downloadButton("downloadWildfires", "Wildfire"),
    #       downloadButton("downloadLakes", "Lakes"),
    #       downloadButton("downloadWetlands", "Wetlands"),
    #       downloadButton("downloadGlaciers", "Glaciers"),
    #       downloadButton("downloadRoads", "Roads"), br(), br(), br()
    #   else
    #     tableOutput('table_named')
    # })

  })
  }

shinyApp(ui, server)

# CLOSE ALL DATABASE CONNECTIONS ###############################################

# length(DBI::dbListConnections(RPostgres::dbDriver("PostgreSQL")))
# for(i in DBI::dbListConnections(RPostgres::dbDriver("PostgreSQL"))){
#   print(i)
#   DBI::dbDisconnect(i)}
# pool::poolClose(conn)
