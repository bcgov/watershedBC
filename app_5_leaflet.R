res <- 300

initial_map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery") %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "WorldTopoMap") %>%
  addWMSTiles("http://maps.gov.bc.ca/arcserver/rest/services/province/roads_wm/MapServer/tile/{z}/{y}/{x}",
              layers = "GRB_BSK",
              options = WMSTileOptions(format = "image/png", transparent = TRUE),
              group = "BC Basemap") %>%
  addMeasure(primaryLengthUnit = "kilometers",
             secondaryLengthUnit = "meters",
             primaryAreaUnit = "hectares",
             secondaryAreaUnit = "sqmeters",
             position = "topleft") %>%
  leafem:::addCOG(url = "https://bcbasin.s3.ca-central-1.amazonaws.com/BC_2023v2_4326_v2_bigTiff_JPEG.tif",
                  group = "Sentinel 2023 (slow)",
                  resolution = res,
                  opacity = 1,
                  autozoom = F) %>%
  leafem:::addCOG(url = "https://bcbasin.s3.ca-central-1.amazonaws.com/1985_1990v3_COG_AV_JP_BIG.tif",
                  group = "Landsat 1985-1990 (slow)",
                  resolution = res,
                  opacity = 1,
                  autozoom = F) %>%
  leafem:::addCOG(url = "https://bcbasin.s3.ca-central-1.amazonaws.com/2020_2023v3_COG_AV_JP_BIG.tif",
                  group = "Landsat 2020-2023 (slow)",
                  resolution = res,
                  opacity = 1,
                  autozoom = F) %>%
  addLayersControl(baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
                   overlayGroups = c("Sentinel 2023 (slow)","Landsat 2020-2023 (slow)","Landsat 1985-1990 (slow)"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("Sentinel 2023 (slow)","Landsat 1985-1990 (slow)","Landsat 2020-2023 (slow)")) %>%
  addMouseCoordinates()
