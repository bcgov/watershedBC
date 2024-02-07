res <- 300

wsc_pp_ac <- wsc_pp %>% filter(status == "active") %>% mutate(id = stationnum)
wsc_pp_dc <- wsc_pp %>% filter(status == "discontinued") %>% mutate(id = stationnum)

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
  addCircleMarkers(data = wsc_pp_ac, lng = wsc_pp_ac$lon, lat = wsc_pp_ac$lat, color = "steelblue", radius = 3, group = "WSC Active",
                   label = paste0(wsc_pp_ac$name, " - ", wsc_pp_ac$stationnum, " [active]")) %>%
  addCircleMarkers(data = wsc_pp_dc, lng = wsc_pp_dc$lon, lat = wsc_pp_dc$lat, color = "grey", radius = 3, group = "WSC Discontinued",
                   label = paste0(wsc_pp_dc$name, " - ", wsc_pp_dc$stationnum, " [discontinued]")) %>%
  addLayersControl(baseGroups = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
                   overlayGroups = c("Sentinel 2023 (slow)", "Landsat 2020-2023 (slow)", "Landsat 1985-1990 (slow)", "WSC Active", "WSC Discontinued"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("Sentinel 2023 (slow)","Landsat 1985-1990 (slow)","Landsat 2020-2023 (slow)","WSC Active","WSC Discontinued")) %>%
  addMouseCoordinates()
