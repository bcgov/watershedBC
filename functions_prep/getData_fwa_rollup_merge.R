

# LIST ROLLED UP WATERSHEDS BY REGION (e.g. PARS.shp) ####

  l <- list.files("G:/My Drive/1_FLNRO/!_Websites/watershedBC", full.names = T, pattern = "*.shp$")

# READ AND MERGE ALL RETIONS ####

  all_ws_byRegion <- bind_rows(lapply(l, function(i){st_read(i)}))

# TEST IF WATERSHED IS BROKEN ###

  all_ws_byRegion %>% filter(iFWA == "100-190442-999098") %>% mapview::mapview()
  all_ws_byRegion %>% filter(iFWA == "100-190442") %>% mapview::mapview()
  all_ws_byRegion %>% filter(iFWA == "100") %>% mapview::mapview()

# GET UNIQUE iFWA CODES ####

  ws_iFWA_codes <- all_ws_byRegion$iFWA %>% unique

# GET POLYGON COUNT BY UNIQUE CODE ####

  ws_Broken <- all_ws_byRegion %>%
    st_drop_geometry() %>%
    group_by(iFWA) %>%
    summarize(n = n())

# IF MORE THAN 1 PER CODE, then MERGE ####

  ws_Broken_gt1 <- b_n %>% filter(n != 1) %>% pull(iFWA)

  ws_Broken_gt1_merge <- bind_rows(lapply(1:length(ws_Broken_gt1), function(i=1){
    print(100*(i/length(b_n_todo)))
    all_ws_byRegion %>% filter(iFWA == ws_Broken_gt1[i]) %>% summarize(ORDER = max(WATERSH)) %>%
      mutate(area_km2 = as.numeric(st_area(.))/(1000*1000)) %>%
      mutate(iFWA = ws_Broken_gt1[i])}))

# MERGE WATERSHEDS THAT DIDNT NEED MERGNG AND THOSE THAT DID ####

  all_ws <- bind_rows(all_ws_byRegion %>% filter(!iFWA %in% ws_Broken_gt1) %>% rename(area_km2 = are_km2, ORDER = WATERSH),
            ws_Broken_gt1_merge)


st_write(all_ws, "F:/all_ws_fwa_roll_up.gpkg")
#
# all_ws %>% nrow()
#
# bb <- b %>%
#   # filter(iFWA == "100-190442-999098") %>%
#   group_by(iFWA) %>%
#   summarise(ORDER = max(WATERSH))
#
# # fwa_list <- paste0("iFWA_",1:21)
#
# t <- lapply(fwa_list, function(fwa="iFWA_1"){
#
#   print(fwa)
#
#   all <- bind_rows(lapply(l, function(i=l[1]){
#
#     print(paste(fwa, i))
#
#     name <- st_layers(i)[[1]]
#
#     st_read(i, query = paste0("SELECT * FROM ",name," WHERE class = '",fwa,"'"), quiet = T)
#
#   }))
#
#   all <- all %>% group_by(iFWA) %>% summarize(WATERSHED_ORDER= as.character(max(WATERSHED_ORDER)))
#   st_write(all, paste0("C:/Users/bevin/Desktop/iFWA/",fwa,".gpkg"))
#   all
#   })
#
# tt <- bind_rows(t)
# st_write(tt, paste0("C:/Users/bevin/Desktop/iFWA/iFWA.gpkg"))
#
#
# #
# lapply(t[1], function(i=t[[1]]){
#   print(i)
#   i[1]
#
# })


# all <- bind_rows(lapply(l, function(i=l[1]){
#   name <- st_layers(i)[[1]]
#
#   st_read(i, query = paste0("SELECT * FROM ",name," WHERE class = '",v,"'"), quiet = T)
#   }))
#
# all %>% group_by(iFWA) %>% arrange(iFWA) %>% filter(iFWA %in% c("100","200")) %>% summarize(WATERSHED_ORDER= as.character(max(WATERSHED_ORDER))) %>% mapview::mapview(zcol = "iFWA")
#
#
# v <- "iFWA_2"
#
# all <- bind_rows(lapply(l, function(i=l[1]){
#   name <- st_layers(i)[[1]]
#
#   st_read(i, query = paste0("SELECT * FROM ",name," WHERE class = '",v,"'"), quiet = T)
# }))
#
# all %>% group_by(iFWA) %>% summarize(WATERSHED_ORDER= max(WATERSHED_ORDER)) %>% mapview::mapview(zcol = "WATERSHED_ORDER")
