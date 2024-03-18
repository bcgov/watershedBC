library(stringr)
library(dplyr)
library(bcdata)
library(sf)

# OPTION 2: GET WATERSHEDS FROM ####
gdb <- "X:/Research/Data_watershedBC/hydro_fwa/FWA_WATERSHEDS_POLY.gdb"

l <- st_layers(gdb)[1] %>% as.data.frame() %>%
  filter(!name %in% c("_max_watershed_feature_id",
                      "_fwa_watersheds_poly_updates_BACKUP",
                      "_fwa_watersheds_poly_updates"))


ws_d <- lapply(l[,1], function(id = l[,1][1]){

  print(id)

  if(!file.exists(paste0(id,".shp"))){

    ws <- st_read(gdb, layer = id)

    ws2 <- ws %>% select(WATERSHED_ORDER,FWA_WATERSHED_CODE) %>%
      mutate(iFWA_1 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,1],
             iFWA_2 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,2],
             iFWA_3 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,3],
             iFWA_4 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,4],
             iFWA_5 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,5],
             iFWA_6 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,6],
             iFWA_7 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,7],
             iFWA_8 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,8],
             iFWA_9 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,9],
             iFWA_10 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,10],
             iFWA_11 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,11],
             iFWA_12 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,12],
             iFWA_13 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,13],
             iFWA_14 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,14],
             iFWA_15 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,15],
             iFWA_16 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,16],
             iFWA_17 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,17],
             iFWA_18 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,18],
             iFWA_19 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,19],
             iFWA_20 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,20],
             iFWA_21 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,21])

    iFWA_1 <- ws2 %>%
      group_by(iFWA_1) %>%
      filter(iFWA_1 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1, sep = "-")) %>%
      select(-c(iFWA_1))

    iFWA_2 <- ws2 %>%
      group_by(iFWA_1,iFWA_2) %>%
      filter(iFWA_1 != "000000",
             iFWA_2 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2))

    iFWA_3 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3))

    iFWA_4 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4))

    iFWA_5 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5))

    iFWA_6 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6))

    iFWA_7 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7))

    iFWA_8 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8))

    iFWA_9 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9))

    iFWA_10 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000", iFWA_10 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10))

    iFWA_11 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,
               iFWA_11) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000", iFWA_10 != "000000",
             iFWA_11 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11))

    iFWA_12 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,
               iFWA_11,iFWA_12) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000", iFWA_10 != "000000",
             iFWA_11 != "000000", iFWA_12 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12))

    iFWA_13 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,
               iFWA_11,iFWA_12,iFWA_13) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000", iFWA_10 != "000000",
             iFWA_11 != "000000", iFWA_12 != "000000", iFWA_13 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13))

    iFWA_14 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,
               iFWA_11,iFWA_12,iFWA_13,iFWA_14) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000", iFWA_10 != "000000",
             iFWA_11 != "000000", iFWA_12 != "000000", iFWA_13 != "000000", iFWA_14 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14))

    iFWA_15 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,
               iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000", iFWA_10 != "000000",
             iFWA_11 != "000000", iFWA_12 != "000000", iFWA_13 != "000000", iFWA_14 != "000000", iFWA_15 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15))

    iFWA_16 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,
               iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000", iFWA_10 != "000000",
             iFWA_11 != "000000", iFWA_12 != "000000", iFWA_13 != "000000", iFWA_14 != "000000", iFWA_15 != "000000",
             iFWA_16 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16))

    iFWA_17 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,
               iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000", iFWA_10 != "000000",
             iFWA_11 != "000000", iFWA_12 != "000000", iFWA_13 != "000000", iFWA_14 != "000000", iFWA_15 != "000000",
             iFWA_16 != "000000", iFWA_17 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17))

    iFWA_18 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,
               iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000", iFWA_10 != "000000",
             iFWA_11 != "000000", iFWA_12 != "000000", iFWA_13 != "000000", iFWA_14 != "000000", iFWA_15 != "000000",
             iFWA_16 != "000000", iFWA_17 != "000000", iFWA_18 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18))

    iFWA_19 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,
               iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000", iFWA_10 != "000000",
             iFWA_11 != "000000", iFWA_12 != "000000", iFWA_13 != "000000", iFWA_14 != "000000", iFWA_15 != "000000",
             iFWA_16 != "000000", iFWA_17 != "000000", iFWA_18 != "000000", iFWA_19 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19))

    iFWA_20 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,
               iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19,iFWA_20) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000", iFWA_10 != "000000",
             iFWA_11 != "000000", iFWA_12 != "000000", iFWA_13 != "000000", iFWA_14 != "000000", iFWA_15 != "000000",
             iFWA_16 != "000000", iFWA_17 != "000000", iFWA_18 != "000000", iFWA_19 != "000000", iFWA_20 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19,iFWA_20, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19,iFWA_20))

    iFWA_21 <- ws2 %>%
      group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,
               iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19,iFWA_20,iFWA_21) %>%
      filter(iFWA_1 != "000000", iFWA_2 != "000000", iFWA_3 != "000000", iFWA_4 != "000000", iFWA_5 != "000000",
             iFWA_6 != "000000", iFWA_7 != "000000", iFWA_8 != "000000", iFWA_9 != "000000", iFWA_10 != "000000",
             iFWA_11 != "000000", iFWA_12 != "000000", iFWA_13 != "000000", iFWA_14 != "000000", iFWA_15 != "000000",
             iFWA_16 != "000000", iFWA_17 != "000000", iFWA_18 != "000000", iFWA_19 != "000000", iFWA_20 != "000000",
             iFWA_21 != "000000") %>%
      summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
      mutate(iFWA = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19,iFWA_20,iFWA_21, sep = "-")) %>%
      select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10, iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19,iFWA_20,iFWA_21))


    o <- bind_rows(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,
              iFWA_11,iFWA_12,iFWA_13,iFWA_14,iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19,iFWA_20,iFWA_21) %>%
      mutate(area_km2 = as.numeric(st_area(.))/(1000*1000))

    st_write(o, paste0(id,".shp"), delete_layer = T)

    }
    })

  #   ws2 %>% names
  #
  #   ws2
  #
  #   ws2 %>% select(iFWA_1,iFWA_2,iFWA_3,iFWA_4) %>%
  #     group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4) %>%
  #     summarize()
  #   .Last.value%>% mapview::mapview()
  #
  #   ws3 <- ws2 %>% group_by(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,iFWA_11,iFWA_12,iFWA_13,iFWA_14,
  #                            iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19,iFWA_20,iFWA_21) %>%
  #     summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>%
  #     mutate(FWA_WATERSHED_CODE = paste(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,iFWA_11,iFWA_12,iFWA_13,iFWA_14,
  #                                        iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19,iFWA_20,iFWA_21, sep = "-")) %>%
  #     select(-c(iFWA_1,iFWA_2,iFWA_3,iFWA_4,iFWA_5,iFWA_6,iFWA_7,iFWA_8,iFWA_9,iFWA_10,iFWA_11,iFWA_12,iFWA_13,iFWA_14,
  #            iFWA_15,iFWA_16,iFWA_17,iFWA_18,iFWA_19,iFWA_20,iFWA_21))
  #
  #   ws3 %>% filter(WATERSHED_ORDER == 6) %>% mapview::mapview()
  #
  #   # ws_all <- bind_rows(lapply(paste0("iFWA_",1:21), function(i=paste0("iFWA_",1:21)[1]){
  #   #   print(i)
  #   #   ws2 %>% dplyr::rename(iFWA = i )%>% filter(as.numeric(iFWA) > 0) %>% group_by(iFWA) %>% summarize(WATERSHED_ORDER = max(WATERSHED_ORDER, na.rm = T)) %>% mutate(class = i)
  #   # }))
  #
  #   ws_all <- ws_all %>%
  #     st_make_valid() %>%
  #     mutate(id = id, area_m = round(as.numeric(st_area(.)), 0)) %>%
  #     arrange(-WATERSHED_ORDER)
  #
  #   st_write(ws_all, paste0("C:/Users/bevin/Desktop/",id,".gpkg"))
  # }
  # ws_all
  # })




# OPTION 1: GET WATERSHEDS FROM ####

# ws <- bcdc_query_geodata("freshwater-atlas-watersheds") %>%
#   filter(WATERSHED_GROUP_CODE == "BOWR") %>%
#   collect()
#
# ws2 <- ws %>% select(WATERSHED_ORDER,FWA_WATERSHED_CODE) %>%
#   mutate(iFWA_1 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,1],
#          iFWA_2 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,2],
#          iFWA_3 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,3],
#          iFWA_4 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,4],
#          iFWA_5 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,5],
#          iFWA_6 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,6],
#          iFWA_7 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,7],
#          iFWA_8 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,8],
#          iFWA_9 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,9],
#          iFWA_10 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,10],
#          iFWA_11 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,11],
#          iFWA_12 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,12],
#          iFWA_13 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,13],
#          iFWA_14 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,14],
#          iFWA_15 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,15],
#          iFWA_16 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,16],
#          iFWA_17 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,17],
#          iFWA_18 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,18],
#          iFWA_19 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,19],
#          iFWA_20 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,20],
#          iFWA_21 = str_split(FWA_WATERSHED_CODE, "-", simplify = T)[,21])
#
# ws_all <- bind_rows(lapply(paste0("iFWA_",1:21), function(i=paste0("iFWA_",1:21)[1]){
#   print(i)
#   ws3 <- ws2 %>% rename(iFWA = i)
#   ws3 %>% pull(iFWA) %>% unique() %>% length()
#   ws3 %>% filter(as.numeric(iFWA) > 0) %>% group_by(iFWA) %>% summarize(WATERSHED_ORDER = max(WATERSHED_ORDER))
# }))
#
# st_write(ws_all, "C:/Users/bevin/Desktop/BOWR.gpkg")
