named <- bcdata::bcdc_query_geodata("freshwater-atlas-named-watersheds") %>% collect()
named %>%
  mutate(area_m2 = as.numeric(st_area(.))) %>%
  mutate(name = paste0(GNIS_NAME, " (id:",GNIS_ID,") ", round(area_m2/(1000*1000),0), " sq.km")) %>%
  arrange(name, -area_m2) %>%
  st_drop_geometry() %>% select(name) %>% saveRDS("named.RDS")
