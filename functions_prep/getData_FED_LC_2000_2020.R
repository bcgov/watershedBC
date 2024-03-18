# https://open.canada.ca/data/en/dataset/fa84a70f-03ad-4946-b0f8-a3b481dd5248
base_url <- "https://agriculture.canada.ca/atlas/data_donnees/landuse/data_donnees/tif/"


years <- seq(2000,2020,5)
utms <- stringr::str_pad(seq(7,12,1), 2, "left",0)

for(y in years){
  for(u in utms){
    url <- paste0(base_url,y,"/LU",y,"_u",u,".zip")
    out <- paste0("C:/Users/bevin/Downloads/LU",y,"_u",u,".zip")
    # download.file(url, destfile = out)
    unzip(out)
    print(url)
  }
}


paste0(url,,"/LU2000_u")


,c("07","08"))


l <- list.files("C:/Users/bevin/Downloads/AA", pattern = "*.tif$", full.names = T)
library(stars)
library(sf)

r <- read_stars(l[1])
plot(r)
s <- r %>% st_as_sf(merge = T)

s %>% plot()

s %>% mutate(LU = case_when(21 ~ "Settlement",
                            24 ~ "Settlement Forest",
                            28 ~ "Settlement Vegetated",
                            22 ~ "Settlement High Reflectance",
                            29 ~ "Settlement Very High Reflectance",
                            25 ~ "Roads",
                            31 ~ "Water",
                            41 ~ "Forest",
                            42 ~ "Forest Wetland",
                            43 ~ "Forest Renerating after Harvest <20 years",
                            44 ~ "Forest Wetland Renerating after Harvest <20 years",
                            49 ~ "Forest Renerating after Fire <20 years",
                            43 ~ "Forest Renerating after Harvest <20 years",
                            44 ~ "Forest Wetland Renerating after Harvest <20 years",

                            ))


Settlement
21 Urban and rural residential, commercial,
industrial, transportation or other built
infrastructure use
Settlement Forest
24 Settlement areas mostly or entirely covered by
tree canopy
Vegetated Settlement
28 Settlement areas with observable vegetation
such as lawns, golf courses, and settlement
areas with 30-50% tree canopy
High Reflectance Settlement
22 Settlement areas with high spectral reflectance
such as pavement, buildings, or other surfaces
with little to no observable vegetation
Very High Reflectance
Settlement
29 Settlement areas with very high spectral
reflectance such as pavement, buildings, or
other surfaces with no observable vegetation
Roads 25 Primary, secondary and tertiary roads
Water 31 Open water
Forest
41 Land covered by trees with a canopy cover
>10% and a minimum height of 5m, or capable
of growing to those measurements within 50
years
Forest Wetland
42 Wetland with forest cover (canopy cover over
10% and minimum height 5m, or capable of
growing to those measurements within 50 years)
Forest Regenerating after
Harvest <20 years
43 Forest regenerating from tree harvesting activity
that took place less than 20 years prior
Forest Wetland Regenerating
after Harvest <20 years
44 Wetland with forest cover regenerating from tree
harvesting activity that took place less than 20
years prior
Forest Regenerating after Fire
<20 years
49 Forest Regenerating after a fire less than 20
years prior
