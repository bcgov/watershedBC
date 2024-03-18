
library(dplyr)
library(stars)
library(ClimateNAr)

wd <- "C:/Users/bevington/Downloads/test"
dir.create(wd)
vars = c("MAT","MAP","CMD")

rasterDownload(region='BC', res='800m', period='Normal_1961_1990', varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period='Normal_1971_2000', varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period='Normal_1981_2010', varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period='Normal_1991_2020', varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="8GCMs_ensemble_ssp126_2011-2040", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="8GCMs_ensemble_ssp126_2041-2070", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="8GCMs_ensemble_ssp126_2071-2100", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="8GCMs_ensemble_ssp245_2011-2040", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="8GCMs_ensemble_ssp245_2041-2070", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="8GCMs_ensemble_ssp370_2071-2100", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="8GCMs_ensemble_ssp370_2011-2040", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="8GCMs_ensemble_ssp585_2041-2070", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="8GCMs_ensemble_ssp585_2071-2100", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="13GCMs_ensemble_ssp126_2011-2040", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="13GCMs_ensemble_ssp126_2041-2070", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="13GCMs_ensemble_ssp126_2071-2100", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="13GCMs_ensemble_ssp245_2011-2040", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="13GCMs_ensemble_ssp245_2041-2070", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="13GCMs_ensemble_ssp370_2071-2100", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="13GCMs_ensemble_ssp370_2011-2040", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="13GCMs_ensemble_ssp585_2041-2070", varList=vars, sDir=wd)
rasterDownload(region='BC', res='800m', period="13GCMs_ensemble_ssp585_2071-2100", varList=vars, sDir=wd)

clim_to_cog <- function(region='BC', res='800m', period="13GCMs_ensemble_ssp585_2071-2100", varList="MAP", sDir=wd){

  (tif <- paste0(wd,"/",region,"/",res,"/",period,"/",varList,".tif"))
  (outtif <- paste0(wd,"/",region,"_",res,"_",period,"_",varList,"_cog.tif"))

  sf::gdal_utils(util = "translate",
                 source = tif,
                 destination = outtif,
                 options = c("-of","COG",
                             "-co","COMPRESS=LZW"))
  }

periods <- c( 'Normal_1961_1990',
              'Normal_1971_2000',
              'Normal_1981_2010',
              'Normal_1991_2020',
              "8GCMs_ensemble_ssp126_2011-2040",
              "8GCMs_ensemble_ssp126_2041-2070",
              "8GCMs_ensemble_ssp126_2071-2100",
              "8GCMs_ensemble_ssp245_2011-2040",
              "8GCMs_ensemble_ssp245_2041-2070",
              "8GCMs_ensemble_ssp370_2071-2100",
              "8GCMs_ensemble_ssp370_2011-2040",
              "8GCMs_ensemble_ssp585_2041-2070",
              "8GCMs_ensemble_ssp585_2071-2100",
              "13GCMs_ensemble_ssp126_2011-2040",
              "13GCMs_ensemble_ssp126_2041-2070",
              "13GCMs_ensemble_ssp126_2071-2100",
              "13GCMs_ensemble_ssp245_2011-2040",
              "13GCMs_ensemble_ssp245_2041-2070",
              "13GCMs_ensemble_ssp370_2071-2100",
              "13GCMs_ensemble_ssp370_2011-2040",
              "13GCMs_ensemble_ssp585_2041-2070",
              "13GCMs_ensemble_ssp585_2071-2100")

for(p in periods){
  for(v in vars){
    clim_to_cog(region = "BC", period = p, varList = v)
  }
}

l <- list.files(wd, recursive = F, full.names = T, pattern = "*.tif$")

stars::read_stars(l, along = "band") %>% write_stars("climateBC.tif")

terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/climateBC.tif")

# annual_vars <- c("MAT",#mean annual temperature (°C),
#                  "MWMT",#mean warmest month temperature (°C),
#                  "MCMT",#mean coldest month temperature (°C),
#                  "TD",#temperature difference between MWMT and MCMT, or continentality (°C),
#                  "MAP",#mean annual precipitation (mm),
#                  "MSP",#May to September precipitation (mm),
#                  "AHM",#annual heat-moisture index (MAT+10)/(MAP/1000))
#                  "SHM",#summer heat-moisture index ((MWMT)/(MSP/1000))
#                  "DD<0",#degree-days below 0°C, chilling degree-days
#                  "DD>5",#degree-days above 5°C, growing degree-days
#                  "DD<18",#degree-days below 18°C, heating degree-days
#                  "DD>18",#degree-days above 18°C, cooling degree-days
#                  "NFFD",#the number of frost-free days
#                  "FFP",#frost-free period
#                  "bFFP",#the day of the year on which FFP begins
#                  "eFFP",#the day of the year on which FFP ends
#                  "PAS",#precipitation as snow (mm) between August in previous year and July in current year
#                  "EMT",#extreme minimum temperature over 30 years
#                  "EXT",#extreme maximum temperature over 30 years
#                  "Eref",#Hargreaves reference evaporation (mm)
#                  "CMD",#Hargreaves climatic moisture deficit (mm)
#                  "MAR",#mean annual solar radiation (MJ m‐2 d‐1)
#                  "RH",#mean annual relative humidity (%)
#                  "CMI",#Hogg’s climate moisture index (mm)
#                  "DD1040")# (10<DD<40)    degree-days above 10°C and below 40°C
#
# seasonal_vars <- c("Tave_wt",#winter mean temperature (°C)
#                    "Tave_sp",#spring mean temperature (°C)
#                    "Tave_sm",#summer mean temperature (°C)
#                    "Tave_at",#autumn mean temperature (°C)
#                    "Tmax_wt",#winter mean maximum temperature (°C)
#                    "Tmax_sp",#spring mean maximum temperature (°C)
#                    "Tmax_sm",#summer mean maximum temperature (°C)
#                    "Tmax_at",#autumn mean maximum temperature (°C)
#                    "Tmin_wt",#winter mean minimum temperature (°C)
#                    "Tmin_sp",#spring mean minimum temperature (°C)
#                    "Tmin_sm",#summer mean minimum temperature (°C)
#                    "Tmin_at",#autumn mean minimum temperature (°C)
#                    "PPT_wt",#winter precipitation (mm)
#                    "PPT_sp",#spring precipitation (mm)
#                    "PPT_sm",#summer precipitation (mm)
#                    "PPT_at",#autumn precipitation (mm)
#                    "RAD_wt",#winter solar radiation (MJ m‐2 d‐1)
#                    "RAD_sp",#spring solar radiation (MJ m‐2 d‐1)
#                    "RAD_sm",#summer solar radiation (MJ m‐2 d‐1)
#                    "RAD_at",#autumn solar radiation (MJ m‐2 d‐1)
#                    "DD_0_wt",#winter degree-days below 0°C
#                    "DD_0_sp",#spring degree-days below 0°C
#                    "DD_0_sm",#summer degree-days below 0°C
#                    "DD_0_at",#autumn degree-days below 0°C
#                    "DD5_wt",#winter degree-days below 5°C
#                    "DD5_sp",#spring degree-days above 5°C
#                    "DD5_sm",#summer degree-days above 5°C
#                    "DD5_at",#autumn degree-days above 5°C
#                    "DD_18_wt",#winter degree-days below 18°C
#                    "DD_18_sp",#spring degree-days below 18°C
#                    "DD_18_sm",#summer degree-days below 18°C
#                    "DD_18_at",#autumn degree-days below 18°C
#                    "DD18_wt",#winter degree-days below 18°C
#                    "DD18_sp",#spring degree-days above 18°C
#                    "DD18_sm",#summer degree-days above 18°C
#                    "DD18_at",#autumn degree-days above 18°C
#                    "NFFD_wt",#winter number of frost-free days
#                    "NFFD_sp",#spring number of frost-free days
#                    "NFFD_sm",#summer number of frost-free days
#                    "NFFD_at",#autumn number of frost-free days
#                    "PAS_wt",#winter precipitation as snow (mm)
#                    "PAS_sp",#spring precipitation as snow (mm)
#                    "PAS_sm",#summer precipitation as snow (mm)
#                    "PAS_at",#autumn precipitation as snow (mm)
#                    "Eref_wt",#winter Hargreaves reference evaporation (mm)
#                    "Eref_sp",#spring Hargreaves reference evaporation (mm)
#                    "Eref_sm",#summer Hargreaves reference evaporation (mm)
#                    "Eref_at",#autumn Hargreaves reference evaporation (mm)
#                    "CMD_wt",#winter Hargreaves climatic moisture deficit (mm)
#                    "CMD_sp",#spring Hargreaves climatic moisture deficit (mm)
#                    "CMD_sm",#summer Hargreaves climatic moisture deficit (mm)
#                    "CMD_at",#autumn Hargreaves climatic moisture deficit (mm)
#                    "RH_wt",#winter relative humidity (%)
#                    "RH_sp",#winter relative humidity (%)
#                    "RH_sm",#winter relative humidity (%)
#                    "RH_at",#winter relative humidity (%)
#                    "CMI_wt",#winter Hogg’s climate moisture index (mm)
#                    "CMI_sp",#spring Hogg’s climate moisture index (mm)
#                    "CMI_sm",#summer Hogg’s climate moisture index (mm)
#                    "CMI_at")#autumn Hogg’s climate moisture index (mm)
#
# monthly_vars <-
#   c(paste0("Tave",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #mean temperatures (°C)
#     paste0("TMX",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #maximum mean temperatures (°C)
#     paste0("TMN",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #minimum mean temperatures (°C)
#     paste0("PPT",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #precipitation (mm)
#     paste0("RAD",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #solar radiation (MJ m‐2 d‐1)
#     paste0("DD_0_",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #degree-days below 0°C
#     paste0("DD5_",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #degree-days above 5°C
#     paste0("DD_18_",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #degree-days below 18°C
#     paste0("DD18_",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #degree-days above 18°C
#     paste0("NFFD01",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #number of frost-free days
#     paste0("PAS",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #precipitation as snow (mm)
#     paste0("Eref",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #Hargreaves reference evaporation (mm)
#     paste0("CMD",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #Hargreaves climatic moisture deficit (mm)
#     paste0("RH",stringr::str_pad(seq(1,12,1), 2, "left", 0)), #relative humidity (%)
#     paste0("CMI",stringr::str_pad(seq(1,12,1), 2, "left", 0))) #Hogg’s climate moisture index (mm)


