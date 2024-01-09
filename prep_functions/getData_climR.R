# remotes::install_github("bcgov/climr")

library(climr)
##provide or create a long, lat, elev dataframe
in_xyz <- structure(list(Long = c(-127.70521, -127.62279, -127.56235, -127.7162,
                                  -127.18585, -127.1254, -126.94957, -126.95507),
                         Lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
                         Elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L)), row.names = c(NA, -8L), class = "data.frame")

##if you just want to downscale points and not think about what happening behind the scenes, use this function
conn <- data_connect()

res <- climr_downscale(xyz = in_xyz, which_normal = "auto",
                       gcm_models = c("ACCESS-ESM1-5", "EC-Earth3"),
                       ssp = c("ssp370","ssp245"),
                       gcm_period = c("2021_2040", "2041_2060","2061_2080"),
                       #gcm_ts_years = 2020:2060,
                       max_run = 3, # we want 3 individual runs for each model
                       vars = c("PPT","CMD","CMI"))

##Functions to show what data are available:

list_gcm()
list_gcm_period()
list_ssp()
list_variables()



d <- climr_downscale(xyz = in_xyz, which_normal = "BC", historic_ts = 1900:2050, return_normal = F)


# ggplotly(

dat <- d %>%
  filter(ID == 1) %>%
  pivot_longer(c(-ID, -PERIOD)) %>%
  separate(name,
           into = c("var", "month"),
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(season = case_when(month %in% c("09","10","11")~"Fall",
                            month %in% c("12","01","02")~"Winter",
                            month %in% c("03","04","05")~"Spring",
                            month %in% c("06","07","08")~"Summer")) %>%
  mutate(season = factor(season, levels = c("Winter","Spring","Summer","Fall"))) %>%
  # filter(var == "Tmin") %>%
  group_by(var, season) %>%
  mutate(mean = mean(value)) %>%
  group_by(var, PERIOD, season, mean) %>%
  summarize(annual = mean(value)) %>%
  mutate(diff = annual-mean)

dat %>%
  filter(var == "Tmax") %>%
  ggplot(aes(as.numeric(PERIOD), diff)) +
    geom_col(aes(fill = diff), color = "black") +
    facet_wrap(.~season, scales = "free_y", ncol = 4) +
    theme_bw() +
    scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(9,"Spectral")),
                         limit = max(abs(dat$diff)) * c(-1, 1))
