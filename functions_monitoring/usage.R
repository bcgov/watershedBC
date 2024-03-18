
rm(list=ls())
library(lubridate)
library(tidyverse)
library(pool)
library(RPostgreSQL)

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
d <- dbReadTable(conn, "usage")
poolClose(conn)

##############################

(df <- d %>%
  # as_tibble() %>%
  select(date_time, gnis_name, processing_time, action, area_km2) %>%
  filter(processing_time != "") %>%
  mutate(time = case_when(str_detect(processing_time, "minute") ~ as.numeric(str_replace(processing_time, " minutes elapsed", "")),
                          str_detect(processing_time, "sec") ~ as.numeric(str_replace(processing_time, " sec elapsed", ""))/60,
                          TRUE ~ -9999)) %>%
  mutate(date_time = as_datetime(date_time) %>% with_tz("America/Vancouver")) %>%
  mutate(date = as.Date.character(as.character(date_time), format = "%Y-%m-%d")) %>%
  mutate(row = row_number()) %>%
  arrange(rev(date_time)))

df %>%
  group_by(date, action) %>%
  filter(action != "start") %>%
  summarise(n = n()) %>%
  ggplot(aes(date, n)) +
  geom_col(aes(fill = action)) +
  theme_bw() +
  scale_x_date(date_breaks = "5 day", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(.~action, scales = "free_y", ncol = 1)


df %>%
  filter(action == "processing") %>%
  filter(date > Sys.Date()-2) %>%
  ggplot(aes(date_time, gnis_name)) +
  geom_point() +
  theme_bw()




df %>% ggplot(aes(area_km2, time)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() + theme(aspect.ratio = 1) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = bquote(Area~(km^2)),
       y = "Minutes to Process") +
  facet_wrap(.~action, scales = "free")

summary(lm(time~area_km2, data = df))


df %>%
  filter(gnis_name == "Bowron River") %>%
  filter(action == "processing") %>%
  ggplot() +
  geom_point(aes(date_time, time))


df %>%
  filter(action == "processing") %>%
  ggplot() +
  geom_point(aes(date_time, time, size = area_km2))

