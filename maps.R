library(sf)
library(ggplot2)
library(ggthemes)

# Downloaded from https://drive.google.com/file/d/1pYYMFtB6rZXiuZtysahr78Foy3n3KRQ3/view
imap <- read_sf("data/batas_prov")
ggplot(imap) + geom_sf()
# Note 28Mb and takes minutes to render

library(rmapshaper)
imap_small <- ms_simplify(imap, keep = 0.5)
ggplot(imap_small) + geom_sf()
# Note 15Mb and takes too long to render

imap_smaller <- ms_simplify(imap, keep = 0.1)
ggplot(imap_smaller) + geom_sf()
# Note 4Mb and renders in seconds

imap_smallest <- ms_simplify(imap, keep = 0.01)
ggplot(imap_smallest) + geom_sf()
# Note 800k and renders in fractions of a second

# Adding things
# https://github.com/horankev/quake_data
library(lubridate)
provinces_df <- readRDS("data/earthquakes/provinces_df.rds")
quakes_df <- readRDS("data/earthquakes/quakes_df.rds")

ggplot(provinces_df) + geom_sf()

ggplot() + 
  geom_sf(data=provinces_df, aes(fill=fault_concentration), colour="black", linewidth=0.5) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) + 
  labs(fill="faultline\nconcentration") + 
  coord_sf(datum=NA)

ggplot() + 
  geom_sf(data=provinces_df, fill="grey70", colour="white", linewidth=0.5) +
  geom_sf(data = filter(quakes_df, year(ymd) > 2020), 
          aes(colour=mag), alpha=0.5) +
  scale_colour_distiller(palette = "YlOrRd", direction = 1) + 
  facet_wrap(~magfact, ncol=2) +
  coord_sf(datum=NA) +
  theme_map() +
  theme(legend.position = "bottom")

quakes_tb <- quakes_df %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry() 
  
ggplot() + 
  geom_sf(data=provinces_df, fill="grey70", colour="white", linewidth=0.5) +
  geom_density2d(data = filter(quakes_tb, year(ymd) > 2020), 
          aes(x=lon, y=lat)) +
  facet_wrap(~magfact, ncol=2) +
  coord_sf(datum=NA) +
  theme_map() +
  theme(legend.position = "bottom")
