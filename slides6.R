## ----include = FALSE---------------------------------------------------------
# Load libraries 
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(plotly)
library(sf)
library(crosstalk)
library(cartogram)
library(ggthemes)


## ----------------------------------------------------------------------------
#| eval: false
#| echo: true
quakes_sf <- readRDS("data/earthquakes/quakes_df.rds")
provinces_sf <- readRDS("data/earthquakes/provinces_df.rds")

# Check map
ggplot(provinces_sf) + geom_sf()


## ----------------------------------------------------------------------------
#| eval: false
#| echo: true
quakes_df <- st_drop_geometry(quakes_sf)
quakes_df <- quakes_df |>
  bind_cols(st_coordinates(quakes_sf))
quakes_11_2023 <- quakes_df |>
  filter(year(ymd) == 2023, month(ymd) == 11)


## ----------------------------------------------------------------------------
#| eval: false
#| echo: true
ggplot(provinces_sf) +
  geom_sf() +
  geom_point(data=quakes_11_2023, aes(x=X, y=Y)) +
  theme_map()


## ----------------------------------------------------------------------------
#| eval: false
#| echo: true
p1 <- ggplot(quakes_11_2023, aes(x=ymd, y=mag)) +
  geom_point()
ggplotly(p1)


## ----------------------------------------------------------------------------
#| eval: false
#| echo: true
quakes_11_2023_shared <- highlight_key(quakes_11_2023)

p1 <- ggplot(quakes_11_2023_shared, aes(x=ymd, y=mag)) +
  geom_point() +
  theme_minimal()

gp1 <- ggplotly(p1, width=400, height=400) |>
  config(displayModeBar = FALSE) |>
  highlight(on = "plotly_selected",
            off = "plotly_doubleclick")

# multiple types of geometry in provinces_sf needs fixing
provinces_sf2 <- st_cast(provinces_sf, "MULTIPOLYGON")
p2 <- ggplot() +
  geom_sf(data=provinces_sf2) +
  geom_point(data=quakes_11_2023_shared, aes(x=X, y=Y)) +
  theme_map()

gp2 <- ggplotly(p2, width=800, height=400) |>
  config(displayModeBar = FALSE) |>
  highlight(on = "plotly_selected",
            off = "plotly_doubleclick")

bscols(gp1, gp2, widths = c(4, 8))


## ----------------------------------------------------------------------------
#| eval: false
#| echo: true
quakes_2023 <- quakes_df |>
  filter(year(ymd) == 2023) |>
  group_by(province) |>
  summarise(mag = mean(mag, na.rm=TRUE),
            n = n(), na.rm=TRUE)

provinces_sf2 <- left_join(provinces_sf2, quakes_2023)

p3 <- ggplot(provinces_sf2) +
  geom_sf(aes(fill=mag, label=province)) +
  theme_map() +
  scale_fill_viridis_c()


## ----------------------------------------------------------------------------
#| eval: false
#| echo: true
pop <- read_csv("data/province_pop.csv") |>
  rename(province = Provinsi)
provinces_sf2 <- left_join(provinces_sf2, pop)

indonesia_carto <- provinces_sf2 |>
  st_transform(3395) |>
  cartogram_cont("2023") |>
  st_transform("WGS84")

indonesia_carto <- st_cast(indonesia_carto, "MULTIPOLYGON")

ggplot(indonesia_carto) + geom_sf()

indonesia_carto <- left_join(indonesia_carto, quakes_2023)

p4 <- ggplot(indonesia_carto) +
  geom_sf(aes(fill=mag, label=province)) +
  theme_map() +
  scale_fill_viridis_c()


## ----------------------------------------------------------------------------
#| eval: false
#| echo: true

ggplotly(p3)

ggplotly(p4)

