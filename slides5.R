## ----include = FALSE---------------------------------------------------------
# Load libraries 
source("libraries.R")

# Set up chunk for all slides
source("chunk_options_and_themes.R")


## ----------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 8
#| fig-height: 6
#| out-width: 70%
gp <- gapminder |> 
  filter (year == 2007) |>
  ggplot(aes(x=lifeExp, 
             y=gdpPercap,
             label=country,
             colour=continent)) +
  geom_point() +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  scale_y_log10("gdpPercap ('000)",
                breaks = seq(0, 50000, 10000), 
                labels = seq(0, 50, 10)) +
  theme(axis.title = element_text(family="Helvetica"),
        axis.text = element_text(family="Helvetica"),
        legend.title = element_text(family="Helvetica"),
        legend.text = element_text(family="Helvetica")) 
gp + geom_text() +
  ggtitle("Too cluttered")



## ----------------------------------------------------------------------------
#| code-fold: true
ggplotly(gp, width=700, height=550) |>
  config(displayModeBar = FALSE)


## ----------------------------------------------------------------------------
#| code-fold: true
ggplotly(gp, width=700, height=550) |>
  config(
         modeBarButtonsToRemove = c('select', 'zoomIn',
                                    'zoomOut', 'autoScale',
                                    'resetScale'))


## ----------------------------------------------------------------------------
#| code-fold: true
# Filter gapminder data for a specific year (e.g., 2007)
gapminder_2007 <- gapminder |>
  filter(year == 2007) |>
  select(country, lifeExp)

# Get world map data
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  select(name, geometry)

# Ensure the CRS is WGS 84 (Leaflet's requirement)
world_sf <- st_transform(world_sf, crs = 4326)

# Handle potential country name mismatches
world_sf <- world_sf |>
  mutate(name = recode(name, 
                       "United States" = "United States of America",
                       "Congo" = "Congo, Rep.",
                       "Democratic Republic of Congo" = "Congo, Dem. Rep.",
                       "Slovakia" = "Slovak Republic",
                       "Myanmar" = "Burma",
                       "Egypt" = "Egypt, Arab Rep.",
                       "Yemen" = "Yemen, Rep."))

# Merge the datasets
map_data <- world_sf |>
  left_join(gapminder_2007, by = c("name" = "country")) |>
  # Remove countries with no life expectancy data in the chosen year
  filter(!is.na(lifeExp))

# Define a color palette
pal <- colorNumeric("RdYlGn", domain = map_data$lifeExp)

# Create the map
leaflet(data = map_data) |>
  addProviderTiles(providers$CartoDB.Positron) |> # Add a base map tile
  addPolygons(
    fillColor = ~pal(lifeExp),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.9,
      bringToFront = TRUE),
    popup = ~paste0("<strong>", name, "</strong><br/>Life Expectancy: ", round(lifeExp, 2), " years") # Add interactive popups
  ) |>
  addLegend(pal = pal, values = ~lifeExp, opacity = 0.7, title = "Life Expectancy (Years)",
            position = "bottomright")


## ----------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 8
#| fig-height: 6
set.seed(802)
gapminder_ts <- gapminder |>
  as_tsibble(index=year, 
             key=c(country, continent)) |>
  sample_n_keys(50)
gphk <- highlight_key(gapminder_ts, ~country)

gpl <- ggplot(gphk, aes(x=year, 
                             y=lifeExp, 
                             group=country)) +
        geom_line() +
        ylab("Life Expectancy") +
        ggtitle("click on a line to highlight a country") +
        theme(axis.title = element_text(family="Helvetica"),
          axis.text = element_text(family="Helvetica"),
          legend.title = element_text(family="Helvetica"),
          legend.text = element_text(family="Helvetica"),
          title = element_text(family="Helvetica"))

ggpl <- ggplotly(gpl, height = 700, width = 1200) |>
  config(displayModeBar = FALSE)
        
highlight(ggpl)
        


## ----------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 8
#| fig-height: 6
gggs <- ggplot(gphk, aes(x=continent, y=country)) +
  geom_point() +
  xlab("") + ylab("") +
  ggtitle("rectangular selection") +
  theme(axis.title = element_text(family="Helvetica"),
      axis.text = element_text(family="Helvetica"),
      legend.title = element_text(family="Helvetica"),
      legend.text = element_text(family="Helvetica"),
      title = element_text(family="Helvetica")) 

gggspl <- ggplotly(gggs, width=500, height=500) |>
  config(displayModeBar = FALSE) |>
  highlight(on = "plotly_selected", 
            off = "plotly_doubleclick") 

ggpl2 <- ggplotly(gpl, height = 500, width = 800) |>
  config(displayModeBar = FALSE) |>
  highlight(on = "plotly_selected", 
            off = "plotly_doubleclick") 
  
bscols(gggspl, highlight(ggpl2), widths = c(5, 7))


## ----------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 12
#| fig-height: 10
gpl <- ggplot(gphk, aes(x=year, 
                             y=lifeExp, 
                             group=country)) +
        geom_line() +
        ylab("Life Expectancy") + xlab("") +
        theme(axis.title = element_text(family="Helvetica"),
          axis.text = element_text(family="Helvetica"),
          legend.title = element_text(family="Helvetica"),
          legend.text = element_text(family="Helvetica"),
          title = element_text(family="Helvetica"))
ggpl2 <- ggplotly(gpl, height = 500, width = 1000) |>
  config(displayModeBar = FALSE)
  
bscols(widths = c(4, 7),
    filter_select("country", "country", gphk, ~country, multiple = TRUE),
  ggpl2)
  


## ----------------------------------------------------------------------------
#| label: plotly
#| eval: false
# penguins_std <- penguins |>
#   rename(bl = bill_len,
#          bd = bill_dep,
#          fl = flipper_len,
#          bm = body_mass) |>
#   select(species, bl:bm) |>
#   na.omit()
# 
# plot_ly(data = penguins_std,
#         x = ~fl,
#         y = ~bl,
#         color = ~species,
#         size = 3,
#         width=600, height=450)


## ----------------------------------------------------------------------------
#| label: plotly
#| warning: false
#| message: false
#| echo: false
penguins_std <- penguins |>
  rename(bl = bill_len,
         bd = bill_dep,
         fl = flipper_len,
         bm = body_mass) |>
  select(species, bl:bm) |>
  na.omit()
  
plot_ly(data = penguins_std, 
        x = ~fl, 
        y = ~bl, 
        color = ~species, 
        size = 3, 
        width=600, height=450)


## ----------------------------------------------------------------------------
#| label: ggplot-plotly
#| code-fold: false
#| eval: false
# gg <- ggplot(data=penguins_std, aes(x = fl,
#                                     y = bl,
#                                     colour = species)) +
#   geom_point(alpha=0.5) +
#   geom_smooth(method = "lm", se=F)
# ggplotly(gg, width=600, height=490)


## ----------------------------------------------------------------------------
#| label: ggplot-plotly
#| message: false
#| echo: false
gg <- ggplot(data=penguins_std, aes(x = fl, 
                                    y = bl, 
                                    colour = species)) +  
  geom_point(alpha=0.5) + 
  geom_smooth(method = "lm", se=F)
ggplotly(gg, width=600, height=490)


## ----------------------------------------------------------------------------
#| label: plotly-map
#| warning: false
#| eval: false
# data(canada.cities, package = "maps")
# viz <- ggplot(canada.cities, aes(long, lat)) +
#   annotation_borders(regions = "canada") +
#   coord_equal() +
#   geom_point(aes(label = name, size = log2(pop)),
#              colour = "red", alpha = 1/4) +
#   theme_map()
# ggplotly(viz)


## ----------------------------------------------------------------------------
#| label: plotly-map
#| warning: false
#| echo: false
data(canada.cities, package = "maps")
viz <- ggplot(canada.cities, aes(long, lat)) +
  annotation_borders(regions = "canada") +
  coord_equal() +
  geom_point(aes(label = name, size = log2(pop)), 
             colour = "red", alpha = 1/4) +
  theme_map()
ggplotly(viz)


## ----------------------------------------------------------------------------
#| label: plotly-modify
#| eval: false
# txh_shared <- highlight_key(txhousing, ~year)
# 
# p <- ggplot(txh_shared, aes(month, median)) +
#    geom_line(aes(group = year)) +
#    geom_smooth(data = txhousing, method = "gam") +
#    scale_x_continuous("", breaks=seq(1, 12, 1),
#         labels=c("J", "F", "M", "A", "M", "J",
#                  "J", "A", "S", "O", "N", "D")) +
#    scale_y_continuous("Median price ('00,000)",
#                       breaks = seq(0,300000,100000),
#                       labels = seq(0,3,1)) +
#    facet_wrap(~ city)
# 
# gg <- ggplotly(p, height = 800, width = 1100) |>
#    plotly::layout(title = "Click on a line to highlight a year")
# 
# highlight(gg)


## ----------------------------------------------------------------------------
#| label: plotly-modify
#| warning: false
#| message: false
#| echo: false
txh_shared <- highlight_key(txhousing, ~year)

p <- ggplot(txh_shared, aes(month, median)) +
   geom_line(aes(group = year)) + 
   geom_smooth(data = txhousing, method = "gam") + 
   scale_x_continuous("", breaks=seq(1, 12, 1),
        labels=c("J", "F", "M", "A", "M", "J", 
                 "J", "A", "S", "O", "N", "D")) +
   scale_y_continuous("Median price ('00,000)", 
                      breaks = seq(0,300000,100000),
                      labels = seq(0,3,1)) +
   facet_wrap(~ city)

gg <- ggplotly(p, height = 800, width = 1100) |>
   plotly::layout(title = "Click on a line to highlight a year")

highlight(gg)


## ----------------------------------------------------------------------------
#| label: tsibbletalk1
#| code-fold: true
#| code-line-numbers: "7,8"
tourism_shared <- tourism |>
  as_shared_tsibble(spec = (State / Region) * Purpose)

tourism_feat <- tourism_shared |>
  features(Trips, feat_stl)

p1 <- tourism_shared |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line(aes(group = Region), alpha = 0.5) +
  facet_wrap(~ Purpose, scales = "free_y") +
  theme(axis.title = element_text(family="Helvetica"),
        axis.text = element_text(family="Helvetica"),
        legend.title = element_text(family="Helvetica"),
        legend.text = element_text(family="Helvetica"))
p2 <- tourism_feat |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(group = Region)) +
  xlab("trend") + ylab("season") +
  theme(axis.title = element_text(family="Helvetica"),
        axis.text = element_text(family="Helvetica"),
        legend.title = element_text(family="Helvetica"),
        legend.text = element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica"))
subplot(
    ggplotly(p1, tooltip = "Region", width = 1200, height = 600) |>
  config(displayModeBar = FALSE),
    ggplotly(p2, tooltip = "Region", width = 1000, height = 500) |>
  config(displayModeBar = FALSE),
    nrows = 1, widths=c(0.5, 0.5), heights=1) |>
  highlight(dynamic = FALSE)



## ----------------------------------------------------------------------------
#| code-fold: true
# Read the data
# Replace null with 0, for three LGAs
# Convert to long form to join with polygons
# Make the date variables a proper date
# Set NAs to 0, this is a reasonable assumption
covid <- read_csv("data/melb_lga_covid.csv") |>
  mutate(Buloke = as.numeric(ifelse(Buloke == "null", "0", Buloke))) |>
   mutate(Hindmarsh = as.numeric(ifelse(Hindmarsh == "null", "0", Hindmarsh))) |>
   mutate(Towong = as.numeric(ifelse(Towong == "null", "0", Towong))) |>
  pivot_longer(cols = Alpine:Yarriambiack, names_to="NAME", values_to="cases") |>
  mutate(Date = ydm(paste0("2020/",Date))) |>
  mutate(cases=replace_na(cases, 0))


## ----------------------------------------------------------------------------
#| code-fold: true

# Case counts are cumulative, keep only latest
covid <- covid |>
  filter(Date == ymd("2020-10-20"))


## ----------------------------------------------------------------------------
#| eval: false
#| echo: false
#| message: false
# # Read the LGA data from strayr package.
# # This has LGAs for all of Australia.
# # Need to filter out Victoria LGAs, avoiding LGAs
# # from other states with same name, and make the names
# # match covid data names. The regex equation is
# # removing () state and LGA type text strings
# # Good reference: https://r-spatial.github.io/sf/articles/sf1.html
# # remotes::install_github("runapp-aus/strayr")
# library(strayr)
# library(ggthemes)
# library(sf)
# lga <- strayr::read_absmap("lga2018") |>
#   rename(lga = lga_name_2018) |>
#   filter(state_name_2016 == "Victoria")
# save(lga, file="data/lga.rda")


## ----------------------------------------------------------------------------
#| echo: false
#| eval: false
# ggplot(lga) + geom_sf() + theme_map()
# 
# lga_sm <- ms_simplify(lga)
# save(lga_sm, file="data/lga_sm.rda")
# ggplot(lga_sm) + geom_sf() + theme_map()


## ----------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 8
#| fig-height: 6
#| out-width: 80%
load("data/lga.rda")

covid_tot <- covid |>
  left_join(lga, by=c("NAME" = "lga")) |>
  st_as_sf()

# Make choropleth map, with appropriate colour palette
cm1 <- ggplot(covid_tot) + 
  geom_sf(aes(fill = cases, label = NAME),
    colour="grey80") + 
  scale_fill_distiller("Cases", 
    palette = "PuBuGn",
    direction=1) + 
  theme_map() +
  theme(legend.position="bottom")
cm1
 


## ----------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 8
#| fig-height: 6
#| out-width: 80%
pop <- read_xlsx("data/VIF2019_Population_Service_Ages_LGA_2036.xlsx", sheet=3, skip=13, col_names = FALSE) |>
  select(`...4`, `...22`) |>
  rename(lga = `...4`, pop=`...22`) |>
  filter(lga != "Unincorporated Vic") |> 
  mutate(lga = str_replace(lga, " \\(.+\\)", "")) |>
  mutate(lga = ifelse(lga == "Colac-Otway", "Colac Otway", lga)) 

covid_tot <- covid_tot |>
  left_join(pop, by=c("NAME" = "lga")) 

covid_tot <- covid_tot |>
  mutate(cases_per10k = cases/pop*10000,
         lcases = log10(cases + 1)) 

covid_tot_carto <- covid_tot |> 
  st_transform(3395) |> 
  cartogram_cont("pop") |>
  st_transform("WGS84")   
  
covid_tot_carto <- st_cast(covid_tot_carto, "MULTIPOLYGON") 

cm2 <- ggplot(covid_tot_carto) + 
  geom_sf(aes(fill = cases, label=NAME),
    colour="grey80") + 
  scale_fill_distiller("Cases", palette = "PuBuGn",
                       direction=1) + 
  theme_map() +
  theme(legend.position="bottom")  
cm2 


## ----------------------------------------------------------------------------
#| code-fold: true
# Placement of hexmaps depends on position relative to
# Melbourne central
data(capital_cities)
covid_hexmap <- create_hexmap(
  shp = covid_tot,
  sf_id = "NAME",
  focal_points = capital_cities, verbose = TRUE)

# Hexagons are made with the `fortify_hexagon` function
covid_hexmap_poly <- covid_hexmap |>
  fortify_hexagon(sf_id = "NAME", hex_size = 0.1869) |>
  left_join(covid_tot, by="NAME") # hexmap code removed cases!
cm3 <- ggplot() +
  geom_sf(data=covid_tot, 
          fill = "white", colour = "grey80", size=0.1) +
  geom_polygon(data=covid_hexmap_poly, 
               aes(x=long, y=lat, group=hex_id, 
                   fill = cases, 
                   colour = cases,
                   label=NAME), size=0.2) +
  scale_fill_distiller("Cases", palette = "PuBuGn",
                       direction=1) +
  scale_colour_distiller("Cases", palette = "PuBuGn",
                       direction=1) +
  theme_map() +
  theme(legend.position="bottom")
cm3


## ----------------------------------------------------------------------------
#| code-fold: true
cm1 <- cm1 + theme(legend.position = "none")
ggplotly(cm1, width=800, height=600) |>
  config(displayModeBar = FALSE)


## ----------------------------------------------------------------------------
#| code-fold: true
cm2 <- cm2 + theme(legend.position = "none")
ggplotly(cm2, width=800, height=600) |>
  config(displayModeBar = FALSE)


## ----------------------------------------------------------------------------
#| code-fold: true
cm3 <- cm3 + theme(legend.position = "none")
ggplotly(cm3, width=800, height=600) |>
  config(displayModeBar = FALSE)

