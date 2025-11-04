## ----include = FALSE--------------------------------
# Load libraries 
source("libraries.R")

# Set up chunk for all slides
source("chunk_options_and_themes.R")


## ----read TB incidence data and check, results='hide'----
tb <- read_csv("data/TB_notifications_2025-07-22.csv")
tb   %>%                                # first we get the tb data
  filter(year == 2023) %>%              # then we focus on the most recent year
  group_by(country) %>%                 # then we group by country
  summarize(
    cases = sum(c_newinc, na.rm=TRUE)   # to create a summary of all new cases
    ) %>%
  arrange(desc(cases))                  # then we sort countries to show highest number of new cases first


## ----read TB incidence data and check base pipe, results='hide'----
tb <- read_csv("data/TB_notifications_2025-07-22.csv")
tb |>                                  # first we get the tb data
  filter(year == 2023) |>              # then we focus on the most recent year
  group_by(country) |>                 # then we group by country
  summarize(
    cases = sum(c_newinc, na.rm=TRUE)   # to create a summary of all new cases
    ) |> 
  arrange(desc(cases))                  # then we sort countries to show highest number new cases first


## ----show-results, echo=FALSE-----------------------
tb <- read_csv("data/TB_notifications_2025-07-22.csv")
tb |>                                  # first we get the tb data
  filter(year == 2023) |>              # then we focus on the most recent year
  group_by(country) |>                 # then we group by country
  summarize(
    cases = sum(c_newinc, na.rm=TRUE)   # to create a summary of all new cases
    ) |> 
  arrange(desc(cases))                  # then we sort countries to show highest number of new cases first


## ----example 1 What are the variables, echo=FALSE----
grad <- read_csv("data/graduate-programs.csv")
head(grad[c(2,3,4,6)])


## ----example 2 whats in the column names, echo=FALSE----
genes <- read_csv("data/genes.csv")
head(genes)


## ----example 3 what are the variables and records, echo=FALSE----
melbtemp <- read.fwf("data/ASN00086282.dly", 
   c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31)), fill=T)
head(melbtemp[,c(1,2,3,4,seq(5,100,4))])


## ----example 4 what are the variables and experimental units, echo=FALSE----
tb <- read_csv("data/tb.csv")
tail(tb)
#colnames(tb)


## ----example 4 what are the variables and observations, echo=FALSE----
pew <- read.delim(
  file = "http://stat405.had.co.nz/data/pew.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = F
)
pew[1:5, 1:5]


## ----example 6 what are the factors measurements and experimental units, echo = FALSE----
load("data/french_fries.rda")
head(french_fries, 4)


## ----setup a simple example, echo = FALSE-----------
dframe <- data.frame(id = 1:2, trtA=c(2.5,4.6), trtB = c(45, 35))


## ----gather the example data into long form---------
# wide format
dframe

# long format
dframe |> pivot_longer(trtA:trtB, names_to="treatment", values_to="outcome")


## ----read in and process the TB data----------------
read_csv("data/TB_notifications_2025-07-22.csv") |> 
  dplyr::select(country, iso3, year, starts_with("new_sp_")) |>
  na.omit() |>
  head()


## ----turn TB data into long form--------------------
tb1 <- read_csv("data/TB_notifications_2025-07-22.csv") |> 
  dplyr::select(country, iso3, year, starts_with("new_sp_")) |>
  pivot_longer(starts_with("new_sp_")) 

tb1 |> na.omit() |> head()


## ----extract variable names from original column names----
tb2 <- tb1 |>
  separate_wider_delim(
    name, delim = "_", 
    names=c("toss_new", "toss_sp", "sexage")) 

tb2 |> na.omit() |> head()


## ----continue extracting variable names-------------
tb3 <- tb2 %>% dplyr::select(-starts_with("toss")) |> # remove the `toss` variables
  separate_wider_position(
    sexage,
    widths = c(sex = 1, age = 4),
    too_few = "align_start"
  )

tb3 |> na.omit() |> head()


## ---------------------------------------------------
genes <- read_csv("data/genes.csv")

names(genes)


## ---------------------------------------------------
#| label: code solution to genes wrangling
#| code-fold: true
gtidy <- genes |>
  pivot_longer(-id, names_to="variable", values_to="expr") |>
  separate_wider_delim(variable, names=c("trt", "leftover"), 
                       delim = "-") |>
  separate_wider_delim(leftover, names=c("time", "rep"), 
                       delim = ".") |>
  mutate(trt = sub("W", "", trt)) |>
  mutate(rep = sub("R", "", rep))


## ---------------------------------------------------
head(gtidy)


## ----compute group means, fig.show='hide'-----------
gmean <- gtidy |> 
  group_by(id, trt, time) |> 
  summarise(expr = mean(expr))
gtidy |> 
  ggplot(aes(x = trt, y = expr, colour=time)) +
  geom_point() +
  geom_line(data = gmean, aes(group = time)) +
  facet_wrap(~id) +
  scale_colour_brewer("", palette="Set1")


## ----plot the genes data overlaid with group means, echo=FALSE, fig.width=5, fig.height = 5----
gtidy |> 
  ggplot(aes(x = trt, y = expr, colour=time)) +
  geom_point() +
  geom_line(data = gmean, aes(group = time)) +
  facet_wrap(~id) +
  scale_colour_brewer("", palette="Set1")


## ----echo = FALSE-----------------------------------
xlsx_df <- read_excel("data/lecture3-example.xlsx",
                 col_types = c("text", "date", "text", "numeric"))  |> 
  mutate(id = as.factor(id), 
         date = as.character(date),
         date = as.Date(date, format = "%Y-%d-%m"))


## ----echo = TRUE------------------------------------
vis_dat(xlsx_df)


## ----echo = TRUE------------------------------------
df <- read_excel("data/lecture3-example.xlsx")
df


## ----echo = TRUE------------------------------------
df <- read_excel("data/lecture3-example.xlsx", 
                 col_types = c("text", 
                               "date", 
                               "text",
                               "numeric"))

df |> 
  mutate(id = as.factor(id),
         date = ydm(date)) |>
  mutate(
         day = day(date),
         month = month(date),
         year = year(date)) 


## ---------------------------------------------------
options(width=80)
raw_dat <- read_csv("data/world-development-indicators.csv", 
                    na = "..", n_max = 11935)
glimpse(raw_dat)


## ---------------------------------------------------
country_code_df <- raw_dat  |>
  distinct(`Country Name`, `Country Code`)  |>
  rename_all(janitor::make_clean_names)  |>
  left_join(
    countrycode::codelist |> select(iso3c, region, continent),
    by = c("country_code" = "iso3c")
  )  |>
  arrange(continent, region) 


## ---------------------------------------------------
#| echo: false
options(width=80)
glimpse(country_code_df)

country_code_df |> count(continent)
country_code_df |> count(region)



## ---------------------------------------------------
country_code_df |> filter(is.na(continent))


## ---------------------------------------------------
wdi_vars <- raw_dat  |>
  select(`Series Name`, `Series Code`) |>
  distinct() |>
  rename_all(janitor::make_clean_names) 


## ---------------------------------------------------
#| echo: false
wdi_vars |> gt::gt()


## ---------------------------------------------------
wdi <- raw_dat  |>
  select(`Country Code`, `Series Code`, `1969 [YR1969]`:`2018 [YR2018]`) |>
  rename_all(janitor::make_clean_names) |>
  pivot_longer(x1969_yr1969:x2018_yr2018,
               names_to = "year", 
               values_to = "value") |>
  mutate(year = as.numeric(str_sub(year, 2, 5)) ) |>
  pivot_wider(names_from = series_code,
              values_from = value)

wdi2017 <- wdi  |> filter(year == 2017)


## ---------------------------------------------------
#| fig-width: 8
#| fig-height: 7
#| code-fold: true
vis_miss(wdi, sort_miss = TRUE)


## ---------------------------------------------------
#| fig-width: 8
#| fig-height: 6
#| code-fold: true
gg_miss_var(wdi, show_pct=TRUE)


## ---------------------------------------------------
#| code-fold: true
#| fig-width: 2
#| fig-height: 8
#| out-width: 100%
wdi_cnt_miss <- wdi |> 
  filter(!is.na(country_code)) |>
  bind_shadow() |>
  select(country_code, year,
         SP.ADO.TFRT_NA:SP.URB.GROW_NA) |>
  pivot_longer(SP.ADO.TFRT_NA:SP.URB.GROW_NA,
               names_to="var",
               values_to="value") |>
  group_by(country_code) |>
  count(value) |>
  mutate(value = fct_recode(value, 
                            miss="NA",
                            not="!NA")) |>
  pivot_wider(names_from = value, values_from = n) |>
  mutate(p_miss = miss/(miss+not)) |>
  select(country_code, p_miss)
wdi_cnt_p <- wdi_cnt_miss |> 
  ggplot(aes(x=1, y=p_miss, 
             label=country_code)) +
  geom_quasirandom() +
  ylim(c(0,1)) + ylab("Prop miss") 
ggplotly(wdi_cnt_p)


## ---------------------------------------------------
#| fig-width: 8
#| fig-height: 6
#| code-fold: true
wdi_cri <- wdi |>
  filter(country_code == "CRI")
vis_miss(wdi_cri, sort_miss=TRUE)


## ---------------------------------------------------
#| fig-width: 6
#| fig-height: 3
#| code-fold: true
wdi_cri_p <- wdi_cri |>
  ggplot(aes(x=year, y=SE.PRM.CMPT.ZS)) +
  geom_miss_point() +
  theme(aspect.ratio=0.5, 
        legend.position = "none") 
wdi_cri_p


## ---------------------------------------------------
#| fig-width: 6
#| fig-height: 3
#| code-fold: true
wdi_cri_v1 <- wdi_cri |>
  mutate(SE.PRM.CMPT.ZS = na_ma(SE.PRM.CMPT.ZS))

wdi_cri_v1  |>
  ggplot(aes(x=year, y=SE.PRM.CMPT.ZS)) +
  geom_point() +
  geom_smooth(se=F, colour="#D55E00") +
  theme(aspect.ratio=0.5) 


## ---------------------------------------------------
#| echo: false

1 + 1


## ---------------------------------------------------
#| echo: false

library(ggplot2)

data(cars)

table_data <- head(cars, 5)

knitr::kable(table_data,
             caption = "Speed and stopping 
             distances of cars") |> 
  kableExtra::kable_paper(
    full_width = TRUE
    )


## ---------------------------------------------------
#| label: cars-plot
#| fig-cap: "Distance taken for a car to stop, against it's speed during the test."
#| fig-height: 3

library(ggplot2)

ggplot(cars,
      aes(x = speed,
          y = dist)
      ) +
  geom_point()


## ----out.width="110%", echo=FALSE-------------------
knitr::include_app("https://ebsmonash.shinyapps.io/VICfire/", height = "550px") 

