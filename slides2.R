## ----include = FALSE--------------------------------
# Load libraries 
source("libraries.R")

# Set up chunk for all slides
source("chunk_options_and_themes.R")


## ---------------------------------------------------
#| label: select_a_subset_of_the_observations
#| code-line-highlight: "3"
load("data/french_fries.rda")
french_fries |>
    filter(subject == 3, time == 1) 
ff_long <- french_fries |> 
  pivot_longer(potato:painty, 
               names_to = "type", 
               values_to = "rating")


## ----order the observations-------------------------
#| code-line-numbers: true
#| code-line-highlight: "2"
french_fries |>
    arrange(desc(rancid)) |> 
    head()


## ----select a subset of the variables---------------
#| code-line-numbers: "2"
french_fries |>
    select(time, treatment, subject, rep, potato) |> 
    head()


## ----summarize observations into one-number statistic----
#| code-line-numbers: "2,5"
french_fries |>
    summarise( 
      mean_rancid = mean(rancid, na.rm=TRUE), 
      sd_rancid = sd(rancid, na.rm = TRUE)
      ) 


## ----summarise and group_by-------------------------
french_fries |>
    group_by(time, treatment) |>
    summarise(mean_rancid = mean(rancid), sd_rancid = sd(rancid))


## ---------------------------------------------------
#| label:  checking design completeness
french_fries |> 
  group_by(subject) |> 
  summarize(n = n()) 


## ----counts for subject by time---------------------
options(width=120)
french_fries |>
  count(subject, time) |>
  pivot_wider(names_from="time", values_from="n")


## ----do-scores-change-over-time, fig.show='hide'----
ggplot(data=ff_long, aes(x=time, y=rating, colour=treatment)) +
  geom_point() +
  facet_grid(subject~type) 


## ----ref.label="do-scores-change-over-time", echo=FALSE, fig.width=12, fig.height=9, out.width="100%"----


## ----ff line plots, fig.width=12, fig.height=9, out.width="80%"----
#| code-fold: true
ff_av <- ff_long |> 
  group_by(subject, time, type, treatment) |>
  summarise(rating=mean(rating))

ggplot(data=ff_long, aes(x=time, y=rating, colour=treatment)) + 
  facet_grid(subject~type) +
  geom_line(data=ff_av, aes(group=treatment))


## ---------------------------------------------------
#| label: read-and-wrangle-TB-data
#| echo: false
tb <- readr::read_csv("data/TB_notifications_2025-07-22.csv") |>
  dplyr::select(country, iso3, year, new_sp_m04:new_sp_fu) |>
  pivot_longer(new_sp_m04:new_sp_fu, names_to="stuff", values_to="count") |>
  separate(stuff, c("stuff1", "stuff2", "sexage")) |>
  dplyr::select(-stuff1, -stuff2) |>
  mutate(sex=substr(sexage, 1, 1), 
         age=substr(sexage, 2, length(sexage))) |>
  dplyr::select(-sexage)

# Filter data to get the US
tb_idn <- tb |> 
  filter(iso3 == "IDN") |>
  filter(!(age %in% c("04", "014", "514", "u"))) |>
  filter(year > 2000, year < 2013) |>
  mutate(
    age_group = factor(age, labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
  )


## ---------------------------------------------------
#| label: make-a-barchart-of-US-TB-incidence
#| code-fold: true
#| out-width: 80%
#| fig-width: 10
#| fig-height: 3
ggplot(tb_idn, aes(x = year, 
                  y = count, 
                  fill = sex)) +
  geom_bar(stat = "identity") +
  facet_grid(~age) 


## ---------------------------------------------------
#| label: compare-proportions-of-males-females
#| out-width: 80%
#| fig-width: 10
#| fig-height: 3
#| code-line-numbers: "3,4"
#| code-fold: true
ggplot(tb_idn, aes(x=year, y=count, fill=sex)) +  
  geom_bar(stat="identity", position="fill") + 
  ylab("proportion") + 
  facet_grid(~age_group) +
  scale_x_continuous("year", breaks = seq(2000, 2012, 5), 
                     labels = c("00", "05", "10"))


## ---------------------------------------------------
#| label: compare-counts-of-males-females
#| out-width: 100%
#| code-fold: true
#| fig-height: 5
#| fig-width: 10
#| code-line-numbers: "5"
# Make separate plots for males and females, focus on counts by category
ggplot(tb_idn, aes(x=year, y=count, fill=sex)) +
  geom_bar(stat="identity") +
  facet_grid(sex~age_group) + 
  scale_x_continuous("year", breaks=seq(2000, 2012, 5), 
                     labels=c("00", "05", "10"))


## ---------------------------------------------------
#| label: rose-plot-of-males-females
#| code-fold: true
#| fig-height: 5
#| fig-width: 10
#| code-line-numbers: "6"
# How to make a pie instead of a barchart - not straight forward
ggplot(tb_idn, aes(x=year, y=count, fill=sex)) +
  geom_bar(stat="identity") + 
  facet_grid(sex~age_group) + 
  coord_polar() + 
  scale_x_continuous("year", breaks=seq(2000, 2012, 5), 
                     labels=c("00", "05", "10"))


## ---------------------------------------------------
#| label: stacked-barchart-of-males-females
#| code-fold: true
#| fig-height: 5
#| fig-width: 8
#| out-width: 100%
#| code-line-numbers: "3"
# Step 1 to make the pie
ggplot(tb_idn, aes(x = 1, y = count, fill = factor(year))) +
  geom_bar(stat="identity", position="fill") + 
  facet_grid(sex~age_group) +
  scale_fill_viridis_d("", option="inferno") 


## ---------------------------------------------------
#| label: pie-chart-of-males-females
#| out-width: 100%
#| code-fold: true
#| fig-height: 6
#| fig-width: 10
#| code-line-numbers: "3,4,7"
# Now we have a pie, note the mapping of variables
# and the modification to the coord_polar
ggplot(tb_idn, aes(x = 1, y = count, fill = factor(year))) + 
  geom_bar(stat="identity", position="fill") + 
  facet_grid(sex~age_group) +
  scale_fill_viridis_d("", option="inferno") +
  coord_polar(theta = "y") 


## ---------------------------------------------------
#| eval: false
#| out-width: 60%
#| code-fold: true
#| fig-height: 5
#| code-line-numbers: "1,3"
# # age for segments, facet by year and sex
# ggplot(tb_idn, aes(x = 1, y = count, fill = factor(age_group))) +
#   geom_bar(stat="identity", position="fill") +
#   facet_grid(sex~year) +
#   scale_fill_viridis_d("", option="inferno") +
#   coord_polar(theta = "y") +
#   theme(legend.position="bottom")


## ----use a line plot instead of bar, fig.width=10, fig.height=3, out.width="100%"----
#| code-fold: true
ggplot(tb_idn, aes(x=year, y=count, colour=sex)) +
  geom_line() + 
  geom_point() +
  facet_grid(~age_group) +
  ylim(c(0,NA)) + 
  scale_x_continuous("year", breaks=seq(2000, 2012, 5), 
                     labels=c("00", "05", "10"),
                     limits=c(2000, 2012))


## ----use a line plot of proportions, fig.width=10, fig.height=3, out.width="100%"----
#| code-fold: true
tb_idn |> group_by(year, age_group) |> 
  summarise(p = count[sex=="m"]/sum(count)) |>
  ggplot(aes(x=year, y=p)) +
    geom_hline(yintercept = 0.50, colour="grey50", linewidth=2) +
    geom_line() + 
    geom_point() +
    facet_grid(~age_group) +
    ylab("Proportion of Males") + 
    scale_x_continuous("year", breaks=seq(2000, 2012, 5), 
                     labels=c("00", "05", "10"),
                     limits=c(2000, 2012))


## ----use a smoother instead of bar, fig.width=10, fig.height=3, out.width="100%"----
#| code-fold: true
ggplot(tb_idn, aes(x=year, y=count, colour=sex)) +
  geom_point() +
  geom_smooth(se=F) + 
  facet_grid(~age_group) +
  ylim(c(0,NA)) + 
  scale_x_continuous("year", breaks=seq(2000, 2012, 5), 
                     labels=c("00", "05", "10"),
                     limits=c(2000, 2012))


## ----use a smoother of proportions, fig.width=10, fig.height=3, out.width="100%"----
#| code-fold: true
tb_idn |> group_by(year, age_group) |> 
  summarise(p = count[sex=="m"]/sum(count)) |>
  ggplot(aes(x=year, y=p)) +
    geom_hline(yintercept = 0.50, colour="grey50", linewidth=2) +
    geom_point() +
    geom_smooth(se=F) + 
    facet_grid(~age_group) +
    ylab("Proportion of Males") + 
    scale_x_continuous("year", breaks=seq(2000, 2012, 5), 
                     labels=c("00", "05", "10"),
                     limits=c(2000, 2012))


## ---------------------------------------------------
#| label: tb-comparisons1
#| fig-width: 5
#| fig-height: 4
#| code-fold: true
tb_inc_100k <- read_csv("data/TB_burden_countries_2025-07-22.csv") |>
  filter(iso3 %in% c("USA", "AUS"))
ggplot(tb_inc_100k, aes(y = iso3, 
                        x = e_inc_100k)) +
  stat_gradientinterval(fill = "darkorange") +
  ylab("") +
  xlab("Inc per 100k") +
  theme_ggdist()


## ---------------------------------------------------
#| label: tb-comparisons2
#| fig-width: 5
#| fig-height: 4
#| code-fold: true
ggplot(tb_inc_100k, aes(y = iso3, 
                        x = e_inc_100k)) +
  stat_halfeye(side = "right") +
  geom_dots(side="left", 
                    fill = "darkorange", color = "darkorange") +
  ylab("") +
  xlab("Inc per 100k") +
  theme_ggdist()


## ---------------------------------------------------
#| label: election1
#| code-fold: true
election <- read_csv("data/election2019.csv",
  skip = 1,
  col_types = cols(
    .default = col_character(),
    OrdinaryVotes = col_double(),
    AbsentVotes = col_double(),
    ProvisionalVotes = col_double(),
    PrePollVotes = col_double(),
    PostalVotes = col_double(),
    TotalVotes = col_double(),
    Swing = col_double()
  )
)
e_grn <- election |>
  group_by(DivisionID) |>
  summarise(
    DivisionNm = unique(DivisionNm),
    State = unique(StateAb),
    votes_GRN = TotalVotes[which(PartyAb == "GRN")],
    votes_total = sum(TotalVotes)
  ) |>
  mutate(perc_GRN = votes_GRN / votes_total * 100)

e_grn |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(x=perc_GRN, y=State)) +
    geom_quasirandom(groupOnX = FALSE, varwidth = TRUE) +
    labs(
      x = "First preference votes %",
      y = ""
    ) +
  xlim(c(0,50))



## ---------------------------------------------------
#| label: election1
#| echo: false
election <- read_csv("data/election2019.csv",
  skip = 1,
  col_types = cols(
    .default = col_character(),
    OrdinaryVotes = col_double(),
    AbsentVotes = col_double(),
    ProvisionalVotes = col_double(),
    PrePollVotes = col_double(),
    PostalVotes = col_double(),
    TotalVotes = col_double(),
    Swing = col_double()
  )
)
e_grn <- election |>
  group_by(DivisionID) |>
  summarise(
    DivisionNm = unique(DivisionNm),
    State = unique(StateAb),
    votes_GRN = TotalVotes[which(PartyAb == "GRN")],
    votes_total = sum(TotalVotes)
  ) |>
  mutate(perc_GRN = votes_GRN / votes_total * 100)

e_grn |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(x=perc_GRN, y=State)) +
    geom_quasirandom(groupOnX = FALSE, varwidth = TRUE) +
    labs(
      x = "First preference votes %",
      y = ""
    ) +
  xlim(c(0,50))



## ---------------------------------------------------
#| label: election1
#| echo: false
#| out-width: 80%
election <- read_csv("data/election2019.csv",
  skip = 1,
  col_types = cols(
    .default = col_character(),
    OrdinaryVotes = col_double(),
    AbsentVotes = col_double(),
    ProvisionalVotes = col_double(),
    PrePollVotes = col_double(),
    PostalVotes = col_double(),
    TotalVotes = col_double(),
    Swing = col_double()
  )
)
e_grn <- election |>
  group_by(DivisionID) |>
  summarise(
    DivisionNm = unique(DivisionNm),
    State = unique(StateAb),
    votes_GRN = TotalVotes[which(PartyAb == "GRN")],
    votes_total = sum(TotalVotes)
  ) |>
  mutate(perc_GRN = votes_GRN / votes_total * 100)

e_grn |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(x=perc_GRN, y=State)) +
    geom_quasirandom(groupOnX = FALSE, varwidth = TRUE) +
    labs(
      x = "First preference votes %",
      y = ""
    ) +
  xlim(c(0,50))



## ---------------------------------------------------
#| label: election2
#| out-width: 80%
#| code-fold: true
e_grn |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(x=perc_GRN, y=State)) +
    geom_boxplot(varwidth = TRUE) +
    labs(
      x = "First preference votes %",
      y = ""
    ) +
  xlim(c(0,50))


## ---------------------------------------------------
#| label: election3
#| out-width: 80%
#| code-fold: true
e_grn |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(x=perc_GRN, y=State)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
      fill="#006dae", alpha=0.5) +
    labs(
      x = "First preference votes %",
      y = ""
    ) +
  xlim(c(0,50))


## ---------------------------------------------------
#| label: election4
#| fig-width: 8
#| fig-height: 6
#| out-width: 100%
#| code-fold: true
oz_states <- ozmaps::ozmap_states %>% filter(NAME != "Other Territories")
oz_votes <- rmapshaper::ms_simplify(ozmaps::abs_ced)
oz_votes_grn <- full_join(oz_votes, e_grn, by=c("NAME"="DivisionNm"))

ggplot(oz_votes_grn, aes(fill=perc_GRN)) +
  geom_sf(colour="white") +
  scale_fill_viridis_c(direction=-1, trans = "log", 
    guide = "colourbar", 
    labels = scales::label_number(accuracy = 0.1)) +
  theme_map() +
  theme(legend.position = "right", 
    legend.title = element_blank())


## ---------------------------------------------------
#| label: model
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
#| code-fold: true
data("wages")
wages_fct <- wages |>
  select(id, ln_wages, xp, high_grade) |>
  mutate(high_grade = factor(high_grade))
wages_fit <- lmer(ln_wages~xp + high_grade + (xp|id), data=wages_fct)
wages_fe <- summary(wages_fit)$coefficients
wages_fe_d <- tibble(xp = rep(seq(0, 13, 1), 7),
     high_grade = rep(c(6, 7, 8, 9, 10, 11, 12), rep(14, 7))) |>
  mutate(ln_wages = case_when(
    high_grade == 6 ~ wages_fe[1,1] + wages_fe[2,1]*xp,
    high_grade == 7 ~ wages_fe[1,1] + wages_fe[3,1] + wages_fe[2,1]*xp,
    high_grade == 8 ~ wages_fe[1,1] + wages_fe[4,1]  + wages_fe[2,1]*xp,
    high_grade == 9 ~ wages_fe[1,1] + wages_fe[5,1]  + wages_fe[2,1]*xp,
    high_grade == 10 ~ wages_fe[1,1] + wages_fe[6,1]  + wages_fe[2,1]*xp,
    high_grade == 11 ~ wages_fe[1,1] + wages_fe[7,1]  + wages_fe[2,1]*xp,
    high_grade == 12 ~ wages_fe[1,1] + wages_fe[8,1]  + wages_fe[2,1]*xp)
  ) |>
  mutate(high_grade = factor(high_grade))
ggplot(wages_fe_d) + 
  geom_line(aes(x=xp, 
                y=ln_wages, 
                colour=high_grade, 
                group=high_grade)) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  labs(x="Experience (years)", y="Wages (ln)", colour="Grade") 


## ---------------------------------------------------
#| label: modelanddata
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
#| code-fold: true
ggplot() + 
  geom_line(data=wages_fct, aes(x=xp, y=ln_wages, group=id), alpha=0.1) +
  geom_line(data=wages_fe_d, aes(x=xp, 
                y=ln_wages, 
                colour=high_grade, 
                group=high_grade)) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  labs(x="Experience (years)", y="Wages (ln)", colour="Grade") 


## ---------------------------------------------------
#| label: SE
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
#| code-fold: true
wages_fe_d <- wages_fe_d |>
  mutate(ln_wages_l = case_when(
    high_grade == 6 ~ wages_fe[1,1] - wages_fe[1,2] +
                      (wages_fe[2,1]-wages_fe[2,2])*xp ,
    high_grade == 7 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[3,1] - wages_fe[3,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 8 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[4,1] - wages_fe[4,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 9 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[5,1] - wages_fe[5,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 10 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[6,1] - wages_fe[6,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 11 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[7,1] - wages_fe[7,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 12 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[8,1] - wages_fe[8,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp)
  ) |>
  mutate(ln_wages_u = case_when(
    high_grade == 6 ~ wages_fe[1,1] + wages_fe[1,2] +
                      (wages_fe[2,1]+wages_fe[2,2])*xp ,
    high_grade == 7 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[3,1] + wages_fe[3,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 8 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[4,1] + wages_fe[4,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 9 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[5,1] + wages_fe[5,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 10 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[6,1] + wages_fe[6,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 11 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[7,1] + wages_fe[7,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 12 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[8,1] + wages_fe[8,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp)
  ) 

ggplot() + 
  geom_ribbon(data=wages_fe_d, 
            aes(x=xp, 
                ymin=ln_wages_l,
                ymax=ln_wages_u,
                fill=high_grade), colour=NA, alpha=0.1) +
  geom_line(data=wages_fe_d, 
            aes(x=xp, 
                y=ln_wages, 
                colour=high_grade, 
                group=high_grade)) +
  scale_fill_discrete_divergingx(palette = "Zissou 1") +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  labs(x="Experience (years)", y="Wages (ln)", colour="Grade", fill="Grade") 



## ---------------------------------------------------
#| label: indiv-fits
#| fig-width: 8
#| fig-height: 5
#| out-width: 100%
#| code-fold: true
wages_full <- wages_fct |>
  add_predictions(wages_fit, 
                  var = "pred") |>
  add_residuals(wages_fit, 
                var = "res")
set.seed(1222)
wages_full |> add_n_obs() |> filter(n_obs > 4) |>
  sample_n_keys(size = 12) |>
  ggplot() + 
  geom_line(aes(x = xp, y = pred, group = id, 
             colour = factor(id))) + 
  geom_point(aes(x = xp, y = ln_wages, 
                 colour = factor(id))) + 
  facet_wrap(~id, ncol=4)  +
  scale_x_continuous("Experience (years)", 
    breaks=seq(0, 12, 2)) +
  ylab("Wages (ln)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## ---------------------------------------------------
#| eval: false
#| echo: false
# ped <- rwalkr::melb_walk(from = as.Date("2025-08-01"),
#                          to = as.Date("2025-08-31"))
# ped <- ped |>
#   mutate(wday = wday(Date, label=TRUE,
#       abbr=TRUE, week_start=1)) |>
#   mutate(Count = as.numeric(Count))
# save(ped, file="data/ped_Aug2025.rda")


## ---------------------------------------------------
#| label: pedestrians
#| echo: false
load("data/ped_Aug2025.rda")
ped_sc <- ped |>
  filter(Sensor == "Southern Cross Station") |>
  filter(Date == ymd("2025-08-31")) |>
  group_by(Time) |>
  summarise(Count = sum(Count), .groups = "drop") |>
  mutate(se = sqrt(Count))
b1 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
  geom_col(fill = "#20794D") +
  xlab("Hour")
b2 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
  geom_col(fill = "#b9ca4a") +
  geom_errorbar(aes(ymin = Count - se, ymax = Count + se),
    width=0.5, colour="#20794D") +
  xlab("Hour")
b3 <- ggplot(ped_sc, aes(x=Time,
    ydist=distributional::dist_normal(Count, se))) +
  stat_pointinterval(colour = "#20794D") +
  xlab("Hour") + ylab("Count")
b4 <- ggplot(ped_sc, aes(x=Time,
    ydist=distributional::dist_normal(Count, se))) +
  stat_gradientinterval(colour = NA, fill="#20794D", 
    .width=1) +
  geom_line(aes(x=Time, y=Count), colour="#20794D") +
  xlab("Hour") + ylab("Count")
b5 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
  geom_ribbon(aes(ymin = Count - qnorm(0.975)*se, 
                  ymax = Count + qnorm(0.975)*se),
    fill = "#b9ca4a") +
  geom_line(colour="#20794D") +
  xlab("Hour")
ped_sc_ci <- ped_sc |>
  mutate(l50 = Count - qnorm(0.75)*se,
         u50 = Count + qnorm(0.75)*se,
         l80 = Count - qnorm(0.9)*se,
         u80 = Count + qnorm(0.9)*se,
         l99 = Count - qnorm(0.995)*se,
         u99 = Count + qnorm(0.995)*se
  ) |>
  pivot_longer(cols=l50:u99, names_to = "intprob", 
    values_to="value") |>
  mutate(bound = str_sub(intprob, 1, 1),
       prob = str_sub(intprob, 2, 3)) |>
  select(Time, Count, se, prob, bound, value) |>
  pivot_wider(names_from = bound, values_from = value)
b6 <- ggplot(ped_sc_ci, aes(x=Time, y=Count)) +
  geom_lineribbon(aes(ymin = l, ymax = u, fill = prob)) +
  labs(x="Hour", fill="Confidence") +
  scale_fill_discrete_sequential(palette = "Greens", 
    rev=FALSE, n=5)  
b7 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
  geom_smooth(colour = "#20794D", fill = "#b9ca4a") +
  geom_point(colour = "#20794D") +
  xlab("Hour")


## ---------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
b1


## ---------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
b2


## ---------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
b3


## ---------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
b4


## ---------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
b5


## ---------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 5
#| out-width: 70%
b6


## ---------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
b7


## ---------------------------------------------------
#| label: pedestrians
#| echo: true
#| eval: false
# load("data/ped_Aug2025.rda")
# ped_sc <- ped |>
#   filter(Sensor == "Southern Cross Station") |>
#   filter(Date == ymd("2025-08-31")) |>
#   group_by(Time) |>
#   summarise(Count = sum(Count), .groups = "drop") |>
#   mutate(se = sqrt(Count))
# b1 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
#   geom_col(fill = "#20794D") +
#   xlab("Hour")
# b2 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
#   geom_col(fill = "#b9ca4a") +
#   geom_errorbar(aes(ymin = Count - se, ymax = Count + se),
#     width=0.5, colour="#20794D") +
#   xlab("Hour")
# b3 <- ggplot(ped_sc, aes(x=Time,
#     ydist=distributional::dist_normal(Count, se))) +
#   stat_pointinterval(colour = "#20794D") +
#   xlab("Hour") + ylab("Count")
# b4 <- ggplot(ped_sc, aes(x=Time,
#     ydist=distributional::dist_normal(Count, se))) +
#   stat_gradientinterval(colour = NA, fill="#20794D",
#     .width=1) +
#   geom_line(aes(x=Time, y=Count), colour="#20794D") +
#   xlab("Hour") + ylab("Count")
# b5 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
#   geom_ribbon(aes(ymin = Count - qnorm(0.975)*se,
#                   ymax = Count + qnorm(0.975)*se),
#     fill = "#b9ca4a") +
#   geom_line(colour="#20794D") +
#   xlab("Hour")
# ped_sc_ci <- ped_sc |>
#   mutate(l50 = Count - qnorm(0.75)*se,
#          u50 = Count + qnorm(0.75)*se,
#          l80 = Count - qnorm(0.9)*se,
#          u80 = Count + qnorm(0.9)*se,
#          l99 = Count - qnorm(0.995)*se,
#          u99 = Count + qnorm(0.995)*se
#   ) |>
#   pivot_longer(cols=l50:u99, names_to = "intprob",
#     values_to="value") |>
#   mutate(bound = str_sub(intprob, 1, 1),
#        prob = str_sub(intprob, 2, 3)) |>
#   select(Time, Count, se, prob, bound, value) |>
#   pivot_wider(names_from = bound, values_from = value)
# b6 <- ggplot(ped_sc_ci, aes(x=Time, y=Count)) +
#   geom_lineribbon(aes(ymin = l, ymax = u, fill = prob)) +
#   labs(x="Hour", fill="Confidence") +
#   scale_fill_discrete_sequential(palette = "Greens",
#     rev=FALSE, n=5)
# b7 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
#   geom_smooth(colour = "#20794D", fill = "#b9ca4a") +
#   geom_point(colour = "#20794D") +
#   xlab("Hour")


## ---------------------------------------------------
#| label: forecast1
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
#| code-fold: true
tourism_melb <- tourism %>%
  filter(Region == "Melbourne", Purpose == "Business")
fit <- tourism_melb %>%
  model(
    ets = ETS(Trips ~ trend("A"))
  )
fc <- fit |>
  forecast(h = "5 years")
fc |>
  autoplot(tourism_melb) +
    theme(aspect.ratio = 0.6)


## ---------------------------------------------------
#| label: forecast2
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
#| code-fold: true
fc_b <- fit |>
  forecast(h = "5 years", bootstrap = TRUE)
fc_b_samples <- fit |>
  generate(h = 20, times = 50, bootstrap = TRUE)
fc_b_samples |>
  ggplot() +
    geom_line(aes(x = Quarter, y = .sim, group = .rep),
      colour = "#027EB6", alpha=0.1) +
    geom_line(data=fc_b, aes(x = Quarter, y = .mean), 
      colour = "#027EB6") +
    autolayer(tourism_melb, Trips) +
    ylab("Trips") +
    theme(aspect.ratio = 0.6)


## ---------------------------------------------------
#| label: setup-choro
#| echo: false
library(sugarbag)

invthm <- theme_map() + 
  theme(
    panel.background = element_rect(fill = "black", colour = NA), 
    plot.background = element_rect(fill = "black", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA),
    text = element_text(colour = "white"),
    axis.text = element_blank()
  )

# function to allocate colours to regions
aus_colours <- function(sir_p50){
  value <- case_when(
    sir_p50 <  0.74 ~ "#33809d",
    sir_p50 >= 0.74 & sir_p50 < 0.98 ~ "#aec6c7",
    sir_p50 >= 0.98 & sir_p50 < 1.05 ~ "#fff4bc",
    sir_p50 >= 1.05 & sir_p50 < 1.45 ~ "#ff9a64",
    sir_p50 >= 1.45 ~ "#ff3500",
    TRUE ~ "#FFFFFF")
  return(value)
}


## ---------------------------------------------------
#| label: thyroiddata
#| code-fold: true
#| eval: false
# sa2 <- strayr::read_absmap("sa22011") |>
#   filter(!st_is_empty(geometry)) |>
#   filter(!state_name_2011 == "Other Territories") |>
#   filter(!sa2_name_2011 == "Lord Howe Island")
# sa2 <- sa2 |> rmapshaper::ms_simplify(keep = 0.5, keep_shapes = TRUE) # Simplify the map!!!
# SIR <- read_csv("data/SIR Downloadable Data.csv") |>
#   filter(SA2_name %in% sa2$sa2_name_2011) |>
#   dplyr::select(Cancer_name, SA2_name, Sex_name, p50) |>
#   filter(Cancer_name == "Thyroid", Sex_name == "Females")
# ERP <- read_csv("data/ERP.csv") |>
#   filter(REGIONTYPE == "SA2", Time == 2011, Region %in% SIR$SA2_name) |>
#   dplyr::select(Region, Value)
# # Alternative maps
# # Join with sa2 sf object
# sa2thyroid_ERP <- SIR |>
#   left_join(sa2, ., by = c("sa2_name_2011" = "SA2_name")) |>
#   left_join(., ERP |>
#               dplyr::select(Region,
#               Population = Value), by = c("sa2_name_2011"= "Region")) |>
#   filter(!st_is_empty(geometry))
# sa2thyroid_ERP <- sa2thyroid_ERP |>
#   #filter(!is.na(Population)) |>
#   filter(!sa2_name_2011 == "Lord Howe Island") |>
#   mutate(SIR = map_chr(p50, aus_colours)) |>
#   st_as_sf()
# save(sa2, file="data/sa2.rda")
# save(sa2thyroid_ERP, file="data/sa2thyroid_ERP.rda")


## ---------------------------------------------------
#| label: choro
#| code-fold: true
#| fig-width: 10
#| fig-height: 8
#| out-width: 100%
# Plot the choropleth
load("data/sa2thyroid_ERP.rda")
aus_ggchoro <- ggplot(sa2thyroid_ERP) + 
  geom_sf(aes(fill = SIR), size = 0.1) + 
  scale_fill_identity() + invthm
aus_ggchoro


## ---------------------------------------------------
#| label: cartogram
#| code-fold: true
#| fig-width: 9
#| fig-height: 10
#| out-width: 70%
# transform to NAD83 / UTM zone 16N
nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet=TRUE)

nc <- nc |>
  mutate(lBIR79 = log(BIR79))
nc_utm <- st_transform(nc, 26916)

orig <- ggplot(nc) + 
  geom_sf(aes(fill = lBIR79)) +
  ggtitle("choropleth") +
  theme_map() +
  theme(legend.position = "none")

nc_utm_carto <- cartogram_cont(nc_utm, weight = "BIR74", itermax = 5)

carto <- ggplot(nc_utm_carto) + 
  geom_sf(aes(fill = lBIR79)) +
  ggtitle("cartogram") +
  theme_map() +
  theme(legend.position = "none")

nc_utm_dorl <- cartogram_dorling(nc_utm, weight = "BIR74")

dorl <- ggplot(nc_utm_dorl) + 
  geom_sf(aes(fill = lBIR79)) +
  ggtitle("dorling") +
  theme_map() +
  theme(legend.position = "none")

orig + carto + dorl + plot_layout(ncol=1)


## ---------------------------------------------------
#| label: hexmap
#| code-fold: true
#| fig-width: 10
#| fig-height: 8
#| out-width: 100%
if (!file.exists("data/aus_hexmap.rda")) {
  
## Create centroids set
centroids <- sa2 |> 
  create_centroids(., "sa2_name_2011")
## Create hexagon grid
grid <- create_grid(centroids = centroids,
                    hex_size = 0.2,
                    buffer_dist = 5)
## Allocate polygon centroids to hexagon grid points
aus_hexmap <- allocate(
  centroids = centroids,
  hex_grid = grid,
  sf_id = "sa2_name_2011",
  ## same column used in create_centroids
  hex_size = 0.2,
  ## same size used in create_grid
  hex_filter = 10,
  focal_points = capital_cities,
  width = 35,
  verbose = FALSE
)
save(aus_hexmap, 
     file = "data/aus_hexmap.rda")
}

load("data/aus_hexmap.rda")
## Prepare to plot
fort_hex <- fortify_hexagon(data = aus_hexmap,
                            sf_id = "sa2_name_2011",
                            hex_size = 0.2) |> 
            left_join(sa2thyroid_ERP |> select(sa2_name_2011, SIR, p50))
## Make a plot
aus_hexmap_plot <- ggplot() +
  geom_sf(data=sa2thyroid_ERP, fill=NA, colour="grey60", size=0.1) +
  geom_polygon(data = fort_hex, aes(x = long, y = lat, group = hex_id, fill = SIR)) +
  scale_fill_identity() +
  invthm 
aus_hexmap_plot  


## ---------------------------------------------------
#| label: tb-lineup1
#| code-fold: true
#| fig-width: 8
#| fig-height: 6
#| out-width: 100%
# Need a binary variable to use simulation
tb_idn_long <- tb_idn |>
  filter(year > 2000, year < 2013) |>
  mutate(sex01 = ifelse(sex=="m", 0, 1)) |>
  select(year, age_group, sex01, count) |>
  uncount(count)

# Generate the null samples
set.seed(733)
pos = sample(1:4)
tb_in_s1 <- tb_idn_long |>
  group_by(year, age_group) |>
  mutate(sex01 = rbinom(n(), 1, 0.5)) |>
  ungroup() |>
  count(year, age_group, sex01) |>
  rename(count = n) |>
  mutate(.sample = pos[1])
tb_in_s2 <- tb_idn_long |>
  group_by(year, age_group) |>
  mutate(sex01 = rbinom(n(), 1, 0.5)) |>
  ungroup() |>
  count(year, age_group, sex01) |>
  rename(count = n) |>
  mutate(.sample = pos[2])
tb_in_s3 <- tb_idn_long |>
  group_by(year, age_group) |>
  mutate(sex01 = rbinom(n(), 1, 0.5)) |>
  ungroup() |>
  count(year, age_group, sex01) |>
  rename(count = n) |>
  mutate(.sample = pos[3])
tb_idn_d <- tb_idn_long |>
  count(year, age_group, sex01) |>
  rename(count = n) |>
  mutate(.sample = pos[4])

# Make the lineup  
tb_l <- bind_rows(tb_idn_d, tb_in_s1, tb_in_s2, tb_in_s3) 
  
ggplot(tb_l, aes(x=year, y=count, colour=factor(sex01))) +
  geom_point() +
  geom_smooth(se=F) + 
  facet_grid(.sample~age_group) +
  ylim(c(0,NA)) + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        strip.text.x = element_blank())


## ---------------------------------------------------
#| label: tb-lineup2
#| code-fold: true
#| fig-width: 8
#| fig-height: 6
#| out-width: 100%
set.seed(854)
pos = sample(1:4)

tb_idn_prop <- tb_idn |> group_by(year, age_group) |> 
  summarise(p = count[sex=="m"]/sum(count)) 

tb_idn_prop_s1 <- tb_idn_prop |>
  group_by(age_group) |>
  mutate(p = sample(p)) |>
  mutate(.sample = pos[1])
tb_idn_prop_s2 <- tb_idn_prop |>
  group_by(age_group) |>
  mutate(p = sample(p)) |>
  mutate(.sample = pos[2])
tb_idn_prop_s3 <- tb_idn_prop |>
  group_by(age_group) |>
  mutate(p = sample(p)) |>
  mutate(.sample = pos[3])
tb_idn_prop <- tb_idn_prop |>
  mutate(.sample = pos[4])

tb_idn_prop_l <- bind_rows(tb_idn_prop, tb_idn_prop_s1, 
  tb_idn_prop_s2, tb_idn_prop_s3)

ggplot(tb_idn_prop_l, aes(x=year, y=p)) +
  geom_hline(yintercept = 0.50, colour="grey50", linewidth=2) +
  geom_point() +
  geom_smooth(se=F) + 
  facet_grid(.sample~age_group) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        strip.text.x = element_blank())


## ---------------------------------------------------
#| label: lineup 1
#| echo: true
# Make a lineup of mtcars data
# 20 plots, one data, 19 nulls
# Which one is different?
set.seed(913)
library(ggplot2)
l <- ggplot(
  lineup(
    null_permute('mpg'), 
    mtcars), 
  aes(mpg, wt)
) +
  geom_point() +
  facet_wrap(~ .sample) +
  theme(axis.text = element_blank(),
        axis.title = element_blank())


## ---------------------------------------------------
# Compute the p-value for 10 observers
pvisual(5, 10, 20)


## ---------------------------------------------------
#| echo: false
#| fig-height: 6
#| fig-width: 6
#| out-width: 80%
l


## ----focus on one year gender side-by-side bars of males females, fig.height=3, echo=FALSE----
tb_idn |> filter(year == 2012) |>
  ggplot(aes(x=sex, y=count, fill=sex)) +
    geom_bar(stat="identity", position="dodge") + 
    facet_wrap(~age, ncol=6) +
    ggtitle("Arrangement A")


## ----focus on one year age side-by-side bars of age group, fig.height=3, fig.width=8, echo=FALSE----
tb_idn |> filter(year == 2012) |>
  ggplot(aes(x=age, y=count, fill=age)) +
    geom_bar(stat="identity", position="dodge") + 
    facet_wrap(~sex, ncol=6) +
    scale_fill_brewer("", palette="Dark2") +
    ggtitle("Arrangement B")


## ----use a line plot instead of bar 2, echo = F-----
#| fig-width: 10
#| fig-height: 3
ggplot(tb_idn, aes(x=year, y=count, colour=sex)) +
  geom_point() +
  geom_smooth(se=F, colour="black") + 
  facet_wrap(~age, ncol=6) +
  ggtitle("Type A") +
  scale_x_continuous("year", breaks = seq(2000, 2012, 5), 
                     labels = c("00", "05", "10"))


## ----colour and axes fixes, echo = F----------------
#| fig-width: 10
#| fig-height: 3
ggplot(tb_idn, aes(x=year, y=count, fill=sex)) +
  geom_bar(stat="identity") + 
  facet_wrap(~age, ncol=6) +
  ggtitle("Type B") +
  scale_x_continuous("year", breaks = seq(2000, 2012, 5), 
                     labels = c("00", "05", "10"))


## ----show different types of color palettes, fig.height=7, fig.width=12, out.width="100%"----
display.brewer.all()


## ----palette1, fig.height=6, fig.width=6, echo = F----
ggplot(tb_idn, aes(x=year, y=count, colour=age)) +
  geom_line() + geom_point() +
  facet_wrap(~sex, ncol=6) +
  scale_colour_brewer("", palette="Dark2") +
  ggtitle("qualitative") +
  scale_x_continuous("year", breaks = seq(2000, 2012, 5), 
                     labels = c("00", "05", "10")) +
  theme(legend.position = "bottom")


## ----palette2, fig.height=6, fig.width=6, echo = F----
ggplot(tb_idn, aes(x=year, y=count, colour=age)) +
  geom_line() + geom_point() +
  facet_wrap(~sex, ncol=6) +
  scale_color_manual(values = brewer.pal(8, "YlGnBu")[3:8]) +
  ggtitle("sequential") +
  scale_x_continuous("year", breaks = seq(2000, 2012, 5), 
                     labels = c("00", "05", "10")) +
  theme(legend.position = "bottom")


## ----palette3, fig.height=6, fig.width=6, echo = F----
ggplot(tb_idn, aes(x=year, y=count, colour=age)) +
  geom_line() + geom_point() +
  facet_wrap(~sex, ncol=6) +
  scale_color_brewer("", palette = "PiYG") +
  ggtitle("diverging") +
  scale_x_continuous("year", breaks = seq(2000, 2012, 5), 
                     labels = c("00", "05", "10")) +
  theme(legend.position = "bottom")


## ----using the dichromat package to check color blind appearance, echo=TRUE, eval = F----
# clrs <- hue_pal()(9)
# d + theme(legend.position = "none")
# 
# clrs <- dichromat(hue_pal()(9))
# d +
#   scale_colour_manual("", values=clrs) +
#   theme(legend.position = "none")


## ----show the default colour scheme, echo=FALSE, fig.width=4, fig.height=4, out.width="80%"----
clrs <- hue_pal()(9)
dsamp <- diamonds |>
  sample_n(1000)
d <- ggplot(
  dsamp, aes(carat, price)) +
  geom_point(aes(
    colour = clarity))
p1 <- d +  
  scale_color_discrete() + 
  theme(legend.position = "none")  
p1


## ----show the dichromat adjusted colors, echo=FALSE, fig.width=4, fig.height=4, out.width="80%"----
clrs <- dichromat(hue_pal()(9))
p2 <- d + 
  scale_color_manual("", values=clrs) + 
  theme(legend.position = "none")

p2


## ----is shape preattentive, echo=FALSE, fig.width=4, fig.height=4, out.width="80%"----
set.seed(1106)
df <- data.frame(x=runif(100), y=runif(100), cl=sample(c(rep("A", 1), rep("B", 99))))
ggplot(data=df, aes(x, y, shape=cl)) + theme_bw() + 
  geom_point(size=3) +
  theme(legend.position="None", aspect.ratio=1, axis.text = element_blank(), axis.ticks=element_blank(), axis.title = element_blank()) 


## ----is color preattentive, echo=FALSE, fig.width=4, fig.height=4, out.width="80%"----
ggplot(data=df, aes(x, y, colour=cl)) + 
  geom_point(size=3) +
  theme_bw() + 
  scale_colour_brewer(palette="Set1") +
  theme(legend.position="None", aspect.ratio=1, axis.text = element_blank(), axis.ticks=element_blank(), axis.title = element_blank()) 


## ----a line plot on sex, fig.height=3, fig.width = 8, echo = F----
ggplot(tb_idn, aes(x=year, y=count, colour=sex)) +
  geom_line() + geom_point() +
  facet_wrap(~age, ncol=6) +
  ggtitle("Arrangement A") +
  scale_x_continuous("year", breaks = seq(2000, 2012, 5), 
                     labels = c("00", "05", "10"))


## ----a line plot on age, fig.height=3, fig.width=8, echo = F----
ggplot(tb_idn, aes(x=year, y=count, colour=age)) +
  geom_line() + geom_point() +
  facet_wrap(~sex, ncol=6) +
  scale_colour_brewer("", palette="Dark2") +
  ggtitle("Arrangement B") +
  scale_x_continuous("year", breaks = seq(2000, 2012, 5), 
                     labels = c("00", "05", "10"))


## ----side-by-side bars of males females, fig.width=8, fig.height=3, echo = F, out.width="70%"----
tb_idn |> filter(year == 2012) |>
  ggplot(aes(x=sex, y=count, fill=sex)) +
    geom_bar(stat="identity", position="dodge") + 
    facet_wrap(~age, ncol=6) +
    scale_y_continuous("count ('000)", breaks=seq(0, 28000, 5000),
      labels=c("0", "5", "10", "15", "20", "25")) +
    ggtitle("Position - common scale ")


## ----piecharts of males females, fig.width=10, fig.height=2.5, echo = F, out.width="100%"----
tb_idn |> filter(year == 2012) |>
  ggplot(aes(x=1, y=count, fill=sex)) +
    geom_bar(stat="identity", position="fill") + 
    facet_wrap(~age, ncol=6) +
    ggtitle("Angle") + xlab("") + ylab("") +
    coord_polar(theta = "y")


## ----side-by-side bars of age, fig.width=8, fig.height=4, echo = F, out.width="100%"----
tb_idn |> filter(year == 2012) |>
  ggplot(aes(x=age, y=count, fill=age)) +
    geom_bar(stat="identity", position="dodge") + 
    facet_wrap(~sex, ncol=6) +
    scale_fill_brewer("", palette="Dark2") +
    scale_y_continuous("count ('000)", breaks=seq(0, 28000, 5000),
      labels=c("0", "5", "10", "15", "20", "25")) +
    ggtitle("Position - common scale ")


## ----piecharts of age, fig.width=8, fig.height=4, echo = F, out.width="100%"----
tb_idn |> filter(year == 2012) |>
  ggplot(aes(x=1, y=count, fill=age)) +
    geom_bar(stat="identity", position="fill") + 
    facet_wrap(~sex, ncol=6) +
    scale_fill_brewer("", palette="Dark2") +
    ggtitle("Angle") + xlab("") + ylab("") +
    coord_polar(theta = "y")


## ----stacked bars of age, fig.width=8, fig.height=6, echo = F, out.width="100%"----
tb_idn |> filter(year == 2012) |>
  ggplot(aes(x=1, y=count, fill=age)) +
    geom_bar(stat="identity", position="fill") + 
    facet_wrap(~sex, ncol=6) +
    scale_fill_brewer("", palette="Dark2") +
    ggtitle("Position - nonaligned") + xlab("") + ylab("")


## ----facetting plots can result in change blindness, echo=TRUE, out.width="50%", fig.width=8, fig.height=5----
#| code-fold: true
ggplot(dsamp, aes(x=carat, y=price, colour = clarity)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  scale_color_brewer(palette="Set1") +
  scale_y_continuous("price ('000)", breaks=seq(0, 18000, 5000),
    labels=c("0", "5", "10", "15")) +
  facet_wrap(~clarity, ncol=4)


## ----averlaying makes comparisons easier, echo=TRUE, out.width="100%", fig.width=7, fig.height=4----
#| code-fold: true
ggplot(dsamp, aes(x=carat, y=price, 
                  colour = clarity)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  scale_y_continuous("price ('000)", breaks=seq(0, 18000, 5000),
    labels=c("0", "5", "10", "15")) +
  scale_color_brewer(palette="Set1") 

