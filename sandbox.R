## ----include = FALSE-----------------------------------
# Load libraries 
library("tidyr")
library("dplyr")
library("ggplot2")
library("stringr")
library("tidymodels")
library("patchwork")
library("naniar")
library("visdat")
library("lubridate")
library("ggbeeswarm")
library("nullabor")
library("ranger")
library("conflicted")
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)

## ------------------------------------------------------
load("data/osiris/osiris_Indonesia_2018.rda")
glimpse(osiris_2018)


## ------------------------------------------------------
# Classify industry based on code
naics_industry <- tibble(
  naics_core_initial = c(
    "11", "21", "22", "23",
    "31", "32", "33", "42",
    "44", "45", "48", "49",
    "51", "52", "53", "54",
    "55", "56", "61", "62",
    "71", "72", "81", "92"
  ),
  industry = c(
    "Agriculture, Forestry, Fishing and Hunting",
    "Mining, Quarrying, and Oil and Gas Extraction",
    "Utilities",
    "Construction",
    "Manufacturing",
    "Manufacturing",
    "Manufacturing",
    "Wholesale Trade",
    "Retail Trade",
    "Retail Trade",
    "Transportation and Warehousing",
    "Transportation and Warehousing",
    "Information",
    "Finance and Insurance",
    "Real Estate and Rental and Leasing",
    "Professional, Scientific, and Technical Services",
    "Management of Companies and Enterprises",
    "Administrative and Support and Waste Management and Remediation Services",
    "Educational Services",
    "Health Care and Social Assistance",
    "Arts, Entertainment, and Recreation",
    "Accommodation and Food Services",
    "Other Services (except Public Administration)",
    "Public Administration"
  )
)
osiris_2018_NAICS <- osiris_2018 |>
  mutate(naics_core_initial =
    str_sub(`NAICS Core Code (caics12cod)`, 1, 2)) |>
  count(naics_core_initial, sort=TRUE) |>
  na.omit() # One row is missing values
osiris_2018_NAICS <- osiris_2018_NAICS |>
  left_join(naics_industry)


## ------------------------------------------------------
osiris_2018_sub <- osiris_2018 |>
  select(`Company Fiscal Year End Date (closdate)`,
         `Company Name (name)`,
         `CITY - City (city)`,
         `NAICS Core Code (caics12cod)`,
         `Currency (currency)`,
         `Total Assets (data13077)`,
         `Loans (data21010)`,
         `Income Tax Payable (data21040)`,
         `Total Liabilities and Debt (data14022)`,
         `No. Of employed (data23000)` ,
         `Total revenues (data13004)`,
         `Net Profit (data13045)`
  )


## ------------------------------------------------------
osiris_2018_sub <- osiris_2018 |>
  select(`Company Fiscal Year End Date (closdate)`,
         `Company Name (name)`,
         `CITY - City (city)`,
         `NAICS Core Code (caics12cod)`,
         `Currency (currency)`,
         `Total Assets (data13077)`,
         `Loans (data21010)`,
         `Income Tax Payable (data21040)`,
         `Total Liabilities and Debt (data14022)`,
         `No. Of employed (data23000)` ,
         `Total revenues (data13004)`,
         `Net Profit (data13045)`
  )
load("data/osiris/osiris_Indonesia_2019.rda")
osiris_2019_sub <- osiris_2019 |>
  select(`Company Fiscal Year End Date (closdate)`,
         `Company Name (name)`,
         `CITY - City (city)`,
         `NAICS Core Code (caics12cod)`,
         `Currency (currency)`,
         `Total Assets (data13077)`,
         `Loans (data21010)`,
         `Income Tax Payable (data21040)`,
         `Total Liabilities and Debt (data14022)`,
         `No. Of employed (data23000)` ,
         `Total revenues (data13004)`,
         `Net Profit (data13045)`
  )
load("data/osiris/osiris_Indonesia_2020.rda")
osiris_2020_sub <- osiris_2020 |>
  select(`Company Fiscal Year End Date (closdate)`,
         `Company Name (name)`,
         `CITY - City (city)`,
         `NAICS Core Code (caics12cod)`,
         `Currency (currency)`,
         `Total Assets (data13077)`,
         `Loans (data21010)`,
         `Income Tax Payable (data21040)`,
         `Total Liabilities and Debt (data14022)`,
         `No. Of employed (data23000)` ,
         `Total revenues (data13004)`,
         `Net Profit (data13045)`
  )
load("data/osiris/osiris_Indonesia_2021.rda")
osiris_2021_sub <- osiris_2021 |>
  select(`Company Fiscal Year End Date (closdate)`,
         `Company Name (name)`,
         `CITY - City (city)`,
         `NAICS Core Code (caics12cod)`,
         `Currency (currency)`,
         `Total Assets (data13077)`,
         `Loans (data21010)`,
         `Income Tax Payable (data21040)`,
         `Total Liabilities and Debt (data14022)`,
         `No. Of employed (data23000)` ,
         `Total revenues (data13004)`,
         `Net Profit (data13045)`
  )
osiris_sub <- bind_rows(osiris_2018_sub,
                        osiris_2019_sub,
                        osiris_2020_sub,
                        osiris_2021_sub)
osiris_sub <- osiris_sub |>
  mutate(`Company Fiscal Year End Date (closdate)` =
           ymd(`Company Fiscal Year End Date (closdate)`)) |>
  mutate(year = year(`Company Fiscal Year End Date (closdate)`),
         naics_core_initial =
    str_sub(`NAICS Core Code (caics12cod)`, 1, 2)) |>
  mutate(pandemic = if_else(year < 2020, "before", "covid")) |>
  left_join(naics_industry)


## ------------------------------------------------------
vis_dat(osiris_sub)


## ------------------------------------------------------
# 5421 observations before
osiris_sub <- osiris_sub |>
  select(-`Loans (data21010)`) |>
  na.omit()
# 5268 after


## ------------------------------------------------------
# Clean up variable names
osiris_sub <- osiris_sub |>
  rename(name =`Company Name (name)`,
    fiscal_end = `Company Fiscal Year End Date (closdate)`,
         city = `CITY - City (city)`,
         NAICS = `NAICS Core Code (caics12cod)`,
         currency = `Currency (currency)`,
         assets = `Total Assets (data13077)`,
         tax = `Income Tax Payable (data21040)`,
         debt = `Total Liabilities and Debt (data14022)`,
         nemploy = `No. Of employed (data23000)`,
         revenues = `Total revenues (data13004)`,
         profit = `Net Profit (data13045)`)


## ------------------------------------------------------
ggplot(osiris_sub,
       aes(x=pandemic,
           y=revenues)) +
  geom_quasirandom() +
  scale_y_log10()

ggplot(osiris_sub,
       aes(x=pandemic,
           y=revenues)) +
  geom_quasirandom() +
  stat_summary(colour="red") +
  scale_y_log10() +
  facet_wrap(~industry, ncol=5, scales="free_y") +
  theme(axis.text = element_blank())

## ------------------------------------------------------
osiris_accom <- osiris_sub |>
  filter(industry == "Accommodation and Food Services")
ggplot(lineup(null_permute("pandemic"),
              true=osiris_accom),
      aes(x=pandemic,
          y=revenues)) +
    facet_wrap(~.sample, ncol=5) +
    stat_summary(linewidth = 1,
                 geom="errorbar", width=0.2) +
    stat_summary(geom="point", size=2) +
    scale_y_log10() +
    theme(axis.text = element_blank(),
          axis.title = element_blank())

## ------------------------------------------------------
set.seed(1200)
osiris_sub_to_fit <- osiris_sub |>
  select(-c(fiscal_end, name, city,
            NAICS, naics_core_initial, year))
osiris_splits      <- initial_split(osiris_sub_to_fit,
                             strata = pandemic)

osiris_other <- training(osiris_splits)
osiris_test  <- testing(osiris_splits)

# validation set
set.seed(1204)
osiris_val_set <- validation_split(osiris_other,
                            strata = pandemic,
                            prop = 0.80)

# model
cores <- parallel::detectCores()
rf_mod <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |>
  set_engine("ranger", num.threads = cores) |>
  set_mode("classification")

# recipe
rf_recipe <-
  recipe(pandemic ~ ., data = osiris_other)

# workflow
rf_workflow <-
  workflow() |>
  add_model(rf_mod) |>
  add_recipe(rf_recipe)

# tune
set.seed(1213)
rf_res <-
  rf_workflow |>
  tune_grid(osiris_val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_res |>
  show_best(metric = "roc_auc")

autoplot(rf_res)

# Select model
rf_best <-
  rf_res |>
  select_best(metric = "roc_auc")
rf_best

rf_res |>
  collect_predictions()

rf_res |>
  collect_predictions(parameters = rf_best) |>
  roc_curve(pandemic, .pred_before) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path(lwd = 1.5, alpha = 0.8) +
    geom_abline(lty = 3) +
    coord_equal()

# final
# the last model
last_rf_mod <-
  rand_forest(mtry = 8, min_n = 7, trees = 1000) |>
  set_engine("ranger", num.threads = cores, importance = "impurity") |>
  set_mode("classification")

# the last workflow
last_rf_workflow <-
  rf_workflow |>
  update_model(last_rf_mod)

# the last fit
set.seed(1223)
last_rf_fit <-
  last_rf_workflow |>
  last_fit(osiris_splits)

last_rf_fit |>
  collect_metrics()

# variable importance
last_rf_fit |>
  extract_fit_parsnip() |>
  vip()


## ------------------------------------------------------
osiris_test_pred <- tibble(osiris_test) |>
  mutate(
    pandemic_pcl = last_rf_fit$.predictions[[1]]$.pred_class,
    pandemic_pbefore = last_rf_fit$.predictions[[1]]$.pred_before,
    pandemic_pcovid = last_rf_fit$.predictions[[1]]$.pred_covid)
osiris_test_pred |>
  select(pandemic_pcl,
         pandemic_pbefore,
         pandemic_pcovid) |>
  ggplot(aes(x=pandemic_pbefore,
             fill=pandemic_pcl,
             colour=pandemic_pcl)) +
    geom_density(alpha=0.5)

osiris_test_pred <- osiris_test_pred |>
  mutate(error = if_else(pandemic != pandemic_pcl,
                         "yes", "no"))
ggplot(osiris_test_pred, aes(x=revenues, y=profit,
             colour = pandemic)) +
  geom_point(alpha=0.1) +
  geom_point(data=filter(osiris_test_pred,
                         error == "yes")) +
  facet_wrap(~industry, ncol=5, scales="free") +
  theme(aspect.ratio=1,
        axis.text = element_blank())


## ------------------------------------------------------
ggplot(osiris_sub, aes(x=year,
                       y=profit)) +
  geom_point(alpha=0.5) +
  geom_smooth(se=FALSE, method="loess") +
  facet_wrap(~industry, ncol=5, scales="free_y") +
  xlab("")

osiris_sub_smry <- osiris_sub |>
  group_by(industry, year) |>
  summarise(profit_m = mean(profit),
            profit_s = sd(profit),
            .groups = "drop")
osiris_sub_smry |>
  filter(industry %in%
  c("Accommodation and Food Services",
    "Construction",
    "Retail Trade",
    "Health Care and Social Assistance")) |>
  ggplot() +
  geom_ribbon(aes(x=year,
                  ymin=profit_m-profit_s,
                  ymax=profit_m+profit_s),
              alpha=0.5) +
  geom_line(aes(x=year, y=profit_m)) +
  facet_wrap(~industry, ncol=2, scales="free_y") +
  xlab("") +
  ylab("profit")


