## ----include = FALSE--------------------------------
# Load libraries 
source("libraries.R")

# Set up chunk for all slides
source("chunk_options_and_themes.R")


## ---------------------------------------------------
#| label: setting_up
#| echo: false
load("data/french_fries.rda")


## ----fig.width=9, fig.height=4, out.width="100%", fig.align='center', echo=FALSE----
#| label: overview-methods
load("data/flea.rda")
p1 <- ggplot(flea, aes(x=tars1, y=aede1, colour = species)) + 
  geom_point() + 
  scale_colour_brewer(palette = "Dark2") +
  xlab("Var 1") + ylab("Var 2") +
  ggtitle("Classification") +
  theme(legend.position="None") + 
  theme(aspect.ratio=1)
p2 <- ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point() +
  geom_smooth(se=F) +
  ggtitle("Regression") +
  theme(aspect.ratio=0.7)
p3 <- ggplot(flea, aes(x=tars1, y=aede1)) + 
  geom_point() + xlab("Var 1") + ylab("Var 2") +  
  ggtitle("Clustering") + 
  theme(aspect.ratio=1)
p1 + p2 + p3 + plot_layout(ncol=3)


## ---------------------------------------------------
#| echo: false
#| eval: false
#| label: sine-curve-data
# # Generate the sine-curve data
# set.seed(1259)
# x1 <- runif(340)
# x2 <- runif(340)
# y <- 3*x1+sin(x1*15)
# y <- (y-min(y))/(max(y)-min(y))
# d <- tibble(x1, x2, y)
# d$cl <- ifelse(x2 > y, "A", "B")
# d$cl[sample(1:340, 25)] <- sample(c("A", "B"), 25, replace=TRUE)
# write_csv(d, file="data/sine-curve.csv")
# # Test set
# x1 <- runif(212)
# x2 <- runif(212)
# y <- 3*x1+sin(x1*15)
# y <- (y-min(y))/(max(y)-min(y))
# d <- tibble(x1, x2, y)
# d$cl <- ifelse(x2 > y, "A", "B")
# d$cl[sample(1:212, 18)] <- sample(c("A", "B"), 18, replace=TRUE)
# write_csv(d, file="data/sine-curve-test.csv")


## ---------------------------------------------------
#| label: sine-curve-plot
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
w <- read_csv("data/sine-curve.csv") |>
  mutate(cl = factor(cl))
ggplot(w, aes(x=x1, y=x2, colour = cl)) +
  geom_point(size=2.5, alpha=0.8) +
  geom_line(aes(x=x1, y=y), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("Observed data") + 
  theme(legend.position = "none")


## ---------------------------------------------------
#| label: sin-curve-models
#| echo: false
#| fig-width: 4
#| fig-height: 8
library(geozoo)
w_grid <- cube.solid.random(p=2)$points
colnames(w_grid) <- c("x1", "x2")
w_grid <- w_grid |> as_tibble() |>
  mutate(x1 = x1*(max(w$x1)-min(w$x1)+min(w$x1)),
         x2 = x2*(max(w$x2)-min(w$x2)+min(w$x2)))
w_lda <- lda(cl~x1+x2, data=w)
w_lda_pred <- predict(w_lda, w_grid)$class
w_rf <- randomForest(cl~x1+x2, data=w, ntree=2)
w_rf_pred <- predict(w_rf, w_grid)
w_grid <- w_grid |>
  mutate(plda = w_lda_pred,
         prf = w_rf_pred)
p1 <- ggplot(w_grid, aes(x=x1, y=x2, colour = plda)) +
  geom_point(alpha=0.5, size=2) +
  geom_line(data=w, aes(x=x1, y=y), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("Parametric") +
  theme(legend.position = "none",
        axis.text = element_blank())
p2 <- ggplot(w_grid, aes(x=x1, y=x2, colour = prf)) +
  geom_point(alpha=0.5, size=2) +
  geom_line(data=w, aes(x=x1, y=y), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("Non-parametric") +
  theme(legend.position = "none",
        axis.text = element_blank())
p1 + p2 + plot_layout(ncol=1)


## ---------------------------------------------------
#| label: model-errors1
#| echo: false
#| fig-width: 5
#| fig-height: 5
w <- w |>
  mutate(plda = predict(w_lda, w)$class,
         prf = predict(w_rf, w)) |>
  mutate(lda_err = ifelse(cl != plda, "1", "0"),
         rf_err = ifelse(cl != prf, "1", "0"))

ggplot(w, aes(x=x1, y=x2, 
                   colour = cl, 
                   shape=lda_err)) +
  geom_point(size=5, alpha=0.8) +
  geom_line(aes(x=x1, y=y), 
            inherit.aes = FALSE, 
            colour="black") +
  scale_color_discrete_qualitative() +
  scale_shape_manual(values=c(1, 19)) +
  ggtitle("Parametric errors") + 
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())


## ---------------------------------------------------
#| label: model-errors2
#| echo: false
#| fig-width: 5
#| fig-height: 5
ggplot(w, aes(x=x1, y=x2, 
                   colour = cl, 
                   shape=rf_err)) +
  geom_point(size=5, alpha=0.8) +
  geom_line(aes(x=x1, y=y), 
            inherit.aes = FALSE,
            colour="black") +
  scale_color_discrete_qualitative() +
  scale_shape_manual(values=c(1, 19)) +
  ggtitle("Non-parametric errors") + 
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())


## ---------------------------------------------------
#| label: flexible-model
#| echo: false
#| fig-width: 9
#| fig-height: 3
p1 <- ggplot(w_grid, aes(x=x1, y=x2, colour = plda)) +
  geom_point(size=2, alpha=0.5) +
  geom_line(data=w, aes(x=x1, y=y), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("Less flexible") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())
w_rf <- randomForest(cl~x1+x2, data=w, ntree=4)
w_rf_pred <- predict(w_rf, w_grid)
w_grid <- w_grid |>
  mutate(prf = w_rf_pred)
p2 <- ggplot(w_grid, aes(x=x1, y=x2, colour = prf)) +
  geom_point(size=2, alpha=0.5) +
  geom_line(data=w, aes(x=x1, y=y), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("<--------------->") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())
w_rf <- randomForest(cl~x1+x2, data=w, ntree=100)
w_rf_pred <- predict(w_rf, w_grid)
w_grid <- w_grid |>
  mutate(prf = w_rf_pred)
p3 <- ggplot(w_grid, aes(x=x1, y=x2, colour = prf)) +
  geom_point(size=2, alpha=0.5) +
  geom_line(data=w, aes(x=x1, y=y), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("More flexible") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())

p1 + p2 + p3 + plot_layout(ncol=3)


## ---------------------------------------------------
#| label: bias1
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
p1 + ggtitle("Large bias")


## ---------------------------------------------------
#| label: bias2
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
p3 + ggtitle("Small bias")


## ---------------------------------------------------
#| label: variance1
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
p1 + ggtitle("Small variance")


## ---------------------------------------------------
#| label: variance2
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
p3 + ggtitle("Large variance")


## ---------------------------------------------------
#| label: balanced-data
d_bal <- tibble(y=c(rep("A", 6), rep("B", 6)),
                x=c(runif(12)))
d_bal$y
set.seed(130)
d_bal_split <- initial_split(d_bal, prop = 0.70)
training(d_bal_split)$y
testing(d_bal_split)$y


## ---------------------------------------------------
#| label: unbalanced-data
d_unb <- tibble(y=c(rep("A", 2), rep("B", 10)),
                x=c(runif(12)))
d_unb$y
set.seed(132)
d_unb_split <- initial_split(d_unb, prop = 0.70)
training(d_unb_split)$y
testing(d_unb_split)$y


## ---------------------------------------------------
#| label: unbalanced-split
d_unb_strata <- initial_split(d_unb, prop = 0.70, strata=y)
training(d_unb_strata)$y
testing(d_unb_strata)$y


## ---------------------------------------------------
#| echo: false
#| label: predictive-class-example
a2 <- tibble(y = c(rep("bilby", 12),
                      rep("quokka", 15)),
             pred = c(rep("bilby", 9),
                      rep("quokka", 13), 
                      rep("bilby", 5)),
             bilby = c(0.9, 0.8, 0.9, 
                       0.7, 0.6, 0.8, 
                       0.9, 0.7, 0.6,# true
                       0.4, 0.3, 0.4,# error
                       0.2, 0.4, 0.1,# true
                       0.4, 0.1, 0.3, 
                       0.2, 0.4, 0.3,# true 
                       0.4, 
                       0.6, 0.7,# error 
                       0.6, 0.9, 0.7)) |>
  mutate(quokka = 1-bilby)

a3 <- a2 |>
  bind_rows(tibble(
    y = rep("numbat", 8), 
    pred = c(rep("numbat", 6),
                     rep("quokka", 2))))

a2 <- a2 |> 
  mutate(y = factor(y),
         pred = factor(pred))
a3 <- a3 |> 
  mutate(y = factor(y),
         pred = factor(pred))


## ---------------------------------------------------
#| eval: false
#| echo: false
# # tidymodels has it transposed
# #| label: confusion-matrix
# cm <- conf_mat(a2, y, pred)
# autoplot(cm)
# # Make it show in right direction
# conf_mat(a2, pred, y, dnn=c("Truth", "Pred"))


## ---------------------------------------------------
# Write out the confusion matrix in standard form
#| label: oconfusion-matrix-tidy
cm <- a2 |> count(y, pred) |>
  group_by(y) |>
  mutate(cl_acc = n[pred==y]/sum(n)) 
cm |>
  pivot_wider(names_from = pred, 
              values_from = n) |>
  select(y, bilby, quokka, cl_acc)


## ---------------------------------------------------
accuracy(a2, y, pred) |> pull(.estimate)
bal_accuracy(a2, y, pred) |> pull(.estimate)
sens(a2, y, pred) |> pull(.estimate)
specificity(a2, y, pred) |> pull(.estimate)


## ---------------------------------------------------
# Write out the confusion matrix in standard form
cm3 <- a3 |> count(y, pred) |>
  group_by(y) |>
  mutate(cl_err = n[pred==y]/sum(n)) 
cm3 |>
  pivot_wider(names_from = pred, 
              values_from = n, values_fill=0) |>
  select(y, bilby, quokka, numbat, cl_err)


## ---------------------------------------------------
accuracy(a3, y, pred) |> pull(.estimate)
bal_accuracy(a3, y, pred) |> pull(.estimate)


## ---------------------------------------------------
#| label: roc-curve
a2 |> slice_head(n=3)
roc_curve(a2, y, bilby) |>
  autoplot()


## ---------------------------------------------------
#| label: training-test
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
w_test <- read_csv("data/sine-curve-test.csv")
ggplot() +
  geom_point(data=w_grid, aes(x=x1, y=x2, 
                              colour = prf), 
             alpha=0.05, size=4) +
  geom_point(data=w, aes(x=x1, y=x2, colour = cl), 
             shape=3, size=3) +
  geom_point(data=w_test, aes(x=x1, y=x2, colour = cl), 
             shape=19, size=3) +
  scale_color_discrete_qualitative() +
  theme(legend.position = "none",
        axis.text = element_blank())


## ---------------------------------------------------
#| label: data-in-model-space1
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
#| code-fold: true
w <- read_csv("data/sine-curve.csv") |>
  mutate(cl = factor(cl))
w_rf <- randomForest(cl~x1+x2, data=w, ntree=200)
w_rf_pred <- predict(w_rf, w, type="prob") |>
  as_tibble(.name_repair="unique")
w_rf_pred |> 
  bind_cols(w) |>
  select(`A`, cl) |>
  ggplot(aes(x=`A`, colour=cl, fill=cl)) + 
    geom_density(alpha=0.7) +
    xlab("Prob class A") +
    ggtitle("Random forest") + 
    theme(legend.position="none")
  


## ---------------------------------------------------
#| label: data-in-model-space2
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
#| code-fold: true
w_lda <- lda(cl~x1+x2, data=w)
w_lda_pred <- predict(w_lda, w, method="predictive")$posterior |>
  as_tibble(.name_repair="unique")
w_lda_pred |> 
  bind_cols(w) |>
  select(`A`, cl) |>
  ggplot(aes(x=`A`, colour=cl, fill=cl)) + 
    geom_density(alpha=0.7) +
    xlab("Prob class A") +
    ggtitle("Linear DA") + 
    theme(legend.position="none")


## ---------------------------------------------------
#| label: model-in-the-data-space1
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
library(geozoo)
w_grid <- cube.solid.random(p=2)$points
colnames(w_grid) <- c("x1", "x2")
w_grid <- w_grid |> as_tibble() |>
  mutate(x1 = x1*(max(w$x1)-min(w$x1)+min(w$x1)),
         x2 = x2*(max(w$x2)-min(w$x2)+min(w$x2)))
w_lda <- lda(cl~x1+x2, data=w)
w_lda_pred <- predict(w_lda, w_grid)$class
w_rf <- randomForest(cl~x1+x2, data=w, ntree=200)
w_rf_pred <- predict(w_rf, w_grid)
w_grid <- w_grid |>
  mutate(plda = w_lda_pred,
         prf = w_rf_pred)
ggplot(w_grid, aes(x=x1, y=x2, colour = plda)) +
  geom_point(alpha=0.5, size=2) +
  geom_text(data=w, aes(x=x1, y=x2, label=cl), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("Linear DA") +
  theme(legend.position = "none",
        axis.text = element_blank())


## ---------------------------------------------------
#| label: model-in-the-data-space2
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
ggplot(w_grid, aes(x=x1, y=x2, colour = prf)) +
  geom_point(alpha=0.5, size=2) +
  geom_text(data=w, aes(x=x1, y=x2, label=cl), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("Random forest") +
  theme(legend.position = "none",
        axis.text = element_blank())


## ---------------------------------------------------
#| label: surreal1
#| code-fold: true
#| fig-width: 10
#| fig-height: 2.5
#| out-width: 100%
# Load the Jack-o'-Lantern data
d <- jackolantern_surreal_data[sample(1:5395, 2000),]

ggduo(d, c("x1", "x2", "x3", "x4"), "y")


## ---------------------------------------------------
#| label: surreal2
#| code-fold: true
#| fig-width: 5
#| fig-height: 5
#| out-width: 70%
# Fit a linear model to the surreal Jack-o'-Lantern data
d_lm <- lm(y ~ ., data = d)

# Plot the residuals to reveal the hidden image
d_all <- augment(d_lm, d)
ggplot(d_all, aes(x=.fitted, y=.resid)) +
  geom_point() +
  theme(aspect.ratio=1)


## ---------------------------------------------------
#| label: tips-lineup
#| fig-height: 10
#| fig-width: 10
#| out-width: 60%
#| code-fold: true
x <- lm(tip ~ total_bill, data = tips)
tips.reg <- data.frame(tips, .resid = residuals(x), 
                       .fitted = fitted(x))
library(ggplot2)
ggplot(lineup(null_lm(tip ~ total_bill, 
                      method = 'rotate'), 
              tips.reg)) +
  geom_point(aes(x = total_bill, 
                 y = .resid)) +
  facet_wrap(~ .sample) +
  theme(axis.text = element_blank(),
        axis.title = element_blank())


## ---------------------------------------------------
#| code-fold: true
glance(d_lm)
tidy(d_lm)


## ---------------------------------------------------
#| code-fold: true
library(tidymodels)
load("data/hotels.rda")
glimpse(hotels)


## ---------------------------------------------------
#| code-fold: true
hotels |> 
  count(children) |>
  mutate(prop = n/sum(n))


## ---------------------------------------------------
#| code-fold: true
set.seed(656)
splits      <- initial_split(hotels, strata = children)

hotel_other <- training(splits)
hotel_test  <- testing(splits)

# training set proportions by children
hotel_other |> 
  count(children) |> 
  mutate(prop = n/sum(n))

# test set proportions by children
hotel_test  |> 
  count(children) |> 
  mutate(prop = n/sum(n))


## ---------------------------------------------------
#| code-fold: true
set.seed(703)
hotel_val_set <- validation_split(hotel_other, 
                            strata = children, 
                            prop = 0.80)
hotel_val_set


## ---------------------------------------------------
#| code-fold: true
p1 <- ggplot(hotel_other, aes(x=children, y=lead_time)) +
  geom_quasirandom()
p2 <- ggplot(hotel_other, aes(x=children, y=stays_in_weekend_nights)) +
  geom_quasirandom()
p1 + p2 + plot_layout(ncol=2)


## ---------------------------------------------------
#| code-fold: true
#| fig-width: 10
#| fig-height: 8
p1 <- ggplot(hotel_other, aes(x=children, y=lead_time)) +
  geom_boxplot()
p2 <- ggplot(hotel_other, aes(x=children, y=stays_in_weekend_nights)) +
  geom_boxplot()
p3 <- ggplot(hotel_other, aes(x=children, y=average_daily_rate)) +
  geom_boxplot()
p4 <- ggplot(hotel_other, 
             aes(x=reserved_room_type, 
                 fill=children)) +
  geom_bar(position="fill") 
p1 + p2 + p3 + p4 + plot_layout(ncol=2)


## ---------------------------------------------------
#| code-fold: false
lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet")


## ---------------------------------------------------
#| code-fold: true
holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

lr_recipe <- 
  recipe(children ~ ., data = hotel_other) |> 
  step_date(arrival_date) |> 
  step_holiday(arrival_date, holidays = holidays) |> 
  step_rm(arrival_date) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_predictors())


## ---------------------------------------------------
#| code-fold: true
lr_workflow <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(lr_recipe)


## ---------------------------------------------------
#| code-fold: true
lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_reg_grid |> top_n(-5) # lowest penalty values

lr_reg_grid |> top_n(5)  # highest penalty values


## ---------------------------------------------------
#| code-fold: true
lr_res <- 
  lr_workflow |> 
  tune_grid(hotel_val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))


## ---------------------------------------------------
#| code-fold: true
lr_plot <- 
  lr_res |> 
  collect_metrics() |> 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylim(c(0.75, 0.9)) +
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 


## ---------------------------------------------------
#| code-fold: true
lr_best <- 
  lr_res |> 
  collect_metrics() |> 
  arrange(penalty) |> 
  slice(12)
lr_best


## ---------------------------------------------------
#| code-fold: true
lr_auc <- 
  lr_res |> 
  collect_predictions(parameters = lr_best) |> 
  roc_curve(children, .pred_children) |> 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)


## ---------------------------------------------------
#| code-fold: true
cores <- parallel::detectCores()
cores


## ---------------------------------------------------
#| code-fold: false
rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |> 
  set_engine("ranger", num.threads = cores) |> 
  set_mode("classification")


## ---------------------------------------------------
#| code-fold: false
rf_recipe <- 
  recipe(children ~ ., data = hotel_other) |> 
  step_date(arrival_date) |> 
  step_holiday(arrival_date) |> 
  step_rm(arrival_date) 


## ---------------------------------------------------
#| code-fold: true
rf_workflow <- 
  workflow() |> 
  add_model(rf_mod) |> 
  add_recipe(rf_recipe)


## ---------------------------------------------------
#| code-fold: true
rf_mod

extract_parameter_set_dials(rf_mod)


## ---------------------------------------------------
#| code-fold: true
#| eval: false
# # This takes a while so the results are saved
# set.seed(1012)
# rf_res <-
#   rf_workflow |>
#   tune_grid(hotel_val_set,
#             grid = 25,
#             control = control_grid(save_pred = TRUE),
#             metrics = metric_set(roc_auc))
# save(rf_res, file="data/rf_res.rda")


## ---------------------------------------------------
#| code-fold: true
load("data/rf_res.rda")
rf_res |> 
  show_best(metric = "roc_auc")


## ---------------------------------------------------
#| code-fold: true
#| out-width: 50%
autoplot(rf_res)


## ---------------------------------------------------
#| code-fold: true
rf_best <- 
  rf_res |> 
  select_best(metric = "roc_auc")
rf_best


## ---------------------------------------------------
#| code-fold: true
rf_res |> 
  collect_predictions()


## ---------------------------------------------------
#| code-fold: true
rf_auc <- 
  rf_res |> 
  collect_predictions(parameters = rf_best) |> 
  roc_curve(children, .pred_children) |> 
  mutate(model = "Random Forest")


## ---------------------------------------------------
#| code-fold: true
bind_rows(rf_auc, lr_auc) |> 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
    geom_path(lwd = 1.5, alpha = 0.8) +
    geom_abline(lty = 3) + 
    coord_equal() + 
    scale_color_viridis_d(option = "plasma", end = .6)


## ---------------------------------------------------
#| code-fold: true
# the last model
last_rf_mod <- 
  rand_forest(mtry = 8, min_n = 7, trees = 1000) |> 
  set_engine("ranger", num.threads = cores, importance = "impurity") |> 
  set_mode("classification")

# the last workflow
last_rf_workflow <- 
  rf_workflow |> 
  update_model(last_rf_mod)


## ---------------------------------------------------
#| code-fold: true
#| eval: false
# # the last fit
# set.seed(345)
# last_rf_fit <-
#   last_rf_workflow |>
#   last_fit(splits)
# 
# last_rf_fit
# save(last_rf_fit, file="data/last_rf_fit.rda")


## ---------------------------------------------------
#| code-fold: true
load("data/last_rf_fit.rda")
last_rf_fit |> 
  collect_metrics()



## ---------------------------------------------------
#| code-fold: true
last_rf_fit |> 
  extract_fit_parsnip() |> 
  vip(num_features = 20)


## ---------------------------------------------------
#| code-fold: true
last_rf_fit |> 
  collect_predictions() |> 
  roc_curve(children, .pred_children) |> 
  autoplot()


## ---------------------------------------------------
#| code-fold: true
hotel_test_pred <- tibble(hotel_test) |>
  mutate(
    pchild_cl = last_rf_fit$.predictions[[1]]$.pred_class,
    pchild_pc = last_rf_fit$.predictions[[1]]$.pred_children,
    pchild_pn = last_rf_fit$.predictions[[1]]$.pred_none)
hotel_test_pred |>
  select(pchild_pc, pchild_pn, pchild_cl) |>
  ggplot(aes(x=pchild_pc, 
             fill=pchild_cl, 
             colour=pchild_cl)) +
    geom_density()


## ---------------------------------------------------
#| code-fold: true
hotel_test_pred <- hotel_test_pred |>
  mutate(error = if_else(children != pchild_cl,
                         "yes", "no")) 
ggplot(hotel_test_pred, aes(x=children, 
                            y=average_daily_rate,
                            colour = children)) +
  geom_boxplot() + 
  scale_x_discrete("", labels = c("C", "N")) +
  facet_grid(error~reserved_room_type) +
  theme(legend.position = "none")


## ---------------------------------------------------
#| code-fold: true
glimpse(hotels)


## ---------------------------------------------------
#| code-fold: true
hotels_dr <- hotels |>
  select(lead_time:adults, 
         is_repeated_guest:previous_bookings_not_canceled,
         booking_changes, days_in_waiting_list,
         average_daily_rate, total_of_special_requests)


## ---------------------------------------------------
#| code-fold: true
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
hotels_pca <- prcomp(hotels_dr, scale=TRUE)
hotels_pca$sdev^2
mulgar::ggscree(hotels_pca, q=11)


## ---------------------------------------------------
#| code-fold: true
#| fig-width: 6
#| fig-height: 6
#| out-width: 80%
ggfortify:::autoplot.prcomp(hotels_pca, 
                            loadings = TRUE,
                            loadings.label = TRUE)


## ---------------------------------------------------
#| code-fold: true
#| fig-width: 6
#| fig-height: 6
#| out-width: 80%
ggfortify:::autoplot.prcomp(hotels_pca, x=6, y=7,
                            loadings = TRUE,
                            loadings.label = TRUE)


## ---------------------------------------------------
#| code-fold: true
#| fig-width: 8
#| fig-height: 6
#| out-width: 60%
library(lionfish)
data("risk")
colnames(risk) <- c("Rec", "Hea", "Car", "Fin", "Saf", "Soc")
risk_d  <- apply(risk, 2, function(x) (x-mean(x))/sd(x))

# Two clusters
nc <- 3
set.seed(1145)
r_km <- kmeans(risk_d, centers=nc,
               iter.max = 500, nstart = 5)

r_km_d <- risk |>
  as_tibble() |>
  mutate(cl = factor(r_km$cluster))

r_km_d |> 
  pivot_longer(Rec:Soc, names_to = "var", values_to = "val") |>
  ggplot(aes(x=val, fill=cl)) +
    geom_bar() +
    facet_wrap(~var, ncol=3, scales="free_y") +
    scale_fill_manual(values = c("#377EB8", "#FF7F00", "#4DAF4A")) +
    xlab("") + ylab("") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_blank())


## ---------------------------------------------------
#| label: ped-reg
options(width=55)
pedestrian 


## ---------------------------------------------------
#| label: nycflights
options(width=55)
flights_ts <- flights |>
  mutate(dt = ymd_hm(paste(paste(year, month, day, sep="-"), 
                           paste(hour, minute, sep=":")))) |>
  as_tsibble(index = dt, key = c(origin, dest, carrier, tailnum), regular = FALSE)
flights_ts 


## ---------------------------------------------------
#| code-fold: true
flights_mth <- flights_ts |> 
  as_tibble() |>
  group_by(month, origin) |>
  summarise(dep_delay = mean(dep_delay, na.rm=TRUE)) |>
  as_tsibble(key=origin, index=month)
ggplot(flights_mth, aes(x=month, y=dep_delay, colour=origin)) +
  geom_point() +
  geom_smooth(se=F) +
  scale_x_continuous("", breaks = seq(1, 12, 1), 
                     labels=c("J","F","M","A","M","J",
                              "J","A","S","O","N","D")) +
  scale_y_continuous("av dep delay (mins)", limits=c(0, 25)) +
  theme(aspect.ratio = 0.5)


## ---------------------------------------------------
#| code-fold: true
flights_mth_arr <- flights_ts |> 
  as_tibble() |>
  group_by(month, origin) |>
  summarise(arr_delay = mean(arr_delay, na.rm=TRUE)) |>
  as_tsibble(key=origin, index=month)
ggplot(flights_mth_arr, aes(x=month, y=arr_delay, colour=origin)) +
  geom_point() +
  geom_smooth(se=F) +
  scale_x_continuous("", breaks = seq(1, 12, 1), 
                     labels=c("J","F","M","A","M","J",
                              "J","A","S","O","N","D")) +
  scale_y_continuous("av arr delay (mins)", limits=c(0, 25)) +
  theme(aspect.ratio = 0.5)


## ---------------------------------------------------
#| code-fold: true
#| fig-height: 8
#| fig-width: 5
#| out-width: 48%
flights_wk <- flights_ts |> 
  as_tibble() |>
  mutate(wday = wday(dt, label=TRUE, week_start = 1)) |>
  group_by(wday, origin) |>
  summarise(dep_delay = mean(dep_delay, na.rm=TRUE)) |>
  mutate(weekend = ifelse(wday %in% c("Sat", "Sun"), "yes", "no")) |>
  as_tsibble(key=origin, index=wday)
ggplot(flights_wk, aes(x=wday, y=dep_delay, fill=weekend)) +
  geom_col() +
  facet_wrap(~origin, ncol=1, scales="free_y") +
  xlab("") +
  ylab("av dep delay (mins)") +
  theme(aspect.ratio = 0.5, legend.position = "none")


## ---------------------------------------------------
#| code-fold: true
flights_airtm <- flights |>
  mutate(dep_min = dep_time %% 100,
         dep_hr = dep_time %/% 100,
         arr_min = arr_time %% 100,
         arr_hr = arr_time %/% 100) |>
  mutate(dep_dt = ymd_hm(paste(paste(year, month, day, sep="-"), 
                           paste(dep_hr, dep_min, sep=":")))) |>
  mutate(arr_dt = ymd_hm(paste(paste(year, month, day, sep="-"), 
                           paste(arr_hr, arr_min, sep=":")))) |>
  mutate(air_time2 = as.numeric(difftime(arr_dt, dep_dt)))

fp <- flights_airtm |> 
  sample_n(3000) |>
  ggplot(aes(x=air_time, y=air_time2, label = paste(origin, dest))) + 
    geom_abline(intercept=0, slope=1) +
    geom_point()
ggplotly(fp, width=500, height=500)


## ---------------------------------------------------
#| label: missings-simple
set.seed(328)
harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 
           2012, 2013),
  fruit = rep(c("kiwi", "cherry"), 
              each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
harvest


## ---------------------------------------------------
#| label: missing-gaps
has_gaps(harvest, .full = TRUE) 


## ---------------------------------------------------
#| label: missings-simple
#| echo: false
set.seed(328)
harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 
           2012, 2013),
  fruit = rep(c("kiwi", "cherry"), 
              each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
harvest


## ---------------------------------------------------
#| label: count-gaps
count_gaps(harvest,  .full=TRUE)


## ---------------------------------------------------
#| label: missings-simple
#| echo: false
set.seed(328)
harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 
           2012, 2013),
  fruit = rep(c("kiwi", "cherry"), 
              each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
harvest


## ---------------------------------------------------
#| label: fill-gaps
harvest <- fill_gaps(harvest, 
                     .full=TRUE) 
harvest 


## ---------------------------------------------------
#| label: missings-simple
#| echo: false
set.seed(328)
harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 
           2012, 2013),
  fruit = rep(c("kiwi", "cherry"), 
              each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
harvest


## ---------------------------------------------------
#| label: impute-gaps
harvest_nomiss <- harvest |> 
  group_by(fruit) |> 
  mutate(kilo = 
    na_interpolation(kilo)) |> 
  ungroup()
harvest_nomiss 


## ---------------------------------------------------
#| fig-width: 15
#| fig-height: 10
#| out-width: 100%
#| code-fold: true
ped_QV <-  pedestrian |>
  mutate(year = year(Date),
         month = month(Date)) |>
  dplyr::filter(Sensor == "QV Market-Elizabeth St (West)",
                year == 2016, month %in% c(2:5))
ggplot(ped_QV) +   
    geom_line(aes(x=Time, y=Count)) +
    facet_calendar(~Date) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        aspect.ratio = 0.5) 


## ----echo=TRUE--------------------------------------
#| code-fold: true
has_gaps(pedestrian, .full = TRUE)
ped_gaps <- pedestrian |> 
  dplyr::filter(Sensor == "QV Market-Elizabeth St (West)") |>
  count_gaps(.full = TRUE)
ped_gaps
ped_qvm_filled <- pedestrian |> 
  dplyr::filter(Sensor == "QV Market-Elizabeth St (West)") |>
  fill_gaps(.full = TRUE) |>
  mutate(Date = as.Date(Date_Time, tz="Australia/Melbourne"),
         Time = hour(Date_Time)) |>
  mutate(year = year(Date),
    month = month(Date)) 


## ---------------------------------------------------
#| fig-width: 15
#| fig-height: 3
#| out-width: 100%
#| code-fold: true
# Which days are holidays
hol <- holiday_aus(2015:2016, state = "VIC")
ped_qvm_filled <- ped_qvm_filled |> 
  mutate(hol = is.weekend(Date)) |>
  mutate(hol = if_else(Date %in% hol, TRUE, hol)) |>
  mutate(Time = factor(Time))

# Fit linear model to inpute missings
ped_qvm_lm <- lm(Count~Time*hol, data=ped_qvm_filled)
ped_qvm_filled <- ped_qvm_filled |>
  mutate(pCount = predict(ped_qvm_lm, ped_qvm_filled))

# Select subset and plot results
ped_qvm_sub <- ped_qvm_filled |>
  filter(Date > ymd("2015-12-17"), Date < ymd("2016-01-01")) 
ggplot(ped_qvm_sub) +   
    geom_line(aes(x=Date_Time, y=Count)) +
    geom_line(data=filter(ped_qvm_sub, is.na(Count)), 
                      aes(x=Date_Time, 
                          y=pCount), 
                      colour="seagreen3") +
  scale_x_datetime("", date_breaks = "1 day", 
                   date_labels = "%a %d") +
  theme(aspect.ratio = 0.15)


## ---------------------------------------------------
#| fig-width: 5
#| fig-height: 5
#| out-width: 50%
#| echo: false
#| eval: false
# ggplot() +
#     geom_line(data=filter(ped_qvm_sub),
#               aes(x=Time, y=Count, group=Date)) +
#     geom_line(data=filter(ped_qvm_sub, is.na(Count)),
#                       aes(x=Time,
#                           y=pCount,
#                           group=Date),
#                       colour="seagreen3",
#                       linewidth=2) +
#   facet_wrap(~hol) +
#   theme(aspect.ratio = 1)

