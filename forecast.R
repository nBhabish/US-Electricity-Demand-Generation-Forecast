
# Libraries ---------------------------------------------------------------

library(tidyverse)

library(timetk)
library(lubridate)

library(tidymodels)

library(modeltime)
library(modeltime.resample)
library(modeltime.ensemble)

# Data --------------------------------------------------------------------
load("Desktop/Electricity Forecast/elec_df.rda")

# Data Cleaning -----------------------------------------------------------

## Creating Daily DF ----
elec_df_daily <- elec_df %>%
  pad_by_time(
    .date_var = date_time,
    .by = "hour",
    .pad_value = NA,
    .fill_na_direction = "downup"
  ) %>%
  group_by(type) %>%
  summarise_by_time(.date_var = date_time,
                    .by = "day",
                    series = sum(series)) %>%
  ungroup() %>%
  filter(date_time != (Sys.Date()))


## Quick Visualization ----
elec_df %>%
  pad_by_time(
    .date_var = date_time,
    .by = "hour",
    .pad_value = NA,
    .fill_na_direction = "downup"
  ) %>%
  group_by(type) %>%
  summarise_by_time(.date_var = date_time,
                    .by = "day",
                    series = sum(series)) %>%
  ungroup() %>%
  filter(date_time != (Sys.Date())) %>%
  plot_time_series(.date_var = date_time, series, type, .smooth = F)

## Full Data ----

full_data_tbl <- elec_df_daily %>%
  
  # Group-wise Transformation
  group_by(type) %>%
  mutate(series = log(series)) %>%
  future_frame(
    .data = .,
    date_time,
    .length_out = "364 days",
    .bind_data = TRUE
  ) %>%
  ungroup() %>%
  
  # Lags & Rolling Features + Fourier Features
  mutate(type = as_factor(type)) %>%
  group_by(type) %>%
  tk_augment_lags(series, .lags = 364) %>%
  tk_augment_slidify(
    series_lag364,
    .f       = ~ mean(.x, na.rm = TRUE),
    .period  = c(90, 182),
    .partial = TRUE,
    .align   = "center"
  ) %>%
  ungroup() %>%
  rowid_to_column(var = "rowid") %>%
  rename_with(.cols = contains("lag"), .fn = ~ str_c("lag_", .))

## Data Prepared ----

data_prepared_tbl <- full_data_tbl %>%
  filter(!is.na(series)) %>%
  drop_na()

data_prepared_tbl %>%
  filter(type == "demand") %>%
  select(-rowid, -type) %>%
  plot_time_series_regression(
    date_time,
    .formula = series ~ as.numeric(date_time) + wday(date_time, label = T) + month(date_time, label = T) + . - date_time,
    .show_summary = T
  )

future_tbl <- full_data_tbl %>%
  filter(is.na(series))

future_tbl %>% glimpse()


# Time Split --------------------------------------------------------------

splits <- data_prepared_tbl %>%
  time_series_split(date_time, assess = 364, cumulative = TRUE)


splits

## Visualizing time series split ----

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date_time, series)

## Training and Testing Splits ----

train_split <- training(splits)

test_split <- testing(splits)


# Recipe ------------------------------------------------------------------

recipe_spec <- recipe(series ~ ., data = train_split) %>%
  
  # Updating role for rowid feature to keep track of the observation
  update_role(rowid, new_role = "indicator") %>%
  
  # Creating timeseries features
  step_timeseries_signature(date_time) %>%
  
  # Removing features that don't really relate to daily forecast
  step_rm(contains(c(
    "xts", ".iso", "hour", "minute", "second", "am.pm"
  ))) %>%
  
  # Standarizing with step_normalize
  step_normalize(date_time_index.num, date_time_year) %>%
  
  # Creating Dummy features (One-Hot Encoding)
  step_dummy(all_nominal(), one_hot = TRUE)


recipe_spec %>%
  prep() %>%
  juice() %>%
  glimpse()


# Models  -----------------------------------------------------------------
# Only using non-sequential models

# * PROPHET ----

wflw_fit_prophet <- workflow() %>%
  add_model(spec = prophet_reg(mode = "regression") %>%
              set_engine("prophet")) %>%
  add_recipe(recipe_spec) %>%
  fit(train_split)

modeltime_table(wflw_fit_prophet) %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_accuracy()

# * XGBOOST ----

wflw_fit_xgboost <- workflow() %>%
  add_model(spec = boost_tree(mode = "regression") %>%
              set_engine("xgboost")) %>%
  add_recipe(recipe_spec %>% update_role(date_time, new_role = "indicator")) %>%
  fit(train_split)

modeltime_table(wflw_fit_prophet,
                wflw_fit_xgboost) %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_accuracy()

# * Prophet Boost ----

# Why? XGBoost will model seasonality and is good with it while Prophet will be used for trend


wflw_fit_prophet_boost <- workflow() %>%
  add_model(
    spec = prophet_boost(
      seasonality_yearly = FALSE,
      seasonality_daily  = FALSE,
      seasonality_weekly = FALSE
    ) %>%
      set_engine("prophet_xgboost")
  ) %>%
  add_recipe(recipe_spec) %>%
  fit(train_split)

modeltime_table(wflw_fit_prophet,
                wflw_fit_xgboost,
                wflw_fit_prophet_boost) %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_accuracy()


# * SVM ----

wflw_fit_svm <- workflow() %>%
  add_model(spec = svm_rbf(mode = "regression") %>%
              set_engine("kernlab")) %>%
  add_recipe(recipe_spec %>%
               update_role(date_time, new_role = "indicator")) %>%
  fit(train_split)


modeltime_table(wflw_fit_prophet,
                wflw_fit_xgboost,
                wflw_fit_prophet_boost,
                wflw_fit_svm) %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_accuracy()


# * RANDOM FOREST ----

wflw_fit_rf <- workflow() %>%
  add_model(spec = rand_forest(mode = "regression") %>%
              set_engine("ranger")) %>%
  add_recipe(recipe_spec %>%
               update_role(date_time, new_role = "indicator")) %>%
  fit(train_split)

modeltime_table(
  wflw_fit_prophet,
  wflw_fit_xgboost,
  wflw_fit_prophet_boost,
  wflw_fit_svm,
  wflw_fit_rf
) %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_accuracy()


# * NNET ----

wflw_fit_nnet <- workflow() %>%
  add_model(spec = mlp(mode = "regression") %>%
              set_engine("nnet")) %>%
  add_recipe(recipe_spec %>%
               update_role(date_time, new_role = "indicator")) %>%
  fit(train_split)

modeltime_table(
  wflw_fit_prophet,
  wflw_fit_xgboost,
  wflw_fit_prophet_boost,
  wflw_fit_svm,
  wflw_fit_rf,
  wflw_fit_nnet
) %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_accuracy()

# * MARS ----


wflw_fit_mars <- workflow() %>%
  add_model(spec = mars(mode = "regression") %>%
              set_engine("earth")) %>%
  add_recipe(recipe_spec %>% update_role(date_time, new_role = "indicator")) %>%
  fit(train_split)

## Quick Accuracy Check ----

submodels_1_tbl <- modeltime_table(
  wflw_fit_prophet,
  wflw_fit_xgboost,
  wflw_fit_prophet_boost,
  wflw_fit_svm,
  wflw_fit_rf,
  wflw_fit_nnet,
  wflw_fit_mars
)

submodels_1_tbl %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_accuracy() %>%
  arrange(desc(rmse))

train_split


# HyperParameter Tuning ---------------------------------------------------
# Tuning Ranger, Kernlab, and Prophet model

# * Resamples | k-folds ---------------------------------------------------
set.seed(123)

resamples_kfold <- train_split %>% 
  vfold_cv(v = 5)

resamples_kfold %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(date_time, series, .facet_ncol = 2)

# * XGBOOST TUNE  ---------------------------------------------------------

# ** Tunable Specification ----

model_spec_rf_tune <- rand_forest(
  mode     = "regression",
  mtry     = tune(),
  trees    = tune(),
  min_n    = tune()
) %>% 
  set_engine("ranger")


wflw_spec_rf_tune <- workflow() %>% 
  add_model(model_spec_rf_tune) %>% 
  add_recipe(recipe_spec %>% 
               update_role(date_time, new_role = "indicator"))

# ** Tuning ----

modeltime::parallel_start(4)
tune_results_rf <- wflw_spec_rf_tune %>% 
  tune_grid(
    resamples = resamples_kfold,
    grid      = 10, 
    metrics   = default_forecast_accuracy_metric_set(),
    control   = control_grid(verbose = T, allow_par = F)
  )
modeltime::parallel_stop()

# ** Results ----

tune_results_rf %>% 
  show_best("rmse")

# ** Finalize RF Workflow ----

wflw_fit_rf_tuned <- wflw_spec_rf_tune %>% 
  finalize_workflow(select_best(tune_results_rf, "rmse")) %>% 
  fit(train_split)



# * SVM TUNE  ---------------------------------------------------------------

# ** Tunable Specification ----

model_spec_svm_tune <- svm_rbf(mode      = "regression", 
                               cost      = tune(),
                               rbf_sigma = tune(),
                               margin    = tune()
) %>% 
  set_engine("kernlab")

wflw_spec_svm_tune <- workflow() %>% 
  add_model(model_spec_svm_tune) %>% 
  add_recipe(recipe_spec %>% 
               update_role(date_time, new_role = "indicator"))

# ** Tuning ----

modeltime::parallel_start(4)
tune_results_svm <- wflw_spec_svm_tune %>% 
  tune_grid(
    resamples    = resamples_kfold,
    grid         = 10,
    metrics      = default_forecast_accuracy_metric_set(),
    control      = control_grid(verbose = T, allow_par = F)
  ) 
modeltime::parallel_stop()

# ** Results ----

tune_results_svm %>% 
  show_best("rmse")

# ** Finalize SVM Workflow ----

wflw_fit_svm_tuned <- wflw_spec_svm_tune %>% 
  finalize_workflow(select_best(tune_results_svm, "rmse")) %>% 
  fit(train_split)

# * Prophet Tune ----------------------------------------------------------

# ** Tunable Specification ----

model_spec_prophet_tune <- prophet_reg(
  mode = "regression",
  seasonality_yearly = TRUE,
  seasonality_weekly = TRUE,
  seasonality_daily = TRUE,
  changepoint_num = tune(),
  changepoint_range = tune(),
  prior_scale_changepoints = tune(),
  prior_scale_holidays = tune()
) %>% 
  set_engine("prophet")

wflw_spec_prophet_tune <- workflow() %>% 
  add_model(model_spec_prophet_tune) %>% 
  add_recipe(recipe_spec)


# ** Tuning ----


modeltime::parallel_start(4)
tune_results_prophet <- wflw_spec_prophet_tune %>% 
  tune_grid(
    resamples    = resamples_kfold,
    grid         = 10,
    metrics      = default_forecast_accuracy_metric_set(),
    control      = control_grid(verbose = TRUE, allow_par = F)
  )
modeltime::parallel_stop()

# ** Results ----

tune_results_prophet %>% 
  show_best("rmse")

# ** Finalize Prophet Workflow ----

wflw_fit_prophet_tuned <- wflw_spec_prophet_tune %>% 
  finalize_workflow(select_best(tune_results_prophet, "rmse")) %>% 
  fit(train_split)


# Evaluating Forecasts ----------------------------------------------------

# * Model Table ----

submodels_2_tbl <- modeltime_table(wflw_fit_prophet_tuned,
                                   wflw_fit_rf_tuned,
                                   wflw_fit_svm_tuned) %>% 
  update_model_description(1, "PROPHET W/ REGRESSORS - Tuned") %>% 
  update_model_description(2, "RANGER - Tuned") %>% 
  update_model_description(3, "KERNLAB - Tuned") %>% 
  combine_modeltime_tables(submodels_1_tbl)

# * Calibration ----

calibration_tbl <- submodels_2_tbl %>% 
  modeltime_calibrate(test_split)


# * Accuracy (come back to this)----

calibration_tbl %>% 
  modeltime_accuracy() %>% 
  arrange(rmse)


# * Visualizing calibration_tbl (come back to this {forecast on test set})-----

calibration_tbl %>% 
  modeltime_forecast(new_data = test_split,
                     actual_data = data_prepared_tbl,
                     keep_data = TRUE) %>% 
  group_by(type) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)

# Resampling (testing the durability of models) ---------------------------

resamples_tscv <- train_split %>% 
  time_series_cv(
    assess      = 364,
    skip        = 182,
    cumulative  = TRUE,
    slice_limit = 4
  )

resamples_tscv %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(date_time, series)

# each model on the modeltime table will be refit on each CV training set, then the predictions are made on the cv test set
# we keep these cv test predictions for analysis for model performance

# * Fitting Resamples  ----------------------------------------------------

model_tbl_tuned_resamples <- submodels_2_tbl %>% 
  modeltime_fit_resamples(
    resamples = resamples_tscv,
    control   = control_resamples(verbose = TRUE, allow_par = TRUE)
  )

# * Resampling Accuracy Table ---------------------------------------------

model_tbl_tuned_resamples %>% 
  modeltime_resample_accuracy() %>% 
  arrange(rmse)


# * Resampling Accuracy Plot ----------------------------------------------

model_tbl_tuned_resamples %>% 
  plot_modeltime_resamples(.metric_set  = metric_set(mae, rmse, rsq),
                           .point_size  = 4,
                           .point_alpha = 0.8,
                           .facet_ncol  = 1, .summary_line_show = F)

# Ensemble Panel Models ---------------------------------------------------

# * Average Ensemble ------------------------------------------------------

submodels_2_ids_to_keep <- c(3, 2)

ensemble_fit <- submodels_2_tbl %>% 
  filter(.model_id %in% submodels_2_ids_to_keep) %>% 
  ensemble_average(type = "median")

model_ensemble_tbl <- modeltime_table(ensemble_fit)

# * Ensemble Accuracy -----------------------------------------------------

model_ensemble_tbl %>% 
  modeltime_accuracy(test_split)

# * Ensemble Forecast -----------------------------------------------------

forecast_ensemble_test_tbl <- model_ensemble_tbl %>% 
  modeltime_forecast(
    new_data = test_split,
    actual_data = data_prepared_tbl,
    keep_data = TRUE
  ) %>% 
  
  # inverting the transformation
  
  mutate(
    across(.cols = c(.value, series), .fns = exp)
  )

forecast_ensemble_test_tbl %>% 
  group_by(type) %>% 
  plot_modeltime_forecast()


# * Accuracy by Group -----------------------------------------------------

forecast_ensemble_test_tbl %>% 
  filter(.key == "prediction") %>% 
  select(type, .value, series) %>% 
  group_by(type) %>% 
  summarize_accuracy_metrics(truth = series, estimate = .value,
                             metric_set = default_forecast_accuracy_metric_set())

# * Ensemble Refitting ----------------------------------------------------

model_ensemble_refit_tbl <- model_ensemble_tbl %>% 
  modeltime_refit(data = data_prepared_tbl)


submodels_2_refit_tbl <- submodels_2_tbl %>% 
  modeltime_refit(data = data_prepared_tbl)

model_ensemble_refit_tbl %>% 
  modeltime_forecast(
    new_data    = future_tbl,
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) %>% 
  mutate(.value = exp(.value),
         series = exp(series)) %>% 
  group_by(type) %>% 
  plot_modeltime_forecast()


















































