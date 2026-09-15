source("code/00_setup.R")

model_setup <- readRDS("data/resamples/model_setup.rds")

linear_spec <- linear_reg() |>
  set_engine("lm")

linear_workflow <- workflow() |>
  add_recipe(model_setup$recipes$linear_regression) |>
  add_model(linear_spec)

set.seed(project_seed)
linear_resamples <- fit_resamples(
  linear_workflow,
  resamples = model_setup$folds,
  metrics = metric_set(rmse),
  control = control_resamples(save_pred = FALSE)
)

linear_output <- list(
  workflow = linear_workflow,
  resamples = linear_resamples
)

saveRDS(linear_output, "data/models/linear_regression.rds")
readr::write_csv(
  collect_metrics(linear_resamples),
  "data/results/linear_regression_cv_metrics.csv"
)

message("Linear regression complete.")
