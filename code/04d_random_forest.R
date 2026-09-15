source("code/00_setup.R")

model_setup <- readRDS("data/resamples/model_setup.rds")

random_forest_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = 10
) |>
  set_engine("ranger", importance = "permutation", num.threads = 1) |>
  set_mode("regression")

random_forest_workflow <- workflow() |>
  add_recipe(model_setup$recipes$random_forest) |>
  add_model(random_forest_spec)

set.seed(project_seed)
random_forest_tuned <- tune_grid(
  random_forest_workflow,
  resamples = model_setup$folds,
  grid = model_setup$grids$random_forest,
  metrics = metric_set(rmse),
  control = control_grid(save_pred = FALSE)
)

random_forest_output <- list(
  workflow = random_forest_workflow,
  tuned = random_forest_tuned,
  best = select_best(random_forest_tuned, metric = "rmse")
)

saveRDS(random_forest_output, "data/models/random_forest.rds")
readr::write_csv(
  collect_metrics(random_forest_tuned),
  "data/results/random_forest_tuning_metrics.csv"
)

random_forest_plot_data <- collect_metrics(random_forest_tuned) |>
  filter(.metric == "rmse")

random_forest_plot <- ggplot(random_forest_plot_data, aes(x = mtry, y = mean)) +
  geom_line(color = report_colors[["blue"]], linewidth = 0.9) +
  geom_point(color = report_colors[["navy"]], size = 2.7) +
  scale_x_continuous(breaks = model_setup$grids$random_forest$mtry) +
  labs(
    title = "Random Forest Cross-Validation RMSE",
    subtitle = "500 trees and a fixed minimum node size of 10",
    x = "Predictors randomly considered at each split (mtry)",
    y = expression("Mean RMSE (" * log[10] * " kt CO"[2] * ")")
  )

ggsave("graphs/random_forest_tuning_rmse.png", random_forest_plot, width = 8, height = 5, dpi = 300)
message("Random forest tuning complete.")
