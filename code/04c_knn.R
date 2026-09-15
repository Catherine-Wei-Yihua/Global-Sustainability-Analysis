source("code/00_setup.R")

model_setup <- readRDS("data/resamples/model_setup.rds")

knn_spec <- nearest_neighbor(
  neighbors = tune(),
  weight_func = "rectangular",
  dist_power = 2
) |>
  set_engine("kknn") |>
  set_mode("regression")

knn_workflow <- workflow() |>
  add_recipe(model_setup$recipes$knn) |>
  add_model(knn_spec)

set.seed(project_seed)
knn_tuned <- tune_grid(
  knn_workflow,
  resamples = model_setup$folds,
  grid = model_setup$grids$knn,
  metrics = metric_set(rmse),
  control = control_grid(save_pred = FALSE)
)

knn_output <- list(
  workflow = knn_workflow,
  tuned = knn_tuned,
  best = select_best(knn_tuned, metric = "rmse")
)

saveRDS(knn_output, "data/models/knn.rds")
readr::write_csv(
  collect_metrics(knn_tuned),
  "data/results/knn_tuning_metrics.csv"
)

knn_plot_data <- collect_metrics(knn_tuned) |>
  filter(.metric == "rmse")

knn_plot <- ggplot(knn_plot_data, aes(x = neighbors, y = mean)) +
  geom_line(color = report_colors[["blue"]], linewidth = 0.9) +
  geom_point(color = report_colors[["navy"]], size = 2.7) +
  scale_x_continuous(breaks = model_setup$grids$knn$neighbors) +
  labs(
    title = "K-Nearest Neighbors Cross-Validation RMSE",
    x = "Number of neighbors (K)",
    y = expression("Mean RMSE (" * log[10] * " kt CO"[2] * ")")
  )

ggsave("graphs/knn_tuning_rmse.png", knn_plot, width = 8, height = 5, dpi = 300)
message("K-nearest neighbors tuning complete.")
