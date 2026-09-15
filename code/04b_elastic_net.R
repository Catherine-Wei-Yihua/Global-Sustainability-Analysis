source("code/00_setup.R")

model_setup <- readRDS("data/resamples/model_setup.rds")

elastic_spec <- linear_reg(
  penalty = tune(),
  mixture = tune()
) |>
  set_engine("glmnet")

elastic_workflow <- workflow() |>
  add_recipe(model_setup$recipes$elastic_net) |>
  add_model(elastic_spec)

set.seed(project_seed)
elastic_tuned <- tune_grid(
  elastic_workflow,
  resamples = model_setup$folds,
  grid = model_setup$grids$elastic_net,
  metrics = metric_set(rmse),
  control = control_grid(save_pred = FALSE)
)

elastic_output <- list(
  workflow = elastic_workflow,
  tuned = elastic_tuned,
  best = select_best(elastic_tuned, metric = "rmse")
)

saveRDS(elastic_output, "data/models/elastic_net.rds")
readr::write_csv(
  collect_metrics(elastic_tuned),
  "data/results/elastic_net_tuning_metrics.csv"
)

elastic_plot_data <- collect_metrics(elastic_tuned) |>
  filter(.metric == "rmse") |>
  mutate(mixture = factor(round(mixture, 2)))

elastic_plot <- ggplot(
  elastic_plot_data,
  aes(x = penalty, y = mean, color = mixture, group = mixture)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.4) +
  scale_x_log10() +
  scale_color_manual(values = c(
    report_colors[["coral"]], report_colors[["sand"]],
    report_colors[["blue"]], report_colors[["navy"]]
  )) +
  labs(
    title = "Elastic Net Cross-Validation RMSE",
    x = "Penalty (log scale)",
    y = expression("Mean RMSE (" * log[10] * " kt CO"[2] * ")"),
    color = "Mixture"
  )

ggsave("graphs/elastic_net_tuning_rmse.png", elastic_plot, width = 8, height = 5, dpi = 300)
message("Elastic net tuning complete.")
