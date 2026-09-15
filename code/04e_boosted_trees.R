source("code/00_setup.R")

model_setup <- readRDS("data/resamples/model_setup.rds")

boosted_tree_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  learn_rate = tune(),
  min_n = 10,
  loss_reduction = 0,
  sample_size = 1
) |>
  set_engine("xgboost", nthread = 1, verbose = 0) |>
  set_mode("regression")

boosted_tree_workflow <- workflow() |>
  add_recipe(model_setup$recipes$boosted_trees) |>
  add_model(boosted_tree_spec)

set.seed(project_seed)
boosted_tree_tuned <- tune_grid(
  boosted_tree_workflow,
  resamples = model_setup$folds,
  grid = model_setup$grids$boosted_trees,
  metrics = metric_set(rmse),
  control = control_grid(save_pred = FALSE)
)

boosted_tree_output <- list(
  workflow = boosted_tree_workflow,
  tuned = boosted_tree_tuned,
  best = select_best(boosted_tree_tuned, metric = "rmse")
)

saveRDS(boosted_tree_output, "data/models/boosted_trees.rds")
readr::write_csv(
  collect_metrics(boosted_tree_tuned),
  "data/results/boosted_trees_tuning_metrics.csv"
)

boosted_plot_data <- collect_metrics(boosted_tree_tuned) |>
  filter(.metric == "rmse") |>
  mutate(tree_depth = factor(tree_depth))

boosted_plot <- ggplot(
  boosted_plot_data,
  aes(x = learn_rate, y = mean, color = tree_depth, group = tree_depth)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.7) +
  scale_x_continuous(breaks = c(0.01, 0.05, 0.10)) +
  scale_color_manual(values = c(report_colors[["coral"]], report_colors[["navy"]])) +
  labs(
    title = "Gradient-Boosted Trees Cross-Validation RMSE",
    subtitle = "1,000 trees and a fixed minimum node size of 10",
    x = "Learning rate",
    y = expression("Mean RMSE (" * log[10] * " kt CO"[2] * ")"),
    color = "Tree depth"
  )

ggsave("graphs/boosted_trees_tuning_rmse.png", boosted_plot, width = 8, height = 5, dpi = 300)
message("Gradient-boosted trees tuning complete.")
