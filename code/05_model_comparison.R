source("code/00_setup.R")

linear_output <- readRDS("data/models/linear_regression.rds")
elastic_output <- readRDS("data/models/elastic_net.rds")
knn_output <- readRDS("data/models/knn.rds")
random_forest_output <- readRDS("data/models/random_forest.rds")
boosted_tree_output <- readRDS("data/models/boosted_trees.rds")

best_metric <- function(tune_result, best_parameters) {
  collect_metrics(tune_result) |>
    filter(.metric == "rmse") |>
    semi_join(best_parameters, by = names(best_parameters)) |>
    slice_min(mean, n = 1, with_ties = FALSE) |>
    select(mean, std_err)
}

linear_metric <- collect_metrics(linear_output$resamples) |>
  filter(.metric == "rmse") |>
  select(mean, std_err)

comparison <- bind_rows(
  linear_metric |>
    mutate(model = "Linear Regression", selected_parameters = "None"),
  best_metric(elastic_output$tuned, elastic_output$best) |>
    mutate(
      model = "Elastic Net",
      selected_parameters = paste0(
        "penalty = ", signif(elastic_output$best$penalty, 4),
        "; mixture = ", round(elastic_output$best$mixture, 3)
      )
    ),
  best_metric(knn_output$tuned, knn_output$best) |>
    mutate(
      model = "K-Nearest Neighbors",
      selected_parameters = paste0("K = ", knn_output$best$neighbors)
    ),
  best_metric(random_forest_output$tuned, random_forest_output$best) |>
    mutate(
      model = "Random Forest",
      selected_parameters = paste0(
        "mtry = ", random_forest_output$best$mtry,
        "; trees = 500; min_n = 10"
      )
    ),
  best_metric(boosted_tree_output$tuned, boosted_tree_output$best) |>
    mutate(
      model = "Gradient-Boosted Trees",
      selected_parameters = paste0(
        "learn_rate = ", boosted_tree_output$best$learn_rate,
        "; depth = ", boosted_tree_output$best$tree_depth,
        "; trees = 1000; min_n = 10"
      )
    )
) |>
  select(model, mean_rmse = mean, std_err, selected_parameters) |>
  arrange(mean_rmse) |>
  mutate(rank = row_number(), .before = model)

readr::write_csv(comparison, "data/results/cv_model_comparison.csv")

top_two <- comparison |>
  slice_head(n = 2)

combined_standard_error <- sqrt(sum(top_two$std_err^2))
rmse_difference <- diff(top_two$mean_rmse)
models_are_close <- abs(rmse_difference) <= combined_standard_error

selected_models <- comparison |>
  slice_head(n = if_else(models_are_close, 2L, 1L)) |>
  mutate(
    selection_reason = if_else(
      models_are_close,
      "The two lowest mean RMSE values differ by no more than their combined standard error.",
      "The lowest mean RMSE is separated from the second-lowest by more than their combined standard error."
    )
  )

readr::write_csv(selected_models, "data/results/selected_models.csv")

comparison_plot <- comparison |>
  mutate(model = forcats::fct_reorder(model, mean_rmse)) |>
  ggplot(aes(x = mean_rmse, y = model)) +
  geom_errorbarh(
    aes(xmin = mean_rmse - std_err, xmax = mean_rmse + std_err),
    height = 0.18,
    color = report_colors[["light_blue"]],
    linewidth = 1
  ) +
  geom_point(color = report_colors[["navy"]], size = 3) +
  labs(
    title = "Cross-Validation RMSE by Model",
    subtitle = "Points show mean RMSE; bars show +/- one standard error",
    x = expression("RMSE (" * log[10] * " kt CO"[2] * ")"),
    y = NULL
  )

ggsave("graphs/cv_rmse_model_comparison.png", comparison_plot, width = 8, height = 5.2, dpi = 300)

message(
  "Model comparison complete. Selected for final testing: ",
  paste(selected_models$model, collapse = " and "), "."
)
