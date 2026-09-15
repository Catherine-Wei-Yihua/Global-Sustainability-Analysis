source("code/00_setup.R")

model_setup <- readRDS("data/resamples/model_setup.rds")
comparison <- readr::read_csv(
  "data/results/cv_model_comparison.csv",
  show_col_types = FALSE
)
selected_models <- readr::read_csv(
  "data/results/selected_models.csv",
  show_col_types = FALSE
)

model_files <- c(
  "Random Forest" = "data/models/random_forest.rds"
)

finalize_selected_workflow <- function(model_name) {
  output <- readRDS(unname(model_files[[model_name]]))
  finalize_workflow(output$workflow, output$best)
}

prediction_results <- vector("list", nrow(selected_models))
names(prediction_results) <- selected_models$model

for (model_name in selected_models$model) {
  finalized_workflow <- finalize_selected_workflow(model_name)
  final_fit <- fit(finalized_workflow, data = model_setup$training_data)
  test_predictions <- predict(final_fit, new_data = model_setup$testing_data) |>
    bind_cols(model_setup$testing_data) |>
    mutate(
      model = model_name,
      observed_co2_kt = 10^log10_co2_emissions,
      predicted_co2_kt = 10^.pred
    )

  safe_model_name <- model_name |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_remove("_$")

  saveRDS(final_fit, file.path("data/models", paste0(safe_model_name, "_final_fit.rds")))
  prediction_results[[model_name]] <- test_predictions
}

all_predictions <- bind_rows(prediction_results)

test_metrics <- all_predictions |>
  group_by(model) |>
  summarise(
    test_rmse_log10 = rmse_vec(log10_co2_emissions, .pred),
    multiplicative_error_factor = 10^test_rmse_log10,
    test_rmse_original_kt = rmse_vec(observed_co2_kt, predicted_co2_kt),
    .groups = "drop"
  ) |>
  left_join(
    comparison |>
      select(model, cv_rmse_log10 = mean_rmse, cv_std_err = std_err),
    by = "model"
  ) |>
  mutate(generalization_gap = test_rmse_log10 - cv_rmse_log10) |>
  arrange(test_rmse_log10)

readr::write_csv(all_predictions, "data/results/final_test_predictions.csv")
readr::write_csv(test_metrics, "data/results/final_test_metrics.csv")

prediction_plot <- ggplot(
  all_predictions,
  aes(x = observed_co2_kt, y = predicted_co2_kt)
) +
  geom_abline(slope = 1, intercept = 0, color = report_colors[["gray"]], linetype = 2) +
  geom_point(alpha = 0.55, size = 1.6, color = report_colors[["navy"]]) +
  scale_x_log10(labels = scales::label_number(big.mark = ",")) +
  scale_y_log10(labels = scales::label_number(big.mark = ",")) +
  labs(
    title = "Random Forest: Predicted versus Observed CO2 Emissions",
    subtitle = "Untouched 2016-2019 test years; both axes use log10 scales",
    x = "Observed CO2 emissions (kt)",
    y = "Predicted CO2 emissions (kt)"
  )

ggsave("graphs/test_predicted_vs_observed.png", prediction_plot, width = 8, height = 6, dpi = 300)

if ("Random Forest" %in% selected_models$model) {
  rf_fit <- readRDS("data/models/random_forest_final_fit.rds")
  rf_engine <- extract_fit_engine(rf_fit)
  importance_data <- tibble(
    predictor = names(rf_engine$variable.importance),
    importance = as.numeric(rf_engine$variable.importance)
  ) |>
    arrange(desc(importance))

  readr::write_csv(importance_data, "data/results/random_forest_variable_importance.csv")

  importance_plot <- importance_data |>
    slice_max(importance, n = 10) |>
    mutate(
      predictor = stringr::str_replace_all(predictor, "_", " "),
      predictor = forcats::fct_reorder(predictor, importance)
    ) |>
    ggplot(aes(x = importance, y = predictor)) +
    geom_col(fill = report_colors[["navy"]], width = 0.7) +
    labs(
      title = "Random Forest Variable Importance",
      subtitle = "Permutation importance; larger values indicate greater predictive contribution",
      x = "Permutation importance",
      y = NULL
    )

  ggsave("graphs/random_forest_variable_importance.png", importance_plot, width = 8, height = 5.5, dpi = 300)
}

message("Final evaluation complete for: ", paste(selected_models$model, collapse = " and "), ".")
