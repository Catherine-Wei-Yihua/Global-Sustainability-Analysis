source("code/00_setup.R")

prepared_data <- readr::read_csv(clean_data_path, show_col_types = FALSE) |>
  mutate(entity = factor(entity))

available_years <- sort(unique(prepared_data$year))
n_training_years <- floor(length(available_years) * 0.80)
training_years <- available_years[seq_len(n_training_years)]
testing_years <- setdiff(available_years, training_years)

training_data <- prepared_data |>
  filter(year %in% training_years) |>
  droplevels()

testing_data <- prepared_data |>
  filter(year %in% testing_years) |>
  droplevels()

new_testing_entities <- setdiff(
  unique(as.character(testing_data$entity)),
  unique(as.character(training_data$entity))
)

if (length(new_testing_entities) > 0) {
  stop(
    "The chronological test set contains entities absent from training: ",
    paste(new_testing_entities, collapse = ", ")
  )
}

readr::write_csv(training_data, "data/processed/sustainability_train.csv")
readr::write_csv(testing_data, "data/processed/sustainability_test.csv")

training_model_data <- training_data |>
  select(all_of(model_predictors), all_of(outcome_model))

testing_model_data <- testing_data |>
  select(all_of(model_predictors), all_of(outcome_model))

set.seed(project_seed)
cv_folds <- rsample::vfold_cv(
  training_model_data,
  v = 10,
  strata = all_of(outcome_model),
  breaks = 5
)

saveRDS(cv_folds, "data/resamples/cv_folds.rds")

linear_recipe <- recipe(log10_co2_emissions ~ ., data = training_model_data) |>
  step_rm(land_area_km2) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_lincomb(all_numeric_predictors())

elastic_recipe <- recipe(log10_co2_emissions ~ ., data = training_model_data) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

knn_recipe <- recipe(log10_co2_emissions ~ ., data = training_model_data) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

random_forest_recipe <- recipe(log10_co2_emissions ~ ., data = training_model_data) |>
  step_novel(all_nominal_predictors()) |>
  step_zv(all_predictors())

boosted_tree_recipe <- recipe(log10_co2_emissions ~ ., data = training_model_data) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

elastic_grid <- tidyr::crossing(
  penalty = 10^seq(-4, 0, length.out = 5),
  mixture = seq(0, 1, length.out = 4)
)

knn_grid <- tibble(neighbors = c(7L, 15L, 25L, 35L, 45L, 65L))

random_forest_grid <- tibble(mtry = c(1L, 3L, 5L, 7L, 9L, 12L))

boosted_tree_grid <- tidyr::crossing(
  learn_rate = c(0.01, 0.05, 0.10),
  tree_depth = c(1L, 2L)
)

model_setup <- list(
  training_data = training_model_data,
  testing_data = testing_model_data,
  folds = cv_folds,
  recipes = list(
    linear_regression = linear_recipe,
    elastic_net = elastic_recipe,
    knn = knn_recipe,
    random_forest = random_forest_recipe,
    boosted_trees = boosted_tree_recipe
  ),
  grids = list(
    elastic_net = elastic_grid,
    knn = knn_grid,
    random_forest = random_forest_grid,
    boosted_trees = boosted_tree_grid
  ),
  training_years = training_years,
  testing_years = testing_years
)

saveRDS(model_setup, "data/resamples/model_setup.rds")

split_summary <- bind_rows(
  training_data |>
    summarise(
      set = "Training",
      observations = n(),
      countries = n_distinct(entity),
      first_year = min(year),
      last_year = max(year),
      median_co2_kt = median(co2_emissions_kt),
      mean_log10_co2 = mean(log10_co2_emissions),
      sd_log10_co2 = sd(log10_co2_emissions)
    ),
  testing_data |>
    summarise(
      set = "Testing",
      observations = n(),
      countries = n_distinct(entity),
      first_year = min(year),
      last_year = max(year),
      median_co2_kt = median(co2_emissions_kt),
      mean_log10_co2 = mean(log10_co2_emissions),
      sd_log10_co2 = sd(log10_co2_emissions)
    )
) |>
  mutate(percent = observations / sum(observations) * 100)

readr::write_csv(split_summary, "data/results/split_summary.csv")

recipe_tuning_summary <- tribble(
  ~model, ~dummy_variables, ~center, ~scale, ~tuned, ~tuned_parameters, ~grid_size,
  "Linear Regression", "Yes", "No", "No", "No", "None", 1L,
  "Elastic Net", "Yes", "Yes", "Yes", "Yes", "penalty; mixture", 20L,
  "K-Nearest Neighbors", "Yes", "Yes", "Yes", "Yes", "neighbors", 6L,
  "Random Forest", "No", "No", "No", "Yes", "mtry", 6L,
  "Gradient-Boosted Trees", "Yes", "No", "No", "Yes", "learn_rate; tree_depth", 6L
)

readr::write_csv(recipe_tuning_summary, "data/results/recipe_tuning_summary.csv")

message(
  "Training years: ", min(training_years), "-", max(training_years),
  "; testing years: ", min(testing_years), "-", max(testing_years), "."
)
