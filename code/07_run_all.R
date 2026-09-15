scripts <- c(
  "code/01_data_preparation.R",
  "code/02_exploratory_data_analysis.R",
  "code/03_model_setup.R",
  "code/04a_linear_regression.R",
  "code/04b_elastic_net.R",
  "code/04c_knn.R",
  "code/04d_random_forest.R",
  "code/04e_boosted_trees.R",
  "code/05_model_comparison.R",
  "code/06_final_evaluation.R"
)

for (script in scripts) {
  message("Running ", script)
  source(script, local = new.env(parent = globalenv()))
}

message("Complete analysis pipeline finished successfully.")
