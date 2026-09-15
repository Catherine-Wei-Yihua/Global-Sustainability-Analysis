suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(janitor)
  library(naniar)
  library(patchwork)
})

tidymodels_prefer()

project_seed <- 131
set.seed(project_seed)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("data/resamples", recursive = TRUE, showWarnings = FALSE)
dir.create("data/models", recursive = TRUE, showWarnings = FALSE)
dir.create("data/results", recursive = TRUE, showWarnings = FALSE)
dir.create("graphs", recursive = TRUE, showWarnings = FALSE)

raw_data_path <- "data/raw/sustainable_energy_dataset.csv"
clean_data_path <- "data/processed/sustainability_clean.csv"

outcome_raw <- "co2_emissions_kt"
outcome_model <- "log10_co2_emissions"

model_predictors <- c(
  "entity",
  "year",
  "access_electricity_pct",
  "clean_cooking_fuels_pct",
  "renewable_capacity_per_capita",
  "gdp_per_capita",
  "land_area_km2",
  "fossil_electricity_twh",
  "nuclear_electricity_twh",
  "renewable_electricity_twh",
  "low_carbon_electricity_pct",
  "primary_energy_per_capita_kwh"
)

report_colors <- c(
  navy = "#24435B",
  blue = "#5B7FA3",
  light_blue = "#AFC3D5",
  coral = "#C8796F",
  sand = "#D8B27C",
  gray = "#5E6A71"
)

theme_report <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", color = report_colors[["navy"]]),
      plot.subtitle = element_text(color = report_colors[["gray"]]),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

theme_set(theme_report())

