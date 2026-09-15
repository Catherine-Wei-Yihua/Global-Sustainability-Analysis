source("code/00_setup.R")

eda_data <- readr::read_csv(clean_data_path, show_col_types = FALSE) |>
  mutate(entity = factor(entity))

outcome_original <- ggplot(eda_data, aes(x = co2_emissions_kt)) +
  geom_histogram(bins = 45, fill = report_colors[["blue"]], color = "white") +
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(
    title = "Original scale",
    x = "CO2 emissions (kilotonnes)",
    y = "Country-year observations"
  )

outcome_log <- ggplot(eda_data, aes(x = log10_co2_emissions)) +
  geom_histogram(bins = 35, fill = report_colors[["coral"]], color = "white") +
  labs(
    title = "Logarithmic scale",
    x = "log10(CO2 emissions in kilotonnes)",
    y = "Country-year observations"
  )

outcome_distribution_plot <- outcome_original + outcome_log +
  plot_annotation(
    title = "Distribution of country-level CO2 emissions",
    subtitle = "A logarithmic transformation reduces the influence of extreme emitters"
  )

ggsave(
  "graphs/co2_distribution.png",
  outcome_distribution_plot,
  width = 10,
  height = 4.8,
  dpi = 220
)

annual_summary <- eda_data |>
  group_by(year) |>
  summarise(
    median_co2 = median(co2_emissions_kt),
    countries = n(),
    .groups = "drop"
  )

time_plot <- ggplot(annual_summary, aes(x = year, y = median_co2)) +
  geom_line(color = report_colors[["navy"]], linewidth = 1) +
  geom_point(color = report_colors[["navy"]], size = 2) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_x_continuous(breaks = seq(min(annual_summary$year), max(annual_summary$year), by = 2)) +
  labs(
    title = "Median country-level CO2 emissions increased over time",
    subtitle = "Each point shows the median across countries in that year",
    x = "Year",
    y = "CO2 emissions (kilotonnes, log scale)"
  )

ggsave("graphs/co2_over_time.png", time_plot, width = 9, height = 5.2, dpi = 220)

numeric_predictors <- setdiff(model_predictors, "entity")

correlation_with_outcome <- map_dfr(numeric_predictors, function(variable_name) {
  tibble(
    variable = variable_name,
    pearson_with_log10_co2 = cor(
      eda_data[[variable_name]],
      eda_data$log10_co2_emissions,
      method = "pearson"
    ),
    spearman_with_log10_co2 = cor(
      eda_data[[variable_name]],
      eda_data$log10_co2_emissions,
      method = "spearman"
    )
  )
}) |>
  arrange(desc(abs(spearman_with_log10_co2)))

readr::write_csv(correlation_with_outcome, "data/results/correlation_with_outcome.csv")

relationship_variables <- correlation_with_outcome |>
  filter(variable != "year") |>
  slice_head(n = 3) |>
  pull(variable)

variable_labels <- c(
  year = "Year",
  access_electricity_pct = "Electricity access (%)",
  clean_cooking_fuels_pct = "Clean cooking fuels (%)",
  renewable_capacity_per_capita = "Renewable capacity per capita",
  gdp_per_capita = "GDP per capita",
  land_area_km2 = "Land area (km2)",
  fossil_electricity_twh = "Fossil electricity (TWh)",
  nuclear_electricity_twh = "Nuclear electricity (TWh)",
  renewable_electricity_twh = "Renewable electricity (TWh)",
  low_carbon_electricity_pct = "Low-carbon electricity (%)",
  primary_energy_per_capita_kwh = "Primary energy per capita (kWh)",
  log10_co2_emissions = "log10 CO2 emissions"
)

relationship_data <- eda_data |>
  select(log10_co2_emissions, all_of(relationship_variables)) |>
  pivot_longer(
    cols = all_of(relationship_variables),
    names_to = "predictor",
    values_to = "value"
  ) |>
  mutate(predictor = factor(
    predictor,
    levels = relationship_variables,
    labels = unname(variable_labels[relationship_variables])
  ))

relationship_plot <- ggplot(
  relationship_data,
  aes(x = value, y = log10_co2_emissions)
) +
  geom_point(alpha = 0.22, size = 1.1, color = report_colors[["blue"]]) +
  geom_smooth(method = "loess", se = FALSE, color = report_colors[["coral"]], linewidth = 0.9) +
  facet_wrap(vars(predictor), scales = "free_x", ncol = 3) +
  labs(
    title = "CO2 emissions and the strongest numeric predictors",
    subtitle = "Predictors are selected by absolute Spearman correlation with log10 emissions",
    x = NULL,
    y = "log10(CO2 emissions in kilotonnes)"
  )

ggsave(
  "graphs/emissions_predictor_relationships.png",
  relationship_plot,
  width = 11,
  height = 4.4,
  dpi = 220
)

distribution_variables <- c(
  "gdp_per_capita",
  "renewable_capacity_per_capita",
  "primary_energy_per_capita_kwh"
)

distribution_data <- eda_data |>
  select(all_of(distribution_variables)) |>
  pivot_longer(everything(), names_to = "predictor", values_to = "value") |>
  mutate(
    transformed_value = log10(value + 1),
    predictor = factor(
      predictor,
      levels = distribution_variables,
      labels = unname(variable_labels[distribution_variables])
    )
  )

predictor_distribution_plot <- ggplot(distribution_data, aes(x = transformed_value)) +
  geom_histogram(bins = 35, fill = report_colors[["sand"]], color = "white") +
  facet_wrap(vars(predictor), scales = "free", ncol = 3) +
  labs(
    title = "Selected predictors have uneven distributions",
    subtitle = "Each horizontal axis shows log10(x + 1)",
    x = NULL,
    y = "Country-year observations"
  )

ggsave(
  "graphs/selected_predictor_distributions.png",
  predictor_distribution_plot,
  width = 10.5,
  height = 4.2,
  dpi = 220
)

correlation_variables <- c(setdiff(model_predictors, "entity"), outcome_model)
correlation_matrix <- cor(
  eda_data[correlation_variables],
  method = "spearman",
  use = "complete.obs"
)

correlation_long <- as.data.frame(as.table(correlation_matrix)) |>
  as_tibble() |>
  rename(variable_1 = Var1, variable_2 = Var2, correlation = Freq) |>
  mutate(
    variable_1 = factor(variable_1, levels = rev(correlation_variables)),
    variable_2 = factor(variable_2, levels = correlation_variables),
    correlation_label = if_else(
      abs(correlation) > 0.995,
      "1",
      sprintf("%.2f", correlation)
    )
  )

short_labels <- c(
  year = "Year",
  access_electricity_pct = "Electricity access",
  clean_cooking_fuels_pct = "Clean cooking",
  renewable_capacity_per_capita = "Renewable capacity",
  gdp_per_capita = "GDP per capita",
  land_area_km2 = "Land area",
  fossil_electricity_twh = "Fossil electricity",
  nuclear_electricity_twh = "Nuclear electricity",
  renewable_electricity_twh = "Renewable electricity",
  low_carbon_electricity_pct = "Low-carbon share",
  primary_energy_per_capita_kwh = "Primary energy/capita",
  log10_co2_emissions = "CO2 emissions"
)

correlation_plot <- ggplot(correlation_long, aes(x = variable_2, y = variable_1)) +
  geom_tile(fill = "white", color = "#D2D2D2", linewidth = 0.45) +
  geom_point(
    aes(size = abs(correlation), fill = correlation),
    shape = 21,
    color = "#6F6F6F",
    stroke = 0.25
  ) +
  geom_text(aes(label = correlation_label), color = "black", size = 2.8) +
  scale_size_area(max_size = 14, limits = c(0, 1), guide = "none") +
  scale_fill_gradient2(
    low = "#C96A5A",
    mid = "white",
    high = "#2B6087",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Spearman\ncorrelation"
  ) +
  scale_x_discrete(labels = short_labels, position = "bottom") +
  scale_y_discrete(labels = short_labels) +
  coord_equal(clip = "off") +
  labs(
    title = "Correlation among numeric modeling variables",
    subtitle = "Circle size and color show the strength and direction of each correlation",
    x = NULL,
    y = NULL
  ) +
  theme_report(10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    plot.margin = margin(10, 20, 20, 10)
  )

ggsave(
  "graphs/numeric_correlation_heatmap.png",
  correlation_plot,
  width = 10.8,
  height = 9.2,
  dpi = 220
)

outcome_summary <- eda_data |>
  summarise(
    observations = n(),
    countries = n_distinct(entity),
    first_year = min(year),
    last_year = max(year),
    minimum_co2_kt = min(co2_emissions_kt),
    median_co2_kt = median(co2_emissions_kt),
    mean_co2_kt = mean(co2_emissions_kt),
    maximum_co2_kt = max(co2_emissions_kt),
    raw_mean_to_median = mean(co2_emissions_kt) / median(co2_emissions_kt),
    log10_mean = mean(log10_co2_emissions),
    log10_sd = sd(log10_co2_emissions)
  )

readr::write_csv(annual_summary, "data/results/annual_emissions_summary.csv")
readr::write_csv(outcome_summary, "data/results/outcome_summary.csv")
readr::write_csv(
  tibble(variable = relationship_variables),
  "data/results/eda_relationship_variables.csv"
)

message("EDA figures and summaries saved.")
