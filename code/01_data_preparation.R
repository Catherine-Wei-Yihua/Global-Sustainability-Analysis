source("code/00_setup.R")

raw_data <- readr::read_csv(raw_data_path, show_col_types = FALSE)
clean_names_data <- raw_data |>
  janitor::clean_names()

if ("density_n_p_km2" %in% names(clean_names_data)) {
  clean_names_data <- clean_names_data |>
    rename(population_density = density_n_p_km2)
}

clean_names_data <- clean_names_data |>
  mutate(
    entity = as.character(entity),
    year = as.integer(year),
    population_density = readr::parse_number(as.character(population_density)),
    land_area_km2 = as.numeric(land_area_km2)
  )

duplicate_rows <- sum(duplicated(clean_names_data))
duplicate_entity_years <- clean_names_data |>
  count(entity, year, name = "records") |>
  filter(records > 1) |>
  nrow()

selected_data <- clean_names_data |>
  transmute(
    entity,
    year,
    access_electricity_pct = access_to_electricity_percent_of_population,
    clean_cooking_fuels_pct = access_to_clean_fuels_for_cooking,
    renewable_capacity_per_capita = renewable_electricity_generating_capacity_per_capita,
    gdp_per_capita,
    land_area_km2,
    fossil_electricity_twh = electricity_from_fossil_fuels_t_wh,
    nuclear_electricity_twh = electricity_from_nuclear_t_wh,
    renewable_electricity_twh = electricity_from_renewables_t_wh,
    low_carbon_electricity_pct = low_carbon_electricity_percent_electricity,
    primary_energy_per_capita_kwh = primary_energy_consumption_per_capita_k_wh_person,
    co2_emissions_kt = value_co2_emissions_kt_by_country
  )

missing_summary <- selected_data |>
  summarise(across(everything(), list(
    missing = ~ sum(is.na(.x)),
    percent = ~ mean(is.na(.x)) * 100
  ))) |>
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(missing|percent)$"
  ) |>
  arrange(desc(percent))

readr::write_csv(missing_summary, "data/results/missing_summary.csv")

missing_plot_data <- selected_data |>
  rename(
    Country = entity,
    Year = year,
    `Electricity access` = access_electricity_pct,
    `Clean cooking` = clean_cooking_fuels_pct,
    `Renewable capacity` = renewable_capacity_per_capita,
    `GDP per capita` = gdp_per_capita,
    `Land area` = land_area_km2,
    `Fossil electricity` = fossil_electricity_twh,
    `Nuclear electricity` = nuclear_electricity_twh,
    `Renewable electricity` = renewable_electricity_twh,
    `Low-carbon share` = low_carbon_electricity_pct,
    `Primary energy` = primary_energy_per_capita_kwh,
    `CO2 emissions` = co2_emissions_kt
  ) |>
  relocate(`Primary energy`, .before = `Electricity access`)

missing_plot <- naniar::vis_miss(
  missing_plot_data,
  sort_miss = TRUE,
  show_perc_col = TRUE,
  warn_large_data = FALSE
) +
  guides(x = guide_axis(angle = 45)) +
  labs(
    title = "Missingness in the selected variables",
    subtitle = "Dark cells indicate missing observations",
    x = NULL,
    y = "Observation"
  ) +
  theme_report(11) +
  theme(axis.text.x = element_text(size = 9))

ggsave(
  "graphs/missing_data_pattern.png",
  missing_plot,
  width = 11.5,
  height = 6.2,
  dpi = 220
)

outcome_by_year <- selected_data |>
  group_by(year) |>
  summarise(
    observations = n(),
    outcome_available = sum(!is.na(co2_emissions_kt)),
    outcome_missing = sum(is.na(co2_emissions_kt)),
    .groups = "drop"
  )

readr::write_csv(outcome_by_year, "data/results/outcome_missingness_by_year.csv")

after_outcome_filter <- selected_data |>
  filter(!is.na(co2_emissions_kt))

prepared_data <- after_outcome_filter |>
  drop_na() |>
  distinct() |>
  filter(co2_emissions_kt > 0) |>
  mutate(
    entity = factor(entity),
    log10_co2_emissions = log10(co2_emissions_kt)
  ) |>
  arrange(year, entity)

readr::write_csv(prepared_data, clean_data_path)

preparation_summary <- tibble(
  stage = c(
    "Raw data",
    "Outcome observed",
    "Complete selected variables",
    "Prepared data"
  ),
  observations = c(
    nrow(selected_data),
    nrow(after_outcome_filter),
    sum(complete.cases(after_outcome_filter)),
    nrow(prepared_data)
  )
)

data_quality_checks <- tibble(
  check = c(
    "Exact duplicate rows",
    "Duplicate country-year combinations",
    "Nonpositive observed outcomes"
  ),
  count = c(
    duplicate_rows,
    duplicate_entity_years,
    sum(after_outcome_filter$co2_emissions_kt <= 0, na.rm = TRUE)
  )
)

readr::write_csv(preparation_summary, "data/results/preparation_summary.csv")
readr::write_csv(data_quality_checks, "data/results/data_quality_checks.csv")

codebook <- tribble(
  ~original_variable, ~clean_variable, ~type, ~role, ~description,
  "Entity", "entity", "Categorical", "Predictor and identifier", "Country or region associated with the observation.",
  "Year", "year", "Numeric", "Predictor and time index", "Calendar year associated with the observation, from 2000 through 2020 in the raw data.",
  "Access to electricity (% of population)", "access_electricity_pct", "Numeric", "Predictor", "Percentage of the population with access to electricity.",
  "Access to clean fuels for cooking", "clean_cooking_fuels_pct", "Numeric", "Predictor", "Percentage of the population primarily relying on clean fuels and technologies for cooking.",
  "Renewable-electricity-generating-capacity-per-capita", "renewable_capacity_per_capita", "Numeric", "Predictor", "Installed renewable electricity-generating capacity per person.",
  "Financial flows to developing countries (US $)", "financial_flows_to_developing_countries_us", "Numeric", "Excluded", "International financial flows supporting clean-energy research, development, and production in developing countries.",
  "Renewable energy share in the total final energy consumption (%)", "renewable_energy_share_in_total_consumption_pct", "Numeric", "Excluded", "Renewable energy as a percentage of total final energy consumption.",
  "Electricity from fossil fuels (TWh)", "fossil_electricity_twh", "Numeric", "Predictor", "Electricity generated from coal, oil, and gas, measured in terawatt-hours.",
  "Electricity from nuclear (TWh)", "nuclear_electricity_twh", "Numeric", "Predictor", "Electricity generated from nuclear power, measured in terawatt-hours.",
  "Electricity from renewables (TWh)", "renewable_electricity_twh", "Numeric", "Predictor", "Electricity generated from renewable sources, measured in terawatt-hours.",
  "Low-carbon electricity (% electricity)", "low_carbon_electricity_pct", "Numeric", "Predictor", "Percentage of electricity generated from nuclear and renewable sources.",
  "Primary energy consumption per capita (kWh/person)", "primary_energy_per_capita_kwh", "Numeric", "Predictor", "Primary energy consumption per person, measured in kilowatt-hours per person.",
  "Energy intensity level of primary energy (MJ/$2017 PPP GDP)", "energy_intensity_mj_per_2017_ppp_gdp", "Numeric", "Excluded", "Primary energy use per unit of GDP at 2017 purchasing-power parity.",
  "Value_co2_emissions_kt_by_country", "co2_emissions_kt", "Numeric", "Outcome", "Country-level carbon dioxide emissions, measured in kilotonnes.",
  "Renewables (% equivalent primary energy)", "renewables_equivalent_primary_energy_pct", "Numeric", "Excluded", "Renewable energy as a percentage of equivalent primary energy use.",
  "gdp_growth", "gdp_growth", "Numeric", "Excluded", "Annual percentage growth in gross domestic product.",
  "gdp_per_capita", "gdp_per_capita", "Numeric", "Predictor", "Gross domestic product per person.",
  "Density\\n(P/Km2)", "population_density", "Numeric after parsing", "Excluded", "Population density measured in people per square kilometer.",
  "Land Area(Km2)", "land_area_km2", "Numeric", "Predictor", "Total land area measured in square kilometers.",
  "Latitude", "latitude", "Numeric", "Excluded", "Latitude of the country centroid in decimal degrees.",
  "Longitude", "longitude", "Numeric", "Excluded", "Longitude of the country centroid in decimal degrees."
)

readr::write_csv(codebook, "data/codebook.csv")

message("Prepared data: ", nrow(prepared_data), " rows and ", ncol(prepared_data), " columns.")
