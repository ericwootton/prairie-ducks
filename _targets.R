# Reproducible pipeline: raw WBPHS files -> tidy data -> models -> data and figures for the page.
# Run with targets::tar_make(); inspect with targets::tar_visnetwork().

library(targets)

tar_option_set(packages = c("dplyr", "tidyr", "readr", "purrr", "sf", "mgcv", "jsonlite", "V8", "ggplot2"))
tar_source("R")

list(
  # ---- Raw data (downloaded once, then tracked by hash) ----
  tar_target(estimates_file, {
    fetch_file(source_urls[["estimates"]], "data-raw/WBPHS_Traditional_Area_Stratum_Estimates.zip",
               unzip_to = "data-raw/stratum_estimates")
    "data-raw/stratum_estimates/WBPHS_Traditional_Area_Stratum_Estimates/wbphs_traditionalarea_estimates_forDistribution.csv"
  }, format = "file"),
  tar_target(strata_file, {
    fetch_file(source_urls[["strata"]], "data-raw/WBPHS_Stratum_Boundaries.zip", unzip_to = "data-raw/strata")
    "data-raw/strata/WBPHS_Stratum_Boundaries.shp"
  }, format = "file"),
  tar_target(provinces_file, fetch_file(source_urls[["provinces"]], "data-raw/basemap/ne_50m_admin_1_states_provinces.geojson"), format = "file"),
  tar_target(lakes_file, fetch_file(source_urls[["lakes"]], "data-raw/basemap/ne_50m_lakes.geojson"), format = "file"),

  # ---- Tidy data ----
  tar_target(estimates, read_estimates(estimates_file)),
  tar_target(species, species_info()),
  tar_target(bpop, complete_bpop(estimates)),
  tar_target(ponds, tidy_ponds(estimates)),
  tar_target(model_data, build_model_data(bpop, ponds)),
  tar_target(strata_sf, read_strata(strata_file)),
  tar_target(basemap, read_basemap(provinces_file, lakes_file, strata_sf)),
  tar_target(locator, read_locator(provinces_file)),

  # ---- Models (one per species) ----
  tar_target(species_codes, species$species),
  tar_target(fit, fit_species_model(model_data, species_codes), pattern = map(species_codes), iteration = "list"),
  tar_target(draws, elasticity_draws(fit), pattern = map(fit), iteration = "list"),
  tar_target(fitted, fitted_series(fit), pattern = map(fit)),
  tar_target(diagnostics, model_diagnostics(fit), pattern = map(fit)),

  # ---- Sensitivity refits: trends with doubled flexibility; AR(1) residuals; shared year effects ----
  tar_target(fit_trend, fit_species_model(model_data, species_codes, k_trend = 40, k_stratum = 20),
             pattern = map(species_codes), iteration = "list"),
  tar_target(fit_ar1, fit_species_model(model_data, species_codes, rho = 0.2),
             pattern = map(species_codes), iteration = "list"),
  tar_target(fit_year, fit_species_model(model_data, species_codes, year_re = TRUE),
             pattern = map(species_codes), iteration = "list"),
  tar_target(diagnostics_trend, model_diagnostics(fit_trend), pattern = map(fit_trend)),
  tar_target(diagnostics_year, model_diagnostics(fit_year), pattern = map(fit_year)),
  tar_target(recent_year, recent_elasticity(elasticity_draws(fit_year), ref_years), pattern = map(fit_year)),
  tar_target(curves_year, elasticity_curve(elasticity_draws(fit_year)), pattern = map(fit_year)),
  tar_target(shift_year, elasticity_shift(elasticity_draws(fit_year), ref_years), pattern = map(fit_year)),
  tar_target(recent_trend, recent_elasticity(elasticity_draws(fit_trend), ref_years), pattern = map(fit_trend)),
  tar_target(recent_ar1, recent_elasticity(elasticity_draws(fit_ar1), ref_years), pattern = map(fit_ar1)),
  tar_target(curves_trend, elasticity_curve(elasticity_draws(fit_trend)), pattern = map(fit_trend)),
  tar_target(curves_ar1, elasticity_curve(elasticity_draws(fit_ar1)), pattern = map(fit_ar1)),
  tar_target(sensitivity, sensitivity_table(recent, recent_trend, recent_ar1, recent_year,
                                            diagnostics, diagnostics_trend, diagnostics_year)),

  # ---- Summaries ----
  tar_target(ref_years, reference_years(ponds$year)),
  tar_target(ref_levels, reference_levels(bpop, ponds, ref_years)),
  tar_target(curves, elasticity_curve(draws), pattern = map(draws)),
  tar_target(recent, recent_elasticity(draws, ref_years), pattern = map(draws)),
  tar_target(shift, elasticity_shift(draws, ref_years), pattern = map(draws)),
  tar_target(stratum_table, stratum_summary(draws, ref_levels, ref_years), pattern = map(draws)),
  tar_target(payload, scenario_payload(draws, ref_levels, ref_years)),
  tar_target(totals, prairie_totals(estimates, bpop, ponds)),
  tar_target(series, stratum_series(bpop, ponds, fitted)),
  tar_target(headline, headline_numbers(totals, payload, ref_years)),
  tar_target(overflight, share_response(totals)),

  # ---- Data for the webpage ----
  tar_target(site_geo, export_geo(dplyr::filter(strata_sf, prairie | stratum %in% 41:49), basemap, locator, "data/site/geo.json"), format = "file"),
  tar_target(site_curves, write_json_file(round_df(curves), "data/site/elasticity_curves.json"), format = "file"),
  tar_target(site_strata, write_json_file(
    round_df(dplyr::left_join(stratum_table, sf::st_drop_geometry(dplyr::select(strata_sf, stratum, province, area_km2)), by = "stratum")),
    "data/site/strata.json"), format = "file"),
  tar_target(site_totals, write_json_file(round_df(totals), "data/site/totals.json"), format = "file"),
  tar_target(site_recent, write_json_file(round_df(recent), "data/site/recent_elasticity.json"), format = "file"),
  tar_target(site_scenario, write_json_file(payload, "data/site/scenario.json"), format = "file"),
  tar_target(site_headline, write_json_file(headline, "data/site/headline.json", digits = 4), format = "file"),

  # ---- Figures for the Methods PDF ----
  tar_target(fig_study, fig_study_area(strata_sf, basemap)),
  tar_target(fig_trends, fig_ponds_birds(totals, species)),
  tar_target(fig_curves, fig_elasticity_curves(curves, curves_trend, curves_ar1, curves_year, recent, species)),
  tar_target(fig_strata, fig_stratum_elasticity(strata_sf, stratum_table, recent, species)),
  tar_target(fig_perpond, fig_per_pond(totals, species)),

  # ---- Checks: scenario arithmetic in the browser must match R's ----
  tar_target(viz_js, "js/viz.js", format = "file"),
  tar_target(d3_js, "tests/vendor/d3.v7.min.js", format = "file"),
  tar_target(scenario_parity, check_scenario_parity(site_scenario, viz_js, d3_js)),
  tar_target(csv_parity, write_csv_file(scenario_parity, "data/csv/scenario_parity_check.csv"), format = "file"),
  tar_target(published_check, check_published_totals(totals)),
  tar_target(csv_published, write_csv_file(published_check, "data/csv/published_totals_check.csv"), format = "file"),

  # ---- CSV downloads linked from the page ----
  tar_target(csv_series, write_csv_file(series, "data/csv/breeding_population_by_stratum.csv"), format = "file"),
  tar_target(csv_ponds, write_csv_file(ponds, "data/csv/may_ponds_by_stratum.csv"), format = "file"),
  tar_target(csv_curves, write_csv_file(curves, "data/csv/pond_elasticity_by_year.csv"), format = "file"),
  tar_target(csv_strata, write_csv_file(stratum_table, "data/csv/pond_elasticity_by_stratum.csv"), format = "file"),
  tar_target(csv_recent, write_csv_file(recent, "data/csv/pond_elasticity_recent.csv"), format = "file"),
  tar_target(csv_totals, write_csv_file(totals, "data/csv/prairie_canada_totals.csv"), format = "file"),
  tar_target(csv_diagnostics, write_csv_file(diagnostics, "data/csv/model_diagnostics.csv"), format = "file"),
  tar_target(csv_shift, write_csv_file(dplyr::bind_rows(main = shift, year_re = shift_year, .id = "model"),
                                       "data/csv/elasticity_shift.csv"), format = "file"),
  tar_target(csv_sensitivity, write_csv_file(sensitivity, "data/csv/sensitivity_checks.csv"), format = "file"),
  tar_target(csv_overflight, write_csv_file(overflight, "data/csv/prairie_share_response.csv"), format = "file"),
  tar_target(csv_curves_sens, write_csv_file(
    dplyr::bind_rows(main = curves, trend_x2 = curves_trend, ar1_0.2 = curves_ar1, year_re = curves_year, .id = "model"),
    "data/csv/sensitivity_elasticity_by_year.csv"), format = "file"),
  tar_target(dictionary, check_dictionary(data_dictionary(), c(
    csv_series, csv_ponds, csv_totals, csv_curves, csv_recent, csv_strata, csv_diagnostics,
    csv_sensitivity, csv_curves_sens, csv_shift, csv_overflight, csv_parity, csv_published))),
  tar_target(csv_dictionary, write_csv_file(dictionary, "data/csv/data_dictionary.csv"), format = "file")
)
