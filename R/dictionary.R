# Data dictionary for CSV downloads: 1 row for each column in each file. If a column not listed here
# is added to or removed from a file, check_dictionary() will stop the pipeline.

data_dictionary <- function() {
  ref <- "the last ten surveys (2015 to 2019 and 2022 to 2026)"
  ci <- function(what) paste0(what, ": 2.5th percentile over 300 draws")
  e_short <- "Short-term pond elasticity (this spring's ponds)"
  e_sus <- "Sustained pond elasticity (this spring's ponds plus the carry-over from last spring's)"
  species <- "Species code: MALL Mallard, NOPI Northern Pintail, BWTE Blue-winged Teal, GADW Gadwall, NSHO Northern Shoveler, CANV Canvasback"
  stratum <- "WBPHS stratum number; Prairie Canada is strata 26 to 40"
  year <- "Survey year"
  model <- "Model: main; trend_x2 (trend smooths with twice the basis dimension); ar1_0.2 (AR(1) residuals, rho = 0.2); year_re (random effect for each survey year)"
  med <- function(x) paste0("Median of ", x, " over 300 draws")
  hi <- function(x) paste0(x, ": 97.5th percentile over 300 draws")
  # It is .file, not file: the entry describing the dictionary itself includes a column named file.
  row <- function(.file, ...) {
    x <- c(...)
    tibble::tibble(file = .file, column = names(x), description = unname(x))
  }
  dplyr::bind_rows(
    row("breeding_population_by_stratum.csv",
        stratum = stratum, year = year, species = species,
        estimate = "Breeding population estimate (birds) from the WBPHS file; 0 where the stratum was flown but the species was not recorded",
        se = "Standard error of estimate (birds); 0 where the species was not recorded",
        ponds = "May ponds in the stratum that year",
        fit = "Breeding population fitted by the main model (birds); empty where the previous year's ponds are unknown",
        lo = "Lower end of the 95% confidence interval on fit",
        hi = "Upper end of the 95% confidence interval on fit"),
    row("may_ponds_by_stratum.csv",
        stratum = stratum, year = year,
        ponds = "May pond estimate",
        ponds_se = "Standard error of the May pond estimate",
        log_ponds_c = "Natural log of ponds, centred on the stratum's mean log ponds over all years",
        log_ponds_lag_c = "log_ponds_c for the previous year; empty when the previous year was not surveyed"),
    row("prairie_canada_totals.csv",
        species = species, year = year,
        prairie_total = "Breeding population summed over the Prairie Canada strata flown that year",
        n_strata = "Prairie Canada strata flown (14 in 2002 and 2023, when stratum 36 was not flown)",
        tsa_total = "Breeding population summed over every stratum of the traditional survey area",
        prairie_ponds = "May ponds summed over the Prairie Canada strata flown that year",
        prairie_share = "prairie_total divided by tsa_total"),
    row("pond_elasticity_by_year.csv",
        species = species, year = year,
        lo = ci("Prairie-average short-term pond elasticity, main model"),
        med = med("the prairie-average short-term pond elasticity"),
        hi = hi("Prairie-average short-term pond elasticity")),
    row("pond_elasticity_recent.csv",
        species = species,
        e_lo = ci(paste(e_short, "averaged over the strata and", ref)),
        e_med = med("the short-term elasticity"), e_hi = hi("Short-term elasticity"),
        es_lo = ci(paste(e_sus, "averaged over the strata and", ref)),
        es_med = med("the sustained elasticity"), es_hi = hi("Sustained elasticity")),
    row("pond_elasticity_by_stratum.csv",
        species = species, stratum = stratum,
        bpop_ref = paste("Mean breeding population over", ref),
        ponds_ref = paste("Mean May ponds over", ref),
        e_lo = ci(paste(e_short, "for the stratum, averaged over", ref)),
        e_med = med("the short-term elasticity"), e_hi = hi("Short-term elasticity"),
        es_lo = ci(paste(e_sus, "for the stratum, averaged over", ref)),
        es_med = med("the sustained elasticity"), es_hi = hi("Sustained elasticity"),
        per_pond_lo = ci("Breeding birds lost per May pond lost at the margin, es x bpop_ref / ponds_ref"),
        per_pond_med = med("birds lost per pond"), per_pond_hi = hi("Birds lost per pond")),
    row("model_diagnostics.csv",
        species = species,
        n = "Stratum-years in the fit",
        dev_expl = "Proportion of deviance explained",
        r_sq = "Adjusted R-squared",
        tweedie_p = "Estimated Tweedie power parameter",
        lag_coef = "Coefficient on last year's log ponds (log_ponds_lag_c)",
        lag_se = "Standard error of lag_coef",
        resid_acf1_mean = "Lag-1 autocorrelation of Pearson residuals within each stratum, averaged over strata",
        resid_acf1_max = "Largest absolute lag-1 residual autocorrelation in any one stratum",
        k_index_min = "Smallest k-index across smooth terms, from mgcv::k.check()",
        edf_elasticity = "Effective degrees of freedom of the time-varying pond effect, s(year, by = log_ponds_c)",
        p_elasticity = "Approximate p-value of the time-varying pond effect"),
    row("sensitivity_checks.csv",
        species = species, model = model,
        es_lo = ci(paste(e_sus, "averaged over the strata and", ref)),
        es_med = med("the sustained elasticity"), es_hi = hi("Sustained elasticity"),
        k_index_min = "As in model_diagnostics.csv; empty for ar1_0.2",
        dev_expl = "As in model_diagnostics.csv; empty for ar1_0.2"),
    row("sensitivity_elasticity_by_year.csv",
        model = model, species = species, year = year,
        lo = ci("Prairie-average short-term pond elasticity"),
        med = med("the prairie-average short-term pond elasticity"),
        hi = hi("Prairie-average short-term pond elasticity")),
    row("elasticity_shift.csv",
        model = "Model: main or year_re (see sensitivity_checks.csv)", species = species,
        early_from = "First year of the early window (the first ten surveys with a modelled elasticity)",
        early_to = "Last year of the early window",
        early_med = "Median prairie-average short-term elasticity over the early window",
        late_med = paste("Median prairie-average short-term elasticity over", ref),
        shift_lo = ci("Late minus early elasticity, computed within each draw"),
        shift_med = med("the shift"), shift_hi = hi("Shift"),
        p_increase = "Share of draws in which the late elasticity is higher"),
    row("prairie_share_response.csv",
        species = species,
        era = "early or late",
        from = "First year of the era", to = "Last year of the era",
        n = "Survey years in the era",
        share_mean = "Mean prairie_share over the era",
        slope = "Least-squares slope of logit(prairie_share) on log(prairie_ponds)",
        slope_se = "Standard error of the slope, treating years as independent (so too small)"),
    row("scenario_parity_check.csv",
        scenario = "Test scenario number",
        species = "Species included, joined with +",
        strata = "Strata included",
        loss = "Proportion of May ponds lost",
        sustained = "TRUE if the sustained elasticity is used",
        stat = "lo, med, or hi: 5th, 50th, or 95th percentile over draws of the change in breeding birds",
        r = "Change in breeding birds computed in R",
        js = "The same computed by the site's JavaScript, run in V8",
        rel_diff = "Relative difference between r and js"),
    row("published_totals_check.csv",
        year = year,
        quantity = "What is compared",
        ours = "Total rebuilt from the stratum file by this pipeline",
        published_millions = "Figure published in the report, in millions",
        source = "USFWS report the figure comes from",
        match = "TRUE if ours rounds to the published figure"),
    row("data_dictionary.csv",
        file = "CSV file name", column = "Column name", description = "What the column holds")
  )
}

check_dictionary <- function(dictionary, paths) {
  own <- dictionary$column[dictionary$file == "data_dictionary.csv"]
  if (!setequal(own, names(dictionary))) {
    stop("data_dictionary.csv: its own entry does not match its columns", call. = FALSE)
  }
  for (p in paths) {
    have <- names(readr::read_csv(p, n_max = 0, show_col_types = FALSE))
    documented <- dictionary$column[dictionary$file == basename(p)]
    if (!setequal(have, documented)) {
      stop(basename(p), ": undocumented [", paste(setdiff(have, documented), collapse = ", "),
           "], missing [", paste(setdiff(documented, have), collapse = ", "), "]", call. = FALSE)
    }
  }
  dictionary
}
