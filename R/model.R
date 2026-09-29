# Hierarchical GAMs of breeding population on May ponds, one per species.
#
# log E[BPOP_st] = f(year) + f_s(year)                  global + stratum trends
#                  + beta(year) * x_st + b_s * x_st      time-varying pond elasticity,
#                                                        with stratum deviations
#                  + gamma * x_s,t-1                     last year's ponds
# where x is log May ponds centred within stratum. The year smooths absorb
# long-term change, so the pond terms are identified from year-to-year swings.
# Tweedie errors handle the handful of zero counts without a log(0) fudge.

# k_trend and k_stratum control the flexibility of the trend smooth. When rho > 0, AR(1) residuals
# are included within consecutive annual sequences in each stratum. When year_re = TRUE, a random
# effect is included for each survey year to absorb spring shocks shared across all strata that
# year. The main model uses the default values, while the pipeline uses other values to refit the
# model for sensitivity analysis.
fit_species_model <- function(model_data, sp, k_trend = 20, k_stratum = 10, rho = 0, year_re = FALSE) {
  d <- model_data |>
    dplyr::filter(species == sp, !is.na(log_ponds_lag_c)) |>
    dplyr::arrange(stratum, year) |>
    dplyr::group_by(stratum) |>
    dplyr::mutate(ar_start = dplyr::row_number() == 1 | year - dplyr::lag(year) != 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(year_f = factor(year))
  f <- stats::as.formula(paste(
    sprintf("estimate ~ s(year, k = %d) + s(year, stratum_f, bs = 'fs', k = %d, m = 2) +
       s(year, by = log_ponds_c, k = 8) + s(stratum_f, log_ponds_c, bs = 're') + log_ponds_lag_c",
            k_trend, k_stratum),
    if (year_re) "+ s(year_f, bs = 're')" else ""))
  m <- mgcv::bam(
    f, family = mgcv::tw(), data = d, weights = w,
    method = "fREML", discrete = TRUE,
    rho = rho, AR.start = if (rho > 0) d$ar_start else NULL
  )
  # Elasticities are reported for all years with a survey during the period covered by the model
  # fit, including 2022, when a survey was conducted but the previous year's pond data required for
  # modelling were missing.
  yrs <- sort(unique(model_data$year))
  list(species = sp, model = m, data = d, years = yrs[yrs >= min(d$year) & yrs <= max(d$year)])
}

# Approximate posterior draws of pond elasticity for each stratum-year: coefficient draws from the
# Bayesian posterior approximation are used to evaluate the change in the linear predictor for a
# one-unit change in log ponds. Each species gets a separate seed, keeping draws independent across
# species and preventing a multi-species total from being assigned a correlation the models never
# estimated. Sensitivity refits reuse each species' random seed to keep model comparisons like for
# like.
elasticity_draws <- function(fit, n_draws = 300, seed = 1000 + match(fit$species, species_info()$species)) {
  m <- fit$model
  d <- fit$data
  lv <- levels(d$stratum_f)
  # year_f is included only in the year-effect refit, and cancels out in the difference below.
  nd <- tidyr::expand_grid(
    stratum_f = factor(lv, levels = lv),
    year = fit$years
  ) |>
    dplyr::mutate(log_ponds_lag_c = 0, year_f = factor(levels(d$year_f)[1], levels = levels(d$year_f)))
  X1 <- mgcv::predict.bam(m, dplyr::mutate(nd, log_ponds_c = 1), type = "lpmatrix")
  X0 <- mgcv::predict.bam(m, dplyr::mutate(nd, log_ponds_c = 0), type = "lpmatrix")
  set.seed(seed)
  B <- mgcv::rmvn(n_draws, stats::coef(m), stats::vcov(m, unconditional = TRUE))
  list(
    species = fit$species,
    grid = dplyr::mutate(nd, stratum = as.integer(as.character(stratum_f))),
    E = (X1 - X0) %*% t(B),                      # rows = stratum and year combinations, columns = draws
    lag = B[, which(names(stats::coef(m)) == "log_ponds_lag_c")]
  )
}

# Fitted breeding population values (response scale) and 95% confidence intervals for each modelled
# stratum–year combination.
fitted_series <- function(fit) {
  p <- mgcv::predict.bam(fit$model, fit$data, type = "link", se.fit = TRUE)
  inv <- fit$model$family$linkinv
  fit$data |>
    dplyr::transmute(
      species, stratum, year,
      fit = inv(p$fit),
      lo = inv(p$fit - 1.96 * p$se.fit),
      hi = inv(p$fit + 1.96 * p$se.fit)
    )
}

model_diagnostics <- function(fit) {
  m <- fit$model
  s <- summary(m)
  r <- stats::residuals(m, type = "pearson")
  acf1 <- vapply(split(r, fit$data$stratum_f),
                 function(x) stats::acf(x, plot = FALSE, lag.max = 1)$acf[2], numeric(1))
  kc <- mgcv::k.check(m)
  tibble::tibble(
    species = fit$species,
    n = nrow(fit$data),
    dev_expl = s$dev.expl,
    r_sq = s$r.sq,
    tweedie_p = m$family$getTheta(TRUE),
    lag_coef = unname(stats::coef(m)["log_ponds_lag_c"]),
    lag_se = unname(sqrt(diag(stats::vcov(m)))["log_ponds_lag_c"]),
    resid_acf1_mean = mean(acf1),
    resid_acf1_max = max(abs(acf1)),
    k_index_min = min(kc[, "k-index"], na.rm = TRUE),
    edf_elasticity = s$s.table["s(year):log_ponds_c", "edf"],
    p_elasticity = s$s.table["s(year):log_ponds_c", "p-value"]
  )
}
