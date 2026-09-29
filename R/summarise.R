# Convert model draws to quantities displayed on the website.

# Most recent `n` survey years (excluding the COVID-related survey suspension in 2020-21).
reference_years <- function(years, n = 10) {
  utils::head(sort(unique(years), decreasing = TRUE), n)
}

qsum <- function(x, level = 0.95) {
  a <- (1 - level) / 2
  q <- stats::quantile(x, c(a, 0.5, 1 - a), names = FALSE)
  c(lo = q[1], med = q[2], hi = q[3])
}

# Annual prairie mean elasticity: calculate the mean across strata within each draw.
elasticity_curve <- function(draws) {
  g <- draws$grid
  purrr::map_dfr(split(seq_len(nrow(g)), g$year), function(ix) {
    s <- qsum(colMeans(draws$E[ix, , drop = FALSE]))
    tibble::tibble(year = g$year[ix[1]], lo = s[["lo"]], med = s[["med"]], hi = s[["hi"]])
  }) |>
    dplyr::mutate(species = draws$species, .before = 1)
}

# Mean prairie elasticity over the reference years (short-term elasticity and sustained elasticity).
recent_elasticity <- function(draws, ref_years) {
  g <- draws$grid
  e <- colMeans(draws$E[g$year %in% ref_years, , drop = FALSE])
  s <- qsum(e)
  ss <- qsum(e + draws$lag)
  tibble::tibble(
    species = draws$species,
    e_lo = s[["lo"]], e_med = s[["med"]], e_hi = s[["hi"]],
    es_lo = ss[["lo"]], es_med = ss[["med"]], es_hi = ss[["hi"]]
  )
}

# Represent stratum elasticities averaged over the reference years as a strata x draws matrix.
stratum_elasticity_matrix <- function(draws, ref_years) {
  g <- draws$grid
  keep <- g$year %in% ref_years
  idx <- split(which(keep), g$stratum[keep])
  M <- t(vapply(idx, function(ix) colMeans(draws$E[ix, , drop = FALSE]), numeric(ncol(draws$E))))
  rownames(M) <- names(idx)
  M
}

reference_levels <- function(bpop, ponds, ref_years) {
  bpop |>
    dplyr::filter(year %in% ref_years) |>
    dplyr::group_by(species, stratum) |>
    dplyr::summarise(bpop_ref = mean(estimate), .groups = "drop") |>
    dplyr::left_join(
      ponds |>
        dplyr::filter(year %in% ref_years) |>
        dplyr::group_by(stratum) |>
        dplyr::summarise(ponds_ref = mean(ponds), .groups = "drop"),
      by = "stratum"
    )
}

# Table by stratum: short-term elasticity, sustained elasticity, and the number of breeding birds
# per May pond.
stratum_summary <- function(draws, ref_levels, ref_years) {
  M <- stratum_elasticity_matrix(draws, ref_years)
  sustained <- sweep(M, 2, draws$lag, `+`)
  rl <- dplyr::filter(ref_levels, species == draws$species)
  purrr::map_dfr(rownames(M), function(s) {
    r <- rl[rl$stratum == as.integer(s), ]
    e <- qsum(M[s, ])
    es <- qsum(sustained[s, ])
    dpp <- qsum(sustained[s, ] * r$bpop_ref / r$ponds_ref)
    tibble::tibble(
      species = draws$species, stratum = as.integer(s),
      bpop_ref = r$bpop_ref, ponds_ref = r$ponds_ref,
      e_lo = e[["lo"]], e_med = e[["med"]], e_hi = e[["hi"]],
      es_lo = es[["lo"]], es_med = es[["med"]], es_hi = es[["hi"]],
      per_pond_lo = dpp[["lo"]], per_pond_med = dpp[["med"]], per_pond_hi = dpp[["hi"]]
    )
  })
}

# All inputs needed by the browser-based scenario tool, per species, are lag draws together with
# reference levels and elasticity draws for each stratum.
scenario_payload <- function(draws_list, ref_levels, ref_years, digits = 3) {
  out <- lapply(draws_list, function(dr) {
    M <- stratum_elasticity_matrix(dr, ref_years)
    rl <- dplyr::filter(ref_levels, species == dr$species)
    strata <- lapply(rownames(M), function(s) {
      r <- rl[rl$stratum == as.integer(s), ]
      list(bpop_ref = round(r$bpop_ref), ponds_ref = round(r$ponds_ref), e = round(M[s, ], digits))
    })
    names(strata) <- rownames(M)
    list(lag = round(dr$lag, digits), strata = strata)
  })
  names(out) <- vapply(draws_list, `[[`, "", "species")
  out
}

# Same arithmetic as in the browser (runScenario() in js/viz.js): change in breeding birds summed
# across all selected species and strata within each of the draws.
scenario_draws <- function(payload, species, strata, loss, sustained = TRUE) {
  tot <- 0
  for (sp in species) {
    p <- payload[[sp]]
    for (s in as.character(strata)) {
      st <- p$strata[[s]]
      if (is.null(st)) next
      e <- st$e + if (sustained) p$lag else 0
      tot <- tot + st$bpop_ref * ((1 - loss)^e - 1)
    }
  }
  tot
}

scenario_change <- function(payload, species, strata, loss, sustained = TRUE, level = 0.9) {
  qsum(scenario_draws(payload, species, strata, loss, sustained), level)
}

# Total counts observed in Prairie Canada and the proportion of the Traditional Survey Area
# population settling in Prairie Canada (an overflight index).
prairie_totals <- function(est, bpop, ponds, species = species_info()$species) {
  tsa <- est |>
    dplyr::filter(species %in% !!species) |>
    dplyr::group_by(species, year) |>
    dplyr::summarise(tsa_total = sum(estimate), .groups = "drop")
  bpop |>
    dplyr::group_by(species, year) |>
    dplyr::summarise(prairie_total = sum(estimate), n_strata = dplyr::n(), .groups = "drop") |>
    dplyr::left_join(tsa, by = c("species", "year")) |>
    dplyr::left_join(
      ponds |>
        dplyr::group_by(year) |>
        dplyr::summarise(prairie_ponds = sum(ponds), .groups = "drop"),
      by = "year"
    ) |>
    dplyr::mutate(prairie_share = prairie_total / tsa_total)
}

stratum_series <- function(bpop, ponds, fitted) {
  bpop |>
    dplyr::left_join(dplyr::select(ponds, stratum, year, ponds), by = c("stratum", "year")) |>
    dplyr::left_join(fitted, by = c("species", "stratum", "year")) |>
    dplyr::arrange(species, stratum, year)
}

# Key values and their respective uncertainties to be presented on the story page.
headline_numbers <- function(totals, payload, ref_years, loss = 0.10,
                             dabblers = c("MALL", "NOPI", "BWTE", "GADW", "NSHO")) {
  ponds <- dplyr::distinct(totals, year, prairie_ponds)
  latest <- max(ponds$year)
  lta <- mean(ponds$prairie_ponds[ponds$year < latest])

  strata <- names(payload[[dabblers[1]]]$strata)
  n_draws <- length(payload[[dabblers[1]]]$lag)
  change <- scenario_draws(payload, dabblers, strata, loss, sustained = TRUE)
  per_pond <- numeric(n_draws)
  ponds_ref <- sum(vapply(strata, function(s) payload[[dabblers[1]]]$strata[[s]]$ponds_ref, numeric(1)))
  for (sp in dabblers) {
    for (s in strata) {
      st <- payload[[sp]]$strata[[s]]
      e <- st$e + payload[[sp]]$lag
      per_pond <- per_pond + e * st$bpop_ref
    }
  }
  per_pond <- per_pond / ponds_ref

  nopi <- dplyr::filter(totals, species == "NOPI")
  early <- mean(nopi$prairie_total[nopi$year <= 1964])
  recent <- mean(nopi$prairie_total[nopi$year %in% ref_years])

  list(
    latest_year = latest,
    ponds_latest = ponds$prairie_ponds[ponds$year == latest],
    ponds_lta = lta,
    ponds_pct_vs_lta = ponds$prairie_ponds[ponds$year == latest] / lta - 1,
    loss_scenario = loss,
    dabbler_change = as.list(qsum(change, 0.9)),
    dabblers_per_pond = as.list(qsum(per_pond, 0.9)),
    pintail_early = early,
    pintail_recent = recent,
    pintail_pct_change = recent / early - 1,
    ref_years = sort(ref_years)
  )
}

# Sustained elasticity in recent years under the main model and each sensitivity refit. The AR(1)
# rows have no diagnostics, as mgcv's k.check is not run on those fits.
sensitivity_table <- function(main, trend, ar1, year, diag_main, diag_trend, diag_year) {
  pick <- function(x, model) dplyr::transmute(x, species, model = model, es_lo, es_med, es_hi)
  diag <- function(x, model) dplyr::transmute(x, species, model = model, k_index_min, dev_expl)
  dplyr::bind_rows(pick(main, "main"), pick(trend, "trend_x2"), pick(ar1, "ar1_0.2"), pick(year, "year_re")) |>
    dplyr::left_join(
      dplyr::bind_rows(diag(diag_main, "main"), diag(diag_trend, "trend_x2"), diag(diag_year, "year_re")),
      by = c("species", "model")
    )
}

# Change in mean elasticity across the prairies between the first 10 surveys and the reference
# years. Calculated within each draw so that the interval reflects uncertainty in the difference
# itself.
elasticity_shift <- function(draws, ref_years) {
  g <- draws$grid
  early_years <- sort(unique(g$year))[1:10]
  early <- colMeans(draws$E[g$year %in% early_years, , drop = FALSE])
  late <- colMeans(draws$E[g$year %in% ref_years, , drop = FALSE])
  d <- qsum(late - early)
  tibble::tibble(
    species = draws$species,
    early_from = min(early_years), early_to = max(early_years),
    early_med = stats::median(early), late_med = stats::median(late),
    shift_lo = d[["lo"]], shift_med = d[["med"]], shift_hi = d[["hi"]],
    p_increase = mean(late > early)
  )
}

# Coarse overflight index: the extent to which the proportion of the Traditional Survey Area
# population counted in Prairie Canada varies with prairie pond counts in the first and second
# halves of the record. The index is the slope from regressing logit(share) on log(ponds); I treat
# the index as descriptive, as OLS standard errors do not account for autocorrelation.
share_response <- function(totals, split = 1990) {
  totals |>
    dplyr::filter(!is.na(prairie_share), prairie_share > 0, prairie_share < 1) |>
    dplyr::mutate(era = dplyr::if_else(year <= split, "early", "late")) |>
    dplyr::group_by(species, era) |>
    dplyr::group_modify(function(d, key) {
      m <- stats::lm(stats::qlogis(prairie_share) ~ log(prairie_ponds), data = d)
      tibble::tibble(
        from = min(d$year), to = max(d$year), n = nrow(d),
        share_mean = mean(d$prairie_share),
        slope = unname(stats::coef(m)[2]),
        slope_se = unname(summary(m)$coefficients[2, 2])
      )
    }) |>
    dplyr::ungroup()
}
