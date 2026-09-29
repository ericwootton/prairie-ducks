# Checks run within the pipeline that stop the pipeline if they fail.

# Run the website's own scenario code (js/viz.js, using the same d3 build as the website) in V8 on
# the exported scenario.json, and compare the results with scenario_change() run in R for a fixed
# set of scenarios. Both read the same file; thus, any differences are arithmetic bugs.
check_scenario_parity <- function(scenario_json, viz_js, d3_js, tol = 1e-8) {
  ctx <- V8::v8()
  ctx$source(d3_js)
  src <- readLines(viz_js, encoding = "UTF-8", warn = FALSE)
  ctx$eval(paste(sub("^export (const|function)", "\\1", src), collapse = "\n"))
  ctx$eval(paste0("var payload = ", paste(readLines(scenario_json, encoding = "UTF-8", warn = FALSE), collapse = "\n"), ";"))
  ctx$eval("var K = kit({ d3: d3, Plot: null, htl: null, t: {} });")
  payload <- jsonlite::read_json(scenario_json, simplifyVector = TRUE)

  dabblers <- c("MALL", "NOPI", "BWTE", "GADW", "NSHO")
  scenarios <- list(
    list(species = "MALL", strata = 26:40, loss = 0.10, sustained = TRUE),
    list(species = dabblers, strata = 26:40, loss = 0.10, sustained = TRUE),
    list(species = "NOPI", strata = 30:35, loss = 0.25, sustained = FALSE),
    list(species = c("CANV", "GADW"), strata = c(27, 33, 38), loss = 0.50, sustained = TRUE),
    list(species = dabblers, strata = 36:40, loss = 0.01, sustained = FALSE)
  )
  purrr::imap_dfr(scenarios, function(sc, i) {
    r <- scenario_change(payload, sc$species, sc$strata, sc$loss, sc$sustained)
    opts <- jsonlite::toJSON(list(species = I(sc$species), strata = I(sc$strata), loss = sc$loss, sustained = sc$sustained), auto_unbox = TRUE)
    js <- ctx$eval(paste0("JSON.stringify(K.runScenario(payload, ", opts, ").total)"))
    js <- jsonlite::fromJSON(js)
    out <- tibble::tibble(
      scenario = i, species = paste(sc$species, collapse = "+"), strata = if (all(diff(sc$strata) == 1)) paste(range(sc$strata), collapse = "-") else paste(sc$strata, collapse = " "),
      loss = sc$loss, sustained = sc$sustained, stat = c("lo", "med", "hi"),
      r = unname(r), js = c(js$lo, js$med, js$hi)
    )
    out$rel_diff <- abs(out$r - out$js) / pmax(abs(out$r), 1)
    if (any(out$rel_diff > tol)) {
      stop("Scenario parity failed: R and the site's JavaScript disagree.\n",
           paste(utils::capture.output(print(out)), collapse = "\n"), call. = FALSE)
    }
    out
  })
}

# Totals reconstructed from the stratum file must match the values published in the USFWS Waterfowl
# Population Status reports when rounded to 1 decimal place in millions.
check_published_totals <- function(totals) {
  published <- tibble::tribble(
    ~year, ~quantity,                     ~published_millions, ~source,
    2019,  "Mallard, traditional survey area", 9.4,            "Waterfowl Population Status, 2019",
    2019,  "May ponds, Prairie Canada",        2.9,            "Waterfowl Population Status, 2019",
    2024,  "Mallard, traditional survey area", 6.6,            "Waterfowl Population Status, 2024",
    2024,  "May ponds, Prairie Canada",        2.7,            "Waterfowl Population Status, 2024"
  )
  mall <- dplyr::filter(totals, species == "MALL")
  out <- published |>
    dplyr::mutate(ours = ifelse(
      startsWith(quantity, "Mallard"),
      mall$tsa_total[match(year, mall$year)],
      mall$prairie_ponds[match(year, mall$year)]
    ), .before = published_millions) |>
    dplyr::mutate(match = round(ours / 1e6, 1) == published_millions)
  if (!all(out$match)) {
    stop("Totals no longer match the published reports.\n",
         paste(utils::capture.output(print(out)), collapse = "\n"), call. = FALSE)
  }
  out
}
