# Static figures for the Methods PDF. These figures follow the style of the web charts, using the
# Okabe-Ito palette with 1 colour per species, thin lines, and very fine gridlines.
# register_site_fonts() must run within the rendering session.

okabe_ito <- c(
  orange = "#E69F00", sky = "#56B4E9", green = "#009E73", yellow = "#F0E442",
  blue = "#0072B2", vermillion = "#D55E00", purple = "#CC79A7", black = "#000000"
)
species_colours <- c(
  MALL = okabe_ito[["green"]], NOPI = okabe_ito[["vermillion"]], BWTE = okabe_ito[["sky"]],
  GADW = okabe_ito[["orange"]], NSHO = okabe_ito[["purple"]], CANV = okabe_ito[["black"]]
)
site_colours <- c(
  ink = "#1d2125", ink2 = "#4d545b", muted = "#6c737a", grid = "#e7e5df", paper = "#fdfcf9",
  ponds = okabe_ito[["blue"]], dry = okabe_ito[["yellow"]]
)
# Class breaks for the stratum maps, matching the web page.
map_breaks <- c(0.25, 0.5, 0.75, 1, 1.25)

register_site_fonts <- function(dir = "assets/fonts") {
  f <- function(x) file.path(dir, x)
  systemfonts::register_font("Public Sans", plain = f("PublicSans-Regular.ttf"), bold = f("PublicSans-SemiBold.ttf"),
                             italic = f("PublicSans-Italic.ttf"), bolditalic = f("PublicSans-SemiBold.ttf"))
  invisible(TRUE)
}

theme_site <- function(base_size = 9) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "Public Sans") +
    ggplot2::theme(
      text = ggplot2::element_text(colour = site_colours[["ink2"]]),
      plot.title = ggplot2::element_text(colour = site_colours[["ink"]], face = "bold", size = base_size + 1),
      plot.subtitle = ggplot2::element_text(colour = site_colours[["muted"]], size = base_size - 0.5),
      plot.title.position = "plot",
      strip.text = ggplot2::element_text(colour = site_colours[["ink"]], face = "bold", hjust = 0, size = base_size),
      axis.text = ggplot2::element_text(colour = site_colours[["muted"]], size = base_size - 1),
      axis.title = ggplot2::element_text(colour = site_colours[["muted"]], size = base_size - 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = site_colours[["grid"]], linewidth = 0.3),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_text(colour = site_colours[["ink2"]], size = base_size - 0.5),
      legend.text = ggplot2::element_text(colour = site_colours[["ink2"]], size = base_size - 0.5),
      legend.key.size = grid::unit(0.3, "cm"),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

species_labels <- function(species_tbl) stats::setNames(species_tbl$name_en, species_tbl$species)

# Species ordered by recent sustained elasticity, from highest to lowest.
species_order <- function(recent, species_tbl) {
  lab <- species_labels(species_tbl)
  unname(lab[recent$species[order(-recent$es_med)]])
}

# Light-to-dark monochromatic gradient interpolated in Lab colour space (ramp() in js/viz.js).
species_ramp <- function(colour, n = length(map_breaks) + 1) {
  mix <- function(a, b, t) grDevices::colorRampPalette(c(a, b), space = "Lab")(101)[round(100 * t) + 1]
  black <- toupper(colour) == "#000000"
  stops <- c(mix(site_colours[["paper"]], colour, 0.13),
             if (black) "#707070" else colour,
             if (black) "#000000" else mix(colour, "#000000", 0.42))
  grDevices::colorRampPalette(stops, space = "Lab")(n)
}

# The 10 springs with the fewest ponds in the prairie region (driestYears() in js/viz.js).
driest_years <- function(totals, n = 10) {
  d <- dplyr::distinct(totals, year, prairie_ponds)
  sort(d$year[order(d$prairie_ponds)][seq_len(n)])
}

x_years <- function() ggplot2::scale_x_continuous(breaks = seq(1960, 2020, 10), expand = ggplot2::expansion(c(0.01, 0.01)))
y_zero <- function() ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(c(0, 0.05)))

fig_study_area <- function(strata_sf, basemap) {
  prairie <- dplyr::filter(strata_sf, prairie)
  box <- sf::st_bbox(sf::st_buffer(sf::st_union(prairie), 60000))
  us <- dplyr::filter(strata_sf, !prairie, stratum %in% 41:49)
  labs <- suppressWarnings(sf::st_point_on_surface(prairie))
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = basemap$provinces, fill = "#f1efe9", colour = "white", linewidth = 0.6) +
    ggplot2::geom_sf(data = basemap$lakes, fill = "#d4e6f0", colour = NA) +
    ggplot2::geom_sf(data = us, fill = NA, colour = "#d3cfc5", linewidth = 0.3) +
    ggplot2::geom_sf(data = prairie, ggplot2::aes(fill = province), colour = "white", linewidth = 0.4) +
    ggplot2::geom_sf_text(data = labs, ggplot2::aes(label = stratum), family = "Public Sans", size = 2.6, colour = site_colours[["ink"]]) +
    ggplot2::scale_fill_manual(values = c(AB = "#e0dcd2", SK = "#cfcabe", MB = "#e0dcd2"), guide = "none") +
    ggplot2::coord_sf(xlim = box[c("xmin", "xmax")], ylim = box[c("ymin", "ymax")], expand = FALSE, datum = NA) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_site() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

# May ponds are shown above and breeding dabbling ducks stacked by species below, with shading
# indicating the 10 driest springs (Figure 1 on the webpage).
fig_ponds_birds <- function(totals, species_tbl, stack = c("NOPI", "MALL", "BWTE", "GADW", "NSHO")) {
  lab <- species_labels(species_tbl)
  years <- 1955:max(totals$year)
  # Plot periods of consecutive dry springs behind the data, with line markers at both ends.
  dry <- tibble::tibble(year = driest_years(totals)) |>
    dplyr::mutate(run = cumsum(c(1, diff(year) != 1))) |>
    dplyr::group_by(run) |>
    dplyr::summarise(x1 = min(year) - 0.5, x2 = max(year) + 0.5)
  bands <- list(
    ggplot2::geom_rect(data = dry, ggplot2::aes(xmin = x1, xmax = x2, ymin = 0, ymax = Inf),
                       fill = site_colours[["dry"]], alpha = 0.45, inherit.aes = FALSE),
    ggplot2::geom_vline(xintercept = c(dry$x1, dry$x2), colour = site_colours[["dry"]], linewidth = 0.55)
  )

  ponds <- dplyr::distinct(totals, year, ponds = prairie_ponds / 1e6) |>
    dplyr::right_join(tibble::tibble(year = years), by = "year")
  top <- ggplot2::ggplot(ponds, ggplot2::aes(year)) +
    bands +
    # Unlike geom_area, the ribbon is interrupted during 2020-21, when survey data are missing.
    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = ponds), fill = site_colours[["ponds"]], alpha = 0.1, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = ponds), colour = site_colours[["ponds"]], linewidth = 0.55, na.rm = TRUE) +
    x_years() + y_zero() +
    ggplot2::labs(x = NULL, y = NULL, subtitle = "May ponds (millions)") +
    theme_site() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())

  ducks <- totals |>
    dplyr::filter(species %in% stack) |>
    dplyr::select(species, year, n = prairie_total) |>
    tidyr::complete(species = stack, year = years) |>
    dplyr::mutate(species = factor(species, stack)) |>
    dplyr::arrange(year, species) |>
    dplyr::group_by(year) |>
    dplyr::mutate(ymax = cumsum(n) / 1e6, ymin = ymax - n / 1e6) |>
    dplyr::ungroup() |>
    dplyr::mutate(name = factor(lab[as.character(species)], rev(unname(lab[stack]))))
  bottom <- ggplot2::ggplot(ducks, ggplot2::aes(year)) +
    bands +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ymin, ymax = ymax, fill = name), alpha = 0.85, na.rm = TRUE) +
    # Add white separators between bands, but not at the very top of the stack.
    ggplot2::geom_line(data = dplyr::filter(ducks, species != dplyr::last(stack)),
                       ggplot2::aes(y = ymax, group = name), colour = "white", linewidth = 0.25, na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = stats::setNames(species_colours[stack], lab[stack]), name = NULL) +
    x_years() + y_zero() +
    ggplot2::labs(x = NULL, y = NULL, subtitle = "Breeding dabbling ducks (millions)") +
    theme_site() +
    ggplot2::theme(legend.position = "bottom", plot.margin = ggplot2::margin(12, 5.5, 5.5, 5.5))

  patchwork::wrap_plots(top, bottom, ncol = 1, heights = c(1, 1.5))
}

fig_elasticity_curves <- function(curves, curves_trend, curves_ar1, curves_year, recent, species_tbl) {
  lab <- species_labels(species_tbl)
  ord <- species_order(recent, species_tbl)
  # Lines and bands are interrupted at rows corresponding to years when no survey was conducted.
  gaps <- function(d) tidyr::complete(d, species, year = seq(min(year), max(year)))
  main <- gaps(curves) |> dplyr::mutate(name = factor(lab[species], ord))
  variants <- c("Trend basis doubled", "AR(1) residuals, ρ = 0.2", "Year effects")
  sens <- stats::setNames(list(curves_trend, curves_ar1, curves_year), variants) |>
    lapply(gaps) |>
    dplyr::bind_rows(.id = "variant") |>
    dplyr::mutate(name = factor(lab[species], ord), variant = factor(variant, variants))
  ggplot2::ggplot(main, ggplot2::aes(year)) +
    ggplot2::geom_hline(yintercept = 1, colour = "#a8a397", linewidth = 0.3, linetype = "dashed") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi, fill = species), alpha = 0.16, na.rm = TRUE) +
    # Solid, dashed, and dotted lines use different shades of grey to distinguish the 3 refit models
    # even at smaller display sizes.
    ggplot2::geom_line(data = sens, ggplot2::aes(y = med, linetype = variant, colour = variant), linewidth = 0.4, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = med, colour = species), linewidth = 0.6, na.rm = TRUE) +
    ggplot2::facet_wrap(~name, ncol = 3) +
    ggplot2::scale_colour_manual(values = c(species_colours, stats::setNames(c("#8a9096", "#4d545b", "#1d2125"), variants)),
                                 breaks = variants, name = "Sensitivity refits (median)") +
    ggplot2::scale_fill_manual(values = species_colours, guide = "none") +
    ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "42", "11"), variants), name = "Sensitivity refits (median)") +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 1, title.position = "top"),
                    colour = ggplot2::guide_legend(nrow = 1, title.position = "top")) +
    ggplot2::scale_x_continuous(breaks = c(1960, 1990, 2020)) +
    ggplot2::labs(x = NULL, y = "Pond elasticity (prairie average)") +
    theme_site() +
    ggplot2::theme(panel.spacing.x = grid::unit(1, "lines"), legend.key.width = grid::unit(1.2, "cm"))
}

# Create one map for each species using graduated shades of the species colour, and include a key
# showing every species' ramp against the shared breaks.
fig_stratum_elasticity <- function(strata_sf, stratum_table, recent, species_tbl) {
  lab <- species_labels(species_tbl)
  ord <- species_order(recent, species_tbl)
  codes <- names(lab)[match(ord, lab)]
  ramps <- lapply(stats::setNames(codes, codes), function(s) species_ramp(species_colours[[s]]))
  d <- dplyr::filter(strata_sf, prairie) |>
    dplyr::select(stratum) |>
    dplyr::inner_join(stratum_table, by = "stratum") |>
    dplyr::mutate(name = factor(lab[species], ord),
                  class = findInterval(es_med, map_breaks) + 1,
                  fill = purrr::map2_chr(species, class, function(s, k) ramps[[s]][k]))
  maps <- ggplot2::ggplot(d) +
    ggplot2::geom_sf(ggplot2::aes(fill = fill), colour = "white", linewidth = 0.25) +
    ggplot2::facet_wrap(~name, ncol = 3) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_sf(datum = NA) +
    theme_site() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  key <- tidyr::expand_grid(species = codes, class = seq_along(ramps[[1]])) |>
    dplyr::mutate(name = factor(lab[species], rev(ord)),
                  fill = purrr::map2_chr(species, class, function(s, k) ramps[[s]][k]))
  legend <- ggplot2::ggplot(key, ggplot2::aes(class, name, fill = fill)) +
    ggplot2::geom_tile(width = 0.94, height = 0.8) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(breaks = seq_along(map_breaks) + 0.5, labels = formatC(map_breaks, format = "f", digits = 2),
                                expand = c(0, 0)) +
    ggplot2::labs(x = "Sustained pond elasticity, last ten surveys", y = NULL) +
    theme_site(8) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(colour = site_colours[["ink2"]]))

  key_row <- patchwork::wrap_plots(patchwork::plot_spacer(), legend, patchwork::plot_spacer(), widths = c(1, 2.4, 1))
  patchwork::wrap_plots(maps, key_row, ncol = 1, heights = c(2.4, 1))
}

fig_per_pond <- function(totals, species_tbl, species = c("MALL", "NOPI")) {
  lab <- species_labels(species_tbl)
  yrs <- tidyr::expand_grid(species = species, year = 1955:max(totals$year))
  d <- totals |>
    dplyr::filter(species %in% !!species) |>
    dplyr::transmute(species, year, per = prairie_total / prairie_ponds) |>
    dplyr::right_join(yrs, by = c("species", "year"))
  ends <- d |>
    dplyr::filter(!is.na(per)) |>
    dplyr::group_by(species) |>
    dplyr::slice_max(year, n = 1) |>
    dplyr::mutate(label = paste0(lab[species], "\n", formatC(per, format = "f", digits = 2), " per pond"))
  ggplot2::ggplot(d, ggplot2::aes(year, per, colour = species)) +
    ggplot2::geom_line(linewidth = 0.55, na.rm = TRUE) +
    ggplot2::geom_point(data = ends, size = 1.6) +
    ggplot2::geom_text(data = ends, ggplot2::aes(label = label), hjust = 0, nudge_x = 1.5, lineheight = 0.9,
                       family = "Public Sans", size = 2.5, colour = site_colours[["ink2"]]) +
    ggplot2::scale_colour_manual(values = species_colours, guide = "none") +
    ggplot2::scale_x_continuous(breaks = seq(1960, 2020, 10), expand = ggplot2::expansion(c(0.01, 0.2))) +
    y_zero() +
    ggplot2::labs(x = NULL, y = NULL, subtitle = "Breeding birds per May pond") +
    theme_site()
}
