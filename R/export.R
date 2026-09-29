# Export files loaded by the website (data/site) and tidy data files for download (data/csv).

write_json_file <- function(x, path, digits = NA) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, auto_unbox = TRUE, digits = digits, na = "null", dataframe = "rows")
  path
}

write_csv_file <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path, na = "")
  path
}

# Projected geometries simplified for mapping. Since the coordinates are already planar
# (prairie_crs), the browser can render them with an identity projection, so d3's spherical
# winding rules do not apply.
export_geo <- function(strata_sf, basemap, locator, path) {
  simplify <- function(x, keep) {
    rmapshaper::ms_simplify(x, keep = keep, keep_shapes = TRUE)
  }
  to_fc <- function(x) jsonlite::fromJSON(geojsonsf_like(x), simplifyVector = FALSE)
  geo <- list(
    strata = to_fc(simplify(strata_sf, 0.2)),
    provinces = to_fc(simplify(basemap$provinces, 0.15)),
    lakes = to_fc(simplify(dplyr::filter(basemap$lakes, as.numeric(sf::st_area(geometry)) > 2e8), 0.2)),
    locator = to_fc(simplify(locator, 0.02)),
    bbox = as.numeric(sf::st_bbox(strata_sf))
  )
  write_json_file(geo, path, digits = NA)
}

# Convert sf to GeoJSON text without additional dependencies, retaining planar coordinates.
geojsonsf_like <- function(x) {
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp))
  sf::st_write(sf::st_set_crs(x, NA), tmp, driver = "GeoJSON", quiet = TRUE,
               layer_options = c("COORDINATE_PRECISION=0", "RFC7946=NO"))
  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

round_df <- function(x, digits = 3) {
  dplyr::mutate(x, dplyr::across(dplyr::where(is.double), ~ signif(.x, 6))) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with(c("e_", "es_", "lo", "med", "hi", "prairie_share")),
                                ~ round(.x, digits)))
}
