# Retrieve and tidy WBPHS estimates by stratum, survey area geometries, and basemap layers.

prairie_strata <- 26:40

# Sources (all public domain): WBPHS files in USFWS ServCat, and Natural Earth pinned to v5.1.2.
# Licence information is provided on the Data page and in the README.md.
source_urls <- c(
  estimates = "https://ecos.fws.gov/ServCat/DownloadFile/302110",
  strata    = "https://ecos.fws.gov/ServCat/DownloadFile/241521",
  provinces = "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/v5.1.2/geojson/ne_50m_admin_1_states_provinces.geojson",
  lakes     = "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/v5.1.2/geojson/ne_50m_lakes.geojson"
)

# Download `url` to `path` only if the file does not already exist, allowing the pipeline to be
# reproduced from a clean clone while avoiding excessive requests to ServCat.
fetch_file <- function(url, path, unzip_to = NULL) {
  if (!file.exists(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    utils::download.file(url, path, mode = "wb", quiet = TRUE)
  }
  if (!is.null(unzip_to) && !dir.exists(unzip_to)) utils::unzip(path, exdir = unzip_to)
  path
}

species_info <- function() {
  tibble::tribble(
    ~species, ~name_en,            ~nesting,
    "MALL",   "Mallard",           "upland",
    "NOPI",   "Northern Pintail",  "upland",
    "BWTE",   "Blue-winged Teal",  "upland",
    "GADW",   "Gadwall",           "upland",
    "NSHO",   "Northern Shoveler", "upland",
    "CANV",   "Canvasback",        "overwater"
  )
}

read_estimates <- function(path) {
  readr::read_csv(path, na = c("", "NA", "NULL"), show_col_types = FALSE) |>
    dplyr::rename(year = survey_year, species = survey_species, se = standard_error)
}

# The distributed files contain no rows with values of 0. Species not observed in a stratum covered
# by an aerial survey simply have no record. Reconstruct these records with values of 0 for all
# stratum and year combinations covered by an aerial survey.
complete_bpop <- function(est, strata = prairie_strata, species = species_info()$species) {
  flown <- est |>
    dplyr::filter(stratum %in% strata) |>
    dplyr::distinct(stratum, year)
  tidyr::expand_grid(flown, species = species) |>
    dplyr::left_join(est, by = c("stratum", "year", "species")) |>
    dplyr::mutate(
      se = dplyr::if_else(is.na(estimate), 0, se),
      estimate = dplyr::coalesce(estimate, 0)
    )
}

tidy_ponds <- function(est, strata = prairie_strata) {
  est |>
    dplyr::filter(species == "POND", stratum %in% strata) |>
    dplyr::select(stratum, year, ponds = estimate, ponds_se = se) |>
    dplyr::arrange(stratum, year) |>
    dplyr::group_by(stratum) |>
    dplyr::mutate(
      log_ponds_c = log(ponds) - mean(log(ponds)),
      # Previous-year ponds are used only when a survey was conducted in the preceding calendar year
      # (drops 1955, 2022 following the COVID survey interruption, and years with missing data for
      # stratum 36).
      log_ponds_lag_c = dplyr::if_else(dplyr::lag(year) == year - 1, dplyr::lag(log_ponds_c), NA_real_)
    ) |>
    dplyr::ungroup()
}

# Precision weights: downweight less precise stratum–year combinations while preventing the most
# precise combinations from dominating (median CV^2 as the process error floor).
precision_weights <- function(estimate, se) {
  cv <- se / pmax(estimate, 1)
  cv[is.na(cv) | estimate == 0] <- stats::median(cv[estimate > 0], na.rm = TRUE)
  w <- 1 / (cv^2 + stats::median(cv^2))
  w / mean(w)
}

build_model_data <- function(bpop, ponds) {
  bpop |>
    dplyr::inner_join(ponds, by = c("stratum", "year")) |>
    dplyr::group_by(species) |>
    dplyr::mutate(w = precision_weights(estimate, se)) |>
    dplyr::ungroup() |>
    dplyr::mutate(stratum_f = factor(stratum)) |>
    dplyr::arrange(species, stratum, year)
}

# ---- Geometry --------------------------------------------------------------

# Equal-area conic projection centred on the Canadian prairies (metres).
prairie_crs <- "+proj=aea +lat_1=49 +lat_2=55 +lat_0=45 +lon_0=-106 +datum=WGS84 +units=m +no_defs"

read_strata <- function(shp, strata = c(prairie_strata, 41:49, 75, 76)) {
  sf::sf_use_s2(FALSE)
  sf::st_read(shp, quiet = TRUE) |>
    dplyr::filter(stratum %in% strata) |>
    sf::st_make_valid() |>
    dplyr::group_by(stratum) |>
    dplyr::summarise(.groups = "drop") |>
    sf::st_transform(prairie_crs) |>
    dplyr::mutate(
      area_km2 = as.numeric(sf::st_area(geometry)) / 1e6,
      # Assign province by stratum number, following the survey crew areas: S. Alberta 26-29,
      # S. Saskatchewan 30-35, S. Manitoba 36-40.
      province = dplyr::case_when(
        stratum %in% 26:29 ~ "AB",
        stratum %in% 30:35 ~ "SK",
        stratum %in% 36:40 ~ "MB",
        stratum %in% 75:76 ~ "AB",
        TRUE ~ "US"
      ),
      prairie = stratum %in% prairie_strata
    )
}

read_basemap <- function(provinces_path, lakes_path, strata_sf) {
  sf::sf_use_s2(FALSE)
  # Wide enough to cover the web map around the strata, including the space reserved for the inset.
  box <- sf::st_bbox(sf::st_buffer(sf::st_union(strata_sf), 400000))
  keep <- c("Alberta", "Saskatchewan", "Manitoba", "British Columbia", "Ontario",
            "Montana", "North Dakota", "South Dakota", "Minnesota", "Northwest Territories")
  prov <- sf::st_read(provinces_path, quiet = TRUE) |>
    dplyr::filter(name %in% keep) |>
    dplyr::select(name, admin) |>
    sf::st_transform(prairie_crs) |>
    sf::st_crop(box)
  lakes <- sf::st_read(lakes_path, quiet = TRUE) |>
    dplyr::select(name) |>
    sf::st_transform(prairie_crs) |>
    sf::st_make_valid() |>
    sf::st_crop(box)
  list(provinces = prov, lakes = lakes, bbox = box)
}

# Canada and the contiguous 48 U.S. states for a small locator map beside the stratum map.
read_locator <- function(provinces_path) {
  sf::sf_use_s2(FALSE)
  sf::st_read(provinces_path, quiet = TRUE) |>
    dplyr::filter(admin %in% c("Canada", "United States of America"), !name %in% c("Alaska", "Hawaii")) |>
    dplyr::select(name, admin) |>
    sf::st_make_valid() |>
    sf::st_crop(c(xmin = -141, ymin = 24, xmax = -52, ymax = 66)) |>
    sf::st_transform(prairie_crs)
}
