# May Ponds and Breeding Ducks in Prairie Canada

How strongly do breeding ducks on the Canadian prairies follow the number of ponds on the landscape each May? Does that differ by species, and has it changed since the 1950s? This repository holds the analysis and the web page that presents it.

**Site:** <https://ericwootton.github.io/prairie-ducks/>

**Archive:** [doi:10.5281/zenodo.23043511](https://doi.org/10.5281/zenodo.23043511)

This is an independent portfolio project built on public data. It is not affiliated with, commissioned by, or endorsed by Ducks Unlimited Canada, the Institute for Wetland and Waterfowl Research, the U.S. Fish and Wildlife Service, or the Canadian Wildlife Service. The analysis has not been peer reviewed.

## What it finds

The data are the stratum estimates of the Waterfowl Breeding Population and Habitat Survey for the 15 Prairie Canada strata (26 to 40), 1955 to 2026, for five dabbling ducks and canvasback. One hierarchical GAM per species (`mgcv`, Tweedie errors) relates breeding birds to May ponds after removing each stratum's long-term trend.

- Over the last ten surveys, a sustained 1% change in May ponds went with a 1.30% change in breeding northern pintails (95% interval 1.01 to 1.60) and a 0.32% change in mallards (0.17 to 0.45). The other species fall in between.
- At recent numbers, losing 10% of May ponds across Prairie Canada would mean about 660,000 fewer breeding dabbling ducks settling there each spring (90% interval 575,000 to 727,000).
- The pintail's response to ponds has not weakened. In the main model its short-term elasticity rose from 0.79 in 1956–1965 to 1.25 over the last ten surveys. A model with year effects shows a smaller rise whose interval includes zero.

These elasticities come from natural wet and dry years, not from drainage. The limitations section of the [methods PDF](https://ericwootton.github.io/prairie-ducks/prairie-ducks-methods.pdf) sets out what that means for the wetland-loss estimates.

## The site

The site is a single page (`index.qmd`) with six figures: ponds and ducks since 1955, pond elasticity by species, a stratum map with a species switch, elasticity through time, birds per pond, and a small wetland-loss calculator. Each chart has a table view of its data, and the calculator's bars are labelled with their values. The page ends with a short methods section, links to the CSV files, and the references. The full methods are a separate PDF (`methods.qmd`, rendered with Typst).

## Rerun the analysis

You need R 4.4 or later and [Quarto](https://quarto.org/) 1.10 or later. The site was built with R 4.4.3 and Quarto 1.10.18.

```bash
git clone https://github.com/ericwootton/prairie-ducks.git
cd prairie-ducks
Rscript -e "renv::restore()"       # install the package versions in renv.lock
Rscript -e "targets::tar_make()"   # download, clean, fit, check, and export
quarto render                      # build the page and the PDF into _site/
```

`targets::tar_make()` fits 24 models (six species, a main model and three sensitivity models each) and takes a few minutes on a laptop. It stops with an error if any of its checks fail:

- totals rebuilt from the stratum file must round to the figures in the published USFWS reports;
- the calculator's JavaScript, run in V8, must match the same calculation in R;
- every column of every CSV must be described in the data dictionary.

`targets::tar_visnetwork()` shows the whole dependency graph.

## Layout

```text
_targets.R         the pipeline: every target from download to export
R/                 functions the pipeline calls (data, model, summarise, check, export, figures)
data-raw/          the raw downloads, kept so the pipeline does not depend on the source links
data/csv/          tidy tables and model results, with data_dictionary.csv
data/site/         JSON read by the page's charts
js/viz.js          chart code, palette, and the calculator's arithmetic
js/calculator.js   the wetland-loss calculator
index.qmd          the page
methods.qmd        the methods PDF
assets/            theme, banner photo, and the fonts (SIL OFL): TTF for the PDF, woff2 for the page
tests/vendor/      the copy of D3 used to run the calculator's JavaScript in V8
renv.lock          package versions
```

The charts use the Okabe–Ito palette, with one colour per species on the page and in the PDF.

## Data sources and licences

- **Breeding population and pond estimates:** Waterfowl Breeding Population and Habitat Survey, U.S. Fish and Wildlife Service and Canadian Wildlife Service. Stratum estimates for the traditional survey area, 1955 to 2026, from USFWS ServCat ([reference 142673](https://iris.fws.gov/APPS/ServCat/Reference/Profile/142673)). A U.S. Government work in the public domain.
- **Survey strata:** Stratum boundaries from ServCat ([download](https://ecos.fws.gov/ServCat/DownloadFile/241521)).
- **Basemap:** [Natural Earth](https://www.naturalearthdata.com/) 1:50m admin-1 boundaries and lakes, release v5.1.2, public domain.

The code in this repository is released under the [MIT licence](LICENSE). The derived tables, figures, and text are released under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## Cite

See [`CITATION.cff`](CITATION.cff), or use "Cite this repository" on GitHub. Please also cite the survey itself:

> Eyler, M., and A. Walter (2026). *WBPHS Traditional Survey Area Population Estimates, 1955 to Present.* U.S. Fish and Wildlife Service, Division of Migratory Bird Management. ServCat reference 142673.

## Contact

Eric Wootton · eric.wootton@mail.mcgill.ca · or open an issue.
