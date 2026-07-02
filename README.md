# LANDIS-II Climate Library Builder

A Shiny app for building daily, LANDIS-II–ready climate inputs for **historical**
and **future** scenarios — from raw PRISM and NASA NEX-GDDP-CMIP6 data all the way
to bias-corrected, wind-appended CSVs in the LANDIS-II Climate Library v5 format.

It automates the full workflow that used to live in a pair of R Markdown scripts:
download historical climate, download and process future climate, choose future
scenarios with a four-corners analysis, bias-correct the future models against the
historical baseline, and append GRIDMET wind — with diagnostic plots at every step.

---

## What it does

The app is organized as seven tabs, run left to right. **Every tab also accepts
pre-processed uploads**, so if you have already completed a step (or are working
outside CONUS, see below) you can drop in your own files and continue from there.

| Tab | Purpose |
|-----|---------|
| **1 · Setup** | Set landscape code, output folder, and upload your climate regions raster (`.tif`). |
| **2 · Historical PRISM** | Download 800 m daily PRISM (tmin, tmax, ppt, dewpoint), fill missing dates, and format to the LANDIS wide layout. |
| **3 · Future Climate** | Download NASA NEX-GDDP-CMIP6 daily data from AWS S3 and extract per-ecoregion daily series. |
| **4 · Explore & Select** | Four-corners scatter (ΔT vs ΔP%) plus time-series plots to pick warm/dry, warm/wet, cool/dry, cool/wet models. |
| **5 · Bias Correction** | Align future models to the PRISM baseline — additive for temperature, multiplicative for precipitation. |
| **6 · GRIDMET Wind** | Download GRIDMET wind speed/direction and append to both historical and future climate files. |
| **7 · Download** | Review and export all LANDIS-ready CSVs, individually or as a zip. |

---

## Coverage: CONUS vs. the rest of the world

- **Future Climate (NEX-GDDP-CMIP6) is global** — the app can process future
  climate for LANDIS landscapes anywhere.
- **Historical PRISM and GRIDMET Wind are CONUS-only** (continental US, lower 48).

If your landscape is **outside CONUS**, you can still use the app for the
future-climate workflow: obtain historical climate and wind from another source,
format them to the LANDIS wide layout, and upload them on the Historical PRISM and
GRIDMET Wind tabs. Bias correction then aligns the future models to your uploaded
historical baseline.

Your climate regions raster **must carry a valid coordinate reference system (CRS)**
— the app uses it to derive the geographic download extent.

---

## Installation

Requires **R (≥ 4.3)**. The app is tested on R 4.3.3, 4.4.x, and newer R 5.x
releases (see *Notes on R / terra versions* below).

Install the required packages once:

```r
install.packages(c(
  "shiny", "bslib", "tidyverse", "lubridate", "terra", "tidyterra",
  "sf", "glue", "ggrepel", "scales", "patchwork", "plotly", "DT",
  "shinyjs", "shinycssloaders", "climateR", "httr", "aws.s3",
  "furrr", "future", "future.apply", "callr", "progressr",
  "viridisLite", "zip", "forcats", "purrr", "rlang", "stringr"
))
```

`climateR` may need to be installed from GitHub:

```r
# install.packages("remotes")
remotes::install_github("mikejohnson51/climateR")
```

The optional **State** field on the GRIDMET Wind tab uses the `AOI` package. It is
not required — by default the app derives the wind download area from your raster
extent — so only install `AOI` if you specifically want to pull wind by US state.

---

## Running the app

Start a **fresh R session** (this avoids `progressr` handlers left over from other
work interfering with startup), then:

```r
shiny::runApp("path/to/LANDIS_Shiny_Climate")
```

The app runs locally — long downloads (PRISM, AWS S3, GRIDMET) run as background
jobs so the interface stays responsive, with a live log on each tab.

---

## Typical workflow

1. **Setup** — enter a landscape code (e.g. `MORA`), choose an output directory,
   and upload your climate regions raster. Confirm.
2. **Historical PRISM** — pick a date range and variables, set the number of
   parallel workers (use *Check available cores* to see your machine's limit), and
   run the download. Then fill missing dates and format.
3. **Future Climate** — select GCM models, SSP scenarios, and variables, then
   download from AWS S3 and process the NetCDFs into per-ecoregion CSVs.
4. **Explore & Select** — inspect the four-corners plot and time series, then pick
   your four representative models from the dropdowns.
5. **Bias Correction** — align the future models to the PRISM baseline over your
   chosen overlap window.
6. **GRIDMET Wind** — download wind and append it to the historical and future files.
7. **Download** — export the finished, LANDIS-ready climate library.

---

## Output format

CSVs follow the **LANDIS-II Climate Library v5** wide layout: columns
`Year, Month, Day, Variable, eco1, eco2, … ecoN`, one row per date × variable.
Variable names and units follow Table 1 of the Climate Library v5 User Guide
(temperature in °C, precipitation in cm, wind direction in degrees *from*).
Variable names are read case-insensitively, so common spellings
(`Tmax`/`maxtemp`, `precip`/`ppt`, etc.) are all accepted on upload.

---

## Notes on R / terra versions

Future-climate NetCDFs from NEX-GDDP-CMIP6 use mixed model calendars
(`proleptic_gregorian`, `365_day`/noleap, etc.). Rather than rely on
`terra::time()` — whose handling of these files changed between terra versions and
caused `[subset] no (valid) layer selected` errors on newer R — the app
reconstructs daily dates deterministically from each file's year, correctly
handling leap and no-leap calendars. This makes the future-climate pipeline work
consistently across R 4.3–5.x.

---

## Data sources

- **PRISM** — 800 m daily historical climate (Oregon State University).
- **NASA NEX-GDDP-CMIP6** — global downscaled daily CMIP6 projections, via AWS S3
  (`nex-gddp-cmip6`).
- **GRIDMET** — daily gridded surface meteorology (wind), via `climateR`.

## License

See [LICENSE](LICENSE).
