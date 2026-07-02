# ============================================================
# app.R  –  LANDIS-II Climate Library Preparation App
# ============================================================
#
# Run with: shiny::runApp("path/to/this/folder")
#
# Required packages (run once to install):
#   install.packages(c(
#     "shiny", "bslib", "tidyverse", "lubridate", "terra", "tidyterra",
#     "sf", "glue", "ggrepel", "scales", "patchwork", "plotly", "DT",
#     "shinyjs", "shinycssloaders", "climateR", "httr", "aws.s3",
#     "furrr", "future", "future.apply", "callr", "progressr",
#     "viridisLite", "zip", "forcats", "purrr", "rlang", "stringr"
#   ))
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(lubridate)
  library(terra)
  library(glue)
  library(ggrepel)
  library(scales)
  library(DT)
  library(shinyjs)
  library(shinycssloaders)
})

# Raise file upload limit to 2 GB (climate CSVs are large)
options(shiny.maxRequestSize = 2000 * 1024^2)

# Prevent any active progressr handlers (from interactive Rmd work) from
# intercepting Shiny conditions and causing spurious errors on startup.
if (requireNamespace("progressr", quietly = TRUE)) {
  progressr::handlers(global = FALSE)
}

# Source all modules
source("R/utils.R")
source("R/mod_setup.R")
source("R/mod_prism.R")
source("R/mod_future.R")
source("R/mod_explore.R")
source("R/mod_biascorrect.R")
source("R/mod_wind.R")
source("R/mod_download.R")

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "LANDIS-II Climate Library Builder",
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = "#2c7bb6"
  ),
  window_title = "LANDIS-II Climate Builder",
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$style(HTML("
      /* Override spinner default color */
      .shiny-spinner-output-container { min-height: 40px; }
    "))
  ),

  # ── Tabs (each sourced from its module) ────────────────────────────────
  mod_setup_ui("setup"),
  mod_prism_ui("prism"),
  mod_future_ui("future"),
  mod_explore_ui("explore"),
  mod_biascorrect_ui("biascorrect"),
  mod_wind_ui("wind"),
  mod_download_ui("download"),

  # ── Right-side nav items ───────────────────────────────────────────────
  nav_spacer(),
  nav_item(
    tags$a(
      href   = "https://github.com/LANDIS-II-Foundation/Extension-NECN-Succession",
      target = "_blank",
      class  = "nav-link text-muted",
      icon("github"), " LANDIS-II"
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Shared reactive state ─────────────────────────────────────────────
  rv <- reactiveValues(
    # Setup
    park          = NULL,
    out_dir       = NULL,
    climreg_path  = NULL,

    # Data file paths (populated as each step completes)
    prism_csv     = NULL,   # Tab 2 output: LANDIS-formatted historical PRISM
    future_dir    = NULL,   # Tab 3 output: dir of future climate CSVs
    biascorr_dir  = NULL,   # Tab 5 output: dir of bias-corrected future CSVs
    wind_hist_csv = NULL,   # Tab 6 output: historical + GRIDMET wind

    # Model selection (Tab 4)
    selected_models = list(
      warm_dry = NULL,
      warm_wet = NULL,
      cool_dry = NULL,
      cool_wet = NULL
    )
  )

  # ── Wire modules ──────────────────────────────────────────────────────
  mod_setup_server("setup",       rv)
  mod_prism_server("prism",       rv)
  mod_future_server("future",     rv)
  mod_explore_server("explore",   rv)
  mod_biascorrect_server("biascorrect", rv)
  mod_wind_server("wind",         rv)
  mod_download_server("download", rv)
}

shinyApp(ui, server)
