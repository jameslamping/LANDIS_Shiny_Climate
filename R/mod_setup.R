# ============================================================
# mod_setup.R  –  Tab 1: Project Setup
# ============================================================

mod_setup_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "1 · Setup",
    icon = icon("sliders"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        h5("Project Configuration", class = "text-primary fw-bold"),
        hr(),
        textInput(ns("park"), "Landscape / Park Code", placeholder = "e.g. MORA, OLYM, NOCA"),
        textInput(ns("out_dir"), "Output Directory (full path)",
                  placeholder = "/path/to/output/folder"),
        actionButton(ns("browse_out"), "Browse...", icon = icon("folder-open"),
                     class = "btn-sm btn-outline-secondary mb-2"),
        hr(),
        h6("Climate Regions Raster (.tif)", class = "text-secondary"),
        fileInput(ns("climreg_file"), NULL, accept = c(".tif", ".tiff"),
                  placeholder = "Upload .tif"),
        hr(),
        actionButton(ns("confirm"), "Confirm Setup", icon = icon("check-circle"),
                     class = "btn-success w-100")
      ),

      # ── Main panel ────────────────────────────────────────────────────────
      div(
        h4("LANDIS-II Climate Library Preparation", class = "mb-1"),
        p(class = "text-muted",
          "This app guides you through downloading, processing, and assembling",
          "daily climate inputs for LANDIS-II landscape simulations.",
          "Each tab can run independently — you may upload pre-processed files",
          "at any step. Complete the Setup tab first to establish your project context."),

        div(class = "alert alert-warning d-flex align-items-start",
          icon("triangle-exclamation", class = "me-2 mt-1"),
          div(
            tags$b("Data coverage by tab."),
            " The ", tags$b("Future Climate"), " tab uses NASA NEX-GDDP-CMIP6,",
            " which is ", tags$b("global"), " — so this app can process future climate",
            " for LANDIS landscapes anywhere in the world.",
            " However, the ", tags$b("Historical PRISM"), " (PRISM 800m) and ",
            tags$b("GRIDMET Wind"), " tabs cover the ",
            tags$b("continental US (CONUS) only"), ".",
            tags$br(),
            tags$br(),
            "If your landscape is outside CONUS, you can still use this app for the",
            " future-climate workflow: obtain your historical climate and wind data",
            " from another source, format them to the LANDIS wide layout, and upload",
            " them on the Historical PRISM and GRIDMET Wind tabs (each tab accepts",
            " pre-processed uploads). Bias correction will then align the future models",
            " to your uploaded historical baseline.",
            tags$br(),
            tags$br(),
            "Your climate regions raster must carry a valid coordinate reference",
            " system (CRS) so the app can locate it geographically."
          )
        ),
        hr(),

        layout_columns(
          col_widths = c(6, 6),

          card(
            card_header("Workflow Overview"),
            card_body(
              tags$ol(class = "ps-3",
                tags$li(tags$b("Setup"), " — landscape code, output folder, climate regions raster"),
                tags$li(tags$b("Historical PRISM"), " — download & process 800m daily climate (1981–present)"),
                tags$li(tags$b("Future Climate"), " — NASA NEX-GDDP-CMIP6 download & ecoregion extraction"),
                tags$li(tags$b("Explore & Select"), " — four-corners model selection, time-series visualizations"),
                tags$li(tags$b("Bias Correction"), " — align future models to PRISM historical baseline"),
                tags$li(tags$b("GRIDMET Wind"), " — append wind speed & direction to all climate files"),
                tags$li(tags$b("Download"), " — export LANDIS-ready CSVs")
              )
            )
          ),

          card(
            card_header("Required R Packages"),
            card_body(
              tags$pre(class = "small bg-light p-2 rounded",
"# Run once to install all dependencies:
install.packages(c(
  'shiny','bslib','tidyverse','lubridate',
  'terra','tidyterra','sf','glue','ggrepel',
  'scales','patchwork','plotly','DT',
  'shinyjs','shinycssloaders',
  'climateR','httr','aws.s3',
  'furrr','future','future.apply',
  'callr','progressr'
))")
            )
          )
        ),

        hr(),
        uiOutput(ns("setup_status"))
      )
    )
  )
}

mod_setup_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Output dir browser (uses rstudioapi if available) ─────────────────
    observeEvent(input$browse_out, {
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        chosen <- rstudioapi::selectDirectory("Select output directory")
        if (!is.null(chosen)) updateTextInput(session, "out_dir", value = chosen)
      } else {
        showNotification("Directory browser unavailable — type the path manually.", type = "warning")
      }
    })

    # ── Raster preview ────────────────────────────────────────────────────
    r_raster <- reactive({
      req(input$climreg_file)
      terra::rast(input$climreg_file$datapath)
    })

    # ── Confirm ───────────────────────────────────────────────────────────
    observeEvent(input$confirm, {
      park    <- trimws(input$park)
      out_dir <- trimws(input$out_dir)

      if (nchar(park) == 0) {
        showNotification("Please enter a landscape/park code.", type = "error"); return()
      }
      if (nchar(out_dir) == 0) {
        showNotification("Please specify an output directory.", type = "error"); return()
      }
      if (is.null(input$climreg_file)) {
        showNotification("Please upload the climate regions raster.", type = "error"); return()
      }

      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      # Copy uploaded raster to project dir so other modules can find it
      rast_dest <- file.path(out_dir, paste0(park, "_ClimateRegions.tif"))
      file.copy(input$climreg_file$datapath, rast_dest, overwrite = TRUE)

      rv$park         <- park
      rv$out_dir      <- out_dir
      rv$climreg_path <- rast_dest

      showNotification(paste0("Setup confirmed for ", park, "."), type = "message")
    })

    # ── Status card ───────────────────────────────────────────────────────
    output$setup_status <- renderUI({
      if (is.null(rv$park)) return(NULL)

      r <- tryCatch(terra::rast(rv$climreg_path), error = function(e) NULL)
      n_eco <- if (!is.null(r)) length(unique(terra::values(r, na.rm = TRUE))) else "?"

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(icon("check-circle", class = "text-success"), " Project Active",
                      class = "bg-success-subtle"),
          card_body(
            tags$table(class = "table table-sm mb-0",
              tags$tr(tags$th("Park"),    tags$td(rv$park)),
              tags$tr(tags$th("Output"),  tags$td(code(rv$out_dir))),
              tags$tr(tags$th("Raster"),  tags$td(code(basename(rv$climreg_path)))),
              tags$tr(tags$th("Eco regions"), tags$td(n_eco))
            )
          )
        ),

        card(
          card_header("Climate Regions Map"),
          card_body(
            plotOutput(ns("rast_plot"), height = "220px") %>% withSpinner(color = "#2c7bb6")
          )
        )
      )
    })

    output$rast_plot <- renderPlot({
      req(rv$climreg_path)
      r <- terra::rast(rv$climreg_path)
      terra::plot(r, main = paste(rv$park, "Climate Regions"),
                  col = viridisLite::viridis(length(unique(terra::values(r, na.rm = TRUE)))),
                  legend = TRUE, axes = FALSE, box = FALSE)
    })
  })
}
