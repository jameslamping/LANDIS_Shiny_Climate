# ============================================================
# mod_download.R  –  Tab 7: Download Summary & Export
# ============================================================

mod_download_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "7 · Download",
    icon = icon("file-download"),
    div(
      class = "container-fluid py-3",
      h4("Download LANDIS-Ready Climate Files"),
      p(class = "text-muted",
        "All files produced during this session are listed below.",
        " Use the buttons to download individual files or the full batch as a zip archive."),
      hr(),
      uiOutput(ns("file_cards")),
      hr(),
      h5("Batch Download"),
      layout_columns(
        col_widths = c(4, 4, 4),
        card(
          card_header("Historical Climate"),
          card_body(
            p(class = "small text-muted",
              "Historical PRISM with GRIDMET wind — ready for LANDIS."),
            downloadButton(ns("dl_hist"), "Download Historical CSV",
                           class = "btn-outline-primary w-100")
          )
        ),
        card(
          card_header("Future Climate"),
          card_body(
            p(class = "small text-muted",
              "All bias-corrected future CSVs with wind appended."),
            downloadButton(ns("dl_future_zip"), "Download Future CSVs (.zip)",
                           class = "btn-outline-primary w-100")
          )
        ),
        card(
          card_header("Session Summary"),
          card_body(
            p(class = "small text-muted",
              "Text summary of all settings, selected models, and file paths."),
            downloadButton(ns("dl_summary"), "Download Summary (.txt)",
                           class = "btn-outline-secondary w-100")
          )
        )
      )
    )
  )
}

mod_download_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Collect all output files ──────────────────────────────────────────
    all_files <- reactive({
      paths <- list()

      if (!is.null(rv$prism_csv) && file.exists(rv$prism_csv))
        paths[["Historical PRISM"]] <- rv$prism_csv

      if (!is.null(rv$wind_hist_csv) && file.exists(rv$wind_hist_csv))
        paths[["Historical PRISM + Wind"]] <- rv$wind_hist_csv

      if (!is.null(rv$biascorr_dir) && dir.exists(rv$biascorr_dir)) {
        bcs <- list.files(rv$biascorr_dir, "\\.csv$", full.names = TRUE)
        bcs <- bcs[!grepl("^_", basename(bcs))]
        for (f in bcs) paths[[paste0("Future BC: ", basename(f))]] <- f
      }

      wind_fut_dir <- if (!is.null(rv$out_dir))
                        file.path(rv$out_dir, "future_with_wind")
                      else NULL
      if (!is.null(wind_fut_dir) && dir.exists(wind_fut_dir)) {
        wfs <- list.files(wind_fut_dir, "\\.csv$", full.names = TRUE)
        for (f in wfs) paths[[paste0("Future + Wind: ", basename(f))]] <- f
      }

      paths
    })

    # ── File cards ────────────────────────────────────────────────────────
    output$file_cards <- renderUI({
      fl <- all_files()
      if (length(fl) == 0) {
        return(div(class = "alert alert-warning",
                   icon("exclamation-triangle"),
                   " No output files found yet. Complete earlier tabs to generate files."))
      }

      cards <- purrr::imap(fl, function(path, label) {
        sz_mb <- round(file.size(path) / 1e6, 1)
        df    <- tryCatch(
          readr::read_csv(path, show_col_types = FALSE, n_max = 5),
          error = function(e) NULL
        )
        n_rows <- tryCatch(nrow(readr::read_csv(path, show_col_types = FALSE)), error = function(e) "?")

        card(
          class = "mb-2",
          card_header(icon("file-csv"), " ", label),
          card_body(
            layout_columns(
              col_widths = c(8, 4),
              tags$table(class = "table table-sm mb-0",
                tags$tr(tags$th("File"),  tags$td(code(basename(path)))),
                tags$tr(tags$th("Size"),  tags$td(paste(sz_mb, "MB"))),
                tags$tr(tags$th("Rows"),  tags$td(scales::comma(n_rows))),
                tags$tr(tags$th("Path"),  tags$td(code(dirname(path))))
              ),
              div(
                class = "d-flex align-items-center h-100",
                downloadButton(
                  ns(paste0("dl_", make.names(label))),
                  "Download",
                  class = "btn-sm btn-outline-primary w-100"
                )
              )
            )
          )
        )
      })
      tagList(cards)
    })

    # ── Historical CSV download ───────────────────────────────────────────
    output$dl_hist <- downloadHandler(
      filename = function() {
        base <- rv$wind_hist_csv %||% rv$prism_csv
        if (!is.null(base)) basename(base) else paste0(rv$park, "_historical_climate.csv")
      },
      content = function(file) {
        src <- rv$wind_hist_csv %||% rv$prism_csv
        req(!is.null(src), file.exists(src))
        file.copy(src, file)
      }
    )

    # ── Future zip download ───────────────────────────────────────────────
    output$dl_future_zip <- downloadHandler(
      filename = function() {
        paste0(rv$park %||% "LANDIS", "_future_climate_", format(Sys.Date(), "%m%d%Y"), ".zip")
      },
      content = function(file) {
        wind_dir <- if (!is.null(rv$out_dir)) file.path(rv$out_dir, "future_with_wind") else NULL
        bc_dir   <- rv$biascorr_dir

        src_files <- character(0)
        if (!is.null(wind_dir) && dir.exists(wind_dir))
          src_files <- c(src_files, list.files(wind_dir, "\\.csv$", full.names = TRUE))
        else if (!is.null(bc_dir) && dir.exists(bc_dir))
          src_files <- c(src_files, list.files(bc_dir, "\\.csv$", full.names = TRUE))

        src_files <- src_files[!grepl("^_", basename(src_files))]
        req(length(src_files) > 0)

        tmpdir <- tempfile()
        dir.create(tmpdir)
        file.copy(src_files, tmpdir)
        zip::zip(file, files = list.files(tmpdir, full.names = TRUE), mode = "cherry-pick")
      }
    )

    # ── Session summary download ──────────────────────────────────────────
    output$dl_summary <- downloadHandler(
      filename = function() {
        paste0(rv$park %||% "LANDIS", "_climate_session_summary_",
               format(Sys.Date(), "%m%d%Y"), ".txt")
      },
      content = function(file) {
        lines <- c(
          "====================================================",
          "LANDIS-II Climate Preparation — Session Summary",
          paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          "====================================================",
          "",
          paste0("Park / Landscape : ", rv$park %||% "not set"),
          paste0("Output directory : ", rv$out_dir %||% "not set"),
          paste0("Climate regions  : ", rv$climreg_path %||% "not set"),
          "",
          "--- Files Produced ---",
          paste0("Historical PRISM : ", rv$prism_csv %||% "none"),
          paste0("Historical + Wind: ", rv$wind_hist_csv %||% "none"),
          paste0("Future dir       : ", rv$future_dir %||% "none"),
          paste0("Bias-corrected   : ", rv$biascorr_dir %||% "none"),
          "",
          "--- Decision Models (Four Corners) ---",
          paste0("Warm / Dry : ", rv$selected_models$warm_dry %||% "not selected"),
          paste0("Warm / Wet : ", rv$selected_models$warm_wet %||% "not selected"),
          paste0("Cool / Dry : ", rv$selected_models$cool_dry %||% "not selected"),
          paste0("Cool / Wet : ", rv$selected_models$cool_wet %||% "not selected"),
          ""
        )
        writeLines(lines, file)
      }
    )
  })
}
