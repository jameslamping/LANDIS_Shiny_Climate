# ============================================================
# mod_prism.R  –  Tab 2: Historical PRISM Download & Processing
# ============================================================

PRISM_VARS <- c("tmin", "tmax", "ppt", "tdmean")

mod_prism_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "2 · Historical PRISM",
    icon = icon("cloud-download-alt"),
    layout_sidebar(
      sidebar = sidebar(
        width = 310,
        h5("PRISM Options", class = "text-primary fw-bold"),
        hr(),
        h6("Upload Existing File", class = "text-secondary"),
        fileInput(ns("upload_csv"), "Upload processed PRISM CSV",
                  accept = ".csv", placeholder = "LANDIS wide-format CSV"),
        hr(),
        h6("— OR — Run Download", class = "text-secondary text-center"),
        dateRangeInput(ns("date_range"), "Date Range",
                       start = "1981-01-01", end = "2024-12-31"),
        actionButton(ns("check_cores"), "Check available cores",
                     icon = icon("microchip"),
                     class = "btn-sm btn-outline-secondary w-100 mb-1"),
        uiOutput(ns("cores_info")),
        textInput(ns("workers"), "Parallel Workers",
                  value = max(1L, (parallel::detectCores(logical = FALSE) %||% 4L) - 1L)),
        checkboxGroupInput(ns("vars"), "Variables to Download",
                           choices  = PRISM_VARS,
                           selected = PRISM_VARS),
        actionButton(ns("run_download"), "Start PRISM Download",
                     icon = icon("play"), class = "btn-primary w-100 mb-2"),
        actionButton(ns("stop_download"), "Stop", icon = icon("stop"),
                     class = "btn-danger w-100 mb-2"),
        hr(),
        h6("Fill Missing Dates", class = "text-secondary"),
        numericInput(ns("fill_year_start"), "Start year for LANDIS output", 1989, min = 1981, max = 2024),
        actionButton(ns("run_fill"), "Fill & Format for LANDIS",
                     icon = icon("magic"), class = "btn-success w-100")
      ),

      div(
        uiOutput(ns("status_bar")),
        navset_card_tab(
          nav_panel("Log",
            verbatimTextOutput(ns("log_out")) %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Missing Dates",
            plotOutput(ns("missing_plot"), height = "380px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Tmax by Ecoregion",
            plotOutput(ns("tmax_eco_plot"), height = "420px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Precip by Ecoregion",
            plotOutput(ns("ppt_eco_plot"), height = "420px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Annual Tmax Trend",
            plotOutput(ns("tmax_annual_plot"), height = "420px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Data Preview",
            DTOutput(ns("preview_table")) %>% withSpinner(color = "#2c7bb6")
          )
        )
      )
    )
  )
}

mod_prism_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    log_path <- reactive(file.path(tempdir(), "prism_download.log"))
    bg_proc  <- reactiveVal(NULL)
    prism_df <- reactiveVal(NULL)
    prism_raw_path <- reactiveVal(NULL)

    # ── Worker count: parse text input, clamp to [1, available cores] ─────
    n_workers <- reactive({
      avail <- max(1L, parallel::detectCores(logical = TRUE) %||% 4L)
      req   <- suppressWarnings(as.integer(input$workers))
      if (is.na(req) || req < 1L) req <- 1L
      min(req, avail)
    })

    # ── Report available cores on demand ──────────────────────────────────
    observeEvent(input$check_cores, {
      phys <- parallel::detectCores(logical = FALSE) %||% NA
      logi <- parallel::detectCores(logical = TRUE)  %||% NA
      output$cores_info <- renderUI({
        div(class = "text-muted small mb-1",
            icon("microchip"), " ",
            strong(logi), " logical cores",
            if (!is.na(phys)) paste0(" (", phys, " physical)") else "",
            " available.")
      })
    })

    # ── Upload existing CSV ───────────────────────────────────────────────
    observeEvent(input$upload_csv, {
      req(input$upload_csv)
      withProgress(message = "Reading uploaded CSV...", {
        df <- readr::read_csv(input$upload_csv$datapath, show_col_types = FALSE) %>%
          normalize_clim_variables()
        prism_df(df)
        prism_raw_path(input$upload_csv$datapath)
        rv$prism_csv <- input$upload_csv$datapath
      })
      showNotification("PRISM CSV loaded successfully.", type = "message")
    })

    # ── Background download ───────────────────────────────────────────────
    observeEvent(input$run_download, {
      req(rv$climreg_path, rv$out_dir)

      log_file <- log_path()
      writeLines("=== PRISM Download Started ===", log_file)

      dl_dir  <- file.path(rv$out_dir, "prism_raw")
      out_csv <- file.path(rv$out_dir, paste0("prism_raw_", rv$park, ".csv"))

      script_args <- list(
        start_date   = as.character(input$date_range[1]),
        end_date     = as.character(input$date_range[2]),
        vars         = input$vars,
        climreg_path = rv$climreg_path,
        dl_dir       = dl_dir,
        out_csv      = out_csv,
        log_file     = log_file,
        workers      = n_workers()
      )

      proc <- callr::r_bg(
        func = prism_download_worker,
        args = script_args,
        stderr = log_file,
        stdout = log_file
      )
      bg_proc(proc)
      showNotification("PRISM download started in background.", type = "message")
    })

    observeEvent(input$stop_download, {
      p <- bg_proc()
      if (!is.null(p) && p$is_alive()) {
        p$kill()
        bg_proc(NULL)
        showNotification("Download stopped.", type = "warning")
      }
    })

    # Poll log every 2s while download running
    log_timer <- reactiveTimer(2000)
    output$log_out <- renderText({
      log_timer()
      p <- bg_proc()
      if (!is.null(p) && !p$is_alive()) {
        out_csv <- file.path(rv$out_dir, paste0("prism_raw_", rv$park, ".csv"))
        if (file.exists(out_csv)) {
          df <- readr::read_csv(out_csv, show_col_types = FALSE) %>%
            normalize_clim_variables()
          prism_df(df)
          prism_raw_path(out_csv)
          rv$prism_csv <- out_csv
          bg_proc(NULL)
          showNotification("PRISM download complete!", type = "message")
        }
      }
      tail_log(log_path())
    })

    # ── Fill missing dates & format for LANDIS ───────────────────────────
    observeEvent(input$run_fill, {
      req(prism_df())
      withProgress(message = "Filling missing dates and formatting...", value = 0, {
        df <- prism_df()

        # Detect if already in LANDIS wide format (has Variable column)
        if ("Variable" %in% names(df)) {
          # Already formatted; just ensure year filter and ppt conversion
          df <- df %>% dplyr::filter(Year >= input$fill_year_start)
          eco_cols <- get_eco_cols(df)
          if (max(df[df$Variable == "ppt", eco_cols[1]], na.rm = TRUE) > 5) {
            df[df$Variable == "ppt", eco_cols] <- df[df$Variable == "ppt", eco_cols] / 10
          }
          filled <- df
        } else {
          # Raw zonal format: date, ecoregion, tmin, tmax, ppt, tdmean
          setProgress(0.2, "Pivoting to LANDIS format...")
          filled <- df %>%
            dplyr::filter(lubridate::year(date) >= input$fill_year_start) %>%
            dplyr::mutate(Year  = lubridate::year(date),
                          Month = lubridate::month(date),
                          Day   = lubridate::day(date)) %>%
            tidyr::pivot_longer(cols = dplyr::any_of(c("tmin","tmax","ppt","tdmean")),
                                names_to = "Variable", values_to = "Value") %>%
            dplyr::select(Year, Month, Day, Variable, ecoregion, Value) %>%
            tidyr::pivot_wider(names_from = ecoregion, values_from = Value, names_sort = TRUE) %>%
            dplyr::arrange(Year, Month, Day, Variable) %>%
            dplyr::mutate(Variable = dplyr::case_when(
              Variable == "tdmean" ~ "dewpoint", .default = Variable
            ))

          # Rename eco columns with prefix
          eco_cols <- names(filled)[!(names(filled) %in% c("Year","Month","Day","Variable"))]
          names(filled)[names(filled) %in% eco_cols] <- paste0("eco", eco_cols)

          eco_cols <- get_eco_cols(filled)

          # Fill missing dates
          setProgress(0.5, "Filling missing dates...")
          filled <- fill_missing_dates(filled, input$fill_year_start, eco_cols)

          # ppt mm → cm
          filled[filled$Variable == "ppt", eco_cols] <-
            filled[filled$Variable == "ppt", eco_cols] / 10
        }

        setProgress(0.9, "Writing output...")
        out_path <- file.path(rv$out_dir,
                              paste0("hist_prism_", rv$park, "_",
                                     format(Sys.Date(), "%m%d%Y"), "_filled.csv"))
        readr::write_csv(filled, out_path)
        prism_df(filled)
        rv$prism_csv <- out_path
        showNotification(paste0("Saved: ", basename(out_path)), type = "message")
      })
    })

    # ── Status bar ────────────────────────────────────────────────────────
    output$status_bar <- renderUI({
      if (!is.null(prism_df())) {
        df <- prism_df()
        eco_cols <- get_eco_cols(df)
        n_rows <- nrow(df); n_eco <- length(eco_cols)
        vars   <- if ("Variable" %in% names(df)) paste(unique(df$Variable), collapse = ", ") else "raw"
        div(class = "alert alert-success py-2 mb-3",
            icon("check-circle"), " ",
            strong(nrow(df), " rows"), " | ",
            strong(n_eco, " ecoregions"), " | variables: ", vars)
      } else {
        div(class = "alert alert-info py-2 mb-3",
            icon("info-circle"),
            " Upload an existing PRISM CSV or run the download.")
      }
    })

    # ── Missing dates heatmap ─────────────────────────────────────────────
    output$missing_plot <- renderPlot({
      req(prism_df())
      df <- prism_df()
      req("Variable" %in% names(df))

      expected <- tidyr::expand_grid(
        Date     = seq.Date(as.Date(paste0(min(df$Year), "-01-01")),
                            as.Date(paste0(max(df$Year), "-12-31")), by = "day"),
        Variable = unique(df$Variable)
      ) %>%
        dplyr::mutate(Year = lubridate::year(Date), Month = lubridate::month(Date))

      actual <- df %>%
        dplyr::mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
        dplyr::select(Date, Variable) %>% dplyr::distinct()

      missing <- dplyr::anti_join(expected, actual, by = c("Date","Variable")) %>%
        dplyr::group_by(Year, Month) %>%
        dplyr::summarise(n_missing = dplyr::n(), .groups = "drop")

      if (nrow(missing) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No missing dates!",
                            size = 8, color = "forestgreen") +
          ggplot2::theme_void()
      } else {
        ggplot2::ggplot(missing, ggplot2::aes(x = factor(Month), y = factor(Year),
                                               fill = n_missing)) +
          ggplot2::geom_tile(color = "white") +
          ggplot2::scale_fill_gradient(low = "#fee8c8", high = "#b30000",
                                       name = "Missing\nrows") +
          ggplot2::labs(title = "Missing Date × Variable Combinations",
                        x = "Month", y = "Year") +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(panel.grid = ggplot2::element_blank())
      }
    })

    # ── Monthly tmax by ecoregion ─────────────────────────────────────────
    output$tmax_eco_plot <- renderPlot({
      req(prism_df())
      df  <- prism_df()
      var <- if ("tmax" %in% unique(df$Variable)) "tmax" else "maxtemp"
      req(var %in% unique(df$Variable))
      monthly <- compute_monthly_by_eco(df, var)
      ggplot2::ggplot(monthly, ggplot2::aes(x = month_date, y = value,
                                             color = ecoregion, group = ecoregion)) +
        ggplot2::geom_line(alpha = 0.5, linewidth = 0.4) +
        ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "loess", span = 0.15,
                             se = FALSE, color = "black", linewidth = 1) +
        ggplot2::scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
        ggplot2::scale_color_viridis_d() +
        ggplot2::labs(title = paste(rv$park, "– Monthly Mean Maximum Temperature by Ecoregion"),
                      subtitle = paste0("PRISM 800m, ", min(df$Year), "–", max(df$Year)),
                      x = "Year", y = "Tmax (°C)", color = "Ecoregion") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "right", panel.grid.minor = ggplot2::element_blank())
    })

    # ── Monthly ppt by ecoregion ──────────────────────────────────────────
    output$ppt_eco_plot <- renderPlot({
      req(prism_df())
      df <- prism_df()
      req("ppt" %in% unique(df$Variable))
      monthly <- compute_monthly_by_eco(df, "ppt")
      ggplot2::ggplot(monthly, ggplot2::aes(x = month_date, y = value,
                                             color = ecoregion, group = ecoregion)) +
        ggplot2::geom_line(alpha = 0.5, linewidth = 0.4) +
        ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "loess", span = 0.15,
                             se = FALSE, color = "black", linewidth = 1) +
        ggplot2::scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
        ggplot2::scale_color_viridis_d() +
        ggplot2::labs(title = paste(rv$park, "– Monthly Mean Precipitation by Ecoregion"),
                      subtitle = paste0("PRISM 800m, ", min(df$Year), "–", max(df$Year)),
                      x = "Year", y = "Precipitation (cm/day)", color = "Ecoregion") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "right", panel.grid.minor = ggplot2::element_blank())
    })

    # ── Annual mean tmax trend (landscape mean + per ecoregion) ──────────
    output$tmax_annual_plot <- renderPlot({
      req(prism_df())
      df  <- prism_df()
      var <- if ("tmax" %in% unique(df$Variable)) "tmax" else "maxtemp"
      req(var %in% unique(df$Variable))

      eco_cols <- get_eco_cols(df)
      annual_eco <- df %>%
        dplyr::filter(Variable == var) %>%
        tidyr::pivot_longer(dplyr::all_of(eco_cols), names_to = "ecoregion", values_to = "value") %>%
        dplyr::group_by(ecoregion, Year) %>%
        dplyr::summarise(annual_mean = mean(value, na.rm = TRUE), .groups = "drop")

      annual_mean <- annual_eco %>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(landscape_mean = mean(annual_mean, na.rm = TRUE), .groups = "drop")

      ggplot2::ggplot() +
        ggplot2::geom_line(data = annual_eco,
                           ggplot2::aes(x = Year, y = annual_mean,
                                        color = ecoregion, group = ecoregion),
                           alpha = 0.5, linewidth = 0.5) +
        ggplot2::geom_line(data = annual_mean,
                           ggplot2::aes(x = Year, y = landscape_mean),
                           color = "black", linewidth = 1.2, linetype = "dashed") +
        ggplot2::geom_smooth(data = annual_mean,
                             ggplot2::aes(x = Year, y = landscape_mean),
                             method = "lm", se = TRUE, color = "#b30000",
                             linewidth = 1.1, fill = "#fee0d2") +
        ggplot2::scale_color_viridis_d() +
        ggplot2::labs(title = paste(rv$park, "– Annual Mean Tmax by Ecoregion"),
                      subtitle = "Black dashed = landscape mean | Red = linear trend",
                      x = "Year", y = "Annual Mean Tmax (°C)", color = "Ecoregion") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "right", panel.grid.minor = ggplot2::element_blank())
    })

    # ── Data preview table ────────────────────────────────────────────────
    output$preview_table <- renderDT({
      req(prism_df())
      DT::datatable(head(prism_df(), 200),
                    options = list(scrollX = TRUE, pageLength = 15, dom = "tip"),
                    rownames = FALSE)
    })
  })
}

# ── Background worker function (runs in separate R process via callr) ─────────
prism_download_worker <- function(start_date, end_date, vars, climreg_path,
                                  dl_dir, out_csv, log_file, workers) {
  suppressPackageStartupMessages({
    library(terra); library(httr); library(glue); library(future)
    library(future.apply); library(progressr); library(tidyverse)
  })
  options(scipen = 999)
  cat("=== Worker started ===\n", file = log_file, append = TRUE)

  dir.create(dl_dir, recursive = TRUE, showWarnings = FALSE)

  download_prism_day <- function(variable, date, out_dir, retries = 3, timeout_sec = 90) {
    date <- as.Date(date)
    url <- paste0("https://services.nacse.org/prism/data/get/us/800m/",
                  variable, "/", format(date, "%Y%m%d"))
    zip_path <- file.path(out_dir, paste0(variable, "_", format(date, "%Y%m%d"), ".zip"))
    if (!file.exists(zip_path)) {
      for (att in seq_len(retries)) {
        resp <- try(GET(url, write_disk(zip_path, overwrite = TRUE), timeout(timeout_sec)), silent = TRUE)
        if (inherits(resp, "response") && resp$status_code == 200) break
        Sys.sleep(3 * att)
        if (att == retries) { cat("FAIL:", url, "\n", file = log_file, append = TRUE); return(NULL) }
      }
    }
    unzip_dir <- file.path(out_dir, paste0(variable, "_", format(date, "%Y%m%d")))
    if (!dir.exists(unzip_dir)) try(unzip(zip_path, exdir = unzip_dir), silent = TRUE)
    unzip_dir
  }

  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  ecoregion_raster <- rast(climreg_path)

  plan(multisession, workers = workers)
  all_results <- future_lapply(dates, function(date) {
    library(terra); library(httr)
    ecoregion_raster <- rast(climreg_path)
    day_stack <- list()
    for (v in vars) {
      folder <- download_prism_day(v, date, dl_dir)
      if (is.null(folder)) return(NULL)
      tif <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
      if (length(tif) == 0) return(NULL)
      r <- rast(tif)
      try(terra::NAflag(r) <- -9999, silent = TRUE)
      if (v == "ppt") r[r < 0] <- NA
      r <- project(r, crs(ecoregion_raster))
      r <- resample(r, ecoregion_raster, method = "bilinear")
      if (v == "ppt") r[r < 0] <- 0
      names(r) <- v
      day_stack[[v]] <- r
    }
    if (length(day_stack) < length(vars)) return(NULL)
    stack <- rast(day_stack)
    z <- as.data.frame(terra::zonal(stack, ecoregion_raster, fun = "mean", na.rm = TRUE))
    names(z)[1] <- "ecoregion"
    z$date <- date
    # cleanup
    for (v in vars) {
      try(unlink(file.path(dl_dir, paste0(v,"_",format(as.Date(date),"%Y%m%d"))), recursive=TRUE), silent=TRUE)
      try(unlink(file.path(dl_dir, paste0(v,"_",format(as.Date(date),"%Y%m%d"),".zip"))), silent=TRUE)
    }
    z
  }, future.seed = TRUE)

  result <- dplyr::bind_rows(all_results) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::select(date, ecoregion, dplyr::any_of(vars))

  readr::write_csv(result, out_csv)
  cat("=== DONE. Written:", out_csv, "\n", file = log_file, append = TRUE)
}

# ── Fill missing dates via same-month/day sampling ───────────────────────────
fill_missing_dates <- function(df, start_year, eco_cols) {
  df <- df %>% dplyr::mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))
  expected <- tidyr::expand_grid(
    Date     = seq.Date(as.Date(paste0(start_year, "-01-01")),
                        as.Date(paste0(max(df$Year), "-12-31")), by = "day"),
    Variable = unique(df$Variable)
  ) %>%
    dplyr::mutate(Year  = lubridate::year(Date),
                  Month = lubridate::month(Date),
                  Day   = lubridate::day(Date))

  actual  <- df %>% dplyr::select(Date, Variable) %>% dplyr::distinct()
  missing <- dplyr::anti_join(expected, actual, by = c("Date","Variable"))

  if (nrow(missing) == 0) return(df %>% dplyr::select(-Date))

  df2 <- df %>%
    dplyr::mutate(Month_n = lubridate::month(Date),
                  Day_n   = lubridate::day(Date))

  filled_rows <- missing %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Fill_Row = list({
      cur_var  <- Variable; cur_date <- Date
      pool <- df2 %>%
        dplyr::filter(Variable == cur_var,
                      Month_n  == lubridate::month(cur_date),
                      Day_n    == lubridate::day(cur_date))
      if (nrow(pool) == 0) return(NULL)
      dplyr::sample_n(pool, 1) %>%
        dplyr::mutate(Year = lubridate::year(cur_date),
                      Month = lubridate::month(cur_date),
                      Day   = lubridate::day(cur_date),
                      Date  = cur_date)
    })) %>%
    tidyr::unnest(Fill_Row, names_sep = "_")

  filled_clean <- filled_rows %>%
    dplyr::transmute(
      Year     = lubridate::year(Date),
      Month    = lubridate::month(Date),
      Day      = lubridate::day(Date),
      Variable = Variable,
      Date     = Date,
      dplyr::across(dplyr::starts_with("Fill_Row_eco"),
                    .names = "{stringr::str_remove(.col, 'Fill_Row_')}")
    )

  dplyr::bind_rows(
    df %>% dplyr::select(-Date),
    filled_clean %>% dplyr::select(-Date)
  ) %>% dplyr::arrange(Year, Month, Day, Variable)
}
