# ============================================================
# mod_wind.R  –  Tab 6: GRIDMET Wind Download & Append
# ============================================================

mod_wind_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "6 · GRIDMET Wind",
    icon = icon("wind"),
    layout_sidebar(
      sidebar = sidebar(
        width = 310,
        h5("GRIDMET Wind Settings", class = "text-primary fw-bold"),
        hr(),
        h6("Upload Existing Wind CSV (optional)", class = "text-secondary"),
        fileInput(ns("wind_long_csv"), "Pre-downloaded wind CSV (long format)",
                  accept = ".csv"),
        hr(),
        h6("— OR — Download Wind Data", class = "text-secondary text-center"),
        dateRangeInput(ns("wind_dates"), "Date Range",
                       start = "1989-01-01",
                       end   = "2024-12-31"),
        p(class = "text-muted small",
          "Download uses the", code("climateR"), "package. By default the AOI is",
          " derived from the extent of your climate regions raster."),
        textInput(ns("wind_state"), "State (optional)",
                  placeholder = "e.g. WA — leave blank to use raster extent"),
        p(class = "text-muted small",
          "Providing a state uses", code("AOI::aoi_get()"), "instead of the raster",
          " extent. This requires the", code("AOI"), "package to be installed."),
        actionButton(ns("run_wind_download"), "Download GRIDMET Wind",
                     icon = icon("download"), class = "btn-primary w-100 mb-2"),
        hr(),
        h5("Append Wind to Climate Files", class = "text-primary fw-bold"),
        h6("Historical CSV", class = "text-secondary"),
        fileInput(ns("hist_csv"), "Historical PRISM CSV (override)",
                  accept = ".csv"),
        actionButton(ns("append_historical"), "Append Wind → Historical",
                     icon = icon("plus-circle"), class = "btn-success w-100 mb-2"),
        hr(),
        h6("Future CSVs", class = "text-secondary"),
        textInput(ns("fut_dir"), "Future CSV Directory (override)",
                  placeholder = "Defaults to bias-corrected dir"),
        numericInput(ns("wind_seed"), "Random Seed", value = 42, min = 1),
        actionButton(ns("append_future"), "Append Wind → Future Files",
                     icon = icon("plus-circle"), class = "btn-success w-100")
      ),

      div(
        uiOutput(ns("status_bar")),
        navset_card_tab(
          nav_panel("Log",
            verbatimTextOutput(ns("log_out")) %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Wind Speed by Ecoregion",
            plotOutput(ns("wind_speed_plot"), height = "430px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Seasonal Wind Speed",
            plotOutput(ns("wind_seasonal_plot"), height = "430px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Wind Direction",
            plotOutput(ns("wind_dir_plot"), height = "430px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Monthly Heatmap",
            plotOutput(ns("wind_heatmap"), height = "430px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Donor Year Table",
            DTOutput(ns("donor_table")) %>% withSpinner(color = "#2c7bb6")
          )
        )
      )
    )
  )
}

mod_wind_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    log_path  <- reactive(file.path(tempdir(), "wind.log"))
    bg_proc   <- reactiveVal(NULL)
    wind_long <- reactiveVal(NULL)

    # ── Upload existing wind CSV ──────────────────────────────────────────
    observeEvent(input$wind_long_csv, {
      req(input$wind_long_csv)
      df <- readr::read_csv(input$wind_long_csv$datapath, show_col_types = FALSE) %>%
        dplyr::mutate(date = as.Date(date))
      wind_long(df)
      showNotification("Wind CSV loaded.", type = "message")
    })

    # ── Download GRIDMET wind ─────────────────────────────────────────────
    observeEvent(input$run_wind_download, {
      req(rv$climreg_path, rv$out_dir, rv$park)
      log_file <- log_path()
      writeLines("=== GRIDMET Wind Download Started ===", log_file)
      out_csv  <- file.path(rv$out_dir,
                             paste0(rv$park, "_gridmet_wind_long_",
                                    format(Sys.Date(), "%m%d%Y"), ".csv"))

      proc <- callr::r_bg(
        func = function(climreg_path, start_date, end_date, out_csv, log_file, state) {
          suppressPackageStartupMessages({
            library(climateR); library(terra); library(tidyverse); library(lubridate)
          })
          cat("Downloading GRIDMET wind speed...\n", file = log_file, append = TRUE)

          r_climreg <- rast(climreg_path)

          if (nzchar(state)) {
            if (!requireNamespace("AOI", quietly = TRUE)) {
              stop("State '", state, "' requested but the AOI package is not installed. ",
                   "Install AOI or leave the State field blank to use the raster extent.")
            }
            cat("Using AOI from state:", state, "\n", file = log_file, append = TRUE)
            aoi <- AOI::aoi_get(state = state, county = "all")
          } else {
            cat("Deriving AOI from climate regions raster extent...\n",
                file = log_file, append = TRUE)
            if (crs(r_climreg) == "") {
              stop("Climate regions raster has no CRS — cannot derive AOI. ",
                   "Assign a coordinate reference system to your raster, or supply a State.")
            }
            # Project raster extent to WGS84 lon/lat and build an AOI polygon.
            # Buffer by ~0.1 deg (several GRIDMET cells): if the landscape is
            # smaller than one GRIDMET pixel (~4 km / 0.0417 deg), getGridMET
            # returns degenerate point data instead of a grid and rast() fails
            # with "missing value where TRUE/FALSE needed". The buffer is
            # harmless — the data is cropped and resampled back to the raster
            # below before zonal statistics are computed.
            r_wgs <- project(r_climreg, "epsg:4326")
            e     <- ext(r_wgs)
            buf   <- 0.1
            aoi   <- as.polygons(ext(e$xmin - buf, e$xmax + buf,
                                     e$ymin - buf, e$ymax + buf),
                                 crs = "epsg:4326")
          }

          grid_vs <- getGridMET(AOI = aoi, varname = "vs",
                                 startDate = start_date, endDate = end_date)
          grid_th <- getGridMET(AOI = aoi, varname = "th",
                                 startDate = start_date, endDate = end_date)

          vs <- rast(grid_vs); th <- rast(grid_th)
          r_ll <- project(r_climreg, crs(vs), method = "near")
          vs <- vs %>% crop(r_ll) %>% resample(r_ll, method = "bilinear", threads = TRUE)
          th <- th %>% crop(r_ll) %>% resample(r_ll, method = "bilinear", threads = TRUE)

          zs_vs <- zonal(vs, r_ll, fun = "mean", na.rm = TRUE)
          zs_th <- zonal(th, r_ll, fun = "mean", na.rm = TRUE)

          speed_long <- zs_vs %>% as_tibble() %>%
            rename(eco_id = 1) %>% mutate(eco = paste0("eco", eco_id)) %>%
            pivot_longer(cols = -c(eco_id, eco), names_to = "date", values_to = "windSpeed") %>%
            mutate(date = as.Date(sub("^vs_", "", date))) %>%
            select(date, eco, windSpeed)

          dir_long <- zs_th %>% as_tibble() %>%
            rename(eco_id = 1) %>% mutate(eco = paste0("eco", eco_id)) %>%
            pivot_longer(cols = -c(eco_id, eco), names_to = "date", values_to = "windDirection") %>%
            mutate(date = as.Date(sub("^th_", "", date))) %>%
            select(date, eco, windDirection)

          wind_hist_long <- speed_long %>%
            left_join(dir_long, by = c("date","eco")) %>%
            arrange(date)

          write_csv(wind_hist_long, out_csv)
          cat("Saved:", out_csv, "\n", file = log_file, append = TRUE)
        },
        args = list(
          climreg_path = rv$climreg_path,
          start_date   = as.character(input$wind_dates[1]),
          end_date     = as.character(input$wind_dates[2]),
          out_csv      = out_csv,
          log_file     = log_file,
          state        = trimws(input$wind_state %||% "")
        ),
        stderr = log_file, stdout = log_file
      )
      bg_proc(proc)
      showNotification("GRIDMET download started.", type = "message")
    })

    # ── Poll log ──────────────────────────────────────────────────────────
    log_timer <- reactiveTimer(2000)
    output$log_out <- renderText({
      log_timer()
      p <- bg_proc()
      if (!is.null(p) && !p$is_alive()) {
        wind_files <- list.files(rv$out_dir %||% tempdir(),
                                  pattern = paste0(rv$park %||% "", "_gridmet_wind_long.*\\.csv$"),
                                  full.names = TRUE)
        if (length(wind_files) > 0) {
          df <- readr::read_csv(wind_files[1], show_col_types = FALSE) %>%
            dplyr::mutate(date = as.Date(date))
          wind_long(df)
        }
        bg_proc(NULL)
        showNotification("GRIDMET download complete!", type = "message")
      }
      tail_log(log_path())
    })

    # ── Append wind to historical ─────────────────────────────────────────
    observeEvent(input$append_historical, {
      wl <- wind_long(); req(!is.null(wl))
      hist_path <- if (!is.null(input$hist_csv)) input$hist_csv$datapath else rv$prism_csv
      req(!is.null(hist_path))

      withProgress(message = "Appending wind to historical CSV...", {
        clim_hist <- readr::read_csv(hist_path, show_col_types = FALSE)
        eco_cols  <- get_eco_cols(clim_hist)

        wl2 <- wl %>%
          dplyr::mutate(
            hist_year = lubridate::year(date),
            doy       = lubridate::yday(date),
            is_leap   = lubridate::leap_year(hist_year)
          )

        ws_wide <- wind_long_to_wide(wl, "windSpeed",    eco_cols)
        wd_wide <- wind_long_to_wide(wl, "windDirection", eco_cols)

        clim_out <- clim_hist %>%
          add_variable_rows("windSpeed",    ws_wide, eco_cols) %>%
          add_variable_rows("windDirection", wd_wide, eco_cols) %>%
          dplyr::arrange(Year, Month, Day, Variable)

        out_path <- file.path(
          rv$out_dir %||% dirname(hist_path),
          paste0("hist_prism_with_GRIDMET_wind_", rv$park %||% "PARK",
                 "_", format(Sys.Date(), "%m%d%Y"), ".csv")
        )
        readr::write_csv(clim_out, out_path)
        rv$wind_hist_csv <- out_path
        showNotification(paste0("Saved: ", basename(out_path)), type = "message")
      })
    })

    # ── Append wind to future files ───────────────────────────────────────
    observeEvent(input$append_future, {
      wl <- wind_long(); req(!is.null(wl))
      fut_path <- if (nchar(trimws(input$fut_dir)) > 0) trimws(input$fut_dir)
                  else rv$biascorr_dir %||% rv$future_dir
      req(!is.null(fut_path), dir.exists(fut_path))

      set.seed(input$wind_seed)

      log_file <- log_path()
      writeLines("=== Appending wind to future files ===", log_file)

      withProgress(message = "Appending wind to future files...", value = 0, {
        future_files <- list.files(fut_path, pattern = "\\.csv$", full.names = TRUE)
        future_files <- future_files[!grepl("^_", basename(future_files))]

        wl2 <- wl %>%
          dplyr::mutate(
            hist_year = lubridate::year(date),
            doy       = lubridate::yday(date),
            is_leap   = lubridate::leap_year(hist_year)
          )
        hist_year_table <- wl2 %>% dplyr::distinct(hist_year, is_leap)

        out_dir_fut <- file.path(rv$out_dir, "future_with_wind")
        dir.create(out_dir_fut, recursive = TRUE, showWarnings = FALSE)

        for (i in seq_along(future_files)) {
          fp <- future_files[i]
          setProgress(i / length(future_files), paste("Processing", basename(fp)))
          cat("Processing:", basename(fp), "\n", file = log_file, append = TRUE)

          clim_fut <- readr::read_csv(fp, show_col_types = FALSE)
          eco_cols <- get_eco_cols(clim_fut)

          clim_dates  <- lubridate::make_date(clim_fut$Year, clim_fut$Month, clim_fut$Day)
          future_years <- sort(unique(lubridate::year(clim_dates)))

          future_year_table <- tibble::tibble(
            future_year = future_years,
            is_leap     = lubridate::leap_year(future_year)
          )

          donor_table <- future_year_table %>%
            dplyr::rowwise() %>%
            dplyr::mutate(donor_year = sample(
              hist_year_table$hist_year[hist_year_table$is_leap == is_leap], 1)) %>%
            dplyr::ungroup() %>%
            dplyr::select(future_year, donor_year)

          wind_future_long <- tidyr::expand_grid(
            date = unique(clim_dates),
            eco  = unique(wl2$eco)
          ) %>%
            dplyr::mutate(future_year = lubridate::year(date),
                          doy         = lubridate::yday(date)) %>%
            dplyr::left_join(donor_table, by = "future_year") %>%
            dplyr::left_join(
              wl2 %>% dplyr::select(hist_year, doy, eco, windSpeed, windDirection),
              by = c("donor_year" = "hist_year", "doy", "eco")
            ) %>%
            dplyr::select(date, eco, windSpeed, windDirection)

          ws_wide <- wind_long_to_wide(wind_future_long, "windSpeed",    eco_cols)
          wd_wide <- wind_long_to_wide(wind_future_long, "windDirection", eco_cols)

          clim_out <- clim_fut %>%
            dplyr::filter(!Variable %in% c("windSpeed","windDirection")) %>%
            add_variable_rows("windSpeed",     ws_wide, eco_cols) %>%
            add_variable_rows("windDirection", wd_wide, eco_cols) %>%
            dplyr::arrange(Year, Month, Day, Variable)

          out_fn <- paste0(tools::file_path_sans_ext(basename(fp)),
                           "_withGRIDMETwind_", format(Sys.Date(), "%m%d%Y"), ".csv")
          readr::write_csv(clim_out, file.path(out_dir_fut, out_fn))
        }
      })
      showNotification(paste0(length(future_files), " future files updated with wind."),
                       type = "message")
    })

    # ── Status bar ────────────────────────────────────────────────────────
    output$status_bar <- renderUI({
      wl <- wind_long()
      if (!is.null(wl)) {
        div(class = "alert alert-success py-2 mb-3",
            icon("check-circle"), " Wind data loaded: ",
            strong(format(min(wl$date), "%Y-%m-%d"), " – ", format(max(wl$date), "%Y-%m-%d")),
            " | ", strong(length(unique(wl$eco)), " ecoregions"))
      } else {
        div(class = "alert alert-info py-2 mb-3",
            icon("info-circle"),
            " Upload a wind CSV or click 'Download GRIDMET Wind'.")
      }
    })

    # ── Wind speed time series by ecoregion ──────────────────────────────
    output$wind_speed_plot <- renderPlot({
      wl <- wind_long(); req(!is.null(wl))
      monthly <- wl %>%
        dplyr::mutate(month_date = as.Date(paste(lubridate::year(date),
                                                  lubridate::month(date), "15", sep = "-"))) %>%
        dplyr::group_by(eco, month_date) %>%
        dplyr::summarise(windSpeed = mean(windSpeed, na.rm = TRUE), .groups = "drop")

      ggplot2::ggplot(monthly, ggplot2::aes(x = month_date, y = windSpeed,
                                             color = eco, group = eco)) +
        ggplot2::geom_line(alpha = 0.4, linewidth = 0.4) +
        ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "loess", span = 0.1,
                             se = FALSE, color = "black", linewidth = 1) +
        ggplot2::scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
        ggplot2::scale_color_viridis_d() +
        ggplot2::labs(title = "Monthly Mean Wind Speed by Ecoregion",
                      subtitle = "GRIDMET; black line = landscape mean",
                      x = "Year", y = "Wind Speed (m/s)", color = "Ecoregion") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "right", panel.grid.minor = ggplot2::element_blank())
    })

    # ── Seasonal wind speed ───────────────────────────────────────────────
    output$wind_seasonal_plot <- renderPlot({
      wl <- wind_long(); req(!is.null(wl))
      seasonal <- wl %>%
        dplyr::mutate(Month = lubridate::month(date)) %>%
        dplyr::group_by(eco, Month) %>%
        dplyr::summarise(windSpeed = mean(windSpeed, na.rm = TRUE), .groups = "drop")

      landscape_mean <- seasonal %>%
        dplyr::group_by(Month) %>%
        dplyr::summarise(windSpeed = mean(windSpeed, na.rm = TRUE), .groups = "drop")

      ggplot2::ggplot() +
        ggplot2::geom_line(data = seasonal,
                           ggplot2::aes(x = Month, y = windSpeed, color = eco, group = eco),
                           alpha = 0.5, linewidth = 0.6) +
        ggplot2::geom_line(data = landscape_mean,
                           ggplot2::aes(x = Month, y = windSpeed),
                           color = "black", linewidth = 1.3) +
        ggplot2::geom_point(data = landscape_mean,
                            ggplot2::aes(x = Month, y = windSpeed),
                            color = "black", size = 2.5) +
        ggplot2::scale_x_continuous(breaks = 1:12, labels = month.abb) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::labs(title = "Seasonal Wind Speed Pattern",
                      subtitle = "Per ecoregion (colored) and landscape mean (black)",
                      x = "Month", y = "Mean Wind Speed (m/s)", color = "Ecoregion") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "right")
    })

    # ── Wind direction histogram / rose ───────────────────────────────────
    output$wind_dir_plot <- renderPlot({
      wl <- wind_long(); req(!is.null(wl), "windDirection" %in% names(wl))

      wl_dir <- wl %>%
        dplyr::mutate(
          Month       = lubridate::month(date),
          dir_bin     = cut(windDirection, breaks = seq(0, 360, by = 22.5),
                            labels = FALSE, include.lowest = TRUE),
          dir_bin_deg = (dir_bin - 0.5) * 22.5,
          Season      = dplyr::case_when(
            Month %in% c(12,1,2) ~ "Winter",
            Month %in% 3:5       ~ "Spring",
            Month %in% 6:8       ~ "Summer",
            TRUE                  ~ "Fall"
          )
        )

      count_df <- wl_dir %>%
        dplyr::group_by(Season, dir_bin_deg) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop")

      ggplot2::ggplot(count_df,
                      ggplot2::aes(x = dir_bin_deg, y = n, fill = Season)) +
        ggplot2::geom_bar(stat = "identity", width = 22, color = "white", alpha = 0.85) +
        ggplot2::coord_polar(start = 0) +
        ggplot2::scale_x_continuous(breaks = seq(0, 315, 45),
                                    labels = c("N","NE","E","SE","S","SW","W","NW")) +
        ggplot2::scale_fill_brewer(palette = "Set2") +
        ggplot2::facet_wrap(~ factor(Season, levels = c("Winter","Spring","Summer","Fall"))) +
        ggplot2::labs(title = "Wind Direction Frequency by Season",
                      subtitle = "GRIDMET wind direction (degrees FROM)",
                      x = NULL, y = "Count") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "none",
                       axis.text.y = ggplot2::element_blank())
    })

    # ── Monthly wind speed heatmap (eco × month) ──────────────────────────
    output$wind_heatmap <- renderPlot({
      wl <- wind_long(); req(!is.null(wl))
      heat_df <- wl %>%
        dplyr::mutate(Month = lubridate::month(date)) %>%
        dplyr::group_by(eco, Month) %>%
        dplyr::summarise(windSpeed = mean(windSpeed, na.rm = TRUE), .groups = "drop")

      ggplot2::ggplot(heat_df, ggplot2::aes(x = factor(Month), y = forcats::fct_rev(eco),
                                             fill = windSpeed)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::geom_text(ggplot2::aes(label = round(windSpeed, 1)), size = 3.5) +
        ggplot2::scale_fill_gradient(low = "#deebf7", high = "#08306b",
                                     name = "m/s") +
        ggplot2::scale_x_discrete(labels = month.abb) +
        ggplot2::labs(title = "Mean Wind Speed by Ecoregion × Month",
                      x = "Month", y = "Ecoregion") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    })

    # ── Donor year table ─────────────────────────────────────────────────
    output$donor_table <- renderDT({
      wl <- wind_long(); req(!is.null(wl))
      wl2 <- wl %>%
        dplyr::mutate(hist_year = lubridate::year(date),
                      is_leap   = lubridate::leap_year(hist_year))
      hyt <- wl2 %>% dplyr::distinct(hist_year, is_leap)

      set.seed(input$wind_seed)
      future_yrs <- 2015:2100
      donor <- tibble::tibble(
        future_year = future_yrs,
        is_leap     = lubridate::leap_year(future_year)
      ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(donor_year = sample(
          hyt$hist_year[hyt$is_leap == is_leap], 1)) %>%
        dplyr::ungroup()

      DT::datatable(donor, options = list(pageLength = 20, dom = "tip"),
                    rownames = FALSE)
    })
  })
}
