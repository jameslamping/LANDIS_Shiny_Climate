# ============================================================
# mod_biascorrect.R  –  Tab 5: Bias Correction
# ============================================================

mod_biascorrect_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "5 · Bias Correction",
    icon = icon("balance-scale"),
    layout_sidebar(
      sidebar = sidebar(
        width = 310,
        h5("Bias Correction Settings", class = "text-primary fw-bold"),
        hr(),
        h6("Inputs (override if needed)", class = "text-secondary"),
        fileInput(ns("hist_csv"), "Historical PRISM CSV", accept = ".csv"),
        textInput(ns("fut_dir"), "Future CSV Directory",
                  placeholder = "Defaults to Tab 3 output"),
        hr(),
        sliderInput(ns("overlap_start"), "Overlap Start Year", 2015, 2024, 2015),
        sliderInput(ns("overlap_end"),   "Overlap End Year",   2015, 2024, 2024),
        numericInput(ns("dry_floor"), "Dry Month Floor (cm/day)",
                     value = 0.05, min = 0, max = 1, step = 0.01),
        hr(),
        checkboxGroupInput(ns("correct_vars"), "Variables to Correct",
                           choices  = c("Temperature — maxtemp + mintemp (additive)" = "temp",
                                        "Precipitation — ppt (multiplicative)" = "ppt"),
                           selected = c("temp", "ppt")),
        hr(),
        actionButton(ns("run_bc"), "Run Bias Correction",
                     icon = icon("play"), class = "btn-primary w-100 mb-2"),
        hr(),
        uiOutput(ns("out_dir_ui"))
      ),

      div(
        uiOutput(ns("status_bar")),
        navset_card_tab(
          nav_panel("Log",
            verbatimTextOutput(ns("log_out")) %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Correction Factors",
            layout_columns(
              col_widths = c(6, 6),
              div(
                h6("Select Model–Scenario for diagnostics:", class = "text-muted mb-1"),
                uiOutput(ns("factor_model_select"))
              ),
              div()
            ),
            plotOutput(ns("factors_tmax_plot"), height = "350px") %>% withSpinner(color = "#2c7bb6"),
            plotOutput(ns("factors_ppt_plot"),  height = "350px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Before vs After: Tmax",
            plotOutput(ns("ba_tmax_plot"), height = "450px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Annual Tmax Trend",
            plotOutput(ns("annual_tmax_plot"), height = "450px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Before vs After: Precip",
            plotOutput(ns("ba_ppt_plot"),  height = "450px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Annual Precip Trend",
            plotOutput(ns("annual_ppt_plot"), height = "450px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("QA Table",
            DTOutput(ns("qa_table")) %>% withSpinner(color = "#2c7bb6")
          )
        )
      )
    )
  )
}

mod_biascorrect_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    log_path   <- reactive(file.path(tempdir(), "biascorr.log"))
    bg_proc    <- reactiveVal(NULL)
    qa_df      <- reactiveVal(NULL)
    bc_dir     <- reactiveVal(NULL)

    # ── Output dir display ────────────────────────────────────────────────
    output$out_dir_ui <- renderUI({
      d <- if (!is.null(rv$out_dir))
             file.path(rv$out_dir, "future_biascorrected")
           else "(set output dir in Setup)"
      div(class = "text-muted small",
          icon("folder"), " Output: ", code(d))
    })

    # ── Run bias correction ───────────────────────────────────────────────
    observeEvent(input$run_bc, {
      hist_path <- if (!is.null(input$hist_csv)) input$hist_csv$datapath else rv$prism_csv
      fut_path  <- if (nchar(trimws(input$fut_dir)) > 0) trimws(input$fut_dir) else rv$future_dir
      req(!is.null(hist_path), !is.null(fut_path))
      req(dir.exists(fut_path))

      out_d <- file.path(rv$out_dir, "future_biascorrected")
      dir.create(out_d, recursive = TRUE, showWarnings = FALSE)

      log_file <- log_path()
      writeLines("=== Bias Correction Started ===", log_file)

      proc <- callr::r_bg(
        func = bias_correction_worker,
        args = list(
          historic_fp      = hist_path,
          future_dir       = fut_path,
          out_dir          = out_d,
          park             = rv$park %||% "PARK",
          overlap_years    = seq(input$overlap_start, input$overlap_end),
          dry_floor        = input$dry_floor,
          correct_temp     = "temp" %in% input$correct_vars,
          correct_ppt      = "ppt"  %in% input$correct_vars,
          log_file         = log_file
        ),
        stderr = log_file, stdout = log_file
      )
      bg_proc(proc)
      showNotification("Bias correction started.", type = "message")
    })

    # ── Poll log ──────────────────────────────────────────────────────────
    log_timer <- reactiveTimer(2000)
    output$log_out <- renderText({
      log_timer()
      p <- bg_proc()
      if (!is.null(p) && !p$is_alive()) {
        out_d <- file.path(rv$out_dir, "future_biascorrected")
        qa_fp <- list.files(out_d, pattern = "^_corrections_.*\\.csv$", full.names = TRUE)
        if (length(qa_fp) > 0) {
          qa <- readr::read_csv(qa_fp[1], show_col_types = FALSE)
          qa_df(qa)
        }
        bc_dir(out_d)
        rv$biascorr_dir <- out_d
        bg_proc(NULL)
        showNotification("Bias correction complete!", type = "message")
      }
      tail_log(log_path())
    })

    # ── Status bar ────────────────────────────────────────────────────────
    output$status_bar <- renderUI({
      d <- bc_dir()
      if (!is.null(d) && dir.exists(d)) {
        n <- length(list.files(d, "\\.csv$"))
        div(class = "alert alert-success py-2 mb-3",
            icon("check-circle"), " ",
            strong(n, " bias-corrected CSVs"), " in: ", code(d))
      } else {
        div(class = "alert alert-info py-2 mb-3",
            icon("info-circle"),
            " Configure inputs and click 'Run Bias Correction'.")
      }
    })

    # ── Factor model selector ─────────────────────────────────────────────
    output$factor_model_select <- renderUI({
      qa <- qa_df(); req(!is.null(qa))
      choices <- sort(unique(qa$model_scenario))
      selectInput(ns("factor_model"), NULL, choices = choices)
    })

    # ── Correction factors heatmap – tmax ─────────────────────────────────
    output$factors_tmax_plot <- renderPlot({
      qa <- qa_df(); req(!is.null(qa))
      ms <- input$factor_model; req(!is.null(ms))

      sub <- qa %>%
        dplyr::filter(model_scenario == ms, variable == "maxtemp")
      if (nrow(sub) == 0) return(NULL)

      ggplot2::ggplot(sub, ggplot2::aes(x = factor(Month), y = forcats::fct_rev(eco),
                                         fill = correction)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::geom_text(ggplot2::aes(label = round(correction, 2)), size = 3) +
        ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d73027",
                                       midpoint = 0, name = "ΔT offset (°C)") +
        ggplot2::labs(title = paste("Tmax Additive Correction:", ms),
                      subtitle = "ΔT = PRISM mean − CMIP6 mean (per eco × month)",
                      x = "Month", y = "Ecoregion") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    })

    # ── Correction factors heatmap – ppt ──────────────────────────────────
    output$factors_ppt_plot <- renderPlot({
      qa <- qa_df(); req(!is.null(qa))
      ms <- input$factor_model; req(!is.null(ms))

      sub <- qa %>% dplyr::filter(model_scenario == ms, variable == "ppt")
      if (nrow(sub) == 0) return(NULL)

      ggplot2::ggplot(sub, ggplot2::aes(x = factor(Month), y = forcats::fct_rev(eco),
                                         fill = correction)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::geom_text(ggplot2::aes(label = round(correction, 2)), size = 3) +
        ggplot2::scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9641",
                                       midpoint = 1, name = "PPT\nfactor") +
        ggplot2::labs(title = paste("Precipitation Multiplicative Factor:", ms),
                      subtitle = "Factor = PRISM / CMIP6 mean (per eco × month)",
                      x = "Month", y = "Ecoregion") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    })

    # ── Before/after monthly mean tmax across all decision models ─────────
    output$ba_tmax_plot <- renderPlot({
      d <- bc_dir(); req(!is.null(d) && dir.exists(d))
      hist_path <- if (!is.null(input$hist_csv)) input$hist_csv$datapath else rv$prism_csv
      req(!is.null(hist_path))

      fut_dir <- if (nchar(trimws(input$fut_dir)) > 0) trimws(input$fut_dir) else rv$future_dir
      req(!is.null(fut_dir))

      bc_files  <- list.files(d, "\\.csv$", full.names = TRUE)
      pre_files <- list.files(fut_dir, "\\.csv$", full.names = TRUE)
      bc_files  <- bc_files[!grepl("^_", basename(bc_files))]
      req(length(bc_files) > 0, length(pre_files) > 0)

      read_monthly_tmax <- function(files, label) {
        purrr::map_dfr(head(files, 6), function(fp) {
          df <- readr::read_csv(fp, show_col_types = FALSE)
          var <- if ("maxtemp" %in% unique(df$Variable)) "maxtemp" else "tmax"
          if (!(var %in% unique(df$Variable))) return(NULL)
          df %>%
            dplyr::filter(Variable == var) %>%
            dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
            dplyr::group_by(Month) %>%
            dplyr::summarise(mean_val = mean(daily_mean, na.rm = TRUE), .groups = "drop") %>%
            dplyr::mutate(source = label, model = tools::file_path_sans_ext(basename(fp)))
        })
      }

      pre <- read_monthly_tmax(pre_files, "Before bias correction")
      pst <- read_monthly_tmax(bc_files,  "After bias correction")

      hist_df <- readr::read_csv(hist_path, show_col_types = FALSE)
      var_h   <- if ("tmax" %in% unique(hist_df$Variable)) "tmax" else "maxtemp"
      hist_mon <- hist_df %>%
        dplyr::filter(Variable == var_h) %>%
        dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
        dplyr::group_by(Month) %>%
        dplyr::summarise(mean_val = mean(daily_mean, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(source = "PRISM Historical", model = "PRISM")

      combined <- dplyr::bind_rows(pre, pst, hist_mon)

      ggplot2::ggplot(combined, ggplot2::aes(x = factor(Month), y = mean_val,
                                              color = source, group = interaction(source, model))) +
        ggplot2::geom_line(data = combined[combined$source != "PRISM Historical",],
                           alpha = 0.4, linewidth = 0.5) +
        ggplot2::stat_summary(data = combined[combined$source != "PRISM Historical",],
                              ggplot2::aes(group = source),
                              fun = mean, geom = "line", linewidth = 1.2) +
        ggplot2::geom_line(data = hist_mon, ggplot2::aes(group = 1),
                           color = "black", linewidth = 1.3, linetype = "dashed") +
        ggplot2::scale_color_manual(
          values = c("Before bias correction" = "#FC8D59",
                     "After bias correction"  = "#1a9641",
                     "PRISM Historical"       = "black"),
          name = NULL
        ) +
        ggplot2::scale_x_discrete(labels = month.abb) +
        ggplot2::labs(title = "Monthly Mean Tmax: Before vs After Bias Correction",
                      subtitle = "Thin lines = individual models; thick = ensemble mean | Dashed = PRISM",
                      x = "Month", y = "Mean Tmax (°C)") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "top")
    })

    # ── Before/after monthly total ppt ────────────────────────────────────
    output$ba_ppt_plot <- renderPlot({
      d <- bc_dir(); req(!is.null(d) && dir.exists(d))
      hist_path <- if (!is.null(input$hist_csv)) input$hist_csv$datapath else rv$prism_csv
      req(!is.null(hist_path))
      fut_dir   <- if (nchar(trimws(input$fut_dir)) > 0) trimws(input$fut_dir) else rv$future_dir
      req(!is.null(fut_dir))

      bc_files  <- list.files(d, "\\.csv$", full.names = TRUE)
      pre_files <- list.files(fut_dir, "\\.csv$", full.names = TRUE)
      bc_files  <- bc_files[!grepl("^_", basename(bc_files))]
      req(length(bc_files) > 0)

      read_monthly_ppt <- function(files, label) {
        purrr::map_dfr(head(files, 6), function(fp) {
          df <- readr::read_csv(fp, show_col_types = FALSE)
          if (!("ppt" %in% unique(df$Variable))) return(NULL)
          df %>%
            dplyr::filter(Variable == "ppt") %>%
            dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
            dplyr::group_by(Month) %>%
            dplyr::summarise(mean_val = mean(daily_mean, na.rm = TRUE), .groups = "drop") %>%
            dplyr::mutate(source = label, model = tools::file_path_sans_ext(basename(fp)))
        })
      }

      pre  <- read_monthly_ppt(pre_files, "Before bias correction")
      pst  <- read_monthly_ppt(bc_files,  "After bias correction")

      hist_df  <- readr::read_csv(hist_path, show_col_types = FALSE)
      hist_mon <- hist_df %>%
        dplyr::filter(Variable == "ppt") %>%
        dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
        dplyr::group_by(Month) %>%
        dplyr::summarise(mean_val = mean(daily_mean, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(source = "PRISM Historical", model = "PRISM")

      combined <- dplyr::bind_rows(pre, pst, hist_mon)

      ggplot2::ggplot(combined, ggplot2::aes(x = factor(Month), y = mean_val,
                                              color = source, group = interaction(source, model))) +
        ggplot2::geom_line(data = combined[combined$source != "PRISM Historical",],
                           alpha = 0.4, linewidth = 0.5) +
        ggplot2::stat_summary(data = combined[combined$source != "PRISM Historical",],
                              ggplot2::aes(group = source),
                              fun = mean, geom = "line", linewidth = 1.2) +
        ggplot2::geom_line(data = hist_mon, ggplot2::aes(group = 1),
                           color = "black", linewidth = 1.3, linetype = "dashed") +
        ggplot2::scale_color_manual(
          values = c("Before bias correction" = "#FC8D59",
                     "After bias correction"  = "#1a9641",
                     "PRISM Historical"       = "black"),
          name = NULL
        ) +
        ggplot2::scale_x_discrete(labels = month.abb) +
        ggplot2::labs(title = "Monthly Mean Daily Precip: Before vs After Bias Correction",
                      subtitle = "Thin lines = individual models; thick = ensemble mean | Dashed = PRISM",
                      x = "Month", y = "Mean Daily Precip (cm/day)") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "top")
    })

    # ── Annual tmax time series: before vs after ──────────────────────────
    output$annual_tmax_plot <- renderPlot({
      d <- bc_dir(); req(!is.null(d) && dir.exists(d))
      hist_path <- if (!is.null(input$hist_csv)) input$hist_csv$datapath else rv$prism_csv
      req(!is.null(hist_path))
      fut_dir <- if (nchar(trimws(input$fut_dir)) > 0) trimws(input$fut_dir) else rv$future_dir
      req(!is.null(fut_dir))

      bc_files  <- list.files(d, "\\.csv$", full.names = TRUE)
      pre_files <- list.files(fut_dir, "\\.csv$", full.names = TRUE)
      bc_files  <- bc_files[!grepl("^_", basename(bc_files))]
      req(length(bc_files) > 0, length(pre_files) > 0)

      read_annual_tmax <- function(files, label) {
        purrr::map_dfr(files, function(fp) {
          df <- readr::read_csv(fp, show_col_types = FALSE)
          var <- if ("maxtemp" %in% unique(df$Variable)) "maxtemp" else
                 if ("tmax"    %in% unique(df$Variable)) "tmax" else return(NULL)
          df %>%
            dplyr::filter(Variable == var) %>%
            dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
            dplyr::group_by(Year) %>%
            dplyr::summarise(annual_mean = mean(daily_mean, na.rm = TRUE), .groups = "drop") %>%
            dplyr::mutate(source = label,
                          model  = tools::file_path_sans_ext(basename(fp)))
        })
      }

      pre <- read_annual_tmax(pre_files, "Before bias correction")
      pst <- read_annual_tmax(bc_files,  "After bias correction")

      hist_df  <- readr::read_csv(hist_path, show_col_types = FALSE)
      var_h    <- if ("tmax" %in% unique(hist_df$Variable)) "tmax" else "maxtemp"
      hist_ann <- hist_df %>%
        dplyr::filter(Variable == var_h) %>%
        dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(annual_mean = mean(daily_mean, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(source = "PRISM Historical", model = "PRISM")

      combined <- dplyr::bind_rows(pre, pst, hist_ann)

      ggplot2::ggplot(combined, ggplot2::aes(x = Year, y = annual_mean,
                                              color = source,
                                              group = interaction(source, model))) +
        ggplot2::geom_line(data = combined[combined$source != "PRISM Historical", ],
                           alpha = 0.35, linewidth = 0.5) +
        ggplot2::stat_summary(data = combined[combined$source != "PRISM Historical", ],
                              ggplot2::aes(group = source),
                              fun = mean, geom = "line", linewidth = 1.3) +
        ggplot2::geom_line(data = hist_ann, ggplot2::aes(group = 1),
                           color = "black", linewidth = 1.2, linetype = "dashed") +
        ggplot2::scale_color_manual(
          values = c("Before bias correction" = "#FC8D59",
                     "After bias correction"  = "#1a9641",
                     "PRISM Historical"       = "black"),
          name = NULL
        ) +
        ggplot2::labs(
          title    = "Annual Mean Tmax: Before vs After Bias Correction",
          subtitle = "Thin lines = individual models; thick = ensemble mean | Dashed = PRISM historical",
          x = "Year", y = "Annual Mean Tmax (°C)"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "top")
    })

    # ── Annual ppt time series: before vs after ───────────────────────────
    output$annual_ppt_plot <- renderPlot({
      d <- bc_dir(); req(!is.null(d) && dir.exists(d))
      hist_path <- if (!is.null(input$hist_csv)) input$hist_csv$datapath else rv$prism_csv
      req(!is.null(hist_path))
      fut_dir <- if (nchar(trimws(input$fut_dir)) > 0) trimws(input$fut_dir) else rv$future_dir
      req(!is.null(fut_dir))

      bc_files  <- list.files(d, "\\.csv$", full.names = TRUE)
      pre_files <- list.files(fut_dir, "\\.csv$", full.names = TRUE)
      bc_files  <- bc_files[!grepl("^_", basename(bc_files))]
      req(length(bc_files) > 0, length(pre_files) > 0)

      read_annual_ppt <- function(files, label) {
        purrr::map_dfr(files, function(fp) {
          df <- readr::read_csv(fp, show_col_types = FALSE)
          if (!("ppt" %in% unique(df$Variable))) return(NULL)
          df %>%
            dplyr::filter(Variable == "ppt") %>%
            dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
            dplyr::group_by(Year) %>%
            dplyr::summarise(annual_total = sum(daily_mean, na.rm = TRUE), .groups = "drop") %>%
            dplyr::mutate(source = label,
                          model  = tools::file_path_sans_ext(basename(fp)))
        })
      }

      pre <- read_annual_ppt(pre_files, "Before bias correction")
      pst <- read_annual_ppt(bc_files,  "After bias correction")

      hist_df  <- readr::read_csv(hist_path, show_col_types = FALSE)
      hist_ann <- hist_df %>%
        dplyr::filter(Variable == "ppt") %>%
        dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(annual_total = sum(daily_mean, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(source = "PRISM Historical", model = "PRISM")

      combined <- dplyr::bind_rows(pre, pst, hist_ann)

      ggplot2::ggplot(combined, ggplot2::aes(x = Year, y = annual_total,
                                              color = source,
                                              group = interaction(source, model))) +
        ggplot2::geom_line(data = combined[combined$source != "PRISM Historical", ],
                           alpha = 0.35, linewidth = 0.5) +
        ggplot2::stat_summary(data = combined[combined$source != "PRISM Historical", ],
                              ggplot2::aes(group = source),
                              fun = mean, geom = "line", linewidth = 1.3) +
        ggplot2::geom_line(data = hist_ann, ggplot2::aes(group = 1),
                           color = "black", linewidth = 1.2, linetype = "dashed") +
        ggplot2::scale_color_manual(
          values = c("Before bias correction" = "#FC8D59",
                     "After bias correction"  = "#1a9641",
                     "PRISM Historical"       = "black"),
          name = NULL
        ) +
        ggplot2::labs(
          title    = "Annual Total Precip: Before vs After Bias Correction",
          subtitle = "Thin lines = individual models; thick = ensemble mean | Dashed = PRISM historical",
          x = "Year", y = "Annual Total Precip (cm/day summed)"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "top")
    })

    # ── QA table ──────────────────────────────────────────────────────────
    output$qa_table <- renderDT({
      qa <- qa_df(); req(!is.null(qa))
      DT::datatable(
        qa %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))),
        options  = list(scrollX = TRUE, pageLength = 20, dom = "tip"),
        rownames = FALSE
      ) %>%
        DT::formatStyle("correction",
          background = DT::styleColorBar(range(qa$correction, na.rm = TRUE), "#cce5ff"))
    })
  })
}

# ── Bias correction background worker ────────────────────────────────────────
bias_correction_worker <- function(historic_fp, future_dir, out_dir, park,
                                    overlap_years, dry_floor, correct_temp,
                                    correct_ppt, log_file) {
  suppressPackageStartupMessages({
    library(tidyverse); library(glue); library(fs)
  })
  cat("=== Bias Correction Worker Started ===\n", file = log_file, append = TRUE)

  # Inline variable-name normalisation (mirrors utils.R, case-insensitive)
  norm_clim_vars <- function(df) {
    lut <- c(tmax="tmax", maxtemp="tmax", tmin="tmin", mintemp="tmin",
             precip="ppt", ppt="ppt")
    if (!"Variable" %in% names(df)) return(df)
    canon <- lut[tolower(df$Variable)]
    df$Variable <- ifelse(!is.na(canon), canon, df$Variable)
    df
  }

  read_var_long <- function(filepath, var_name) {
    df <- read_csv(filepath, show_col_types = FALSE) %>%
      norm_clim_vars() %>%
      filter(Variable == var_name)
    if (nrow(df) == 0) return(NULL)
    eco_cols <- names(df)[grepl("^eco\\d+$", names(df))]
    df %>% pivot_longer(all_of(eco_cols), names_to = "eco", values_to = "value")
  }

  var_map <- dplyr::tribble(
    ~hist_name, ~fut_name,  ~method,
    "tmax",     "maxtemp",  "additive",
    "tmin",     "mintemp",  "additive",
    "ppt",      "ppt",      "multiplicative"
  )
  if (!correct_temp) var_map <- var_map %>% filter(method != "additive")
  if (!correct_ppt)  var_map <- var_map %>% filter(method != "multiplicative")

  cat("Computing PRISM monthly climatology...\n", file = log_file, append = TRUE)
  prism_monthly <- var_map %>%
    purrr::pmap_dfr(function(hist_name, fut_name, method) {
      long <- read_var_long(historic_fp, hist_name)
      if (is.null(long)) return(NULL)
      long %>%
        filter(Year %in% overlap_years) %>%
        group_by(eco, Month) %>%
        summarise(prism_mean = mean(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(fut_name = fut_name, method = method)
    })

  csv_files     <- dir_ls(future_dir, glob = "*.csv")
  all_corrections <- list()

  for (fp in csv_files) {
    ms <- basename(fp) %>% str_remove(glue("_{park}\\.csv$"))
    cat("Correcting:", ms, "\n", file = log_file, append = TRUE)

    full_df  <- read_csv(fp, show_col_types = FALSE)
    eco_cols <- names(full_df)[grepl("^eco\\d+$", names(full_df))]

    corrected_pieces <- list()

    for (k in seq_len(nrow(var_map))) {
      hist_name <- var_map$hist_name[k]
      fut_name  <- var_map$fut_name[k]
      method    <- var_map$method[k]

      fut_long <- full_df %>%
        filter(Variable == fut_name) %>%
        pivot_longer(all_of(eco_cols), names_to = "eco", values_to = "value")
      if (nrow(fut_long) == 0) next

      fut_monthly <- fut_long %>%
        filter(Year %in% overlap_years) %>%
        group_by(eco, Month) %>%
        summarise(fut_mean = mean(value, na.rm = TRUE), .groups = "drop")

      prism_this <- prism_monthly %>%
        filter(fut_name == !!fut_name) %>% select(eco, Month, prism_mean)

      corrections <- prism_this %>% inner_join(fut_monthly, by = c("eco","Month"))

      if (method == "additive") {
        corrections <- corrections %>%
          mutate(correction = pmin(pmax(prism_mean - fut_mean, -10), 10))
      } else {
        corrections <- corrections %>%
          mutate(
            raw_factor    = prism_mean / fut_mean,
            shrink_weight = pmin(prism_mean / dry_floor, 1),
            correction    = shrink_weight * raw_factor + (1 - shrink_weight) * 1,
            correction    = pmin(pmax(correction, 0.2), 5)
          )
      }

      all_corrections[[paste(ms, fut_name, sep = "__")]] <- corrections %>%
        mutate(model_scenario = ms, variable = fut_name, method = method) %>%
        select(model_scenario, variable, method, eco, Month, prism_mean, fut_mean, correction)

      fut_corrected <- fut_long %>%
        left_join(corrections %>% select(eco, Month, correction), by = c("eco","Month")) %>%
        mutate(
          correction = replace_na(correction, if (method == "additive") 0 else 1),
          value_bc   = if (method == "additive") value + correction else value * correction
        ) %>%
        select(Year, Month, Day, Variable, eco, value_bc) %>%
        pivot_wider(names_from = eco, values_from = value_bc) %>%
        select(Year, Month, Day, Variable, all_of(eco_cols))

      corrected_pieces[[fut_name]] <- fut_corrected

      pre  <- round(mean(rowMeans(full_df %>% filter(Variable == fut_name) %>%
                                    select(all_of(eco_cols)), na.rm = TRUE), na.rm = TRUE), 3)
      post <- round(mean(rowMeans(fut_corrected %>% select(all_of(eco_cols)), na.rm = TRUE), na.rm = TRUE), 3)
      cat("  ", fut_name, ":", pre, "->", post, "\n", file = log_file, append = TRUE)
    }

    corrected_vars <- names(corrected_pieces)
    other_vars     <- full_df %>% filter(!(Variable %in% corrected_vars))
    out_df <- bind_rows(c(corrected_pieces, list(other_vars))) %>%
      arrange(Year, Month, Day, Variable)

    out_fp <- file.path(out_dir, glue("{ms}_{park}_biascorrected.csv"))
    write_csv(out_df, out_fp)
  }

  correction_qa <- bind_rows(all_corrections)
  write_csv(correction_qa, file.path(out_dir, glue("_corrections_{park}.csv")))
  cat("=== Bias Correction Complete ===\n", file = log_file, append = TRUE)
}
