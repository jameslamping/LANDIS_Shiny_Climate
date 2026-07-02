# ============================================================
# mod_explore.R  –  Tab 4: Model Exploration & Four-Corners Selection
# ============================================================

CORNER_LABELS <- c(
  "Warm / Dry"  = "warm_dry",
  "Warm / Wet"  = "warm_wet",
  "Cool / Dry"  = "cool_dry",
  "Cool / Wet"  = "cool_wet"
)

CORNER_COLORS <- c(
  "Warm / Dry" = "#D73027",
  "Warm / Wet" = "#FC8D59",
  "Cool / Dry" = "#4575B4",
  "Cool / Wet" = "#74ADD1"
)

mod_explore_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "4 · Explore & Select",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        h5("Data Sources", class = "text-primary fw-bold"),
        hr(),
        h6("Historical Baseline", class = "text-secondary"),
        fileInput(ns("hist_csv"), "Historical PRISM CSV (optional override)",
                  accept = ".csv"),
        hr(),
        h6("Future Climate CSVs", class = "text-secondary"),
        textInput(ns("fut_dir"), "Future CSV Directory (optional override)",
                  placeholder = "Defaults to Tab 3 output"),
        actionButton(ns("load_data"), "Load / Reload Data",
                     icon = icon("sync"), class = "btn-primary w-100 mb-2"),
        hr(),
        h5("Four-Corners Settings", class = "text-primary fw-bold"),
        sliderInput(ns("begin_start"), "Begin Period: Start Year", 2015, 2060, 2015),
        sliderInput(ns("begin_end"),   "Begin Period: End Year",   2015, 2060, 2044),
        sliderInput(ns("end_start"),   "End Period: Start Year",   2040, 2100, 2071),
        sliderInput(ns("end_end"),     "End Period: End Year",     2040, 2100, 2100),
        hr(),
        h5("Select Decision Models", class = "text-primary fw-bold"),
        p(class = "text-muted small",
          "Choose a model–scenario from the four-corners plot for each climate envelope."),
        uiOutput(ns("corner_selectors")),
        actionButton(ns("confirm_corners"), "Confirm Selection",
                     icon = icon("check"), class = "btn-success w-100 mt-2")
      ),

      div(
        uiOutput(ns("status_bar")),
        navset_card_tab(
          nav_panel("Four Corners",
            plotOutput(ns("four_corners_plot"), height = "500px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Annual Tmax",
            plotOutput(ns("tmax_all_plot"), height = "480px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Annual Precip",
            plotOutput(ns("ppt_all_plot"), height = "480px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Seasonal Cycles",
            layout_columns(
              col_widths = c(6, 6),
              plotOutput(ns("seasonal_tmax"), height = "380px") %>% withSpinner(color = "#2c7bb6"),
              plotOutput(ns("seasonal_ppt"),  height = "380px") %>% withSpinner(color = "#2c7bb6")
            )
          ),
          nav_panel("Delta Table",
            DTOutput(ns("delta_table")) %>% withSpinner(color = "#2c7bb6")
          )
        )
      )
    )
  )
}

mod_explore_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    all_future  <- reactiveVal(NULL)
    hist_data   <- reactiveVal(NULL)
    corners_df  <- reactiveVal(NULL)
    model_choices <- reactiveVal(character(0))

    # ── Load data ─────────────────────────────────────────────────────────
    observeEvent(input$load_data, {
      # Historical
      hist_path <- if (!is.null(input$hist_csv)) input$hist_csv$datapath
                   else rv$prism_csv
      req(!is.null(hist_path))

      withProgress(message = "Loading climate data...", value = 0, {
        setProgress(0.1, "Reading historical CSV...")
        hist_df <- readr::read_csv(hist_path, show_col_types = FALSE) %>%
          dplyr::mutate(model = "PRISM", scenario = "historical")
        hist_data(hist_df)

        # Future
        fut_path <- if (nchar(trimws(input$fut_dir)) > 0) trimws(input$fut_dir)
                    else rv$future_dir
        req(!is.null(fut_path), dir.exists(fut_path))

        setProgress(0.3, "Loading future CSVs...")
        files <- list.files(fut_path, pattern = "\\.csv$", full.names = TRUE)
        req(length(files) > 0)

        df_list <- purrr::imap(files, function(fp, i) {
          setProgress(0.3 + 0.6 * (i / length(files)), paste("Reading", basename(fp)))
          df <- readr::read_csv(fp, show_col_types = FALSE)
          bn <- basename(fp)
          df$model    <- stringr::str_split_i(bn, "_", 1)
          df$scenario <- stringr::str_split_i(bn, "_", 2)
          df
        })

        fut_df <- dplyr::bind_rows(df_list)
        all_future(fut_df)

        mc <- sort(unique(paste(fut_df$model, fut_df$scenario, sep = "_")))
        model_choices(mc)
      })
      showNotification("Data loaded successfully.", type = "message")
    })

    # ── Compute corners ───────────────────────────────────────────────────
    corners_reactive <- reactive({
      df <- all_future(); req(!is.null(df))
      compute_four_corners(
        df,
        begin_years = seq(input$begin_start, input$begin_end),
        end_years   = seq(input$end_start,   input$end_end)
      )
    })

    observe({ corners_df(corners_reactive()) })

    # ── Corner selectors ──────────────────────────────────────────────────
    output$corner_selectors <- renderUI({
      mc <- model_choices(); req(length(mc) > 0)
      mc_labeled <- c("(None)" = "", mc)
      tagList(
        selectInput(ns("warm_dry"), "Warm / Dry",  choices = mc_labeled,
                    selected = rv$selected_models$warm_dry),
        selectInput(ns("warm_wet"), "Warm / Wet",  choices = mc_labeled,
                    selected = rv$selected_models$warm_wet),
        selectInput(ns("cool_dry"), "Cool / Dry",  choices = mc_labeled,
                    selected = rv$selected_models$cool_dry),
        selectInput(ns("cool_wet"), "Cool / Wet",  choices = mc_labeled,
                    selected = rv$selected_models$cool_wet)
      )
    })

    observeEvent(input$confirm_corners, {
      rv$selected_models <- list(
        warm_dry = if (nchar(input$warm_dry) > 0) input$warm_dry else NULL,
        warm_wet = if (nchar(input$warm_wet) > 0) input$warm_wet else NULL,
        cool_dry = if (nchar(input$cool_dry) > 0) input$cool_dry else NULL,
        cool_wet = if (nchar(input$cool_wet) > 0) input$cool_wet else NULL
      )
      showNotification("Decision models confirmed.", type = "message")
    })

    # ── Status bar ────────────────────────────────────────────────────────
    output$status_bar <- renderUI({
      df <- all_future()
      if (!is.null(df)) {
        div(class = "alert alert-success py-2 mb-3",
            icon("check-circle"), " ",
            strong(length(model_choices()), " model–scenario combinations"), " | Years: ",
            strong(paste(min(df$Year), "–", max(df$Year))))
      } else {
        div(class = "alert alert-info py-2 mb-3",
            icon("info-circle"),
            " Click 'Load / Reload Data' to begin exploration.")
      }
    })

    # ── Four corners scatter ──────────────────────────────────────────────
    output$four_corners_plot <- renderPlot({
      cf <- corners_df(); req(!is.null(cf) && nrow(cf) > 0)

      # Replace NULL selections with a sentinel so case_when always gets
      # a length-1 string and never a zero-length logical vector.
      sel_wd <- rv$selected_models$warm_dry %||% ".none."
      sel_ww <- rv$selected_models$warm_wet %||% ".none."
      sel_cd <- rv$selected_models$cool_dry %||% ".none."
      sel_cw <- rv$selected_models$cool_wet %||% ".none."
      selected <- c(sel_wd, sel_ww, sel_cd, sel_cw)

      cf <- cf %>%
        dplyr::mutate(
          ms    = paste(model, scenario, sep = "_"),
          label = model,
          is_selected = ms %in% selected,
          corner_name = dplyr::case_when(
            ms == sel_wd ~ "Warm / Dry",
            ms == sel_ww ~ "Warm / Wet",
            ms == sel_cd ~ "Cool / Dry",
            ms == sel_cw ~ "Cool / Wet",
            TRUE ~ NA_character_
          )
        )

      p <- ggplot2::ggplot(cf, ggplot2::aes(x = delta_P_pct, y = delta_T_C,
                                              color = scenario)) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
        ggplot2::geom_vline(xintercept = 0, linewidth = 0.4, color = "grey50") +
        ggplot2::geom_point(size = 3.5, alpha = 0.85) +
        ggrepel::geom_text_repel(ggplot2::aes(label = model), size = 3,
                                  max.overlaps = Inf, box.padding = 0.3,
                                  point.padding = 0.15, segment.color = "grey60") +
        ggplot2::scale_color_manual(
          values = c(ssp126 = "#1a9641", ssp245 = "#fdae61",
                     ssp370 = "#d7191c", ssp585 = "#762a83")
        )

      # Overlay selected corners as large colored points
      selected_cf <- cf %>% dplyr::filter(!is.na(corner_name))
      if (nrow(selected_cf) > 0) {
        p <- p +
          ggplot2::geom_point(data = selected_cf,
                              ggplot2::aes(shape = corner_name, fill = corner_name),
                              color = "black", size = 6, stroke = 1.5,
                              show.legend = TRUE) +
          ggplot2::scale_shape_manual(name = "Selected Corner",
                                       values = c(21, 22, 23, 24)) +
          ggplot2::scale_fill_manual(name  = "Selected Corner",
                                     values = CORNER_COLORS)
      }

      p +
        ggplot2::labs(
          title = paste(rv$park %||% "Landscape",
                        "– Projected Temp & Precip Change (All SSPs)"),
          subtitle = paste0("Begin: ", input$begin_start, "–", input$begin_end,
                            "  →  End: ", input$end_start, "–", input$end_end),
          x = expression(Delta ~ "P (%)"),
          y = expression(Delta ~ "T (°C)"),
          color = "Scenario"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "right",
                       panel.grid.minor = ggplot2::element_blank())
    })

    # ── Annual tmax: all models grey + selected colored + historical ──────
    output$tmax_all_plot <- renderPlot({
      df <- all_future(); hdf <- hist_data()
      req(!is.null(df))

      ann_fut <- df %>%
        dplyr::filter(Variable == "maxtemp") %>%
        dplyr::mutate(daily_mean = row_eco_mean(.),
                      ms = paste(model, scenario, sep = "_")) %>%
        dplyr::group_by(model, scenario, ms, Year) %>%
        dplyr::summarise(annual = mean(daily_mean, na.rm = TRUE), .groups = "drop")

      sel_wd <- rv$selected_models$warm_dry %||% ".none."
      sel_ww <- rv$selected_models$warm_wet %||% ".none."
      sel_cd <- rv$selected_models$cool_dry %||% ".none."
      sel_cw <- rv$selected_models$cool_wet %||% ".none."
      sel       <- c(sel_wd, sel_ww, sel_cd, sel_cw)
      bg_data   <- dplyr::filter(ann_fut, !(ms %in% sel))
      dec_data  <- dplyr::filter(ann_fut, ms %in% sel) %>%
        dplyr::mutate(corner = dplyr::case_when(
          ms == sel_wd ~ "Warm / Dry",
          ms == sel_ww ~ "Warm / Wet",
          ms == sel_cd ~ "Cool / Dry",
          ms == sel_cw ~ "Cool / Wet"
        ))

      p <- ggplot2::ggplot()

      if (nrow(bg_data) > 0) {
        p <- p + ggplot2::geom_line(data = bg_data,
                                     ggplot2::aes(x = Year, y = annual, group = ms),
                                     color = "grey70", linewidth = 0.3, alpha = 0.6)
      }
      if (nrow(dec_data) > 0) {
        p <- p + ggplot2::geom_line(data = dec_data,
                                     ggplot2::aes(x = Year, y = annual,
                                                  color = corner, group = ms),
                                     linewidth = 1.1) +
          ggplot2::scale_color_manual(name = "Corner", values = CORNER_COLORS)
      }

      if (!is.null(hdf)) {
        var_h <- if ("tmax" %in% unique(hdf$Variable)) "tmax" else "maxtemp"
        if (var_h %in% unique(hdf$Variable)) {
          ann_hist <- hdf %>%
            dplyr::filter(Variable == var_h) %>%
            dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
            dplyr::group_by(Year) %>%
            dplyr::summarise(annual = mean(daily_mean, na.rm = TRUE), .groups = "drop")
          p <- p + ggplot2::geom_line(data = ann_hist,
                                       ggplot2::aes(x = Year, y = annual,
                                                    linetype = "Historic PRISM"),
                                       color = "black", linewidth = 1.2) +
            ggplot2::scale_linetype_manual(name = "Observed",
                                           values = c("Historic PRISM" = "dashed"))
        }
      }

      p + ggplot2::labs(
        title    = paste(rv$park %||% "Landscape", "– Annual Mean Daily Maximum Temperature"),
        subtitle = "All CMIP6 models shown; decision models highlighted | Landscape-averaged",
        x = "Year", y = "Mean Tmax (°C)",
        caption  = "Grey = all other models | Dashed = PRISM historical"
      ) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(legend.position = "right",
                       panel.grid.minor = ggplot2::element_blank(),
                       plot.caption     = ggplot2::element_text(size = 8, color = "grey50"))
    })

    # ── Annual ppt: all models grey + selected colored + historical ───────
    output$ppt_all_plot <- renderPlot({
      df <- all_future(); hdf <- hist_data()
      req(!is.null(df))
      req("ppt" %in% unique(df$Variable))

      ann_fut <- df %>%
        dplyr::filter(Variable == "ppt") %>%
        dplyr::mutate(daily_mean = row_eco_mean(.),
                      ms = paste(model, scenario, sep = "_")) %>%
        dplyr::group_by(model, scenario, ms, Year) %>%
        dplyr::summarise(annual = sum(daily_mean, na.rm = TRUE), .groups = "drop")

      sel_wd <- rv$selected_models$warm_dry %||% ".none."
      sel_ww <- rv$selected_models$warm_wet %||% ".none."
      sel_cd <- rv$selected_models$cool_dry %||% ".none."
      sel_cw <- rv$selected_models$cool_wet %||% ".none."
      sel      <- c(sel_wd, sel_ww, sel_cd, sel_cw)
      bg_data  <- dplyr::filter(ann_fut, !(ms %in% sel))
      dec_data <- dplyr::filter(ann_fut, ms %in% sel) %>%
        dplyr::mutate(corner = dplyr::case_when(
          ms == sel_wd ~ "Warm / Dry",
          ms == sel_ww ~ "Warm / Wet",
          ms == sel_cd ~ "Cool / Dry",
          ms == sel_cw ~ "Cool / Wet"
        ))

      p <- ggplot2::ggplot()

      if (nrow(bg_data) > 0) {
        p <- p + ggplot2::geom_line(data = bg_data,
                                     ggplot2::aes(x = Year, y = annual, group = ms),
                                     color = "grey70", linewidth = 0.3, alpha = 0.6)
      }
      if (nrow(dec_data) > 0) {
        p <- p + ggplot2::geom_line(data = dec_data,
                                     ggplot2::aes(x = Year, y = annual,
                                                  color = corner, group = ms),
                                     linewidth = 1.1) +
          ggplot2::scale_color_manual(name = "Corner", values = CORNER_COLORS)
      }

      if (!is.null(hdf) && "ppt" %in% unique(hdf$Variable)) {
        ann_hist <- hdf %>%
          dplyr::filter(Variable == "ppt") %>%
          dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
          dplyr::group_by(Year) %>%
          dplyr::summarise(annual = sum(daily_mean, na.rm = TRUE), .groups = "drop")
        p <- p + ggplot2::geom_line(data = ann_hist,
                                     ggplot2::aes(x = Year, y = annual,
                                                  linetype = "Historic PRISM"),
                                     color = "black", linewidth = 1.2) +
          ggplot2::scale_linetype_manual(name = "Observed",
                                         values = c("Historic PRISM" = "dashed"))
      }

      p + ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::labs(
          title    = paste(rv$park %||% "Landscape", "– Annual Total Precipitation"),
          subtitle = "All CMIP6 models | Landscape-averaged across eco units",
          x = "Year", y = "Annual Precip (cm)",
          caption  = "Grey = all other models | Dashed = PRISM historical"
        ) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(legend.position = "right",
                       panel.grid.minor = ggplot2::element_blank(),
                       plot.caption     = ggplot2::element_text(size = 8, color = "grey50"))
    })

    # ── Seasonal cycles for decision models vs historical ─────────────────
    seasonal_data <- reactive({
      df  <- all_future(); hdf <- hist_data()
      sel <- unlist(rv$selected_models)
      req(!is.null(df), length(sel) > 0)

      dec_df <- df %>%
        dplyr::mutate(ms = paste(model, scenario, sep = "_")) %>%
        dplyr::filter(ms %in% sel)

      list(future = dec_df, hist = hdf)
    })

    output$seasonal_tmax <- renderPlot({
      sd <- seasonal_data()
      df  <- sd$future; hdf <- sd$hist
      req(!is.null(df))
      req("maxtemp" %in% unique(df$Variable))

      sel_wd <- rv$selected_models$warm_dry %||% ".none."
      sel_ww <- rv$selected_models$warm_wet %||% ".none."
      sel_cd <- rv$selected_models$cool_dry %||% ".none."
      sel_cw <- rv$selected_models$cool_wet %||% ".none."
      sel <- c(sel_wd, sel_ww, sel_cd, sel_cw)
      seasonal_fut <- df %>%
        dplyr::filter(Variable == "maxtemp") %>%
        dplyr::mutate(ms = paste(model, scenario, sep = "_"),
                      daily_mean = row_eco_mean(.),
                      corner = dplyr::case_when(
                        ms == sel_wd ~ "Warm / Dry",
                        ms == sel_ww ~ "Warm / Wet",
                        ms == sel_cd ~ "Cool / Dry",
                        ms == sel_cw ~ "Cool / Wet"
                      )) %>%
        dplyr::filter(!is.na(corner), Year >= 2071) %>%
        dplyr::group_by(corner, Month) %>%
        dplyr::summarise(mean_val = mean(daily_mean, na.rm = TRUE), .groups = "drop")

      p <- ggplot2::ggplot(seasonal_fut,
                           ggplot2::aes(x = Month, y = mean_val, color = corner)) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::scale_color_manual(values = CORNER_COLORS, name = "Corner") +
        ggplot2::scale_x_continuous(breaks = 1:12,
                                    labels = month.abb) +
        ggplot2::labs(title = "Seasonal Cycle: Mean Daily Tmax",
                      subtitle = "Decision models (2071–2100)",
                      x = "Month", y = "Mean Tmax (°C)") +
        ggplot2::theme_minimal(base_size = 12)

      if (!is.null(hdf)) {
        var_h <- if ("tmax" %in% unique(hdf$Variable)) "tmax" else "maxtemp"
        if (var_h %in% unique(hdf$Variable)) {
          hist_seas <- hdf %>%
            dplyr::filter(Variable == var_h) %>%
            dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
            dplyr::group_by(Month) %>%
            dplyr::summarise(mean_val = mean(daily_mean, na.rm = TRUE), .groups = "drop")
          p <- p + ggplot2::geom_line(data = hist_seas,
                                       ggplot2::aes(x = Month, y = mean_val),
                                       color = "black", linewidth = 1.2,
                                       linetype = "dashed", inherit.aes = FALSE)
        }
      }
      p
    })

    output$seasonal_ppt <- renderPlot({
      sd <- seasonal_data(); df <- sd$future; hdf <- sd$hist
      req(!is.null(df), "ppt" %in% unique(df$Variable))

      sel_wd2 <- rv$selected_models$warm_dry %||% ".none."
      sel_ww2 <- rv$selected_models$warm_wet %||% ".none."
      sel_cd2 <- rv$selected_models$cool_dry %||% ".none."
      sel_cw2 <- rv$selected_models$cool_wet %||% ".none."
      seasonal_fut <- df %>%
        dplyr::filter(Variable == "ppt") %>%
        dplyr::mutate(ms = paste(model, scenario, sep = "_"),
                      daily_mean = row_eco_mean(.),
                      corner = dplyr::case_when(
                        ms == sel_wd2 ~ "Warm / Dry",
                        ms == sel_ww2 ~ "Warm / Wet",
                        ms == sel_cd2 ~ "Cool / Dry",
                        ms == sel_cw2 ~ "Cool / Wet"
                      )) %>%
        dplyr::filter(!is.na(corner), Year >= 2071) %>%
        dplyr::group_by(corner, Month) %>%
        dplyr::summarise(mean_val = mean(daily_mean, na.rm = TRUE), .groups = "drop")

      p <- ggplot2::ggplot(seasonal_fut,
                           ggplot2::aes(x = Month, y = mean_val, color = corner)) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::scale_color_manual(values = CORNER_COLORS, name = "Corner") +
        ggplot2::scale_x_continuous(breaks = 1:12, labels = month.abb) +
        ggplot2::labs(title = "Seasonal Cycle: Mean Daily Precip",
                      subtitle = "Decision models (2071–2100)",
                      x = "Month", y = "Mean Daily Precip (cm)") +
        ggplot2::theme_minimal(base_size = 12)

      if (!is.null(hdf) && "ppt" %in% unique(hdf$Variable)) {
        hist_seas <- hdf %>%
          dplyr::filter(Variable == "ppt") %>%
          dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
          dplyr::group_by(Month) %>%
          dplyr::summarise(mean_val = mean(daily_mean, na.rm = TRUE), .groups = "drop")
        p <- p + ggplot2::geom_line(data = hist_seas,
                                     ggplot2::aes(x = Month, y = mean_val),
                                     color = "black", linewidth = 1.2,
                                     linetype = "dashed", inherit.aes = FALSE)
      }
      p
    })

    # ── Delta table ───────────────────────────────────────────────────────
    output$delta_table <- renderDT({
      cf <- corners_df(); req(!is.null(cf) && nrow(cf) > 0)
      sel_wd <- rv$selected_models$warm_dry %||% ".none."
      sel_ww <- rv$selected_models$warm_wet %||% ".none."
      sel_cd <- rv$selected_models$cool_dry %||% ".none."
      sel_cw <- rv$selected_models$cool_wet %||% ".none."

      cf_show <- cf %>%
        dplyr::mutate(
          ms       = paste(model, scenario, sep = "_"),
          Selected = dplyr::case_when(
            ms == sel_wd ~ "Warm/Dry",
            ms == sel_ww ~ "Warm/Wet",
            ms == sel_cd ~ "Cool/Dry",
            ms == sel_cw ~ "Cool/Wet",
            TRUE ~ ""
          ),
          `ΔT (°C)`    = round(delta_T_C,   2),
          `ΔP (%)`     = round(delta_P_pct, 1)
        ) %>%
        dplyr::select(model, scenario, `ΔT (°C)`, `ΔP (%)`, Selected) %>%
        dplyr::arrange(dplyr::desc(`ΔT (°C)`))

      DT::datatable(cf_show,
        options  = list(pageLength = 25, dom = "tip"),
        rownames = FALSE
      ) %>%
        DT::formatStyle("Selected",
          target          = "row",
          backgroundColor = DT::styleEqual(
            c("Warm/Dry","Warm/Wet","Cool/Dry","Cool/Wet"),
            c("#fee0d2","#fde0dd","#deebf7","#c6dbef")
          )
        )
    })
  })
}
