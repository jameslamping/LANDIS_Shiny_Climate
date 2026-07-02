# ============================================================
# mod_future.R  –  Tab 3: Future Climate (NEX-GDDP-CMIP6)
# ============================================================

ALL_CMIP6_MODELS <- c(
  "ACCESS-CM2","ACCESS-ESM1-5","CanESM5","CMCC-ESM2","CNRM-CM6-1",
  "CNRM-ESM2-1","EC-Earth3","EC-Earth3-Veg-LR","FGOALS-g3","GFDL-ESM4",
  "GISS-E2-1-G","HadGEM3-GC31-LL","INM-CM4-8","INM-CM5-0","KACE-1-0-G",
  "MIROC-ES2L","MPI-ESM1-2-HR","MPI-ESM1-2-LR","MRI-ESM2-0","NorESM2-LM",
  "NorESM2-MM","TaiESM1"
)

ALL_SCENARIOS <- c("ssp126","ssp245","ssp370","ssp585")

CMIP6_VARS <- c("tasmax","tasmin","pr","huss","hurs","sfcWind","rsds","rlds")

mod_future_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "3 · Future Climate",
    icon = icon("globe"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        h5("Future Climate Options", class = "text-primary fw-bold"),
        hr(),
        h6("Upload Processed Future CSVs", class = "text-secondary"),
        p(class = "text-muted small",
          "Select a folder containing one CSV per model-scenario. CSVs must follow",
          " the LANDIS wide format with eco* columns."),
        textInput(ns("future_dir_path"), "Future CSV Directory (full path)",
                  placeholder = "/path/to/future/csvs"),
        actionButton(ns("load_future_dir"), "Load Directory",
                     icon = icon("folder-open"), class = "btn-outline-primary w-100 mb-2"),
        hr(),
        h6("— OR — Download from AWS", class = "text-secondary text-center"),
        checkboxGroupInput(ns("models"), "GCM Models",
                           choices = ALL_CMIP6_MODELS,
                           selected = c("ACCESS-CM2","GISS-E2-1-G","INM-CM5-0","TaiESM1")),
        checkboxGroupInput(ns("scenarios"), "SSP Scenarios",
                           choices = ALL_SCENARIOS, selected = c("ssp370","ssp585")),
        checkboxGroupInput(ns("fut_vars"), "Variables",
                           choices = CMIP6_VARS,
                           selected = c("tasmax","tasmin","pr","hurs","sfcWind")),
        textInput(ns("nc_dir"), "Downloaded NetCDF Directory",
                  placeholder = "/path/to/netcdfs"),
        actionButton(ns("check_cores"), "Check available cores",
                     icon = icon("microchip"),
                     class = "btn-sm btn-outline-secondary w-100 mb-1"),
        uiOutput(ns("cores_info")),
        textInput(ns("fut_workers"), "Parallel Workers",
                  value = max(1L, (parallel::detectCores(logical = FALSE) %||% 4L) - 1L)),
        actionButton(ns("run_aws_download"), "Download from AWS S3",
                     icon = icon("download"), class = "btn-primary w-100 mb-1"),
        actionButton(ns("run_processing"), "Process NetCDFs → LANDIS CSVs",
                     icon = icon("cogs"), class = "btn-success w-100")
      ),

      div(
        uiOutput(ns("status_bar")),
        navset_card_tab(
          nav_panel("Log",
            verbatimTextOutput(ns("log_out")) %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Coverage Matrix",
            plotOutput(ns("coverage_plot"), height = "450px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Annual Tmax Preview",
            plotOutput(ns("ann_tmax_plot"), height = "420px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("Annual Precip Preview",
            plotOutput(ns("ann_ppt_plot"), height = "420px") %>% withSpinner(color = "#2c7bb6")
          ),
          nav_panel("File List",
            DTOutput(ns("file_table")) %>% withSpinner(color = "#2c7bb6")
          )
        )
      )
    )
  )
}

mod_future_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    log_path    <- reactive(file.path(tempdir(), "future_download.log"))
    bg_proc     <- reactiveVal(NULL)
    future_df   <- reactiveVal(NULL)   # combined long tibble for quick plots
    future_files_df <- reactiveVal(NULL)  # tibble of file metadata

    # ── Worker count: parse text input, clamp to [1, available cores] ─────
    n_workers <- reactive({
      avail <- max(1L, parallel::detectCores(logical = TRUE) %||% 4L)
      req   <- suppressWarnings(as.integer(input$fut_workers))
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

    # ── Load pre-processed directory ──────────────────────────────────────
    observeEvent(input$load_future_dir, {
      dir_path <- trimws(input$future_dir_path)
      req(nchar(dir_path) > 0, dir.exists(dir_path))

      files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
      if (length(files) == 0) {
        showNotification("No CSV files found in that directory.", type = "error"); return()
      }

      # Build metadata table
      meta <- tibble::tibble(path = files) %>%
        dplyr::mutate(
          filename = basename(path),
          model    = stringr::str_split_i(filename, "_", 1),
          scenario = stringr::str_split_i(filename, "_", 2),
          size_mb  = round(file.size(path) / 1e6, 1)
        )
      future_files_df(meta)
      rv$future_dir <- dir_path

      # Load a sample for quick plots (first 3 files)
      withProgress(message = "Sampling future files for preview...", {
        sample_files <- head(files, min(length(files), 6))
        df_list <- purrr::map(sample_files, function(fp) {
          df <- readr::read_csv(fp, show_col_types = FALSE)
          bn <- basename(fp)
          df$model    <- stringr::str_split_i(bn, "_", 1)
          df$scenario <- stringr::str_split_i(bn, "_", 2)
          df
        })
        future_df(dplyr::bind_rows(df_list))
      })
      showNotification(paste(length(files), "future climate files loaded."), type = "message")
    })

    # ── AWS S3 Download (background) ──────────────────────────────────────
    observeEvent(input$run_aws_download, {
      req(rv$out_dir)

      # Derive bounding box from the climate regions raster (+ 1.5° buffer)
      req(!is.null(rv$climreg_path) && file.exists(rv$climreg_path))
      clim_r <- terra::rast(rv$climreg_path)
      if (terra::crs(clim_r) == "") {
        showNotification(
          "Climate regions raster has no CRS — cannot determine download bbox. Please assign a coordinate reference system to your raster and reload it in Setup.",
          type = "error", duration = 10
        )
        return()
      }
      clim_wgs    <- terra::project(clim_r, "epsg:4326")
      e           <- terra::ext(clim_wgs)
      buf         <- 1.5
      bbox_coords <- c(e$xmin - buf, e$xmax + buf, e$ymin - buf, e$ymax + buf)
      cat(sprintf("Bbox: xmin=%.2f xmax=%.2f ymin=%.2f ymax=%.2f\n",
                  bbox_coords[1], bbox_coords[2], bbox_coords[3], bbox_coords[4]))

      log_file <- log_path()
      writeLines("=== AWS Download Started ===", log_file)
      nc_out   <- if (nchar(trimws(input$nc_dir)) > 0) input$nc_dir
                  else file.path(rv$out_dir, "nex_gddp_nc")
      dir.create(nc_out, recursive = TRUE, showWarnings = FALSE)

      proc <- callr::r_bg(
        func = function(models, scenarios, vars_keep, bbox_coords, out_dir, log_file, workers) {
          suppressPackageStartupMessages({
            library(aws.s3); library(terra); library(stringr)
            library(dplyr); library(glue); library(furrr); library(future)
          })
          Sys.setenv("AWS_DEFAULT_REGION" = "us-west-2")
          bucket <- "nex-gddp-cmip6"
          cat("Querying S3 bucket...\n", file = log_file, append = TRUE)

          prefixes <- expand.grid(models = models, scenarios = scenarios) %>%
            dplyr::mutate(prefix = glue("NEX-GDDP-CMIP6/{models}/{scenarios}/")) %>%
            dplyr::pull(prefix)

          all_files <- dplyr::bind_rows(lapply(prefixes, function(prefix) {
            tryCatch(
              data.table::rbindlist(
                aws.s3::get_bucket(bucket, prefix = prefix, max = Inf,
                                   use_https = TRUE, region = "us-west-2", url_style = "path"),
                use.names = TRUE, fill = TRUE
              ),
              error = function(e) { cat("FAIL:", prefix, "\n", file = log_file, append = TRUE); NULL }
            )
          }))

          valid_keys <- all_files %>%
            dplyr::mutate(
              variable = str_extract(Key, paste(vars_keep, collapse = "|")),
              version  = str_extract(Key, "v[0-9]+\\.[0-9]+")
            ) %>%
            dplyr::filter(version == "v2.0", !is.na(variable)) %>%
            dplyr::pull(Key)

          cat(length(valid_keys), "files to download\n", file = log_file, append = TRUE)

          # bbox_coords is a plain numeric vector — serializes safely to parallel workers.
          # After terra::rotate() the raster is in WGS84 lon/lat, so we can crop
          # directly with terra::ext() rather than passing a SpatVector (which cannot
          # be serialized across furrr multisession workers).
          plan(multisession, workers = workers)
          furrr::future_walk(valid_keys, function(key) {
            filename <- basename(key)
            parts    <- unlist(stringr::str_split(filename, "_"))
            out_file <- file.path(out_dir, glue("{parts[3]}_{parts[4]}_{parts[1]}_{parts[7]}"))
            if (file.exists(paste0(out_file, ".nc"))) return(NULL)
            tmp <- tempfile(fileext = ".nc")
            tryCatch({
              aws.s3::save_object(key, bucket = bucket, file = tmp,
                                  use_https = TRUE, region = "us-west-2")
              cat("  downloaded:", basename(key), "\n", file = log_file, append = TRUE)
              r <- terra::rast(tmp)
              ti <- terra::time(r)
              r  <- terra::subset(r, which(ti >= as.Date("2015-01-01") & ti <= as.Date("2100-12-31")))
              cat("  subset ok:", basename(key), "\n", file = log_file, append = TRUE)
              r  <- terra::rotate(r)
              r  <- terra::crop(r, terra::ext(bbox_coords[1], bbox_coords[2],
                                              bbox_coords[3], bbox_coords[4]), snap = "out")
              cat("  crop ok:", basename(key), "\n", file = log_file, append = TRUE)
              terra::writeCDF(r, paste0(out_file, ".nc"), overwrite = TRUE,
                              varname = parts[1], xname = "lon", yname = "lat",
                              varunit = terra::units(r)[1])
              unlink(tmp)
              cat("OK:", basename(out_file), "\n", file = log_file, append = TRUE)
            }, error = function(e) {
              cat("FAIL:", key, "-- ERROR:", conditionMessage(e), "\n",
                  file = log_file, append = TRUE)
              unlink(tmp)
            })
          }, .progress = FALSE)
          cat("=== AWS download complete ===\n", file = log_file, append = TRUE)
        },
        args = list(
          models      = input$models,
          scenarios   = input$scenarios,
          vars_keep   = input$fut_vars,
          bbox_coords = bbox_coords,
          out_dir     = nc_out,
          log_file    = log_file,
          workers     = n_workers()
        ),
        stderr = log_file, stdout = log_file
      )
      bg_proc(proc)
      showNotification("AWS download started in background.", type = "message")
    })

    # ── Process NetCDFs → LANDIS CSVs (background) ────────────────────────
    observeEvent(input$run_processing, {
      req(rv$climreg_path, rv$out_dir, rv$park)
      nc_dir <- trimws(input$nc_dir)
      req(nchar(nc_dir) > 0 && dir.exists(nc_dir))
      log_file <- log_path()
      writeLines("=== NetCDF Processing Started ===", log_file)
      out_dir  <- file.path(rv$out_dir, "future_processed")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      proc <- callr::r_bg(
        func = future_processing_worker,
        args = list(
          nc_dir       = nc_dir,
          climreg_path = rv$climreg_path,
          out_dir      = out_dir,
          park         = rv$park,
          models       = input$models,
          scenarios    = input$scenarios,
          log_file     = log_file,
          workers      = n_workers()
        ),
        stderr = log_file, stdout = log_file
      )
      bg_proc(proc)
      showNotification("NetCDF processing started in background.", type = "message")
    })

    # ── Poll log ──────────────────────────────────────────────────────────
    log_timer <- reactiveTimer(2000)
    output$log_out <- renderText({
      log_timer()
      p <- bg_proc()
      if (!is.null(p) && !p$is_alive()) {
        out_dir <- file.path(rv$out_dir, "future_processed")
        if (dir.exists(out_dir) && length(list.files(out_dir, "\\.csv$")) > 0) {
          rv$future_dir <- out_dir
          bg_proc(NULL)
          showNotification("Processing complete!", type = "message")
        }
      }
      tail_log(log_path())
    })

    # ── Status bar ────────────────────────────────────────────────────────
    output$status_bar <- renderUI({
      meta <- future_files_df()
      if (!is.null(meta)) {
        div(class = "alert alert-success py-2 mb-3",
            icon("check-circle"), " ",
            strong(nrow(meta), " future climate files"), " loaded | ",
            strong(length(unique(meta$model)), " models"), " × ",
            strong(length(unique(meta$scenario)), " scenarios"))
      } else {
        div(class = "alert alert-info py-2 mb-3",
            icon("info-circle"),
            " Upload a directory of future climate CSVs or run the download + processing pipeline.")
      }
    })

    # ── Coverage heatmap ─────────────────────────────────────────────────
    output$coverage_plot <- renderPlot({
      meta <- future_files_df()
      req(!is.null(meta))
      ggplot2::ggplot(meta, ggplot2::aes(x = scenario, y = forcats::fct_rev(model),
                                          fill = size_mb)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.5) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(size_mb, " MB")),
                           size = 3, color = "black") +
        ggplot2::scale_fill_gradient(low = "#e8f5e9", high = "#2e7d32",
                                     name = "File size (MB)") +
        ggplot2::labs(title = "Future Climate File Coverage",
                      subtitle = "Green = file present, size shown in MB",
                      x = "Scenario", y = "Model") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_text(size = 10))
    })

    # ── Annual tmax preview ───────────────────────────────────────────────
    output$ann_tmax_plot <- renderPlot({
      df <- future_df(); req(!is.null(df))
      req("maxtemp" %in% unique(df$Variable))
      ann <- df %>%
        dplyr::filter(Variable == "maxtemp") %>%
        dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
        dplyr::group_by(model, scenario, Year) %>%
        dplyr::summarise(annual = mean(daily_mean, na.rm = TRUE), .groups = "drop")

      ggplot2::ggplot(ann, ggplot2::aes(x = Year, y = annual,
                                         color = interaction(model, scenario))) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::labs(title = "Annual Mean Daily Tmax — Future Models (Sample)",
                      y = "Mean Tmax (°C)", color = "Model × Scenario") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "right")
    })

    # ── Annual ppt preview ────────────────────────────────────────────────
    output$ann_ppt_plot <- renderPlot({
      df <- future_df(); req(!is.null(df))
      req("ppt" %in% unique(df$Variable))
      ann <- df %>%
        dplyr::filter(Variable == "ppt") %>%
        dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
        dplyr::group_by(model, scenario, Year) %>%
        dplyr::summarise(annual = sum(daily_mean, na.rm = TRUE), .groups = "drop")

      ggplot2::ggplot(ann, ggplot2::aes(x = Year, y = annual,
                                         color = interaction(model, scenario))) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::labs(title = "Annual Total Precipitation — Future Models (Sample)",
                      y = "Annual Total Precip (cm)", color = "Model × Scenario") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "right")
    })

    # ── File list table ───────────────────────────────────────────────────
    output$file_table <- renderDT({
      meta <- future_files_df(); req(!is.null(meta))
      DT::datatable(
        meta %>% dplyr::select(filename, model, scenario, size_mb),
        options = list(pageLength = 20, dom = "tip", scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}

# ── Background NetCDF processing worker ──────────────────────────────────────
future_processing_worker <- function(nc_dir, climreg_path, out_dir, park,
                                      models, scenarios, log_file, workers) {
  suppressPackageStartupMessages({
    library(terra); library(dplyr); library(stringr); library(lubridate)
    library(glue); library(furrr); library(future); library(tidyr); library(readr)
  })
  options(scipen = 999)
  cat("=== Processing NetCDFs ===\n", file = log_file, append = TRUE)

  nc_files <- list.files(nc_dir, pattern = "\\.nc$", full.names = TRUE)
  eco_r    <- rast(climreg_path); names(eco_r) <- "ecoregion"

  file_info <- tibble(file = nc_files) %>%
    mutate(
      name     = basename(file),
      model    = str_split_i(name, "_", 1),
      scenario = str_split_i(name, "_", 2),
      variable = str_split_i(name, "_", 3),
      group    = glue("{model}_{scenario}")
    ) %>%
    filter(model %in% models, scenario %in% scenarios)

  groups <- unique(file_info$group)

  for (g in groups) {
    cat("Processing group:", g, "\n", file = log_file, append = TRUE)
    sub_files <- file_info %>% filter(group == g)
    plan(multisession, workers = workers)

    group_df <- furrr::future_map_dfr(seq_len(nrow(sub_files)), function(i) {
      f        <- sub_files[i,]$file
      model    <- sub_files[i,]$model
      scenario <- sub_files[i,]$scenario
      variable <- sub_files[i,]$variable
      eco_r2   <- rast(climreg_path); names(eco_r2) <- "ecoregion"
      r        <- rast(f)
      dates    <- as.Date(time(r))
      names(r) <- as.character(dates)
      r <- project(r, crs(eco_r2))
      r <- resample(r, eco_r2, method = "bilinear", threads = FALSE)
      z <- as.data.frame(zonal(r, eco_r2, fun = "mean", na.rm = TRUE))
      rm(r); terra::tmpFiles(remove = TRUE)
      z_long <- z %>%
        pivot_longer(cols = -ecoregion, names_to = "date", values_to = "value") %>%
        mutate(date = as.Date(date), variable = variable, model = model, scenario = scenario)
      z_long
    })

    final_df <- group_df %>%
      mutate(Year = year(date), Month = month(date), Day = day(date)) %>%
      select(Year, Month, Day, variable, ecoregion, value) %>%
      pivot_wider(names_from = ecoregion, values_from = value, names_prefix = "eco") %>%
      rename(Variable = variable) %>%
      mutate(Variable = case_when(
        Variable == "pr"      ~ "ppt",
        Variable == "tasmax"  ~ "maxtemp",
        Variable == "tasmin"  ~ "mintemp",
        Variable == "huss"    ~ "SH",
        Variable == "hurs"    ~ "RH",
        Variable == "sfcWind" ~ "windSpeed",
        .default = Variable
      )) %>%
      mutate(across(starts_with("eco"), ~ case_when(
        Variable == "ppt"      ~ .x * 86400 / 10,
        Variable == "maxtemp"  ~ .x - 273.15,
        Variable == "mintemp"  ~ .x - 273.15,
        TRUE                   ~ .x
      )))

    out_fp <- file.path(out_dir, glue("{g}_{park}.csv"))
    write_csv(final_df, out_fp)
    cat("Saved:", basename(out_fp), "\n", file = log_file, append = TRUE)
    gc(verbose = FALSE)
  }
  cat("=== NetCDF processing complete ===\n", file = log_file, append = TRUE)
}
