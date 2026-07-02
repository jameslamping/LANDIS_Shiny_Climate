# ============================================================
# utils.R  –  shared data helpers for LANDIS Climate App
# ============================================================

get_eco_cols <- function(df) {
  names(df)[grepl("^eco\\d+$", names(df))]
}

read_climate_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) %>% normalize_clim_variables()
}

# ── per-row spatial mean across all eco columns ──────────────────────────────
row_eco_mean <- function(df) {
  eco_cols <- get_eco_cols(df)
  rowMeans(df[, eco_cols, drop = FALSE], na.rm = TRUE)
}

# ── Monthly mean (or sum) by ecoregion, for ggplot line charts ───────────────
compute_monthly_by_eco <- function(df, var_name, fun = "mean") {
  eco_cols <- get_eco_cols(df)
  df %>%
    dplyr::filter(Variable == var_name) %>%
    tidyr::pivot_longer(dplyr::all_of(eco_cols), names_to = "ecoregion", values_to = "value") %>%
    dplyr::mutate(
      ecoregion  = factor(ecoregion),
      month_date = as.Date(paste(Year, Month, "15", sep = "-"))
    ) %>%
    dplyr::group_by(ecoregion, month_date) %>%
    dplyr::summarise(value = if (fun == "mean") mean(value, na.rm = TRUE)
                             else                sum(value, na.rm = TRUE),
                     .groups = "drop")
}

# ── Annual mean tmax (landscape average) ─────────────────────────────────────
compute_annual_tmax <- function(df, tmax_varname = "tmax") {
  df %>%
    dplyr::filter(Variable == tmax_varname) %>%
    dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(annual_mean_tmax = mean(daily_mean, na.rm = TRUE), .groups = "drop")
}

# ── Annual total ppt (landscape average daily, summed) ───────────────────────
compute_annual_ppt <- function(df, ppt_varname = "ppt") {
  df %>%
    dplyr::filter(Variable == ppt_varname) %>%
    dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(annual_total_ppt = sum(daily_mean, na.rm = TRUE), .groups = "drop")
}

# ── Load all future CSVs from a directory into one combined tibble ────────────
load_future_dir <- function(dir_path, park_tag = NULL) {
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop("No CSV files found in: ", dir_path)

  purrr::map_dfr(files, function(fp) {
    df  <- readr::read_csv(fp, show_col_types = FALSE)
    bn  <- basename(fp)
    # strip optional _biascorrected and park tag suffixes
    tag <- if (!is.null(park_tag))
             stringr::str_remove(bn, glue::glue("_{park_tag}.*\\.csv$"))
           else
             tools::file_path_sans_ext(bn)

    parts    <- stringr::str_split_fixed(tag, "_", 2)
    df$model    <- parts[1]
    df$scenario <- parts[2]
    df
  })
}

# ── Four-corners delta calculation ────────────────────────────────────────────
compute_four_corners <- function(future_all_df, begin_years, end_years) {
  eco_cols <- get_eco_cols(future_all_df)

  # annual landscape mean per model/scenario/variable
  ann <- future_all_df %>%
    dplyr::filter(Variable %in% c("maxtemp", "ppt")) %>%
    dplyr::mutate(daily_mean = row_eco_mean(.)) %>%
    dplyr::group_by(model, scenario, Variable, Year) %>%
    dplyr::summarise(
      annual_value = if (dplyr::first(Variable) == "ppt")
                       sum(daily_mean, na.rm = TRUE)
                     else
                       mean(daily_mean, na.rm = TRUE),
      .groups = "drop"
    )

  deltas <- ann %>%
    dplyr::filter(Year >= min(c(begin_years, end_years)),
                  Year <= max(c(begin_years, end_years))) %>%
    dplyr::mutate(period = dplyr::case_when(
      Year %in% begin_years ~ "begin",
      Year %in% end_years   ~ "end",
      TRUE                  ~ NA_character_
    )) %>%
    dplyr::filter(!is.na(period)) %>%
    dplyr::group_by(model, scenario, Variable, period) %>%
    dplyr::summarise(period_mean = mean(annual_value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = period, values_from = period_mean) %>%
    dplyr::filter(!is.na(begin), !is.na(end)) %>%
    dplyr::mutate(delta_abs = end - begin,
                  delta_pct = 100 * (end - begin) / begin) %>%
    dplyr::select(model, scenario, Variable, delta_abs, delta_pct) %>%
    tidyr::pivot_wider(names_from = Variable,
                       values_from = c(delta_abs, delta_pct),
                       names_sep = "_") %>%
    dplyr::transmute(
      model, scenario,
      delta_T_C   = delta_abs_maxtemp,
      delta_P_pct = delta_pct_ppt
    ) %>%
    dplyr::filter(!is.na(delta_T_C), !is.na(delta_P_pct))

  deltas
}

# ── LANDIS variable name normalisation (case-insensitive) ────────────────────
# Maps any accepted Climate Library v5 spelling to the canonical internal name.
.CLIM_VAR_MAP <- c(
  tmax             = "tmax",   maxtemp            = "tmax",
  tmin             = "tmin",   mintemp            = "tmin",
  temp             = "temp",
  precip           = "ppt",    ppt                = "ppt",
  winddirection    = "windDirection",
  windspeed        = "windSpeed",
  windnorthing     = "windNorthing",
  windeasting      = "windEasting",
  maxrh            = "maxRH",
  minrh            = "minRH",
  rh               = "RH",
  sh               = "SH",
  dewpoint         = "dewpoint",
  ndeposition      = "Ndeposition", ndep           = "Ndeposition",
  co2              = "CO2",
  par              = "PAR",
  ozone            = "ozone",  o3                 = "ozone",
  swr              = "SWR",    shortwaveradiation  = "SWR",
  pet              = "PET"
)

normalize_clim_variables <- function(df) {
  if (!"Variable" %in% names(df)) return(df)
  canonical <- .CLIM_VAR_MAP[tolower(df$Variable)]
  df$Variable <- dplyr::if_else(!is.na(canonical), canonical, df$Variable)
  df
}

# ── Null-coalescing operator ─────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nchar(a[1]) > 0) a else b

# ── Tail last N lines of a text log file ─────────────────────────────────────
tail_log <- function(path, n = 80) {
  if (!file.exists(path)) return("Waiting for log...")
  lines <- readLines(path, warn = FALSE)
  paste(tail(lines, n), collapse = "\n")
}

# ── Wind long-to-wide helper ─────────────────────────────────────────────────
wind_long_to_wide <- function(wind_long, value_col, eco_cols) {
  wind_long %>%
    dplyr::mutate(
      Year  = lubridate::year(date),
      Month = lubridate::month(date),
      Day   = lubridate::day(date),
      value = !!rlang::sym(value_col)
    ) %>%
    dplyr::select(Year, Month, Day, eco, value) %>%
    tidyr::pivot_wider(names_from = eco, values_from = value) %>%
    dplyr::select(Year, Month, Day, dplyr::all_of(eco_cols))
}

# ── Add variable rows to a climate df if not already present ─────────────────
add_variable_rows <- function(clim_df, var_name, wide_df, eco_cols) {
  if (any(clim_df$Variable == var_name)) return(clim_df)
  dplyr::bind_rows(
    clim_df,
    wide_df %>%
      dplyr::mutate(Variable = var_name) %>%
      dplyr::select(Year, Month, Day, Variable, dplyr::all_of(eco_cols))
  ) %>% dplyr::arrange(Year, Month, Day, Variable)
}
