# remove_SH_variable.R
# Removes all rows where Variable == "SH" from every CSV in
# each park's future_with_wind folder, then overwrites the file.

library(readr)
library(dplyr)

base_dir <- "/Users/jlamping/University of Oregon Dropbox/James Lamping/Lamping/NPS_postdoc/Spatial/Climate/outputs/future_climate_processed_shiny"

csv_files <- list.files(
  path       = base_dir,
  pattern    = "\\.csv$",
  recursive  = TRUE,
  full.names = TRUE
)

# Only touch files inside future_with_wind folders
csv_files <- csv_files[grepl("/future_with_wind/", csv_files, fixed = TRUE)]

cat(sprintf("Found %d files to process.\n\n", length(csv_files)))

for (fp in csv_files) {
  df      <- read_csv(fp, show_col_types = FALSE)
  n_before <- nrow(df)
  n_sh     <- sum(df$Variable == "SH", na.rm = TRUE)

  if (n_sh == 0) {
    cat(sprintf("  SKIP  %s  (no SH rows)\n", basename(fp)))
    next
  }

  df_clean <- df %>% filter(Variable != "SH")
  write_csv(df_clean, fp)

  cat(sprintf("  OK    %s  | removed %d SH rows (%d -> %d rows)\n",
              basename(fp), n_sh, n_before, nrow(df_clean)))
}

cat("\nDone.\n")
