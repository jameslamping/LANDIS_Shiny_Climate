# remove_eco0_column.R
# Removes the eco0 column from all future climate CSVs
# (future_with_wind and future_biascorrected folders), skipping
# QA/corrections files and gridmet wind long files.

library(readr)
library(dplyr)

base_dir <- "/Users/jlamping/University of Oregon Dropbox/James Lamping/Lamping/NPS_postdoc/Spatial/Climate/outputs/future_climate_processed_shiny"

csv_files <- list.files(base_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Only climate matrix files — skip corrections QA files and gridmet long files
csv_files <- csv_files[grepl("/future_with_wind/|/future_biascorrected/", csv_files)]
csv_files <- csv_files[!grepl("^_corrections_", basename(csv_files))]

cat(sprintf("Found %d files to process.\n\n", length(csv_files)))

for (fp in csv_files) {
  df <- read_csv(fp, show_col_types = FALSE)

  if (!("eco0" %in% names(df))) {
    cat(sprintf("  SKIP  %s  (no eco0 column)\n", basename(fp)))
    next
  }

  df_clean <- df %>% select(-eco0)
  write_csv(df_clean, fp)

  cat(sprintf("  OK    %s  | dropped eco0 (%d cols -> %d cols)\n",
              basename(fp), ncol(df), ncol(df_clean)))
}

cat("\nDone.\n")
