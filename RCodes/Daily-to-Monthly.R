## aggregate_climate_to_monthly.R
##
## Reads all 25 precipitation CSVs and all 25 temperature CSVs, corrects
## known filename/spelling inconsistencies, combines them into one daily
## dataset, then aggregates to MONTHLY resolution to match the
## leptospirosis dataset's time frequency.
##
## Handles these known irregularities automatically:
##   - Gampaha's precipitation file is missing an underscore (DailyPPTGampaha.csv)
##   - "Vauniya" (typo in precipitation files) vs "Vavuniya" (temperature files)
##   - "NuwaraEliya" -> "Nuwara Eliya", "Puttlam" -> "Puttalam"
##     (to match the spelling used in the leptospirosis dataset)
##   - 2 known missing dates in ALL temperature files (2022-04-01, 2023-09-11)
##     -> this is a genuine, documented ERA5 gap, not an error

library(tidyverse)
library(lubridate)

# ---- Settings: adjust these paths to where you've unzipped your files ----
precip_dir <- "DailyPPT"
temp_dir   <- "DailyMeanTemp"

# ---- Standard 25 district names (matching the leptospirosis dataset) ----
standard_districts <- c(
  "Colombo", "Gampaha", "Kalutara", "Kandy", "Matale", "Nuwara Eliya",
  "Galle", "Hambantota", "Matara", "Jaffna", "Kilinochchi", "Mannar",
  "Vavuniya", "Mullaitivu", "Batticaloa", "Ampara", "Trincomalee",
  "Kurunegala", "Puttalam", "Anuradhapura", "Polonnaruwa", "Badulla",
  "Monaragala", "Ratnapura", "Kegalle"
)

# ---- Function to clean up a raw district name extracted from a filename ----
clean_district_name <- function(raw_name) {
  case_when(
    raw_name == "NuwaraEliya" ~ "Nuwara Eliya",
    raw_name == "Puttlam"     ~ "Puttalam",
    raw_name == "Vauniya"     ~ "Vavuniya",   # typo fix (precipitation batch only)
    TRUE ~ raw_name
  )
}

# ---- Helper: read every CSV in a folder, extract + clean district name ----
combine_district_csvs <- function(folder_path, value_col_name) {
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  cat("Found", length(files), "files in", folder_path, "\n")
  
  combined <- map_dfr(files, function(f) {
    fname <- basename(f)
    # Extract whatever comes after the last "PPT"/"Temp" and before ".csv",
    # stripping any leading underscore. Handles both "DailyPPT_Gampaha.csv"
    # and the malformed "DailyPPTGampaha.csv" the same way.
    raw_district <- fname %>%
      str_remove("^Daily(PPT|MeanTemp)_?") %>%
      str_remove("\\.csv$")
    
    district <- clean_district_name(raw_district)
    
    df <- read_csv(f, show_col_types = FALSE)
    names(df) <- c("date", value_col_name)
    df$district <- district
    df$date <- as_date(df$date)
    df
  })
  
  combined
}

# ---- Combine each variable ----
precip_data <- combine_district_csvs(precip_dir, "precipitation_mm")
temp_data   <- combine_district_csvs(temp_dir, "temperature_c")

# ---- Confirm we have exactly the 25 expected districts, correctly named ----
cat("\nPrecipitation districts found (", length(unique(precip_data$district)), "):\n")
print(sort(unique(precip_data$district)))

cat("\nTemperature districts found (", length(unique(temp_data$district)), "):\n")
print(sort(unique(temp_data$district)))

missing_from_precip <- setdiff(standard_districts, unique(precip_data$district))
missing_from_temp   <- setdiff(standard_districts, unique(temp_data$district))

if (length(missing_from_precip) > 0) {
  cat("\nWARNING: these expected districts are MISSING from precipitation data:\n")
  print(missing_from_precip)
}
if (length(missing_from_temp) > 0) {
  cat("\nWARNING: these expected districts are MISSING from temperature data:\n")
  print(missing_from_temp)
}
if (length(missing_from_precip) == 0 && length(missing_from_temp) == 0) {
  cat("\nAll 25 districts present and correctly named in both datasets.\n")
}

# ---- Merge precipitation + temperature into one daily dataset ----
daily_climate <- precip_data %>%
  full_join(temp_data, by = c("district", "date")) %>%
  arrange(district, date)

cat("\nMerged daily climate dataset:", nrow(daily_climate), "rows,",
    length(unique(daily_climate$district)), "districts\n")
cat("Missing value summary:\n")
print(colSums(is.na(daily_climate)))

# ---- Aggregate to MONTHLY resolution ----
# Precipitation -> SUM within the month; Temperature -> MEAN and MAX within the month
daily_climate <- daily_climate %>%
  mutate(year_month = floor_date(date, unit = "month"))

monthly_climate <- daily_climate %>%
  group_by(district, year_month) %>%
  summarise(
    precipitation_total_mm = sum(precipitation_mm, na.rm = TRUE),
    rainy_days_count       = sum(precipitation_mm >= 1, na.rm = TRUE),
    temperature_mean_c     = mean(temperature_c, na.rm = TRUE),
    temperature_max_c      = max(temperature_c, na.rm = TRUE),
    n_days_in_month        = n(),
    .groups = "drop"
  ) %>%
  arrange(district, year_month)

cat("\nMonthly climate dataset:", nrow(monthly_climate), "rows,",
    length(unique(monthly_climate$district)), "districts,",
    length(unique(monthly_climate$year_month)), "months\n")

# ---- Flag any months with fewer days than expected (e.g. the 2 missing ERA5 dates) ----
monthly_climate <- monthly_climate %>%
  mutate(expected_days = days_in_month(year_month),
         incomplete_month = n_days_in_month < expected_days)

cat("\nMonths with incomplete daily coverage (expected - due to the 2 known",
    "missing ERA5 dates):", sum(monthly_climate$incomplete_month), "\n")

# ---- Save outputs ----
dir.create("data_processed", showWarnings = FALSE)
write_csv(daily_climate, "data_processed/daily_climate_combined.csv")
write_csv(monthly_climate, "data_processed/monthly_climate_combined.csv")

cat("\nSaved:\n  data_processed/daily_climate_combined.csv\n  data_processed/monthly_climate_combined.csv\n")

print(head(monthly_climate, 10))
