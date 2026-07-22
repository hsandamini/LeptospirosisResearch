## convert_weekly_to_monthly.R
##
## Converts the completed weekly leptospirosis dataset (wide format, one
## column per district) into a monthly dataset.
##
## Method: each week is assigned to whichever calendar month contains the
## MAJORITY of its 7 days (since epidemiological weeks don't align cleanly
## with calendar months), then weekly case counts are SUMMED within each
## month for each district.

library(tidyverse)
library(lubridate)
library(readxl)

# ---- Load the completed weekly dataset ----
weekly <- read_excel(
  "Sri_Lanka_Leptospirosis_WIDE_FORMAT.xlsx",
  sheet = "Leptospirosis Weekly (Wide)"
)

# District columns are everything except the first 3 (Start date, End date, Week Period)
district_cols <- names(weekly)[4:ncol(weekly)]

cat("Loaded weekly dataset:", nrow(weekly), "weeks,",
    length(district_cols), "districts\n")

# ---- Function: find the month containing the majority of a week's 7 days ----
majority_month <- function(start_date) {
  days_in_week <- start_date + days(0:6)
  months_of_days <- floor_date(days_in_week, unit = "month")
  # Return whichever month appears most often among the 7 days
  tab <- table(months_of_days)
  as_date(names(tab)[which.max(tab)])
}

# ---- Assign each week to its majority month ----
weekly <- weekly %>%
  mutate(month = map_vec(`Start date`, majority_month))

# ---- Aggregate: SUM weekly case counts within each month, per district ----
monthly <- weekly %>%
  group_by(month) %>%
  summarise(across(all_of(district_cols), ~ sum(.x, na.rm = TRUE)),
            n_weeks_in_month = n(),
            .groups = "drop") %>%
  arrange(month)

# ---- Add a total-across-all-districts column (optional, useful for quick checks) ----
monthly <- monthly %>%
  mutate(total_all_districts = rowSums(across(all_of(district_cols)), na.rm = TRUE)) %>%
  relocate(total_all_districts, .after = month)

cat("\nMonthly dataset:", nrow(monthly), "months\n")
print(head(monthly, 6))

# ---- Save the result ----
write_csv(monthly, "leptospirosis_monthly_by_district.csv")
cat("\nSaved to leptospirosis_monthly_by_district.csv\n")
