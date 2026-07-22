library(tidyverse)
library(lubridate)

# ============================================================
# STEP 1 — Load and prepare the leptospirosis case data
# ============================================================

lepto <- read_csv("leptospirosis_monthly_by_district.csv")

lepto <- lepto %>%
  mutate(month = mdy(month)) %>%
  mutate(year = year(month), month_num = month(month))

# Reshape to long format: one row per district-month
lepto_long <- lepto %>%
  select(-total_all_districts, -n_weeks_in_month) %>%
  pivot_longer(cols = -c(month, year, month_num),
               names_to = "district", values_to = "cases")

glimpse(lepto_long)

# ============================================================
# STEP 2 — Load and prepare the population data
# ============================================================

pop_data <- read_csv("district_population_2007_2025.csv")

# Reshape to long format: one row per district-year
pop_long <- pop_data %>%
  pivot_longer(cols = -district, names_to = "year", values_to = "population") %>%
  mutate(year = as.numeric(year))

glimpse(pop_long)

# ============================================================
# STEP 3 — Confirm district names match between the two datasets
# ============================================================

print(sort(unique(lepto_long$district)))
print(sort(unique(pop_long$district)))

# Should return character(0) if every district matches
setdiff(unique(lepto_long$district), unique(pop_long$district))

# ============================================================
# STEP 4 — Merge case data with population data (by district AND year)
# and calculate monthly incidence rate per 100,000 population
# ============================================================

lepto_incidence <- lepto_long %>%
  left_join(pop_long, by = c("district", "year")) %>%
  mutate(
    incidence_per_100k = (cases / population) * 100000
  )

glimpse(lepto_incidence)

# ============================================================
# STEP 5 — Check for any missing matches (e.g. years outside 2007-2025)
# ============================================================

lepto_incidence %>%
  filter(is.na(population)) %>%
  distinct(district, year)

# ============================================================
# STEP 6 — Inspect the final dataset
# ============================================================

lepto_incidence %>%
  arrange(district, month) %>%
  select(district, month, year, month_num, cases, population, incidence_per_100k) %>%
  head(20)

# ============================================================
# STEP 7 — Save the final incidence dataset for later steps
# ============================================================

write_csv(lepto_incidence, "leptospirosis_monthly_incidence.csv")

