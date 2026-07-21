# Step 0: Convert into long format

library(tidyverse)
library(lubridate)

lepto <- read_csv("leptospirosis_monthly_by_district.csv")

lepto <- lepto %>%
  mutate(month = mdy(month)) %>%
  mutate(year = year(month), month_num = month(month))

# Long format: one row per district-month (excludes the pre-computed total & n_weeks columns)
lepto_long <- lepto %>%
  select(-total_all_districts, -n_weeks_in_month) %>%
  pivot_longer(cols = -c(month, year, month_num),
               names_to = "district", values_to = "cases")

districts <- sort(unique(lepto_long$district))

glimpse(lepto_long)

###########################################################################
# Step 1: National-level descriptive statistics

national_stats <- lepto %>%
  summarise(
    n_months        = n(),
    mean_cases       = mean(total_all_districts),
    median_cases     = median(total_all_districts),
    sd_cases         = sd(total_all_districts),
    min_cases        = min(total_all_districts),
    max_cases        = max(total_all_districts),
    total_cases      = sum(total_all_districts)
  )

print(national_stats)

#########################################################################
# Step 2 — National time series plot

ggplot(lepto, aes(x = month, y = total_all_districts)) +
  geom_line(colour = "steelblue") +
  labs(title = "National Monthly Leptospirosis Cases (2007–2025)",
       x = "Month", y = "Reported Cases") +
  theme_minimal()

#########################################################################
# Step 3 — National seasonal plot (each year overlaid on a common month axis)
library(Polychrome)

# Generate 19 maximally distinct colours
set.seed(42)  # for reproducibility of the palette
year_colours <- createPalette(19, seedcolors = c("#ff0000", "#00ff00", "#0000ff"))
names(year_colours) <- sort(unique(lepto$year))

ggplot(lepto, aes(x = month_num, y = total_all_districts,
                  group = year, colour = factor(year))) +
  geom_line(alpha = 0.85, linewidth = 0.6) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_colour_manual(values = year_colours, name = "Year") +
  labs(title = "Seasonal Pattern of National Leptospirosis Cases by Year",
       x = "Month", y = "Reported Cases") +
  theme_minimal()

###########################################################
ggplot(lepto, aes(x = month_num, y = total_all_districts, group = year)) +
  geom_line(colour = "steelblue", linewidth = 0.6) +
  facet_wrap(~ year, ncol = 5) +
  scale_x_continuous(breaks = c(1, 6, 12)) +
  labs(title = "Seasonal Pattern of National Leptospirosis Cases by Year",
       x = "Month", y = "Reported Cases") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 0))

########################################################################
# Step 4 — National tile heatmap (year × month)

ggplot(lepto, aes(x = month_num, y = factor(year), fill = total_all_districts)) +
  geom_tile(colour = "white") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_fill_viridis_c(name = "Cases", option = "inferno", direction = -1) +
  labs(title = "National Leptospirosis Cases: Year x Month Heatmap",
       x = "Month", y = "Year") +
  theme_minimal()

########################################################################
# Step 5 — District-level descriptive statistics

district_stats <- lepto_long %>%
  group_by(district) %>%
  summarise(
    n_months    = n(),
    mean_cases   = mean(cases),
    median_cases = median(cases),
    sd_cases     = sd(cases),
    min_cases    = min(cases),
    max_cases    = max(cases),
    total_cases  = sum(cases),
    .groups = "drop"
  ) %>%
  arrange(desc(total_cases))

View(district_stats)

#########################################################################
# Step 6 — Panel of district-level time series (one panel per district)

ggplot(lepto_long, aes(x = month, y = cases)) +
  geom_line(colour = "darkred", linewidth = 0.3) +
  facet_wrap(~ district, scales = "free_y", ncol = 5) +
  labs(title = "Monthly Leptospirosis Cases by District",
       x = "Month", y = "Cases") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



for (dist_name in districts) {
  
  dist_data <- lepto_long %>%
    filter(district == dist_name) %>%
    arrange(month)
  
  p <- ggplot(dist_data, aes(x = month, y = cases)) +
    geom_line(colour = "darkred", linewidth = 0.5) +
    labs(title = paste("Monthly Leptospirosis Cases —", dist_name),
         x = "Month", y = "Cases") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(paste0("timeseries_", dist_name, ".png"), p, width = 9, height = 5, dpi = 300)
}

#########################################################################
# Step 7 — District x month heatmaps split into panels

library(tidyverse)

# --- Order districts by total burden (descending: highest at top) ---
district_order_desc <- district_stats %>%
  arrange(desc(total_cases)) %>%
  pull(district)

# --- Discretise case counts into categories, with 0 as its own category ---
lepto_long_cat <- lepto_long %>%
  mutate(case_cat = case_when(
    cases == 0 ~ "0",
    cases <= 5 ~ "1-5",
    cases <= 10 ~ "6-10",
    cases <= 20 ~ "11-20",
    cases <= 50 ~ "21-50",
    TRUE ~ "50+"
  )) %>%
  mutate(case_cat = factor(case_cat,
                           levels = c("0", "1-5", "6-10", "11-20", "21-50", "50+"))) %>%
  mutate(district = factor(district, levels = rev(district_order_desc)))

panel_colours <- c("0" = "grey90", "1-5" = "#fee5d9", "6-10" = "#fcae91",
                   "11-20" = "#fb6a4a", "21-50" = "#de2d26", "50+" = "#a50f15")

make_heatmap_panel <- function(data, yr_start, yr_end) {
  data %>%
    filter(year >= yr_start, year <= yr_end) %>%
    ggplot(aes(x = month_num, y = district, fill = case_cat)) +
    geom_tile(colour = "white", linewidth = 0.2) +
    facet_wrap(~ year, nrow = 1) +
    scale_x_continuous(breaks = c(1, 6, 12)) +
    scale_fill_manual(values = panel_colours, name = "Cases", drop = FALSE) +
    labs(x = "Month", y = "District") +
    theme_minimal(base_size = 8) +
    theme(axis.text.x = element_text(angle = 0))
}

panel1 <- make_heatmap_panel(lepto_long_cat, 2007, 2013)
panel2 <- make_heatmap_panel(lepto_long_cat, 2014, 2020)
panel3 <- make_heatmap_panel(lepto_long_cat, 2021, 2026)

panel1
panel2
panel3

########################################################################
# Step 9 — ACF & PACF by district

library(tidyverse)
library(forecast)

districts <- sort(unique(lepto_long$district))
start_year  <- year(min(lepto_long$month))
start_month <- month(min(lepto_long$month))

# Loop through each district, printing its ACF and PACF one at a time
for (dist_name in districts) {
  
  dist_data <- lepto_long %>%
    filter(district == dist_name) %>%
    arrange(month)
  
  dist_ts <- ts(dist_data$cases, start = c(start_year, start_month), frequency = 12)
  
  acf_plot <- ggAcf(dist_ts, lag.max = 36) +
    labs(title = paste("ACF:", dist_name)) +
    theme_minimal()
  
  pacf_plot <- ggPacf(dist_ts, lag.max = 36) +
    labs(title = paste("PACF:", dist_name)) +
    theme_minimal()
  
  print(acf_plot)
  print(pacf_plot)
}

########################################################################
##### Pure Spatial EDA #####

# Step 12 — Load required packages and the district shapefile
library(tidyverse)
library(lubridate)
library(sf)
library(SriLanka)
library(ggrepel)
library(shadowtext)
library(spdep)
library(stringr)

data("lka_adm2")   # Sri Lanka district boundaries (25 districts)

class(lka_adm2)
names(lka_adm2)

ggplot(data = lka_adm2) +
  geom_sf(fill = "grey90", colour = "white") +
  theme_minimal()

# Step 12a — Clean district names and confirm the join
lka_adm2 <- lka_adm2 %>%
  mutate(district_clean = str_remove(NAME, " District$") %>% str_trim())

print(sort(unique(lka_adm2$district_clean)))
print(sort(unique(lepto_long$district)))

setdiff(unique(lepto_long$district), unique(lka_adm2$district_clean))


# Step 12b — Check the range of totals to pick sensible bin cutpoints
summary(district_stats$total_cases)


# Step 13 — Overall choropleth: total cases by district (2007–2026), absolute-count categories
map_data <- lka_adm2 %>%
  left_join(district_stats, by = c("district_clean" = "district")) %>%
  mutate(case_bin = case_when(
    total_cases <= 1000  ~ "≤1000",
    total_cases <= 3500  ~ "1001-3500",
    total_cases <= 7000  ~ "3501-7000",
    total_cases <= 10000 ~ "7001-10000",
    TRUE ~ "10000+"
  )) %>%
  mutate(case_bin = factor(case_bin,
                           levels = c("≤1000", "1001-3500", "3501-7000",
                                      "7001-10000", "10000+")))

bin_colours <- c(
  "≤1000"      = "#fee5d9",
  "1001-3500"  = "#fcae91",
  "3501-7000"  = "#fb6a4a",
  "7001-10000" = "#de2d26",
  "10000+"     = "#67000d"
)

ggplot(map_data) +
  geom_sf(aes(fill = case_bin), colour = "white", linewidth = 0.5) +
  geom_shadowtext(aes(label = district_clean, geometry = geometry),
                  stat = "sf_coordinates", size = 2.4,
                  colour = "black", bg.colour = "white", bg.r = 0.15) +
  scale_fill_manual(values = bin_colours, name = "Total Cases", drop = FALSE) +
  labs(title = "Total Leptospirosis Cases by District (2007–2026)") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank())

# Step 14 — Choropleth per year (separate files), absolute-count categories, consistent across years
yearly_district <- lepto_long %>%
  group_by(district, year) %>%
  summarise(annual_cases = sum(cases), .groups = "drop")

years <- sort(unique(yearly_district$year))

yearly_bin_colours <- c(
  "0-50"    = "#fee5d9",
  "51-150"  = "#fcae91",
  "151-300" = "#fb6a4a",
  "301-600" = "#de2d26",
  "600+"    = "#67000d"
)

for (yr in years) {
  
  map_yr <- lka_adm2 %>%
    left_join(yearly_district %>% filter(year == yr),
              by = c("district_clean" = "district")) %>%
    mutate(annual_cases = replace_na(annual_cases, 0),
           case_bin = case_when(
             annual_cases <= 50  ~ "0-50",
             annual_cases <= 150 ~ "51-150",
             annual_cases <= 300 ~ "151-300",
             annual_cases <= 600 ~ "301-600",
             TRUE ~ "600+"
           ),
           case_bin = factor(case_bin,
                             levels = c("0-50", "51-150", "151-300", "301-600", "600+")))
  
  p <- ggplot(map_yr) +
    geom_sf(aes(fill = case_bin), colour = "white", linewidth = 0.5) +
    geom_shadowtext(aes(label = district_clean, geometry = geometry),
                    stat = "sf_coordinates", size = 2.2,
                    colour = "black", bg.colour = "white", bg.r = 0.15) +
    scale_fill_manual(values = yearly_bin_colours, name = "Cases", drop = FALSE) +
    labs(title = paste("Leptospirosis Cases by District —", yr)) +
    theme_minimal() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid = element_blank())
  
  ggsave(paste0("choropleth_", yr, ".png"), p, width = 8, height = 8, dpi = 300)
}

# Step 15 — Global Moran's I, calculated per year
nb <- poly2nb(lka_adm2, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

moran_by_year <- map_dfr(years, function(yr) {
  
  vals <- lka_adm2 %>%
    left_join(yearly_district %>% filter(year == yr),
              by = c("district_clean" = "district")) %>%
    pull(annual_cases) %>%
    replace_na(0)
  
  mt <- moran.test(vals, lw, zero.policy = TRUE)
  
  tibble(year = yr,
         moran_I = mt$estimate[["Moran I statistic"]],
         p_value = mt$p.value)
})

print(moran_by_year, n = Inf)

# Step 16 — LISA cluster maps, one per year, saved separately
for (yr in years) {
  
  map_yr <- lka_adm2 %>%
    left_join(yearly_district %>% filter(year == yr),
              by = c("district_clean" = "district")) %>%
    mutate(annual_cases = replace_na(annual_cases, 0))
  
  z <- scale(map_yr$annual_cases)[, 1]
  z_lag <- lag.listw(lw, z, zero.policy = TRUE)
  
  local_mor <- localmoran(map_yr$annual_cases, lw, zero.policy = TRUE)
  p_vals <- local_mor[, "Pr(z != E(Ii))"]
  
  map_yr <- map_yr %>%
    mutate(
      z = z, z_lag = z_lag, p_value = p_vals,
      quadrant = case_when(
        p_value > 0.05     ~ "Not Significant",
        z > 0  & z_lag > 0 ~ "High-High (Hot-Hot)",
        z < 0  & z_lag < 0 ~ "Low-Low (Cold-Cold)",
        z > 0  & z_lag < 0 ~ "High-Low (Hot-Cold)",
        z < 0  & z_lag > 0 ~ "Low-High (Cold-Hot)",
        TRUE               ~ "Not Significant"
      ),
      quadrant = factor(quadrant,
                        levels = c("High-High (Hot-Hot)", "Low-Low (Cold-Cold)",
                                   "High-Low (Hot-Cold)", "Low-High (Cold-Hot)",
                                   "Not Significant"))
    )
  
  p <- ggplot(map_yr) +
    geom_sf(aes(fill = quadrant), colour = "white", linewidth = 0.5) +
    geom_shadowtext(aes(label = district_clean, geometry = geometry),
                    stat = "sf_coordinates", size = 2.2,
                    colour = "black", bg.colour = "white", bg.r = 0.15) +
    scale_fill_manual(values = c(
      "High-High (Hot-Hot)"  = "red",
      "Low-Low (Cold-Cold)"  = "blue",
      "High-Low (Hot-Cold)"  = "pink",
      "Low-High (Cold-Hot)"  = "lightblue",
      "Not Significant"      = "grey90"
    ), name = "LISA Cluster", drop = FALSE) +
    labs(title = paste("LISA Cluster Map —", yr)) +
    theme_minimal() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid = element_blank())
  
  ggsave(paste0("lisa_", yr, ".png"), p, width = 8, height = 8, dpi = 300)
}

