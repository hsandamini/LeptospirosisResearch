# Step 0 — Load the dataset and build the national-level incidence series
library(tidyverse)
library(lubridate)

lepto_incidence <- read_csv("leptospirosis_monthly_incidence.csv") %>%
  mutate(month = as.Date(month))

# National monthly totals: sum cases and population across all districts each month
national_incidence <- lepto_incidence %>%
  group_by(month, year, month_num) %>%
  summarise(
    total_cases = sum(cases),
    total_population = sum(population),
    .groups = "drop"
  ) %>%
  mutate(incidence_per_100k = (total_cases / total_population) * 100000)

glimpse(national_incidence)

# National-level time series EDA
# Step 1 — National descriptive statistics (incidence)
national_incidence_stats <- national_incidence %>%
  summarise(
    n_months = n(),
    mean_incidence = mean(incidence_per_100k),
    median_incidence = median(incidence_per_100k),
    sd_incidence = sd(incidence_per_100k),
    min_incidence = min(incidence_per_100k),
    max_incidence = max(incidence_per_100k)
  )

print(national_incidence_stats)

# Step 2 — National incidence time series plot
ggplot(national_incidence, aes(x = month, y = incidence_per_100k)) +
  geom_line(colour = "steelblue") +
  labs(title = "National Monthly Leptospirosis Incidence (2007–2025)",
       x = "Month", y = "Incidence per 100,000") +
  theme_minimal()

# Step 3 — National seasonal plot (incidence, faceted by year to avoid the colour-overlap problem)
ggplot(national_incidence, aes(x = month_num, y = incidence_per_100k, group = year)) +
  geom_line(colour = "steelblue", linewidth = 0.6) +
  facet_wrap(~ year, ncol = 5) +
  scale_x_continuous(breaks = c(1, 6, 12)) +
  labs(title = "Seasonal Pattern of National Leptospirosis Incidence by Year",
       x = "Month", y = "Incidence per 100,000") +
  theme_minimal(base_size = 9)

# Step 4 — National incidence tile heatmap (year × month)
ggplot(national_incidence, aes(x = month_num, y = factor(year), fill = incidence_per_100k)) +
  geom_tile(colour = "white") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_fill_viridis_c(name = "Incidence\nper 100k", option = "inferno", direction = -1) +
  labs(title = "National Leptospirosis Incidence: Year x Month Heatmap",
       x = "Month", y = "Year") +
  theme_minimal()

# District-level time series EDA
# Step 5 — District-level descriptive statistics (incidence)
district_incidence_stats <- lepto_incidence %>%
  group_by(district) %>%
  summarise(
    n_months = n(),
    mean_incidence = mean(incidence_per_100k),
    median_incidence = median(incidence_per_100k),
    sd_incidence = sd(incidence_per_100k),
    min_incidence = min(incidence_per_100k),
    max_incidence = max(incidence_per_100k),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_incidence))

print(district_incidence_stats, n = Inf)

# Step 6 — Panel of district-level incidence time series
ggplot(lepto_incidence, aes(x = month, y = incidence_per_100k)) +
  geom_line(colour = "darkred", linewidth = 0.3) +
  facet_wrap(~ district, scales = "free_y", ncol = 5) +
  labs(title = "Monthly Leptospirosis Incidence by District",
       x = "Month", y = "Incidence per 100,000") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


districts <- sort(unique(lepto_long$district))

for (dist_name in districts) {
  
  dist_data <- lepto_incidence %>%
    filter(district == dist_name) %>%
    arrange(month)
  
  p <- ggplot(dist_data, aes(x = month, y = incidence_per_100k)) +
    geom_line(colour = "darkred", linewidth = 0.5) +
    labs(title = paste("Monthly Leptospirosis Incidence —", dist_name),
         x = "Month", y = "Incidence per 100,000") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(paste0("timeseries_incidence_", dist_name, ".png"), p, width = 9, height = 5, dpi = 300)
}
# Step 7 — District x month heatmap panels (incidence categories instead of raw-count categories)
summary(lepto_incidence$incidence_per_100k)   # check range before setting bins

lepto_incidence_cat <- lepto_incidence %>%
  mutate(inc_cat = case_when(
    incidence_per_100k == 0   ~ "0",
    incidence_per_100k <= 1   ~ "0-1",
    incidence_per_100k <= 3   ~ "1-3",
    incidence_per_100k <= 6   ~ "3-6",
    incidence_per_100k <= 12  ~ "6-12",
    TRUE ~ "12+"
  )) %>%
  mutate(inc_cat = factor(inc_cat, levels = c("0", "0-1", "1-3", "3-6", "6-12", "12+")))

panel_colours <- c("0" = "grey90", "0-1" = "#fee5d9", "1-3" = "#fcae91",
                   "3-6" = "#fb6a4a", "6-12" = "#de2d26", "12+" = "#a50f15")

make_incidence_panel <- function(data, yr_start, yr_end) {
  data %>%
    filter(year >= yr_start, year <= yr_end) %>%
    ggplot(aes(x = month_num, y = district, fill = inc_cat)) +
    geom_tile(colour = "white", linewidth = 0.2) +
    facet_wrap(~ year, nrow = 1) +
    scale_x_continuous(breaks = c(1, 6, 12)) +
    scale_fill_manual(values = panel_colours, name = "Incidence\nper 100k", drop = FALSE) +
    labs(x = "Month", y = "District") +
    theme_minimal(base_size = 8)
}

panel1 <- make_incidence_panel(lepto_incidence_cat, 2007, 2013)
panel2 <- make_incidence_panel(lepto_incidence_cat, 2014, 2020)
panel3 <- make_incidence_panel(lepto_incidence_cat, 2021, 2025)

panel1
panel2
panel3

# Step 8 — Combine the three panels
library(patchwork)

panel1 / panel2 / panel3 +
  plot_layout(guides = "collect") +
  plot_annotation(title = "District x Month Leptospirosis Incidence (2007–2025)")


# ACF & PACF per district (incidence), shown individually
library(tidyverse)
library(forecast)   # <-- this line was missing/not re-run in your current session

districts <- sort(unique(lepto_incidence$district))

for (dist_name in districts) {
  
  dist_data <- lepto_incidence %>%
    filter(district == dist_name) %>%
    arrange(month)
  
  dist_ts <- ts(dist_data$incidence_per_100k, start = c(2007, 1), frequency = 12)
  
  acf_plot <- ggAcf(dist_ts, lag.max = 36) +
    labs(title = paste("ACF (Incidence):", dist_name)) + theme_minimal()
  
  pacf_plot <- ggPacf(dist_ts, lag.max = 36) +
    labs(title = paste("PACF (Incidence):", dist_name)) + theme_minimal()
  
  print(acf_plot)
  print(pacf_plot)
}

## Spatial EDA
# Step 12 — Load shapefile (same as before, no change needed)
library(sf)
library(SriLanka)
library(ggrepel)
library(shadowtext)
library(spdep)
library(stringr)

data("lka_adm2")

lka_adm2 <- lka_adm2 %>%
  mutate(district_clean = str_remove(NAME, " District$") %>% str_trim())

setdiff(unique(lepto_incidence$district), unique(lka_adm2$district_clean))


# Step 13 — Overall choropleth: mean incidence per district (whole period)
map_data <- lka_adm2 %>%
  left_join(district_incidence_stats, by = c("district_clean" = "district")) %>%
  mutate(inc_bin = case_when(
    mean_incidence <= 1   ~ "≤1",
    mean_incidence <= 2   ~ "1-2",
    mean_incidence <= 3   ~ "2-3",
    mean_incidence <= 4   ~ "3-4",
    TRUE ~ "4+"
  )) %>%
  mutate(inc_bin = factor(inc_bin, levels = c("≤1", "1-2", "2-3", "3-4", "4+")))

bin_colours <- c("≤1" = "#fee5d9", "1-2" = "#fcae91", "2-3" = "#fb6a4a",
                 "3-4" = "#de2d26", "4+" = "#67000d")

ggplot(map_data) +
  geom_sf(aes(fill = inc_bin), colour = "white", linewidth = 0.5) +
  geom_shadowtext(aes(label = district_clean, geometry = geometry),
                  stat = "sf_coordinates", size = 2.4,
                  colour = "black", bg.colour = "white", bg.r = 0.15) +
  scale_fill_manual(values = bin_colours, name = "Mean Incidence\nper 100k", drop = FALSE) +
  labs(title = "Mean Leptospirosis Incidence by District (2007–2025)") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

# Step 14 — Annual incidence choropleth, one map per year
yearly_incidence <- lepto_incidence %>%
  group_by(district, year) %>%
  summarise(annual_cases = sum(cases), population = first(population), .groups = "drop") %>%
  mutate(incidence_per_100k = (annual_cases / population) * 100000)

years <- sort(unique(yearly_incidence$year))


yearly_bin_colours <- c("0-7" = "#fee5d9", "7-16" = "#fcae91", "16-33" = "#fb6a4a",
                        "33-60" = "#de2d26", "60+" = "#67000d")

for (yr in years) {
  
  map_yr <- lka_adm2 %>%
    left_join(yearly_incidence %>% filter(year == yr),
              by = c("district_clean" = "district")) %>%
    mutate(incidence_per_100k = replace_na(incidence_per_100k, 0),
           inc_bin = case_when(
             incidence_per_100k <= 7  ~ "0-7",
             incidence_per_100k <= 16 ~ "7-16",
             incidence_per_100k <= 33 ~ "16-33",
             incidence_per_100k <= 60 ~ "33-60",
             TRUE ~ "60+"
           ),
           inc_bin = factor(inc_bin, levels = c("0-7", "7-16", "16-33", "33-60", "60+")))
  
  p <- ggplot(map_yr) +
    geom_sf(aes(fill = inc_bin), colour = "white", linewidth = 0.5) +
    geom_shadowtext(aes(label = district_clean, geometry = geometry),
                    stat = "sf_coordinates", size = 2.2,
                    colour = "black", bg.colour = "white", bg.r = 0.15) +
    scale_fill_manual(values = yearly_bin_colours, name = "Incidence\nper 100k", drop = FALSE) +
    labs(title = paste("Leptospirosis Incidence by District —", yr)) +
    theme_minimal() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
  
  ggsave(paste0("incidence_choropleth_", yr, ".png"), p, width = 8, height = 8, dpi = 300)
}



# Step 15 — Global Moran's I per year (on incidence, not raw cases)
nb <- poly2nb(lka_adm2, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

moran_by_year <- map_dfr(years, function(yr) {
  
  vals <- lka_adm2 %>%
    left_join(yearly_incidence %>% filter(year == yr),
              by = c("district_clean" = "district")) %>%
    pull(incidence_per_100k) %>%
    replace_na(0)
  
  mt <- moran.test(vals, lw, zero.policy = TRUE)
  
  tibble(year = yr,
         moran_I = mt$estimate[["Moran I statistic"]],
         p_value = mt$p.value)
})

print(moran_by_year, n = Inf)

# Step 16 — LISA cluster maps per year (on incidence)
for (yr in years) {
  
  map_yr <- lka_adm2 %>%
    left_join(yearly_incidence %>% filter(year == yr),
              by = c("district_clean" = "district")) %>%
    mutate(incidence_per_100k = replace_na(incidence_per_100k, 0))
  
  z <- scale(map_yr$incidence_per_100k)[, 1]
  z_lag <- lag.listw(lw, z, zero.policy = TRUE)
  
  local_mor <- localmoran(map_yr$incidence_per_100k, lw, zero.policy = TRUE)
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
      "High-High (Hot-Hot)"  = "red", "Low-Low (Cold-Cold)"  = "blue",
      "High-Low (Hot-Cold)"  = "pink", "Low-High (Cold-Hot)"  = "lightblue",
      "Not Significant"      = "grey90"
    ), name = "LISA Cluster", drop = FALSE) +
    labs(title = paste("LISA Cluster Map (Incidence) —", yr)) +
    theme_minimal() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
  
  ggsave(paste0("lisa_incidence_", yr, ".png"), p, width = 8, height = 8, dpi = 300)
}


### Spatio temporal
# Step 17 — District x Year heatmap (compact space-time view in one plot)
library(tidyverse)

district_order <- district_incidence_stats %>%
  arrange(desc(mean_incidence)) %>%
  pull(district)

yearly_incidence <- yearly_incidence %>%
  mutate(district = factor(district, levels = rev(district_order)))

ggplot(yearly_incidence, aes(x = year, y = district, fill = incidence_per_100k)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  scale_fill_viridis_c(name = "Incidence\nper 100k", option = "inferno", direction = -1) +
  labs(title = "District x Year Leptospirosis Incidence Heatmap",
       x = "Year", y = "District") +
  theme_minimal(base_size = 9)

# Step 18 — Animated choropleth (spatial pattern evolving over time)
library(gganimate)

map_all_years <- lka_adm2 %>%
  left_join(yearly_incidence, by = c("district_clean" = "district")) %>%
  mutate(incidence_per_100k = replace_na(incidence_per_100k, 0))

anim <- ggplot(map_all_years) +
  geom_sf(aes(fill = incidence_per_100k), colour = "white", linewidth = 0.3) +
  scale_fill_viridis_c(name = "Incidence\nper 100k", option = "inferno", direction = -1,
                       limits = c(0, max(yearly_incidence$incidence_per_100k))) +
  labs(title = "Leptospirosis Incidence by District — Year: {closest_state}") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  transition_states(year, transition_length = 1, state_length = 1) +
  ease_aes("linear")

animate(anim, nframes = 100, fps = 5, width = 700, height = 700)
anim_save("incidence_spacetime_animation.gif")

# Step 19 — Moran's I trend over time (spatial clustering strength through time)
ggplot(moran_by_year, aes(x = year, y = moran_I)) +
  geom_line(colour = "darkred", linewidth = 0.8) +
  geom_point(aes(colour = p_value < 0.05), size = 3) +
  scale_colour_manual(values = c("TRUE" = "darkred", "FALSE" = "grey60"),
                      name = "Significant\n(p < 0.05)") +
  labs(title = "Spatial Clustering Strength of Leptospirosis Incidence Over Time",
       x = "Year", y = "Global Moran's I") +
  theme_minimal()

# Step 20 — LISA cluster persistence (which districts are consistently hotspots vs coldspots over time)
lisa_all_years <- map_dfr(years, function(yr) {
  
  map_yr <- lka_adm2 %>%
    left_join(yearly_incidence %>% filter(year == yr),
              by = c("district_clean" = "district")) %>%
    mutate(incidence_per_100k = replace_na(incidence_per_100k, 0))
  
  z <- scale(map_yr$incidence_per_100k)[, 1]
  z_lag <- lag.listw(lw, z, zero.policy = TRUE)
  local_mor <- localmoran(map_yr$incidence_per_100k, lw, zero.policy = TRUE)
  p_vals <- local_mor[, "Pr(z != E(Ii))"]
  
  tibble(
    district = map_yr$district_clean,
    year = yr,
    quadrant = case_when(
      p_vals > 0.05  ~ "Not Significant",
      z > 0 & z_lag > 0 ~ "High-High (Hot-Hot)",
      z < 0 & z_lag < 0 ~ "Low-Low (Cold-Cold)",
      z > 0 & z_lag < 0 ~ "High-Low (Hot-Cold)",
      z < 0 & z_lag > 0 ~ "Low-High (Cold-Hot)"
    )
  )
})

# Count how many years (out of 19) each district was classified as each cluster type
lisa_persistence <- lisa_all_years %>%
  count(district, quadrant) %>%
  pivot_wider(names_from = quadrant, values_from = n, values_fill = 0)

print(lisa_persistence, n = Inf)

# Step 20a — Map the "dominant" cluster type per district (the classification that occurred most often across all 19 years)
dominant_cluster <- lisa_all_years %>%
  count(district, quadrant) %>%
  group_by(district) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

map_dominant <- lka_adm2 %>%
  left_join(dominant_cluster, by = c("district_clean" = "district"))

ggplot(map_dominant) +
  geom_sf(aes(fill = quadrant), colour = "white", linewidth = 0.5) +
  geom_shadowtext(aes(label = district_clean, geometry = geometry),
                  stat = "sf_coordinates", size = 2.2,
                  colour = "black", bg.colour = "white", bg.r = 0.15) +
  scale_fill_manual(values = c(
    "High-High (Hot-Hot)"  = "red", "Low-Low (Cold-Cold)"  = "blue",
    "High-Low (Hot-Cold)"  = "pink", "Low-High (Cold-Hot)"  = "lightblue",
    "Not Significant"      = "grey90"
  ), name = "Dominant LISA\nCluster (2007–2025)") +
  labs(title = "Most Frequent LISA Classification per District (2007–2025)") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

# Step 21 — District rank changes over time (bump chart)
rank_data <- yearly_incidence %>%
  group_by(year) %>%
  mutate(rank = rank(-incidence_per_100k)) %>%
  ungroup() %>%
  filter(district %in% district_order[1:8])  # top 8 districts, to keep it readable

ggplot(rank_data, aes(x = year, y = rank, colour = district)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_reverse(breaks = 1:8) +
  labs(title = "Rank of Top 8 Districts by Incidence Over Time",
       x = "Year", y = "Rank (1 = highest incidence)") +
  theme_minimal()

