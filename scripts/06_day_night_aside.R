# Description -------------------------------------------------------------

# Aside analysis of depth stratified total abundances across replicates for
# each taxa against solar dayness and depth


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(glmmTMB)


# Source code -------------------------------------------------------------

source(here("scripts/02_prepare_community_data.R"))


# Inventory matched day and night tows -------------------------------------

# Use the quantitative fish inventory before the community abundance cutoff.
# Exclude samples with a recorded spill, including partially lost samples.
lost_mocness_samples <- bind_rows(
  transmute(mocness_2018_02_fish_abundance, year = 2018, haul_number = Haul.no, notes),
  transmute(mocness_2019_03_fish_abundance, year = 2019, haul_number = Haul.no, notes)
) %>%
  filter(str_detect(str_to_lower(notes), "lost.*spill")) %>%
  distinct(year, haul_number)

day_night_inventory <- mocness_clean_by_side %>%
  filter(year %in% c(2018, 2019), net %in% 1:4, mocness_side %in% c("1", "4"),
         !is.na(day_night), is.finite(volume_m3_best), volume_m3_best > 0,
         is.finite(minimum_depth_m), is.finite(maximum_depth_m),
         minimum_depth_m >= 0, maximum_depth_m > minimum_depth_m) %>%
  anti_join(lost_mocness_samples, by = c("year", "haul_number")) %>%
  # A recorded empty sample is a zero. A missing count otherwise remains missing.
  mutate(individuals_in_tow = if_else(str_to_lower(str_trim(taxon)) == "no fish",
                                      0, individuals_in_tow,
                                      missing = individuals_in_tow)) %>%
  group_by(year, haul_number) %>%
  filter(all(is.finite(individuals_in_tow) & individuals_in_tow >= 0)) %>%
  ungroup()

# Pool the available sides of each net tow and use their corresponding volumes.
# Assign 25 m strata from observed mean depth because actual fishing depths
# can differ from the depth range intended for a given net number.
day_night_valid_tows <- day_night_inventory %>%
  distinct(year, transect, station, replicate, net, collection_date,
           transect_station_rep_year_net, mocness_side, start_time_pt, day_night, solar_dayness,
           volume_m3_best, minimum_depth_m, maximum_depth_m) %>%
  group_by(year, transect, station, replicate, net, collection_date,
           transect_station_rep_year_net) %>%
  filter(n_distinct(day_night) == 1) %>%
  summarize(day_night = first(day_night),
            solar_dayness = mean(solar_dayness),
            start_time_pt = min(start_time_pt),
            mocness_sides = paste(sort(unique(mocness_side)), collapse = ", "),
            n_sides = n_distinct(mocness_side),
            volume_filtered_m3 = sum(volume_m3_best),
            depth_mean_m = mean((minimum_depth_m + maximum_depth_m) / 2),
            minimum_depth_m = min(minimum_depth_m),
            maximum_depth_m = max(maximum_depth_m),
            .groups = "drop") %>%
  mutate(depth_stratum = cut(depth_mean_m, breaks = seq(0, 100, 25), include.lowest = TRUE,
                             labels = c("0-25 m", "25-50 m", "50-75 m", "75-100 m"))) %>%
  drop_na(depth_stratum)

# Keep all tows in a block if the same station, year, and stratum has both periods.
matched_day_night_blocks <- day_night_valid_tows %>%
  group_by(year, transect, station, depth_stratum) %>%
  summarize(day_tows = sum(day_night == "Day"),
            night_tows = sum(day_night == "Night"), .groups = "drop") %>%
  filter(day_tows > 0, night_tows > 0)

matched_day_night_tows <- day_night_valid_tows %>%
  semi_join(matched_day_night_blocks, by = c("year", "transect", "station", "depth_stratum")) %>%
  arrange(year, transect, station, depth_stratum, start_time_pt)

# Summarize positive catches for the taxa in the community analysis. Complete
# taxon counts only within valid matched tows, where an unrecorded taxon is absent.
matched_day_night_counts <- day_night_inventory %>%
  semi_join(matched_day_night_tows, by = c("transect_station_rep_year_net", "collection_date")) %>%
  filter(taxon %in% mocness_major_taxa$taxon) %>%
  group_by(transect_station_rep_year_net, collection_date, taxon) %>%
  summarize(individuals_in_tow = sum(individuals_in_tow), .groups = "drop") %>%
  right_join(
    crossing(matched_day_night_tows, taxon = sort(unique(mocness_major_taxa$taxon))),
    by = c("transect_station_rep_year_net", "collection_date", "taxon")
  ) %>%
  replace_na(list(individuals_in_tow = 0)) %>%
  mutate(individuals_per_m3 = individuals_in_tow / volume_filtered_m3)

matched_day_night_taxon_summary <- matched_day_night_counts %>%
  group_by(taxon, day_night) %>%
  summarize(n_tows = n(),
            positive_tows = sum(individuals_in_tow > 0),
            total_individuals = sum(individuals_in_tow), .groups = "drop") %>%
  pivot_wider(names_from = day_night, values_from = c(n_tows, positive_tows, total_individuals))

write_csv(matched_day_night_blocks, here("output", "matched_day_night_blocks.csv"))
write_csv(matched_day_night_tows, here("output", "matched_day_night_tows.csv"))
write_csv(matched_day_night_taxon_summary, here("output", "matched_day_night_taxon_summary.csv"))

# Export the number of matched tows with at least one larva of each taxon.
matched_day_night_taxon_summary %>%
  select(taxon, positive_tows_Day, positive_tows_Night) %>%
  write_csv(here("output", "matched_day_night_positive_tows_by_taxon.csv"))


# Compare total larval abundance between day and night ---------------------

# Restrict the day/night comparisons to 2018 and 2019, when stations were
# sampled during both periods. Use the community analysis's tow and taxon
# selection, including the abundance threshold pooled across MOCNESS sides.
retained_tow_taxa <- mocness_major_taxa_nets %>%
  filter(year %in% c(2018, 2019)) %>%
  distinct(transect_station_rep_year_net, taxon)

night_taxa <- c("Lestidiops_ringens", "Hemilepidotus_spp", "Psettichthys_melanostictus",
              "Cyclopsettidae", "Scorpaenichthys_marmoratus", "Parophrys_vetulus")

day_taxa <- c("Isopsetta_isolepis", "Osmeridae", "Glyptocephalus_zachirus",
                     "Gadidae", "Psychrolutidae", "Lyopsetta_exilis")

# Sum the original counts within each side of a tow before pooling.
day_night_tow_counts <- mocness_clean_by_side %>%
  semi_join(retained_tow_taxa, by = c("transect_station_rep_year_net", "taxon")) %>%
  group_by(transect_station_rep_year_net, mocness_side) %>%
  summarize(total_individuals = sum(individuals_in_tow, na.rm = TRUE),
            night_taxa_individuals = sum(individuals_in_tow[taxon %in% night_taxa], na.rm = TRUE),
            day_taxa_individuals = sum(individuals_in_tow[taxon %in% day_taxa], na.rm = TRUE),
            .groups = "drop")

# Keep sampled sides with zero larvae of the retained taxa. Day and night are
# based on the start time of each side's tow relative to sunrise and sunset.
day_night_tow_abundance <- mocness_clean_by_side %>%
  semi_join(retained_tow_taxa, by = "transect_station_rep_year_net") %>%
  filter(mocness_side %in% c("1", "4")) %>%
  drop_na(day_night) %>%
  distinct(transect_station_rep_year_net, transect_station, year, mocness_side, day_night) %>%
  left_join(day_night_tow_counts, by = c("transect_station_rep_year_net", "mocness_side")) %>%
  replace_na(list(total_individuals = 0, night_taxa_individuals = 0,
                  day_taxa_individuals = 0)) %>%
  mutate(day_night = factor(day_night, levels = c("Night", "Day")),
         mocness_side = factor(mocness_side, levels = c("1", "4")))

# Draw each tow once, including the observations beyond the boxplot whiskers.
# Add one before taking the natural log to keep zero-abundance tows.
day_night_tow_plot <- ggplot(day_night_tow_abundance, aes(x = day_night, y = log1p(total_individuals))) +
  geom_boxplot(width = 0.5, outlier.shape = NA, fill = "grey95") +
  geom_point(position = position_jitter(width = 0.15, height = 0, seed = 1),
             size = 1.2, alpha = 0.45) +
  facet_wrap(~ mocness_side, nrow = 1, drop = FALSE,
             labeller = labeller(mocness_side = as_labeller(
               c("1" = "'1-'*m^2~mouth", "4" = "'4-'*m^2~mouth"), default = label_parsed))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = NULL, y = expression(ln("Abundance (ind. tow" ^{-1} ~ "+ 1)"))) +
  theme_classic(base_size = 12)

ggsave("day_night_total_larval_abundance_by_mocness_side.png", plot = day_night_tow_plot,
       path = here("output"), width = 7, height = 4.5, units = "in", dpi = 300)

# Use the same sampled tows for the six-taxon comparison, including zero catches.
day_night_nighttime_taxa_plot <- day_night_tow_plot +
  aes(y = log1p(night_taxa_individuals)) +
  labs(y = expression(ln("Abundance (ind. tow" ^{-1} ~ "+ 1)")))

ggsave("day_night_nighttime_taxa_abundance_by_mocness_side.png", plot = day_night_nighttime_taxa_plot,
       path = here("output"), width = 7, height = 4.5, units = "in", dpi = 300)

# Compare the second set of six taxa on the same tows, including zero catches.
day_night_daytime_taxa_plot <- day_night_tow_plot +
  aes(y = log1p(day_taxa_individuals)) +
  labs(y = expression(ln("Abundance (ind. tow" ^{-1} ~ "+ 1)")))

ggsave("day_night_daytime_taxa_abundance_by_mocness_side.png", plot = day_night_daytime_taxa_plot,
       path = here("output"), width = 7, height = 4.5, units = "in", dpi = 300)


# Prepare data ------------------------------------------------------------

# Keep nets 1-4. Net 0 has already been removed from this DVM analysis.
scaled_model_covariates <- setdiff(dbRDA_covariates, "year")

mocness_major_taxa_dvm <- mocness_major_taxa %>%
  filter(net %in% 1:4) %>%
  left_join(env_wide %>%
              select(transect_station_rep_year_net, all_of(scaled_model_covariates)) %>%
              distinct(),
            by = "transect_station_rep_year_net")
  


# Classify taxa by habitat affinity and create color vectors ---------------
ordered_taxa_dvm <- c(mesopelagic_species, flatfish_species, sculpin_relatives_species, other_species)

mocness_major_taxa_dvm <- mocness_major_taxa_dvm %>%
  #Reorder taxa
  mutate(taxon = factor(taxon, levels = ordered_taxa_dvm)) %>%
  #Reorder stations
  mutate(station = factor(station, levels = rev(sort(unique(station))))) %>%
  mutate(color_scheme_group = case_when(taxon %in% mesopelagic_species ~ "Mesopelagic",
                                            taxon %in% flatfish_species ~ "Flatfish",
                                            taxon %in% sculpin_relatives_species ~ "Sculpins_and_related",
                                            taxon %in% other_species ~ "Other",
                                            TRUE ~ "Other"))

model_data <- mocness_major_taxa_dvm %>%
  select(year, collection_date, start_latitude_dd, start_longitude_dd, transect_station_rep_year_net,
         taxon, depth_mean_m, depth_mean_m_scaled, solar_dayness, solar_dayness_scaled,
         volume_best_m3_both_sides, individuals_in_tow,
         start_latitude_dd_scaled, seafloor_depth_m_scaled, mean_temperature_c_scaled,
         mean_salinity_psu_scaled, dissolved_oxygen_ml_l_scaled,
         mean_chl_0_100_m_mgm3_scaled) %>%
  # Remove net 0 data for DVM vignette
  filter(!str_detect(transect_station_rep_year_net, "0$")) %>%
  mutate(taxon = droplevels(taxon)) %>%
  complete(nesting(year, collection_date, start_latitude_dd, start_longitude_dd,
                   transect_station_rep_year_net, depth_mean_m, depth_mean_m_scaled,
                   solar_dayness, solar_dayness_scaled,
                   volume_best_m3_both_sides, start_latitude_dd_scaled,
                   seafloor_depth_m_scaled, mean_temperature_c_scaled,
                   mean_salinity_psu_scaled, dissolved_oxygen_ml_l_scaled,
                   mean_chl_0_100_m_mgm3_scaled),
           taxon, fill = list(individuals_in_tow = 0)) %>%
  mutate(year = factor(year),
         individuals_per_m3 = individuals_in_tow / volume_best_m3_both_sides)


# Avg taxa concentrations across replicates ------------------------------------

avgd_mocness_major_taxa_dvm <- mocness_major_taxa_dvm %>%
  group_by(taxon, solar_dayness, depth_range, depth_mean_m, depth_diff_m) %>%
  summarise(avg_taxa_concentration = mean(individuals_per_m3, na.rm=TRUE)) %>%
  ungroup()


# Plot taxa concentrations by depths and solar dayness --------------------
# Barplot
ggplot(avgd_mocness_major_taxa_dvm, aes(x = depth_range, y = avg_taxa_concentration, fill = taxon)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  labs(title = "Taxa concentrations at depth ranges",
       x = "Depth sampled (m)", y = "average individuals per m3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Scatterplot
d_n_scatter_all <- ggplot(model_data, aes(x = depth_mean_m, y = log(individuals_per_m3), color = taxon)) +
  geom_point() +
  geom_smooth(method = "lm",se = FALSE) +
  scale_color_manual(values = species_colors, labels = parse(text = taxon_labels)) +
  labs(title = "Taxa concentrations by mean depths",
       x = "Mean tow depth (m)", y = "Concentration (log(ind./m3)", color = "Taxon") +
  theme_classic()
ggsave("dayness_scatter_all.png", plot = d_n_scatter_all, path = here("output"),
         width = 7, height = 5, units = "in", dpi = 300)

taxa_panel_order <- c(
  "Agonidae",
  "Protomyctophum_spp",
  "Bathylagus_ochotensis",
  "Tarletonbeania_crenularis",
  "Lestidiops_ringens",
  "Hemilepidotus_spp",
  "Psettichthys_melanostictus",
  "Cyclopsettidae",
  "Scorpaenichthys_marmoratus",
  "Parophrys_vetulus",
  "Hexagrammidae",
  "Ammodytidae",
  "Sebastes_spp",
  "Cottidae",
  "Isopsetta_isolepis",
  "Osmeridae",
  "Glyptocephalus_zachirus",
  "Gadidae",
  "Psychrolutidae",
  "Lyopsetta_exilis",
  "Liparis_spp",
  "Stenobrachius_leucopsarus"
)
taxon_facet_labeller <- labeller(
  taxon = as_labeller(taxon_labels[taxa_panel_order], default = label_parsed)
)

# Scale predicted and observed concentrations separately within each taxon.
plot_relative_abundance_surface <- function(surface_predictions, observed_tows) {
  surface_predictions <- surface_predictions %>%
    group_by(taxon) %>%
    mutate(normalized_fit = fit / max(fit, na.rm = TRUE)) %>%
    ungroup()

  # Draw the highest point values last where tows overlap.
  observed_tows <- observed_tows %>%
    mutate(taxon = factor(taxon, levels = taxa_panel_order)) %>%
    arrange(normalized_observed)

  ggplot(surface_predictions, aes(x = solar_dayness, y = depth_mean_m)) +
    geom_raster(aes(fill = normalized_fit), interpolate = TRUE) +
    geom_contour(aes(z = normalized_fit), color = "white", linewidth = 0.2, alpha = 0.6) +
    geom_point(data = observed_tows, aes(fill = normalized_observed),
               shape = 21, color = "white", size = 0.5, stroke = 0.35 / 3) +
    scale_y_reverse() +
    scale_fill_viridis_c(option = "magma", limits = c(0, 1)) +
    facet_wrap(~ taxon, labeller = taxon_facet_labeller,
               axes = "all", axis.labels = "margins") +
    labs(x = "Time of day",
         y = "Mean tow depth (m)",
         fill = "Relative abundance") +
    theme_classic()
}


# Mark the solar dayness of the sampled tows. Darker ticks show positive catches.
glmm_sampling_rug <- function(observed_tows) {
  rug_data <- observed_tows %>%
    transmute(taxon = factor(taxon, levels = taxa_panel_order), solar_dayness,
              positive_catch = individuals_in_tow > 0) %>%
    arrange(positive_catch)

  list(
    geom_rug(data = rug_data, aes(x = solar_dayness, alpha = positive_catch),
             inherit.aes = FALSE, sides = "b", linewidth = 0.25, show.legend = FALSE),
    scale_alpha_manual(values = c("FALSE" = 0.18, "TRUE" = 0.8))
  )
}


# GLMM for the matched day and night tows ----------------------------------

# Center and scale predictors using the matched tows, once per tow.
glmm_solar_dayness_center <- mean(matched_day_night_tows$solar_dayness)
glmm_solar_dayness_scale <- sd(matched_day_night_tows$solar_dayness)
glmm_depth_mean_center <- mean(matched_day_night_tows$depth_mean_m)
glmm_depth_mean_scale <- sd(matched_day_night_tows$depth_mean_m)

glmm_model_data <- matched_day_night_counts %>%
  mutate(taxon = factor(taxon),
         transect_station_rep_year_net = factor(transect_station_rep_year_net),
         solar_dayness_scaled = (solar_dayness - glmm_solar_dayness_center) / glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - glmm_depth_mean_center) / glmm_depth_mean_scale)

glmm_dayness_grid <- seq(min(glmm_model_data$solar_dayness, na.rm = TRUE),
                        max(glmm_model_data$solar_dayness, na.rm = TRUE),
                        length.out = 100)
glmm_depth_grid <- seq(min(glmm_model_data$depth_mean_m, na.rm = TRUE),
                      max(glmm_model_data$depth_mean_m, na.rm = TRUE),
                      length.out = 100)

glmm_min_depth <- min(glmm_model_data$depth_mean_m, na.rm = TRUE)
glmm_max_depth <- max(glmm_model_data$depth_mean_m, na.rm = TRUE)

glmm_integration_depth_grid <- seq(glmm_min_depth, glmm_max_depth, length.out = 80)
glmm_integration_depth_step <- diff(range(glmm_integration_depth_grid)) /
  (length(glmm_integration_depth_grid) - 1)

glmm_integration_depth_weights <- tibble(depth_mean_m = glmm_integration_depth_grid,
                                        depth_weight = glmm_integration_depth_step) %>%
  mutate(depth_weight = if_else(row_number() %in% c(1, n()),
                                depth_weight / 2,
                                depth_weight))

glmm_taxa <- levels(glmm_model_data$taxon)

# The matched tows come from a single year. Use the filtered volume
# corresponding to the usable sides of each tow.
glmm_dayness_depth_model <- glmmTMB(
  individuals_in_tow ~ taxon * solar_dayness_scaled * depth_mean_m_scaled +
    offset(log(volume_filtered_m3)) +
    (1 | transect_station_rep_year_net),
  family = nbinom2,
  data = glmm_model_data
)

summary(glmm_dayness_depth_model)

# The fixed-effect matrix uses a unit volume and zero random effects.
glmm_fixed_terms <- terms(~ taxon * solar_dayness_scaled * depth_mean_m_scaled)
glmm_coef <- fixef(glmm_dayness_depth_model)$cond
glmm_vcov <- vcov(glmm_dayness_depth_model)$cond


# Use log1p in the relative plot to retain zero catches, including all-zero taxa.
glmm_observed_tows <- glmm_model_data %>%
  group_by(taxon) %>%
  mutate(log_observed = log1p(individuals_per_m3),
         peak_log_observed = max(log_observed),
         normalized_observed = if (peak_log_observed[1] > 0) log_observed / peak_log_observed else 0) %>%
  ungroup() %>%
  mutate(taxon = factor(taxon, levels = taxa_panel_order)) %>%
  arrange(individuals_per_m3)

# GLMM predicted abundance surfaces ---------------------------------------

glmm_surface_predictions <- expand_grid(
  taxon = factor(glmm_taxa, levels = glmm_taxa),
  solar_dayness = glmm_dayness_grid,
  depth_mean_m = glmm_depth_grid
) %>%
  mutate(solar_dayness_scaled = (solar_dayness - glmm_solar_dayness_center) / glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - glmm_depth_mean_center) / glmm_depth_mean_scale)

glmm_surface_model_matrix <- model.matrix(glmm_fixed_terms, glmm_surface_predictions)
glmm_surface_model_matrix <- glmm_surface_model_matrix[, names(glmm_coef), drop = FALSE]

glmm_surface_predictions <- glmm_surface_predictions %>%
  mutate(link_fit = as.numeric(glmm_surface_model_matrix %*% glmm_coef),
         link_se = sqrt(rowSums((glmm_surface_model_matrix %*% glmm_vcov) *
                                  glmm_surface_model_matrix)),
         fit = exp(link_fit),
         lwr = exp(link_fit - 1.96 * link_se),
         upr = exp(link_fit + 1.96 * link_se)) %>%
  mutate(log_fit = log(fit),
         taxon = factor(as.character(taxon), levels = taxa_panel_order))

# Put zero catches at the lower color limit on the log concentration plot.
glmm_surface_limits <- range(glmm_surface_predictions$log_fit, finite = TRUE)

glmm_surface_plot <- ggplot(glmm_surface_predictions,
                            aes(x = solar_dayness, y = depth_mean_m, fill = log_fit)) +
  geom_raster(interpolate = TRUE) +
  geom_contour(aes(z = log_fit), color = "white", linewidth = 0.2, alpha = 0.6) +
  geom_point(data = glmm_observed_tows, aes(fill = log(individuals_per_m3)),
             shape = 21, color = "white", size = 0.5, stroke = 0.35 / 3) +
  scale_y_reverse() +
  scale_fill_viridis_c(option = "magma", limits = glmm_surface_limits,
                       oob = scales::oob_squish_any) +
  facet_wrap(~ taxon, labeller = taxon_facet_labeller,
             axes = "all", axis.labels = "margins") +
  labs(x = "Time of day",
       y = "Mean tow depth (m)",
       fill = expression(paste("Log concentration (", m^{-3}, ")"))) +
  theme_classic()

ggsave(here("output/glmm_matched_predicted_abundance_surfaces_all_taxa.png"),
       plot = glmm_surface_plot,
       width = 12,
       height = 8,
       dpi = 300)

glmm_surface_normalized_plot <- plot_relative_abundance_surface(
  glmm_surface_predictions, glmm_observed_tows
)

ggsave(here("output/glmm_matched_predicted_abundance_surfaces_normalized_all_taxa.png"),
       plot = glmm_surface_normalized_plot,
       width = 12,
       height = 8,
       dpi = 300)


# GLMM depth-slope function across solar dayness --------------------------

glmm_slope_predictions <- expand_grid(
  taxon = factor(glmm_taxa, levels = glmm_taxa),
  solar_dayness = glmm_dayness_grid
)

glmm_shallow_predictions <- glmm_slope_predictions %>%
  mutate(depth_mean_m = glmm_min_depth,
         solar_dayness_scaled = (solar_dayness - glmm_solar_dayness_center) / glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - glmm_depth_mean_center) / glmm_depth_mean_scale)

glmm_deep_predictions <- glmm_slope_predictions %>%
  mutate(depth_mean_m = glmm_max_depth,
         solar_dayness_scaled = (solar_dayness - glmm_solar_dayness_center) / glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - glmm_depth_mean_center) / glmm_depth_mean_scale)

glmm_shallow_model_matrix <- model.matrix(glmm_fixed_terms, glmm_shallow_predictions)
glmm_deep_model_matrix <- model.matrix(glmm_fixed_terms, glmm_deep_predictions)
glmm_shallow_model_matrix <- glmm_shallow_model_matrix[, names(glmm_coef), drop = FALSE]
glmm_deep_model_matrix <- glmm_deep_model_matrix[, names(glmm_coef), drop = FALSE]

glmm_slope_model_matrix <- (glmm_deep_model_matrix - glmm_shallow_model_matrix) /
  (glmm_max_depth - glmm_min_depth)

glmm_slope_predictions <- glmm_slope_predictions %>%
  mutate(fit = as.numeric(glmm_slope_model_matrix %*% glmm_coef),
         se = sqrt(rowSums((glmm_slope_model_matrix %*% glmm_vcov) *
                             glmm_slope_model_matrix)),
         lwr = fit - 1.96 * se,
         upr = fit + 1.96 * se,
         taxon = factor(as.character(taxon), levels = taxa_panel_order))

glmm_slope_plot <- ggplot(glmm_slope_predictions,
                          aes(x = solar_dayness, y = fit, ymin = lwr, ymax = upr)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3, linetype = "11") +
  geom_ribbon(alpha = 0.25, fill = "grey60") +
  geom_line(linewidth = 0.5) +
  glmm_sampling_rug(glmm_model_data) +
  facet_wrap(~ taxon, scales = "free_y", labeller = taxon_facet_labeller,
             axes = "all", axis.labels = "all_y") +
  labs(x = "Time of day",
       y = expression(paste("Slope of effect of depth on abundance (", m^{-3}, " ", m^{-1}, ")"))) +
  theme_classic()

ggsave(here("output/glmm_matched_depth_slope_functions_all_taxa.png"),
       plot = glmm_slope_plot,
       width = 12,
       height = 8,
       dpi = 300)


# GLMM integrated predicted abundance across depth ------------------------

glmm_integrated_predictions <- expand_grid(
  taxon = factor(glmm_taxa, levels = glmm_taxa),
  solar_dayness = glmm_dayness_grid,
  depth_mean_m = glmm_integration_depth_grid
) %>%
  left_join(glmm_integration_depth_weights, by = "depth_mean_m") %>%
  mutate(solar_dayness_scaled = (solar_dayness - glmm_solar_dayness_center) / glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - glmm_depth_mean_center) / glmm_depth_mean_scale)

glmm_integrated_summary <- tibble()

for (glmm_taxon in glmm_taxa) {
  glmm_integrated_taxon_predictions <- glmm_integrated_predictions %>%
    filter(taxon == glmm_taxon)
  
  glmm_integrated_model_matrix <- model.matrix(glmm_fixed_terms,
                                               glmm_integrated_taxon_predictions)
  glmm_integrated_model_matrix <- glmm_integrated_model_matrix[, names(glmm_coef), drop = FALSE]
  
  glmm_integrated_taxon_predictions <- glmm_integrated_taxon_predictions %>%
    mutate(point_fit = exp(as.numeric(glmm_integrated_model_matrix %*% glmm_coef)),
           integration_group = sprintf("%.8f", solar_dayness))
  
  glmm_integrated_fit <- rowsum(glmm_integrated_taxon_predictions$point_fit *
                                  glmm_integrated_taxon_predictions$depth_weight,
                                glmm_integrated_taxon_predictions$integration_group,
                                reorder = FALSE)
  
  glmm_integrated_gradient <- rowsum(glmm_integrated_model_matrix *
                                       (glmm_integrated_taxon_predictions$point_fit *
                                          glmm_integrated_taxon_predictions$depth_weight),
                                     glmm_integrated_taxon_predictions$integration_group,
                                     reorder = FALSE)
  
  glmm_integrated_se <- sqrt(rowSums((glmm_integrated_gradient %*% glmm_vcov) *
                                       glmm_integrated_gradient))
  
  glmm_integrated_taxon_summary <- glmm_integrated_taxon_predictions %>%
    distinct(integration_group, taxon, solar_dayness) %>%
    arrange(solar_dayness) %>%
    mutate(fit = as.numeric(glmm_integrated_fit),
           se = glmm_integrated_se,
           lwr = pmax(0, fit - 1.96 * se),
           upr = fit + 1.96 * se)
  
  glmm_integrated_summary <- bind_rows(glmm_integrated_summary,
                                       glmm_integrated_taxon_summary)
}

glmm_integrated_summary <- glmm_integrated_summary %>%
  mutate(taxon = factor(as.character(taxon), levels = taxa_panel_order))

glmm_integrated_plot <- ggplot(glmm_integrated_summary,
                               aes(x = solar_dayness, y = fit, ymin = lwr, ymax = upr)) +
  geom_ribbon(alpha = 0.25, fill = "grey60") +
  geom_line(linewidth = 0.5) +
  glmm_sampling_rug(glmm_model_data) +
  facet_wrap(~ taxon, scales = "free_y", labeller = taxon_facet_labeller,
             axes = "all", axis.labels = "all_y") +
  labs(x = "Time of day",
       y = expression("Integrated predicted larvae " ~ m^{-2})) +
  theme_classic()

ggsave(here("output/glmm_matched_integrated_abundance_functions_all_taxa.png"),
       plot = glmm_integrated_plot,
       width = 12,
       height = 8,
       dpi = 300)


# GLMM for the full data set -----------------------------------------------

full_glmm_model_data <- model_data %>%
  drop_na(year, solar_dayness, solar_dayness_scaled, depth_mean_m, depth_mean_m_scaled,
          start_latitude_dd_scaled, seafloor_depth_m_scaled, mean_temperature_c_scaled,
          mean_salinity_psu_scaled, dissolved_oxygen_ml_l_scaled,
          mean_chl_0_100_m_mgm3_scaled, volume_best_m3_both_sides) %>%
  mutate(taxon = droplevels(taxon),
         year = droplevels(year),
         transect_station_rep_year_net = factor(transect_station_rep_year_net))

full_glmm_taxa <- levels(full_glmm_model_data$taxon)
full_glmm_years <- levels(full_glmm_model_data$year)
full_glmm_solar_dayness_center <- mean(env_wide$solar_dayness, na.rm = TRUE)
full_glmm_solar_dayness_scale <- sd(env_wide$solar_dayness, na.rm = TRUE)
full_glmm_depth_mean_center <- mean(env_wide$depth_mean_m, na.rm = TRUE)
full_glmm_depth_mean_scale <- sd(env_wide$depth_mean_m, na.rm = TRUE)
full_glmm_dayness_grid <- seq(min(full_glmm_model_data$solar_dayness, na.rm = TRUE),
                        max(full_glmm_model_data$solar_dayness, na.rm = TRUE),
                        length.out = 100)
full_glmm_depth_grid <- seq(min(full_glmm_model_data$depth_mean_m, na.rm = TRUE),
                      max(full_glmm_model_data$depth_mean_m, na.rm = TRUE),
                      length.out = 100)

full_glmm_min_depth <- min(full_glmm_model_data$depth_mean_m, na.rm = TRUE)
full_glmm_max_depth <- max(full_glmm_model_data$depth_mean_m, na.rm = TRUE)

full_glmm_integration_depth_grid <- seq(full_glmm_min_depth, full_glmm_max_depth, length.out = 80)
full_glmm_integration_depth_step <- diff(range(full_glmm_integration_depth_grid)) /
  (length(full_glmm_integration_depth_grid) - 1)

full_glmm_integration_depth_weights <- tibble(depth_mean_m = full_glmm_integration_depth_grid,
                                        depth_weight = full_glmm_integration_depth_step) %>%
  mutate(depth_weight = if_else(row_number() %in% c(1, n()),
                                depth_weight / 2,
                                depth_weight))


full_glmm_dayness_depth_model <- glmmTMB(
  individuals_in_tow ~ taxon * solar_dayness_scaled * depth_mean_m_scaled +
    year +
    start_latitude_dd_scaled + seafloor_depth_m_scaled +
    mean_temperature_c_scaled + mean_salinity_psu_scaled +
    dissolved_oxygen_ml_l_scaled + mean_chl_0_100_m_mgm3_scaled +
    offset(log(volume_best_m3_both_sides)) +
    (1 | transect_station_rep_year_net),
  family = nbinom2,
  data = full_glmm_model_data
)

summary(full_glmm_dayness_depth_model)

full_glmm_fixed_terms <- terms(~ taxon * solar_dayness_scaled * depth_mean_m_scaled +
                            year +
                            start_latitude_dd_scaled + seafloor_depth_m_scaled +
                            mean_temperature_c_scaled + mean_salinity_psu_scaled +
                            dissolved_oxygen_ml_l_scaled + mean_chl_0_100_m_mgm3_scaled)
full_glmm_coef <- fixef(full_glmm_dayness_depth_model)$cond
full_glmm_vcov <- vcov(full_glmm_dayness_depth_model)$cond


# Adjust each observed tow to the same reference as the surface: one cubic
# meter, centered environmental covariates, zero tow effect, and equal year
# weights. Keep its original taxon, depth, and solar dayness.
full_glmm_observation_grid <- expand_grid(
  full_glmm_model_data %>%
    transmute(observation = row_number(), taxon, solar_dayness_scaled, depth_mean_m_scaled),
  year = factor(full_glmm_years, levels = full_glmm_years)
) %>%
  mutate(start_latitude_dd_scaled = 0, seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0, mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0, mean_chl_0_100_m_mgm3_scaled = 0)
full_glmm_observation_matrix <- model.matrix(full_glmm_fixed_terms, full_glmm_observation_grid)
full_glmm_observation_matrix <- full_glmm_observation_matrix[, names(full_glmm_coef), drop = FALSE]

full_glmm_reference_fit <- full_glmm_observation_grid %>%
  mutate(fit = exp(as.numeric(full_glmm_observation_matrix %*% full_glmm_coef))) %>%
  group_by(observation) %>%
  summarize(reference_log_fit = log(mean(fit)), .groups = "drop")

# Add conditional working residuals to the reference log concentration.
# For this log-link model, the working residual is (catch - fitted) / fitted,
# where the fitted catch includes the original year, covariates, and tow effect.
# Partial residuals can be negative, so rescale their range within each taxon.
full_glmm_observed_tows <- full_glmm_model_data %>%
  mutate(observation = row_number(),
         working_residual = residuals(full_glmm_dayness_depth_model, type = "working")) %>%
  left_join(full_glmm_reference_fit, by = "observation") %>%
  mutate(partial_residual = reference_log_fit + working_residual,
         taxon = factor(taxon, levels = taxa_panel_order)) %>%
  group_by(taxon) %>%
  mutate(normalized_observed = scales::rescale(partial_residual)) %>%
  ungroup() %>%
  arrange(partial_residual)


# GLMM predicted abundance surfaces ---------------------------------------

full_glmm_surface_predictions <- expand_grid(
  taxon = factor(full_glmm_taxa, levels = full_glmm_taxa),
  year = factor(full_glmm_years, levels = full_glmm_years),
  solar_dayness = full_glmm_dayness_grid,
  depth_mean_m = full_glmm_depth_grid
) %>%
  mutate(solar_dayness_scaled = (solar_dayness - full_glmm_solar_dayness_center) / full_glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - full_glmm_depth_mean_center) / full_glmm_depth_mean_scale,
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0)

full_glmm_surface_model_matrix <- model.matrix(full_glmm_fixed_terms, full_glmm_surface_predictions)
full_glmm_surface_model_matrix <- full_glmm_surface_model_matrix[, names(full_glmm_coef), drop = FALSE]

full_glmm_surface_predictions <- full_glmm_surface_predictions %>%
  mutate(link_fit = as.numeric(full_glmm_surface_model_matrix %*% full_glmm_coef),
         link_se = sqrt(rowSums((full_glmm_surface_model_matrix %*% full_glmm_vcov) *
                                  full_glmm_surface_model_matrix)),
         fit = exp(link_fit),
         lwr = exp(link_fit - 1.96 * link_se),
         upr = exp(link_fit + 1.96 * link_se)) %>%
  group_by(taxon, solar_dayness, depth_mean_m) %>%
  summarise(fit = mean(fit, na.rm = TRUE),
            lwr = mean(lwr, na.rm = TRUE),
            upr = mean(upr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(log_fit = log(fit),
         taxon = factor(as.character(taxon), levels = taxa_panel_order))

full_glmm_surface_plot <- ggplot(full_glmm_surface_predictions,
                            aes(x = solar_dayness, y = depth_mean_m, fill = log_fit)) +
  geom_raster(interpolate = TRUE) +
  geom_contour(aes(z = log_fit), color = "white", linewidth = 0.2, alpha = 0.6) +
  geom_point(data = full_glmm_observed_tows, aes(fill = partial_residual),
             shape = 21, color = "white", size = 0.5, stroke = 0.35 / 3) +
  scale_y_reverse() +
  scale_fill_viridis_c(option = "magma", limits = range(full_glmm_surface_predictions$log_fit, finite = TRUE),
                       oob = scales::oob_squish_any) +
  facet_wrap(~ taxon, labeller = taxon_facet_labeller,
             axes = "all", axis.labels = "margins") +
  labs(x = "Time of day",
       y = "Mean tow depth (m)",
       fill = expression(paste("Log concentration (", m^{-3}, ")"))) +
  theme_classic()

ggsave(here("output/glmm_full_data_predicted_abundance_surfaces_all_taxa.png"),
       plot = full_glmm_surface_plot,
       width = 12,
       height = 8,
       dpi = 300)

full_glmm_surface_normalized_plot <- plot_relative_abundance_surface(
  full_glmm_surface_predictions, full_glmm_observed_tows
)

ggsave(here("output/glmm_full_data_predicted_abundance_surfaces_normalized_all_taxa.png"),
       plot = full_glmm_surface_normalized_plot,
       width = 12,
       height = 8,
       dpi = 300)


# GLMM depth-slope function across solar dayness --------------------------

full_glmm_slope_predictions <- expand_grid(
  taxon = factor(full_glmm_taxa, levels = full_glmm_taxa),
  solar_dayness = full_glmm_dayness_grid
)

full_glmm_shallow_predictions <- full_glmm_slope_predictions %>%
  mutate(depth_mean_m = full_glmm_min_depth,
         solar_dayness_scaled = (solar_dayness - full_glmm_solar_dayness_center) / full_glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - full_glmm_depth_mean_center) / full_glmm_depth_mean_scale,
         year = factor(first(full_glmm_years), levels = full_glmm_years),
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0)

full_glmm_deep_predictions <- full_glmm_slope_predictions %>%
  mutate(depth_mean_m = full_glmm_max_depth,
         solar_dayness_scaled = (solar_dayness - full_glmm_solar_dayness_center) / full_glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - full_glmm_depth_mean_center) / full_glmm_depth_mean_scale,
         year = factor(first(full_glmm_years), levels = full_glmm_years),
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0)

full_glmm_shallow_model_matrix <- model.matrix(full_glmm_fixed_terms, full_glmm_shallow_predictions)
full_glmm_deep_model_matrix <- model.matrix(full_glmm_fixed_terms, full_glmm_deep_predictions)
full_glmm_shallow_model_matrix <- full_glmm_shallow_model_matrix[, names(full_glmm_coef), drop = FALSE]
full_glmm_deep_model_matrix <- full_glmm_deep_model_matrix[, names(full_glmm_coef), drop = FALSE]

full_glmm_slope_model_matrix <- (full_glmm_deep_model_matrix - full_glmm_shallow_model_matrix) /
  (full_glmm_max_depth - full_glmm_min_depth)

full_glmm_slope_predictions <- full_glmm_slope_predictions %>%
  mutate(fit = as.numeric(full_glmm_slope_model_matrix %*% full_glmm_coef),
         se = sqrt(rowSums((full_glmm_slope_model_matrix %*% full_glmm_vcov) *
                             full_glmm_slope_model_matrix)),
         lwr = fit - 1.96 * se,
         upr = fit + 1.96 * se,
         taxon = factor(as.character(taxon), levels = taxa_panel_order))

full_glmm_slope_plot <- ggplot(full_glmm_slope_predictions,
                          aes(x = solar_dayness, y = fit, ymin = lwr, ymax = upr)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3, linetype = "11") +
  geom_ribbon(alpha = 0.25, fill = "grey60") +
  geom_line(linewidth = 0.5) +
  glmm_sampling_rug(full_glmm_model_data) +
  facet_wrap(~ taxon, scales = "free_y", labeller = taxon_facet_labeller,
             axes = "all", axis.labels = "all_y") +
  labs(x = "Time of day",
       y = expression(paste("Slope of effect of depth on abundance (", m^{-3}, " ", m^{-1}, ")"))) +
  theme_classic()

ggsave(here("output/glmm_full_data_depth_slope_functions_all_taxa.png"),
       plot = full_glmm_slope_plot,
       width = 12,
       height = 8,
       dpi = 300)


# GLMM integrated predicted abundance across depth ------------------------

full_glmm_integrated_predictions <- expand_grid(
  taxon = factor(full_glmm_taxa, levels = full_glmm_taxa),
  year = factor(full_glmm_years, levels = full_glmm_years),
  solar_dayness = full_glmm_dayness_grid,
  depth_mean_m = full_glmm_integration_depth_grid
) %>%
  left_join(full_glmm_integration_depth_weights, by = "depth_mean_m") %>%
  mutate(solar_dayness_scaled = (solar_dayness - full_glmm_solar_dayness_center) / full_glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - full_glmm_depth_mean_center) / full_glmm_depth_mean_scale,
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0,
         depth_weight = depth_weight / length(full_glmm_years))

full_glmm_integrated_summary <- tibble()

for (full_glmm_taxon in full_glmm_taxa) {
  full_glmm_integrated_taxon_predictions <- full_glmm_integrated_predictions %>%
    filter(taxon == full_glmm_taxon)
  
  full_glmm_integrated_model_matrix <- model.matrix(full_glmm_fixed_terms,
                                               full_glmm_integrated_taxon_predictions)
  full_glmm_integrated_model_matrix <- full_glmm_integrated_model_matrix[, names(full_glmm_coef), drop = FALSE]
  
  full_glmm_integrated_taxon_predictions <- full_glmm_integrated_taxon_predictions %>%
    mutate(point_fit = exp(as.numeric(full_glmm_integrated_model_matrix %*% full_glmm_coef)),
           integration_group = sprintf("%.8f", solar_dayness))
  
  full_glmm_integrated_fit <- rowsum(full_glmm_integrated_taxon_predictions$point_fit *
                                  full_glmm_integrated_taxon_predictions$depth_weight,
                                full_glmm_integrated_taxon_predictions$integration_group,
                                reorder = FALSE)
  
  full_glmm_integrated_gradient <- rowsum(full_glmm_integrated_model_matrix *
                                       (full_glmm_integrated_taxon_predictions$point_fit *
                                          full_glmm_integrated_taxon_predictions$depth_weight),
                                     full_glmm_integrated_taxon_predictions$integration_group,
                                     reorder = FALSE)
  
  full_glmm_integrated_se <- sqrt(rowSums((full_glmm_integrated_gradient %*% full_glmm_vcov) *
                                       full_glmm_integrated_gradient))
  
  full_glmm_integrated_taxon_summary <- full_glmm_integrated_taxon_predictions %>%
    distinct(integration_group, taxon, solar_dayness) %>%
    arrange(solar_dayness) %>%
    mutate(fit = as.numeric(full_glmm_integrated_fit),
           se = full_glmm_integrated_se,
           lwr = pmax(0, fit - 1.96 * se),
           upr = fit + 1.96 * se)
  
  full_glmm_integrated_summary <- bind_rows(full_glmm_integrated_summary,
                                       full_glmm_integrated_taxon_summary)
}

full_glmm_integrated_summary <- full_glmm_integrated_summary %>%
  mutate(taxon = factor(as.character(taxon), levels = taxa_panel_order))

full_glmm_integrated_plot <- ggplot(full_glmm_integrated_summary,
                               aes(x = solar_dayness, y = fit, ymin = lwr, ymax = upr)) +
  geom_ribbon(alpha = 0.25, fill = "grey60") +
  geom_line(linewidth = 0.5) +
  glmm_sampling_rug(full_glmm_model_data) +
  facet_wrap(~ taxon, scales = "free_y", labeller = taxon_facet_labeller,
             axes = "all", axis.labels = "all_y") +
  labs(x = "Time of day",
       y = expression("Integrated predicted larvae " ~ m^{-2})) +
  theme_classic()

ggsave(here("output/glmm_full_data_integrated_abundance_functions_all_taxa.png"),
       plot = full_glmm_integrated_plot,
       width = 12,
       height = 8,
       dpi = 300)
