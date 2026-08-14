# Description -------------------------------------------------------------

# Aside analysis of depth stratified total abundances across replicates for
# each taxa against solar dayness and depth


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(mgcv)
library(glmmTMB)


# Source code -------------------------------------------------------------

source(here("scripts/02_prepare_community_data.R"))


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

# Fit GAM of abundance across depth and solar dayness ---------------------

gam_model_data <- model_data %>%
  drop_na(year, solar_dayness, solar_dayness_scaled, depth_mean_m, depth_mean_m_scaled,
          start_latitude_dd_scaled, seafloor_depth_m_scaled, mean_temperature_c_scaled,
          mean_salinity_psu_scaled, dissolved_oxygen_ml_l_scaled,
          mean_chl_0_100_m_mgm3_scaled, volume_best_m3_both_sides) %>%
  mutate(taxon = droplevels(taxon),
         year = droplevels(year),
         transect_station_rep_year_net = factor(transect_station_rep_year_net))

gam_dayness_depth_model <- bam(
  individuals_in_tow ~ taxon + year +
    te(solar_dayness_scaled, depth_mean_m_scaled, by = taxon, k = c(5, 5)) +
    s(start_latitude_dd_scaled, k = 5) +
    s(seafloor_depth_m_scaled, k = 5) +
    s(mean_temperature_c_scaled, k = 5) +
    s(mean_salinity_psu_scaled, k = 5) +
    s(dissolved_oxygen_ml_l_scaled, k = 5) +
    s(mean_chl_0_100_m_mgm3_scaled, k = 5) +
    s(transect_station_rep_year_net, bs = "re") +
    offset(log(volume_best_m3_both_sides)),
  family = nb(),
  data = gam_model_data,
  method = "fREML",
  discrete = TRUE
)

summary(gam_dayness_depth_model)

gam_taxa <- levels(gam_model_data$taxon)
gam_years <- levels(gam_model_data$year)
gam_solar_dayness_center <- mean(env_wide$solar_dayness, na.rm = TRUE)
gam_solar_dayness_scale <- sd(env_wide$solar_dayness, na.rm = TRUE)
gam_depth_mean_center <- mean(env_wide$depth_mean_m, na.rm = TRUE)
gam_depth_mean_scale <- sd(env_wide$depth_mean_m, na.rm = TRUE)
gam_dayness_grid <- seq(min(gam_model_data$solar_dayness, na.rm = TRUE),
                        max(gam_model_data$solar_dayness, na.rm = TRUE),
                        length.out = 100)
gam_depth_grid <- seq(min(gam_model_data$depth_mean_m, na.rm = TRUE),
                      max(gam_model_data$depth_mean_m, na.rm = TRUE),
                      length.out = 100)

gam_coef <- coef(gam_dayness_depth_model)
gam_vcov <- vcov(gam_dayness_depth_model, unconditional = TRUE)
gam_re_term <- "s(transect_station_rep_year_net)"


# Predicted abundance surfaces -------------------------------------------

gam_surface_predictions <- expand_grid(
  taxon = factor(gam_taxa, levels = gam_taxa),
  year = factor(gam_years, levels = gam_years),
  solar_dayness = gam_dayness_grid,
  depth_mean_m = gam_depth_grid
) %>%
  mutate(solar_dayness_scaled = (solar_dayness - gam_solar_dayness_center) / gam_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - gam_depth_mean_center) / gam_depth_mean_scale,
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0,
         volume_best_m3_both_sides = 1,
         transect_station_rep_year_net = first(gam_model_data$transect_station_rep_year_net))

gam_surface_prediction_values <- predict(gam_dayness_depth_model,
                                         newdata = gam_surface_predictions,
                                         type = "link",
                                         se.fit = TRUE,
                                         exclude = gam_re_term)

gam_surface_predictions <- gam_surface_predictions %>%
  mutate(fit = exp(gam_surface_prediction_values$fit),
         lwr = exp(gam_surface_prediction_values$fit - 1.96 * gam_surface_prediction_values$se.fit),
         upr = exp(gam_surface_prediction_values$fit + 1.96 * gam_surface_prediction_values$se.fit)) %>%
  group_by(taxon, solar_dayness, depth_mean_m) %>%
  summarise(fit = mean(fit, na.rm = TRUE),
            lwr = mean(lwr, na.rm = TRUE),
            upr = mean(upr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(log_fit = log(fit))

gam_surface_plot <- ggplot(gam_surface_predictions,
                           aes(x = solar_dayness, y = depth_mean_m, fill = log_fit)) +
  geom_raster(interpolate = TRUE) +
  geom_contour(aes(z = log_fit), color = "white", linewidth = 0.2, alpha = 0.6) +
  scale_y_reverse() +
  scale_fill_viridis_c(option = "magma") +
  facet_wrap(~ taxon) +
  labs(x = "Solar dayness",
       y = "Mean tow depth (m)",
       fill = expression("Log predicted larvae " ~ m^{-3})) +
  theme_classic()

ggsave(here("output/gam_predicted_abundance_surfaces_all_taxa.png"),
       plot = gam_surface_plot,
       width = 12,
       height = 8,
       dpi = 300)


# Depth-slope function across solar dayness -------------------------------

gam_min_depth <- min(gam_model_data$depth_mean_m, na.rm = TRUE)
gam_max_depth <- max(gam_model_data$depth_mean_m, na.rm = TRUE)

gam_slope_predictions <- expand_grid(
  taxon = factor(gam_taxa, levels = gam_taxa),
  solar_dayness = gam_dayness_grid
)

gam_shallow_predictions <- gam_slope_predictions %>%
  mutate(depth_mean_m = gam_min_depth,
         solar_dayness_scaled = (solar_dayness - gam_solar_dayness_center) / gam_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - gam_depth_mean_center) / gam_depth_mean_scale,
         year = factor(first(gam_years), levels = gam_years),
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0,
         volume_best_m3_both_sides = 1,
         transect_station_rep_year_net = first(gam_model_data$transect_station_rep_year_net))

gam_deep_predictions <- gam_slope_predictions %>%
  mutate(depth_mean_m = gam_max_depth,
         solar_dayness_scaled = (solar_dayness - gam_solar_dayness_center) / gam_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - gam_depth_mean_center) / gam_depth_mean_scale,
         year = factor(first(gam_years), levels = gam_years),
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0,
         volume_best_m3_both_sides = 1,
         transect_station_rep_year_net = first(gam_model_data$transect_station_rep_year_net))

gam_shallow_lpmatrix <- predict(gam_dayness_depth_model,
                                newdata = gam_shallow_predictions,
                                type = "lpmatrix",
                                exclude = gam_re_term)
gam_deep_lpmatrix <- predict(gam_dayness_depth_model,
                             newdata = gam_deep_predictions,
                             type = "lpmatrix",
                             exclude = gam_re_term)

gam_slope_lpmatrix <- (gam_deep_lpmatrix - gam_shallow_lpmatrix) /
  (gam_max_depth - gam_min_depth)

gam_slope_predictions <- gam_slope_predictions %>%
  mutate(fit = as.numeric(gam_slope_lpmatrix %*% gam_coef),
         se = sqrt(rowSums((gam_slope_lpmatrix %*% gam_vcov) * gam_slope_lpmatrix)),
         lwr = fit - 1.96 * se,
         upr = fit + 1.96 * se)

gam_slope_plot <- ggplot(gam_slope_predictions,
                         aes(x = solar_dayness, y = fit, ymin = lwr, ymax = upr)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3, linetype = "11") +
  geom_ribbon(alpha = 0.25, fill = "grey60") +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ taxon, scales = "free_y") +
  labs(x = "Solar dayness",
       y = expression("Depth gradient of log predicted larvae " ~ m^{-3} ~ m^{-1})) +
  theme_classic()

ggsave(here("output/gam_depth_slope_functions_all_taxa.png"),
       plot = gam_slope_plot,
       width = 12,
       height = 8,
       dpi = 300)


# Integrated predicted abundance across depth -----------------------------

gam_integration_depth_grid <- seq(gam_min_depth, gam_max_depth, length.out = 80)
gam_integration_depth_step <- diff(range(gam_integration_depth_grid)) /
  (length(gam_integration_depth_grid) - 1)

gam_integration_depth_weights <- tibble(depth_mean_m = gam_integration_depth_grid,
                                        depth_weight = gam_integration_depth_step) %>%
  mutate(depth_weight = if_else(row_number() %in% c(1, n()),
                                depth_weight / 2,
                                depth_weight))

gam_integrated_predictions <- expand_grid(
  taxon = factor(gam_taxa, levels = gam_taxa),
  year = factor(gam_years, levels = gam_years),
  solar_dayness = gam_dayness_grid,
  depth_mean_m = gam_integration_depth_grid
) %>%
  left_join(gam_integration_depth_weights, by = "depth_mean_m") %>%
  mutate(solar_dayness_scaled = (solar_dayness - gam_solar_dayness_center) / gam_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - gam_depth_mean_center) / gam_depth_mean_scale,
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0,
         depth_weight = depth_weight / length(gam_years),
         volume_best_m3_both_sides = 1,
         transect_station_rep_year_net = first(gam_model_data$transect_station_rep_year_net))

gam_integrated_summary <- tibble()

for (gam_taxon in gam_taxa) {
  gam_integrated_taxon_predictions <- gam_integrated_predictions %>%
    filter(taxon == gam_taxon)
  
  gam_integrated_lpmatrix <- predict(gam_dayness_depth_model,
                                     newdata = gam_integrated_taxon_predictions,
                                     type = "lpmatrix",
                                     exclude = gam_re_term)
  
  gam_integrated_taxon_predictions <- gam_integrated_taxon_predictions %>%
    mutate(point_fit = exp(as.numeric(gam_integrated_lpmatrix %*% gam_coef)),
           integration_group = sprintf("%.8f", solar_dayness))
  
  gam_integrated_fit <- rowsum(gam_integrated_taxon_predictions$point_fit *
                                 gam_integrated_taxon_predictions$depth_weight,
                               gam_integrated_taxon_predictions$integration_group,
                               reorder = FALSE)
  
  gam_integrated_gradient <- rowsum(gam_integrated_lpmatrix *
                                      (gam_integrated_taxon_predictions$point_fit *
                                         gam_integrated_taxon_predictions$depth_weight),
                                    gam_integrated_taxon_predictions$integration_group,
                                    reorder = FALSE)
  
  gam_integrated_se <- sqrt(rowSums((gam_integrated_gradient %*% gam_vcov) *
                                      gam_integrated_gradient))
  
  gam_integrated_taxon_summary <- gam_integrated_taxon_predictions %>%
    distinct(integration_group, taxon, solar_dayness) %>%
    arrange(solar_dayness) %>%
    mutate(fit = as.numeric(gam_integrated_fit),
           se = gam_integrated_se,
           lwr = pmax(0, fit - 1.96 * se),
           upr = fit + 1.96 * se)
  
  gam_integrated_summary <- bind_rows(gam_integrated_summary,
                                      gam_integrated_taxon_summary)
}

gam_integrated_plot <- ggplot(gam_integrated_summary,
                              aes(x = solar_dayness, y = fit, ymin = lwr, ymax = upr)) +
  geom_ribbon(alpha = 0.25, fill = "grey60") +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ taxon, scales = "free_y") +
  labs(x = "Solar dayness",
       y = expression("Integrated predicted larvae " ~ m^{-2})) +
  theme_classic()

ggsave(here("output/gam_integrated_abundance_functions_all_taxa.png"),
       plot = gam_integrated_plot,
       width = 12,
       height = 8,
       dpi = 300)


# GLMM comparison model ----------------------------------------------------

glmm_model_data <- gam_model_data

glmm_dayness_depth_model <- glmmTMB(
  individuals_in_tow ~ taxon * solar_dayness_scaled * depth_mean_m_scaled +
    year +
    start_latitude_dd_scaled + seafloor_depth_m_scaled +
    mean_temperature_c_scaled + mean_salinity_psu_scaled +
    dissolved_oxygen_ml_l_scaled + mean_chl_0_100_m_mgm3_scaled +
    offset(log(volume_best_m3_both_sides)) +
    (1 | transect_station_rep_year_net),
  family = nbinom2,
  data = glmm_model_data
)

summary(glmm_dayness_depth_model)

glmm_fixed_terms <- terms(~ taxon * solar_dayness_scaled * depth_mean_m_scaled +
                            year +
                            start_latitude_dd_scaled + seafloor_depth_m_scaled +
                            mean_temperature_c_scaled + mean_salinity_psu_scaled +
                            dissolved_oxygen_ml_l_scaled + mean_chl_0_100_m_mgm3_scaled)
glmm_coef <- fixef(glmm_dayness_depth_model)$cond
glmm_vcov <- vcov(glmm_dayness_depth_model)$cond


# GLMM predicted abundance surfaces ---------------------------------------

glmm_surface_predictions <- expand_grid(
  taxon = factor(gam_taxa, levels = gam_taxa),
  year = factor(gam_years, levels = gam_years),
  solar_dayness = gam_dayness_grid,
  depth_mean_m = gam_depth_grid
) %>%
  mutate(solar_dayness_scaled = (solar_dayness - gam_solar_dayness_center) / gam_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - gam_depth_mean_center) / gam_depth_mean_scale,
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0)

glmm_surface_model_matrix <- model.matrix(glmm_fixed_terms, glmm_surface_predictions)
glmm_surface_model_matrix <- glmm_surface_model_matrix[, names(glmm_coef), drop = FALSE]

glmm_surface_predictions <- glmm_surface_predictions %>%
  mutate(link_fit = as.numeric(glmm_surface_model_matrix %*% glmm_coef),
         link_se = sqrt(rowSums((glmm_surface_model_matrix %*% glmm_vcov) *
                                  glmm_surface_model_matrix)),
         fit = exp(link_fit),
         lwr = exp(link_fit - 1.96 * link_se),
         upr = exp(link_fit + 1.96 * link_se)) %>%
  group_by(taxon, solar_dayness, depth_mean_m) %>%
  summarise(fit = mean(fit, na.rm = TRUE),
            lwr = mean(lwr, na.rm = TRUE),
            upr = mean(upr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(log_fit = log(fit))

glmm_surface_plot <- ggplot(glmm_surface_predictions,
                            aes(x = solar_dayness, y = depth_mean_m, fill = log_fit)) +
  geom_raster(interpolate = TRUE) +
  geom_contour(aes(z = log_fit), color = "white", linewidth = 0.2, alpha = 0.6) +
  scale_y_reverse() +
  scale_fill_viridis_c(option = "magma") +
  facet_wrap(~ taxon) +
  labs(x = "Solar dayness",
       y = "Mean tow depth (m)",
       fill = expression("Log predicted larvae " ~ m^{-3})) +
  theme_classic()

ggsave(here("output/glmm_predicted_abundance_surfaces_all_taxa.png"),
       plot = glmm_surface_plot,
       width = 12,
       height = 8,
       dpi = 300)


# GLMM depth-slope function across solar dayness --------------------------

glmm_slope_predictions <- expand_grid(
  taxon = factor(gam_taxa, levels = gam_taxa),
  solar_dayness = gam_dayness_grid
)

glmm_shallow_predictions <- glmm_slope_predictions %>%
  mutate(depth_mean_m = gam_min_depth,
         solar_dayness_scaled = (solar_dayness - gam_solar_dayness_center) / gam_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - gam_depth_mean_center) / gam_depth_mean_scale,
         year = factor(first(gam_years), levels = gam_years),
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0)

glmm_deep_predictions <- glmm_slope_predictions %>%
  mutate(depth_mean_m = gam_max_depth,
         solar_dayness_scaled = (solar_dayness - gam_solar_dayness_center) / gam_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - gam_depth_mean_center) / gam_depth_mean_scale,
         year = factor(first(gam_years), levels = gam_years),
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0)

glmm_shallow_model_matrix <- model.matrix(glmm_fixed_terms, glmm_shallow_predictions)
glmm_deep_model_matrix <- model.matrix(glmm_fixed_terms, glmm_deep_predictions)
glmm_shallow_model_matrix <- glmm_shallow_model_matrix[, names(glmm_coef), drop = FALSE]
glmm_deep_model_matrix <- glmm_deep_model_matrix[, names(glmm_coef), drop = FALSE]

glmm_slope_model_matrix <- (glmm_deep_model_matrix - glmm_shallow_model_matrix) /
  (gam_max_depth - gam_min_depth)

glmm_slope_predictions <- glmm_slope_predictions %>%
  mutate(fit = as.numeric(glmm_slope_model_matrix %*% glmm_coef),
         se = sqrt(rowSums((glmm_slope_model_matrix %*% glmm_vcov) *
                             glmm_slope_model_matrix)),
         lwr = fit - 1.96 * se,
         upr = fit + 1.96 * se)

glmm_slope_plot <- ggplot(glmm_slope_predictions,
                          aes(x = solar_dayness, y = fit, ymin = lwr, ymax = upr)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3, linetype = "11") +
  geom_ribbon(alpha = 0.25, fill = "grey60") +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ taxon, scales = "free_y") +
  labs(x = "Solar dayness",
       y = expression("Depth gradient of log predicted larvae " ~ m^{-3} ~ m^{-1})) +
  theme_classic()

ggsave(here("output/glmm_depth_slope_functions_all_taxa.png"),
       plot = glmm_slope_plot,
       width = 12,
       height = 8,
       dpi = 300)


# GLMM integrated predicted abundance across depth ------------------------

glmm_integrated_predictions <- expand_grid(
  taxon = factor(gam_taxa, levels = gam_taxa),
  year = factor(gam_years, levels = gam_years),
  solar_dayness = gam_dayness_grid,
  depth_mean_m = gam_integration_depth_grid
) %>%
  left_join(gam_integration_depth_weights, by = "depth_mean_m") %>%
  mutate(solar_dayness_scaled = (solar_dayness - gam_solar_dayness_center) / gam_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - gam_depth_mean_center) / gam_depth_mean_scale,
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0,
         depth_weight = depth_weight / length(gam_years))

glmm_integrated_summary <- tibble()

for (glmm_taxon in gam_taxa) {
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

glmm_integrated_plot <- ggplot(glmm_integrated_summary,
                               aes(x = solar_dayness, y = fit, ymin = lwr, ymax = upr)) +
  geom_ribbon(alpha = 0.25, fill = "grey60") +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ taxon, scales = "free_y") +
  labs(x = "Solar dayness",
       y = expression("Integrated predicted larvae " ~ m^{-2})) +
  theme_classic()

ggsave(here("output/glmm_integrated_abundance_functions_all_taxa.png"),
       plot = glmm_integrated_plot,
       width = 12,
       height = 8,
       dpi = 300)

