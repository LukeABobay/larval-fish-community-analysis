# Description -------------------------------------------------------------

#This script may now be obsolete as analysis with only main 4 clusters doesn't seem to apply for objective 3

#Run objective 3 analyses and plots with only 4 main clusters

# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(visreg)
library(DHARMa)
library(glmmTMB)


# Source code -------------------------------------------------------------

source(here("scripts/09_obj_2_main_clusters.R"))


# Prepare data ------------------------------------------------------------

# Filter mocness_major_taxa for only main 4 clusters
main_clust_samples_mocness_major_taxa <- mocness_major_taxa_nets %>% 
  semi_join(main_clust_samples, by = "transect_station_rep_year_net")


# Filter to keep only 2019 rows
main_clust_mocness_major_taxa_19 <- filter(main_clust_samples_mocness_major_taxa, 
                                           collection_date > "2019-01-01" & collection_date < "2019-12-31") %>%
  #add time of day column
  mutate(time_of_day = substr(replicate, 3, 3)) %>%
  mutate(time_of_day = recode(time_of_day, "D" = "Day", "N" = "Night")) %>%
  #Reorder taxa
  mutate(taxon = factor(taxon, levels = ordered_taxa)) %>%
  #Reorder stations
  mutate(station = factor(station, levels = rev(sort(unique(station))))) %>%
  mutate(color_scheme_group = case_when(taxon %in% mesopelagic_species ~ "Mesopelagic",
                                        taxon %in% flatfish_species ~ "Flatfish",
                                        taxon %in% sculpin_relatives_species ~ "Sculpins_and_related",
                                        taxon %in% other_species ~ "Other",
                                        TRUE ~ "Other"))

main_clust_taxa_to_keep_2019 <- main_clust_mocness_major_taxa_19 %>%
  filter(!str_detect(transect_station_rep_year_net, "0$")) %>%
  group_by(taxon) %>%
  summarise(freq = n_distinct(transect_station_rep_year_net), .groups = "drop") %>%
  filter(freq >= 0.05 * n_distinct(
    main_clust_mocness_major_taxa_19 %>%
      filter(!str_detect(transect_station_rep_year_net, "0$")) %>%
      pull(transect_station_rep_year_net))) %>%
  pull(taxon)

main_clust_model_data <- main_clust_mocness_major_taxa_19 %>%
  select(collection_date, start_latitude_dd, start_longitude_dd, transect_station_rep_year_net,
         taxon, depth_mean_m, time_of_day, volume_best_m3_both_sides, individuals_in_tow,
         seafloor_depth_m, mean_temperature_c, mean_salinity_psu, dissolved_oxygen_ml_l, mean_chl_0_100_m_mgm3) %>%
  # Remove net 0 data for DVM vignette
  filter(!str_detect(transect_station_rep_year_net, "0$"),
         taxon %in% main_clust_taxa_to_keep_2019) %>%
  mutate(taxon = droplevels(taxon)) %>%
  complete(nesting(collection_date, start_latitude_dd, start_longitude_dd,
                   transect_station_rep_year_net, depth_mean_m, time_of_day, volume_best_m3_both_sides),
           taxon, fill = list(individuals_in_tow = 0)) %>%
  mutate(time_of_day = factor(time_of_day, levels = c("Day", "Night")),
         depth_mean_scaled = scale(depth_mean_m)[, 1],
         seafloor_depth_scaled = scale(seafloor_depth_m),
         mean_temperature_scaled = scale(mean_temperature_c),
         mean_salinity_scaled = scale(mean_salinity_psu),
         dissolved_oxygen_scaled = scale(dissolved_oxygen_ml_l),
         mean_chl_0_100_m_scaled = scale(mean_chl_0_100_m_mgm3),
         individuals_per_m3 = individuals_in_tow / volume_best_m3_both_sides)


# Avg taxa concentrations across replicates
main_clust_avgd_mocness_major_taxa_19 <- main_clust_mocness_major_taxa_19 %>%
  group_by(taxon, time_of_day, depth_range, depth_mean_m, depth_diff_m) %>%
  summarise(avg_taxa_concentration = mean(individuals_per_m3, na.rm=TRUE)) %>%
  ungroup()


# Plot taxa concentrations by depths and day/night ------------------------
# Barplot
ggplot(main_clust_avgd_mocness_major_taxa_19, aes(x = depth_range, y = avg_taxa_concentration, fill = taxon)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  facet_wrap(~ time_of_day, nrow = 2) +
  labs(title = "Day-night comparison of taxa concentrations at depth ranges",
       x = "Depth sampled (m)", y = "average individuals per m3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Scatterplot
d_n_scatter_main_clust <- ggplot(main_clust_model_data, aes(x = depth_mean_m, y = log(individuals_per_m3), color = taxon)) +
  geom_point() +
  geom_smooth(method = "lm",se = FALSE) +
  facet_wrap(~ time_of_day, nrow = 2) +
  scale_color_manual(values = species_colors, labels = parse(text = taxon_labels)) +
  labs(title = "Day-night comparison of taxa concentrations by mean depths for main cluster samples",
       x = "Mean tow depth (m)", y = "Concentration (log(ind./m3)", color = "Taxon") +
  theme_classic()
ggsave("D_N_scatter_main_clust.png", plot = d_n_scatter_main_clust, path = here("output"),
       width = 7, height = 5, units = "in", dpi = 300)


# Fit linear model(s) of taxa concentrations against depth and time of day -----
main_clust_day_night_depth_model <- lm(avg_taxa_concentration ~ taxon*time_of_day + taxon*depth_range, data = main_clust_avgd_mocness_major_taxa_19)
summary(main_clust_day_night_depth_model)

main_clust_day_night_mean_depth_model <- lm(avg_taxa_concentration ~ taxon*time_of_day + taxon*depth_mean_m, data = main_clust_avgd_mocness_major_taxa_19)
summary(main_clust_day_night_mean_depth_model)


# Model without allowing effects of depth and time of day to vary among taxa
main_clust_simple_model_pois <- glm(individuals_in_tow ~ taxon + time_of_day + depth_mean_scaled + offset(log(volume_best_m3_both_sides)),
                         family = poisson,
                         data = main_clust_model_data)
summary(main_clust_simple_model_pois)
visreg(main_clust_simple_model_pois)
main_clust_simp_res_pois <- simulateResiduals(main_clust_simple_model_pois, n = 1000)
plot(main_clust_simp_res_pois)
testResiduals(main_clust_simp_res_pois)
testDispersion(main_clust_simp_res_pois)
testZeroInflation(main_clust_simp_res_pois)

# Full model with Poisson response distribution
main_clust_full_model_pois <- glmmTMB(individuals_in_tow ~ taxon * time_of_day * depth_mean_scaled +
                             offset(log(volume_best_m3_both_sides)) +
                             (1 | transect_station_rep_year_net),
                           family = poisson,
                           data = main_clust_model_data)
## RM note: this model above is giving an error "negative log-likelihood is NaN at starting parameter values"
##   Tried fixes suggested by CoPilot but still got the same error so will need to discuss what the best approach is.
summary(main_clust_full_model_pois)
visreg(main_clust_full_model_pois)
main_clust_full_res_pois <- simulateResiduals(main_clust_full_model_pois, n = 1000)
plot(main_clust_res_pois)
testResiduals(main_clust_full_res_pois)
testDispersion(main_clust_full_res_pois)
testZeroInflation(main_clust_full_res_pois)


# Full model with negative binomial response distribution
main_clust_full_model_nb <- glmmTMB(individuals_in_tow ~ taxon * time_of_day * depth_mean_scaled +
                           offset(log(volume_best_m3_both_sides)) +
                           (1 | transect_station_rep_year_net),
                         family = nbinom2,
                         data = main_clust_model_data)
## RM note: this model is giving the same error. Perhaps using only main cluster samples for these
##   models is just too restrictive? 
summary(main_clust_full_model_nb)
visreg(main_clust_full_model_nb)
main_clust_res_nb_full <- simulateResiduals(main_clust_full_model_nb, n = 1000)
plot(main_clust_res_nb_full)
testResiduals(main_clust_res_nb_full)
testDispersion(main_clust_res_nb_full)
testZeroInflation(main_clust_res_nb_full)


# Plot fixed-effect predictions from the full negative binomial model.
# Setting volume to 1 makes predictions interpretable as expected individuals per m3.
main_clust_full_model_nb_effects <- expand_grid(taxon = factor(levels(main_clust_model_data$taxon), levels = levels(main_clust_model_data$taxon)),
                                     time_of_day = factor(c("Day", "Night"), levels = c("Day", "Night")),
                                     depth_mean_m = seq(min(main_clust_model_data$depth_mean_m, na.rm = TRUE),
                                                        max(main_clust_model_data$depth_mean_m, na.rm = TRUE),
                                                        length.out = 100)) %>%
  mutate(depth_mean_scaled = (depth_mean_m - mean(main_clust_model_data$depth_mean_m, na.rm = TRUE)) /
           sd(main_clust_model_data$depth_mean_m, na.rm = TRUE),
         volume_best_m3_both_sides = 1,
         transect_station_rep_year_net = first(main_clust_model_data$transect_station_rep_year_net))

main_clust_full_model_nb_effect_predictions <- predict(main_clust_full_model_nb,
                                            newdata = main_clust_full_model_nb_effects,
                                            type = "link",
                                            se.fit = TRUE,
                                            re.form = NA)

main_clust_full_model_nb_effects <- main_clust_full_model_nb_effects %>%
  mutate(fit = exp(main_clust_full_model_nb_effect_predictions$fit),
         lwr = exp(main_clust_full_model_nb_effect_predictions$fit - 1.96 * main_clust_full_model_nb_effect_predictions$se.fit),
         upr = exp(main_clust_full_model_nb_effect_predictions$fit + 1.96 * main_clust_full_model_nb_effect_predictions$se.fit))

main_clust_full_model_nb_taxon_sample_sizes <- main_clust_model_data %>%
  group_by(taxon) %>%
  summarise(n_tows_present = sum(individuals_in_tow > 0, na.rm = TRUE),
            n_individuals = sum(individuals_in_tow, na.rm = TRUE),
            .groups = "drop")

main_clust_full_model_nb_fixed_effects <- fixef(main_clust_full_model_nb)$cond
main_clust_full_model_nb_vcov <- vcov(main_clust_full_model_nb)$cond
main_clust_full_model_nb_terms <- terms(~ taxon * time_of_day * depth_mean_scaled)

main_clust_full_model_nb_wald_test <- function(rows_positive, rows_negative = NULL) {
  positive_matrix <- model.matrix(main_clust_full_model_nb_terms,
                                  data = rows_positive,
                                  xlev = list(taxon = levels(main_clust_full_model_nb_effects$taxon),
                                              time_of_day = levels(main_clust_full_model_nb_effects$time_of_day)))
  negative_matrix <- if (is.null(rows_negative)) {
    positive_matrix * 0
  } else {
    model.matrix(main_clust_full_model_nb_terms,
                 data = rows_negative,
                 xlev = list(taxon = levels(main_clust_full_model_nb_effects$taxon),
                             time_of_day = levels(main_clust_full_model_nb_effects$time_of_day)))
  }
  contrast <- colSums(positive_matrix - negative_matrix)
  contrast_aligned <- setNames(rep(0, length(main_clust_full_model_nb_fixed_effects)),
                               names(main_clust_full_model_nb_fixed_effects))
  contrast_aligned[names(contrast)] <- contrast
  contrast <- contrast_aligned
  estimate <- sum(contrast * main_clust_full_model_nb_fixed_effects)
  se <- sqrt(drop(t(contrast) %*% main_clust_full_model_nb_vcov %*% contrast))
  z <- estimate / se
  2 * pnorm(abs(z), lower.tail = FALSE)
}

main_clust_full_model_nb_p_label <- function(p) {
  case_when(
    is.na(p) ~ "NA",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ ".",
    TRUE ~ "ns"
  )
}

main_clust_full_model_nb_effect_tests <- map_dfr(levels(main_clust_full_model_nb_effects$taxon), function(current_taxon) {
  rows <- tibble(taxon = factor(current_taxon, levels = levels(main_clust_full_model_nb_effects$taxon)),
                 time_of_day = factor(c("Day", "Day", "Night", "Night"),
                                      levels = levels(main_clust_full_model_nb_effects$time_of_day)),
                 depth_mean_scaled = c(1, 0, 1, 0))
  
  depth_time_p <- main_clust_full_model_nb_wald_test(rows[c(3, 2), ], rows[c(4, 1), ])
  
  tibble(taxon = factor(current_taxon, levels = levels(main_clust_model_data$taxon)),
         depth_time_p = depth_time_p)
}) %>%
  mutate(label = paste0("Day-night slope difference: ", main_clust_full_model_nb_p_label(depth_time_p))) %>%
  left_join(main_clust_full_model_nb_taxon_sample_sizes, by = "taxon") %>%
  mutate(label = paste0(label,
                        "\nPresent tows: ", n_tows_present,
                        "; individuals: ", n_individuals)) %>%
  left_join(main_clust_full_model_nb_effects %>%
              group_by(taxon) %>%
              summarise(label_x = max(upr, na.rm = TRUE),
                        label_y = min(depth_mean_m, na.rm = TRUE) +
                          0.5 * diff(range(depth_mean_m, na.rm = TRUE)),
                        .groups = "drop"),
            by = "taxon")

main_clust_full_model_nb_effect_plot <- ggplot(main_clust_full_model_nb_effects,
                                    aes(x = fit, y = depth_mean_m, color = time_of_day, fill = time_of_day)) +
  geom_ribbon(aes(xmin = lwr, xmax = upr), alpha = 0.2, color = NA, orientation = "y") +
  geom_line(linewidth = .5) +
  geom_text(data = main_clust_full_model_nb_effect_tests,
            aes(x = label_x, y = label_y, label = label),
            inherit.aes = FALSE, hjust = 1, vjust = 0, size = 2.3,
            lineheight = 0.95) +
  scale_y_reverse() +
  facet_wrap(~ taxon, scales = "free_x") +
  labs(x = expression("Predicted larvae " ~ m^{-3}),
       y = "Mean tow depth (m)",
       color = "Time of day",
       fill = "Time of day") +
  theme_classic()

ggsave(here("output/main_clust_full_model_nb_day_night_depth_effects.png"),
       plot = main_clust_full_model_nb_effect_plot,
       width = 10,
       height = 6,
       dpi = 300)


# Scatterplot of only 4 species of interest
sp_of_interest <- main_clust_model_data %>%
  filter(taxon %in% c("Sebastes_spp", "Parophrys_vetulus", "Stenobrachius_leucopsarus", "Ammodytidae"))
species_colors_sub <- species_colors[names(species_colors) %in% unique(sp_of_interest$taxon)]
taxon_labels_sub   <- taxon_labels[names(taxon_labels) %in% unique(sp_of_interest$taxon)]

ggplot(sp_of_interest, aes(x = depth_mean_m, y = log(individuals_per_m3), color = taxon, fill = taxon)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.25) +
  facet_wrap(~ time_of_day, nrow = 2) +
  scale_color_manual(values = species_colors_sub, labels = parse(text = taxon_labels_sub)) +
  scale_fill_manual(values = species_colors_sub, labels =  parse(text = taxon_labels_sub)) + 
  guides(fill  = "none", color = guide_legend(override.aes = list(fill = alpha(species_colors_sub, 0.25)))) +
  labs(title = "Day-night comparison of taxa concentrations by mean depths",
       x = "Mean tow depth (m)", y = "Concentration (log(ind./m3)", color = "Taxon", fill = "Taxon") +
  theme_classic()
ggsave("main_clust_D_N_scatter_sp_of_interest.png", plot = get_last_plot(), path = here("output"),
       width = 7, height = 5, units = "in", dpi = 300)

# Linear regression on specific taxa --------------------------------------
# Sebastes
main_clust_seb_df <- main_clust_model_data %>%
  filter(taxon == "Sebastes_spp")
main_clust_seb_lm <- glmmTMB(individuals_in_tow ~ time_of_day * depth_mean_scaled + seafloor_depth_scaled +
                    mean_temperature_scaled + mean_salinity_scaled + dissolved_oxygen_scaled + mean_chl_0_100_m_scaled +
                    offset(log(volume_best_m3_both_sides)),
                  family = nbinom2,
                  data = main_clust_seb_df)
summary(main_clust_seb_lm)
visreg(main_clust_seb_lm)
visreg(main_clust_seb_lm, "depth_mean_scaled", by = "time_of_day",
       ylab = "Sebastes individuals in tow", xlab = "scaled mean depth")
main_clust_res_seb <- simulateResiduals(main_clust_seb_lm, n = 1000)
plot(main_clust_res_seb)
testResiduals(main_clust_res_seb)
testDispersion(main_clust_res_seb)
testZeroInflation(main_clust_res_seb)

# P. vetulus
main_clust_p_vetulus_df <- main_clust_model_data %>%
  filter(taxon == "Parophrys_vetulus")
main_clust_p_vetulus_lm <- glmmTMB(individuals_in_tow ~ time_of_day * depth_mean_scaled + seafloor_depth_scaled +
                          mean_temperature_scaled + mean_salinity_scaled + dissolved_oxygen_scaled + mean_chl_0_100_m_scaled +
                          offset(log(volume_best_m3_both_sides)),
                        family = nbinom2,
                        data = main_clust_p_vetulus_df)
summary(main_clust_p_vetulus_lm)
visreg(main_clust_p_vetulus_lm)
visreg(main_clust_p_vetulus_lm, "depth_mean_scaled", by = "time_of_day",
       ylab = "P. vetulus individuals in tow", xlab = "scaled mean depth")
main_clust_res_p_vetulus <- simulateResiduals(main_clust_p_vetulus_lm, n = 1000)
plot(main_clust_res_p_vetulus)
testResiduals(main_clust_res_p_vetulus)
testDispersion(main_clust_res_p_vetulus)
testZeroInflation(main_clust_res_p_vetulus)

# S. leucopsarus 
main_clust_s_leucopsarus_df <- main_clust_model_data %>%
  filter(taxon == "Stenobrachius_leucopsarus")
main_clust_s_leucopsarus_lm <- glmmTMB(individuals_in_tow ~ time_of_day * depth_mean_scaled + seafloor_depth_scaled +
                              mean_temperature_scaled + mean_salinity_scaled + dissolved_oxygen_scaled + mean_chl_0_100_m_scaled +
                              offset(log(volume_best_m3_both_sides)),
                            family = nbinom2,
                            data = main_clust_s_leucopsarus_df)
summary(main_clust_s_leucopsarus_lm)
visreg(main_clust_s_leucopsarus_lm)
visreg(main_clust_s_leucopsarus_lm, "depth_mean_scaled", by = "time_of_day",
       ylab = "S. leucopsarus individuals in tow", xlab = "scaled mean depth")
main_clust_res_s_leucopsarus <- simulateResiduals(main_clust_s_leucopsarus_lm, n = 1000)
plot(main_clust_res_s_leucopsarus)
testResiduals(main_clust_res_s_leucopsarus)
testDispersion(main_clust_res_s_leucopsarus)
testZeroInflation(main_clust_res_s_leucopsarus)

# Ammodytidae
main_clust_ammodytidae_df <- main_clust_model_data %>%
  filter(taxon == "Ammodytidae")
main_clust_ammodytidae_lm <- glmmTMB(individuals_in_tow ~ time_of_day * depth_mean_scaled + seafloor_depth_scaled +
                           mean_temperature_scaled + mean_salinity_scaled + dissolved_oxygen_scaled + mean_chl_0_100_m_scaled +
                           offset(log(volume_best_m3_both_sides)),
                         family = nbinom2,
                         data = main_clust_ammodytidae_df)
summary(main_clust_ammodytidae_lm)
visreg(main_clust_ammodytidae_lm)
visreg(main_clust_ammodytidae_lm, "depth_mean_scaled", by = "time_of_day",
       ylab = "Ammodytidae individuals in tow", xlab = "scaled mean depth")
main_clust_res_ammodytidae <- simulateResiduals(main_clust_ammodytidae_lm, n = 1000)
plot(main_clust_res_ammodytidae)
testResiduals(main_clust_res_ammodytidae)
testDispersion(main_clust_res_ammodytidae)
testZeroInflation(main_clust_res_ammodytidae)
