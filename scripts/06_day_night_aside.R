# Description -------------------------------------------------------------

#Aside analysis of depth stratified total abundances across replicates for
# each taxa against day/night and depth in 2019


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

source(here("scripts/01_data_wrangling.R"))


# Prepare data ------------------------------------------------------------

#Filter to keep only 2018-19 rows and nets 1-4
mocness_major_taxa_19 <- filter(mocness_major_taxa, collection_date > "2018-01-01" & collection_date < "2019-12-31",
                                net %in% 1:4) %>%
  #add time of day column
  mutate(time_of_day = substr(replicate, 3, 3)) %>%
  mutate(time_of_day = recode(time_of_day, "D" = "Day", "N" = "Night"))
  


# Classify taxa by habitat affinity and create color vectors ---------------
ordered_taxa_19 <- c(mesopelagic_species, flatfish_species, sculpin_relatives_species, other_species)

mocness_major_taxa_19 <- mocness_major_taxa_19 %>%
  #Reorder taxa
  mutate(taxon = factor(taxon, levels = ordered_taxa_19)) %>%
  #Reorder stations
  mutate(station = factor(station, levels = rev(sort(unique(station))))) %>%
  mutate(color_scheme_group = case_when(taxon %in% mesopelagic_species ~ "Mesopelagic",
                                            taxon %in% flatfish_species ~ "Flatfish",
                                            taxon %in% sculpin_relatives_species ~ "Sculpins_and_related",
                                            taxon %in% other_species ~ "Other",
                                            TRUE ~ "Other"))

taxa_to_keep_2019 <- mocness_major_taxa_19 %>%
  filter(!str_detect(transect_station_rep_year_net, "0$")) %>%
  group_by(taxon) %>%
  summarise(freq = n_distinct(transect_station_rep_year_net), .groups = "drop") %>%
  filter(freq >= 0.05 * n_distinct(
    mocness_major_taxa_19 %>%
      filter(!str_detect(transect_station_rep_year_net, "0$")) %>%
      pull(transect_station_rep_year_net)
  )) %>%
  pull(taxon)

model_data <- mocness_major_taxa_19 %>%
  select(collection_date, start_latitude_dd, start_longitude_dd, transect_station_rep_year_net,
         taxon, depth_mean_m, time_of_day, volume_best_m3_both_sides, individuals_in_tow,
         seafloor_depth_m, mean_temperature_c, mean_salinity_psu, dissolved_oxygen_ml_l, mean_chl_0_100_m_mgm3) %>%
  # Remove net 0 data for DVM vignette
  filter(!str_detect(transect_station_rep_year_net, "0$"),
         taxon %in% taxa_to_keep_2019) %>%
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


# Avg taxa concentrations across replicates ------------------------------------

avgd_mocness_major_taxa_19 <- mocness_major_taxa_19 %>%
  group_by(taxon, time_of_day, depth_range, depth_mean_m, depth_diff_m) %>%
  summarise(avg_taxa_concentration = mean(individuals_per_m3, na.rm=TRUE)) %>%
  ungroup()


# Plot taxa concentrations by depths and day/night ------------------------
#Barplot
ggplot(avgd_mocness_major_taxa_19, aes(x = depth_range, y = avg_taxa_concentration, fill = taxon)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  facet_wrap(~ time_of_day, nrow = 2) +
  labs(title = "Day-night comparison of taxa concentrations at depth ranges",
       x = "Depth sampled (m)", y = "average individuals per m3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Scatterplot
d_n_scatter_all <- ggplot(model_data, aes(x = depth_mean_m, y = log(individuals_per_m3), color = taxon)) +
  geom_point() +
  geom_smooth(method = "lm",se = FALSE) +
  facet_wrap(~ time_of_day, nrow = 2) +
  scale_color_manual(values = species_colors, labels = parse(text = taxon_labels)) +
  labs(title = "Day-night comparison of taxa concentrations by mean depths",
       x = "Mean tow depth (m)", y = "Concentration (log(ind./m3)", color = "Taxon") +
  theme_classic()
ggsave("D_N_scatter_all.png", plot = d_n_scatter_all, path = here("output"),
         width = 7, height = 5, units = "in", dpi = 300)

# Fit linear model(s) of taxa concentrations against depth and time of day -----
day_night_depth_model <- lm(avg_taxa_concentration ~ taxon*time_of_day + taxon*depth_range, data = avgd_mocness_major_taxa_19)
summary(day_night_depth_model)

day_night_mean_depth_model <- lm(avg_taxa_concentration ~ taxon*time_of_day + taxon*depth_mean_m, data = avgd_mocness_major_taxa_19)
summary(day_night_mean_depth_model)


# Model without allowing effects of depth and time of day to vary among taxa
simple_model_pois <- glm(individuals_in_tow ~ taxon + time_of_day + depth_mean_scaled + offset(log(volume_best_m3_both_sides)),
                    family = poisson,
                    data = model_data)
summary(simple_model_pois)
visreg(simple_model_pois)
res_pois <- simulateResiduals(simple_model_pois, n = 1000)
plot(res_pois)
testResiduals(res_pois)
testDispersion(res_pois)
testZeroInflation(res_pois)

# Full model with Poisson response distribution
full_model_pois <- glmmTMB(individuals_in_tow ~ taxon * time_of_day * depth_mean_scaled +
                           offset(log(volume_best_m3_both_sides)) +
                           (1 | transect_station_rep_year_net),
                         family = poisson,
                         data = model_data)
summary(full_model_pois)
visreg(full_model_pois)
res_pois <- simulateResiduals(full_model_pois, n = 1000)
plot(res_pois)
testResiduals(res_pois)
testDispersion(res_pois)
testZeroInflation(res_pois)

# Full model with negative binomial response distribution
full_model_nb <- glmmTMB(individuals_in_tow ~ taxon * time_of_day * depth_mean_scaled +
                           offset(log(volume_best_m3_both_sides)) +
                           (1 | transect_station_rep_year_net),
                         family = nbinom2,
                         data = model_data)
summary(full_model_nb)
visreg(full_model_nb)
res_nb_full <- simulateResiduals(full_model_nb, n = 1000)
plot(res_nb_full)
testResiduals(res_nb_full)
testDispersion(res_nb_full)
testZeroInflation(res_nb_full)

# Plot fixed-effect predictions from the full negative binomial model.
# Setting volume to 1 makes predictions interpretable as expected individuals per m3.
full_model_nb_effects <- expand_grid(taxon = factor(levels(model_data$taxon), levels = levels(model_data$taxon)),
                                     time_of_day = factor(c("Day", "Night"), levels = c("Day", "Night")),
                                     depth_mean_m = seq(min(model_data$depth_mean_m, na.rm = TRUE),
                                                        max(model_data$depth_mean_m, na.rm = TRUE),
                                                        length.out = 100)) %>%
  mutate(depth_mean_scaled = (depth_mean_m - mean(model_data$depth_mean_m, na.rm = TRUE)) /
           sd(model_data$depth_mean_m, na.rm = TRUE),
         volume_best_m3_both_sides = 1,
         transect_station_rep_year_net = first(model_data$transect_station_rep_year_net))

full_model_nb_effect_predictions <- predict(full_model_nb,
                                            newdata = full_model_nb_effects,
                                            type = "link",
                                            se.fit = TRUE,
                                            re.form = NA)

full_model_nb_effects <- full_model_nb_effects %>%
  mutate(fit = exp(full_model_nb_effect_predictions$fit),
         lwr = exp(full_model_nb_effect_predictions$fit - 1.96 * full_model_nb_effect_predictions$se.fit),
         upr = exp(full_model_nb_effect_predictions$fit + 1.96 * full_model_nb_effect_predictions$se.fit))

full_model_nb_taxon_sample_sizes <- model_data %>%
  group_by(taxon) %>%
  summarise(n_tows_present = sum(individuals_in_tow > 0, na.rm = TRUE),
            n_individuals = sum(individuals_in_tow, na.rm = TRUE),
            .groups = "drop")

full_model_nb_fixed_effects <- fixef(full_model_nb)$cond
full_model_nb_vcov <- vcov(full_model_nb)$cond
full_model_nb_terms <- terms(~ taxon * time_of_day * depth_mean_scaled)

full_model_nb_wald_test <- function(rows_positive, rows_negative = NULL) {
  positive_matrix <- model.matrix(full_model_nb_terms,
                                  data = rows_positive,
                                  xlev = list(taxon = levels(full_model_nb_effects$taxon),
                                              time_of_day = levels(full_model_nb_effects$time_of_day)))
  negative_matrix <- if (is.null(rows_negative)) {
    positive_matrix * 0
  } else {
    model.matrix(full_model_nb_terms,
                 data = rows_negative,
                 xlev = list(taxon = levels(full_model_nb_effects$taxon),
                             time_of_day = levels(full_model_nb_effects$time_of_day)))
  }
  contrast <- colSums(positive_matrix - negative_matrix)
  contrast_aligned <- setNames(rep(0, length(full_model_nb_fixed_effects)),
                               names(full_model_nb_fixed_effects))
  contrast_aligned[names(contrast)] <- contrast
  contrast <- contrast_aligned
  estimate <- sum(contrast * full_model_nb_fixed_effects)
  se <- sqrt(drop(t(contrast) %*% full_model_nb_vcov %*% contrast))
  z <- estimate / se
  2 * pnorm(abs(z), lower.tail = FALSE)
}

full_model_nb_p_label <- function(p) {
  case_when(
    is.na(p) ~ "NA",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ ".",
    TRUE ~ "ns"
  )
}

full_model_nb_effect_tests <- map_dfr(levels(full_model_nb_effects$taxon), function(current_taxon) {
  rows <- tibble(taxon = factor(current_taxon, levels = levels(full_model_nb_effects$taxon)),
                 time_of_day = factor(c("Day", "Day", "Night", "Night"),
                                      levels = levels(full_model_nb_effects$time_of_day)),
                 depth_mean_scaled = c(1, 0, 1, 0))
  
  depth_time_p <- full_model_nb_wald_test(rows[c(3, 2), ], rows[c(4, 1), ])
  
  tibble(taxon = factor(current_taxon, levels = levels(model_data$taxon)),
         depth_time_p = depth_time_p)
}) %>%
  mutate(label = paste0("Day-night slope difference: ", full_model_nb_p_label(depth_time_p))) %>%
  left_join(full_model_nb_taxon_sample_sizes, by = "taxon") %>%
  mutate(label = paste0(label,
                        "\nPresent tows: ", n_tows_present,
                        "; individuals: ", n_individuals)) %>%
  left_join(full_model_nb_effects %>%
              group_by(taxon) %>%
              summarise(label_x = max(upr, na.rm = TRUE),
                        label_y = min(depth_mean_m, na.rm = TRUE) +
                          0.5 * diff(range(depth_mean_m, na.rm = TRUE)),
                        .groups = "drop"),
            by = "taxon")

full_model_nb_effect_plot <- ggplot(full_model_nb_effects,
                                    aes(x = fit, y = depth_mean_m, color = time_of_day, fill = time_of_day)) +
  geom_ribbon(aes(xmin = lwr, xmax = upr), alpha = 0.2, color = NA, orientation = "y") +
  geom_line(linewidth = .5) +
  geom_text(data = full_model_nb_effect_tests,
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

ggsave(here("output/full_model_nb_day_night_depth_effects.png"),
       plot = full_model_nb_effect_plot,
       width = 10,
       height = 6,
       dpi = 300)


#Scatterplot of only 4 species of interest
sp_of_interest <- model_data %>%
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
ggsave("D_N_scatter_sp_of_interest.png", plot = get_last_plot(), path = here("output"),
       width = 7, height = 5, units = "in", dpi = 300)

# Linear regression on specific taxa --------------------------------------

#Cluster 1: Sebastes
seb_df <- model_data %>%
  filter(taxon == "Sebastes_spp")
seb_lm <- glmmTMB(individuals_in_tow ~ time_of_day * depth_mean_scaled + seafloor_depth_scaled +
                    mean_temperature_scaled + mean_salinity_scaled + dissolved_oxygen_scaled + mean_chl_0_100_m_scaled +
                    offset(log(volume_best_m3_both_sides)),
                  family = nbinom2,
                  data = seb_df)
summary(seb_lm)
visreg(seb_lm)
visreg(seb_lm, "depth_mean_scaled", by = "time_of_day",
       ylab = "Sebastes individuals in tow", xlab = "scaled mean depth")
res_seb <- simulateResiduals(seb_lm, n = 1000)
plot(res_seb)
testResiduals(res_seb)
testDispersion(res_seb)
testZeroInflation(res_seb)

#Cluster 2: P. vetulus
p_vetulus_df <- model_data %>%
  filter(taxon == "Parophrys_vetulus")
p_vetulus_lm <- glmmTMB(individuals_in_tow ~ time_of_day * depth_mean_scaled + seafloor_depth_scaled +
                          mean_temperature_scaled + mean_salinity_scaled + dissolved_oxygen_scaled + mean_chl_0_100_m_scaled +
                          offset(log(volume_best_m3_both_sides)),
                        family = nbinom2,
                        data = p_vetulus_df)
summary(p_vetulus_lm)
visreg(p_vetulus_lm)
visreg(p_vetulus_lm, "depth_mean_scaled", by = "time_of_day",
       ylab = "P. vetulus individuals in tow", xlab = "scaled mean depth")
res_p_vetulus <- simulateResiduals(p_vetulus_lm, n = 1000)
plot(res_p_vetulus)
testResiduals(res_p_vetulus)
testDispersion(res_p_vetulus)
testZeroInflation(res_p_vetulus)

#Cluster 4: S. leucopsarus 
s_leucopsarus_df <- model_data %>%
  filter(taxon == "Stenobrachius_leucopsarus")
s_leucopsarus_lm <- glmmTMB(individuals_in_tow ~ time_of_day * depth_mean_scaled + seafloor_depth_scaled +
                              mean_temperature_scaled + mean_salinity_scaled + dissolved_oxygen_scaled + mean_chl_0_100_m_scaled +
                              offset(log(volume_best_m3_both_sides)),
                            family = nbinom2,
                            data = s_leucopsarus_df)
summary(s_leucopsarus_lm)
visreg(s_leucopsarus_lm)
visreg(s_leucopsarus_lm, "depth_mean_scaled", by = "time_of_day",
       ylab = "S. leucopsarus individuals in tow", xlab = "scaled mean depth")
res_s_leucopsarus <- simulateResiduals(s_leucopsarus_lm, n = 1000)
plot(res_s_leucopsarus)
testResiduals(res_s_leucopsarus)
testDispersion(res_s_leucopsarus)
testZeroInflation(res_s_leucopsarus)

#Cluster 6: Ammodytidae
ammodytidae_df <- model_data %>%
  filter(taxon == "Ammodytidae")
ammodytidae_lm <- glmmTMB(individuals_in_tow ~ time_of_day * depth_mean_scaled + seafloor_depth_scaled +
                           mean_temperature_scaled + mean_salinity_scaled + dissolved_oxygen_scaled + mean_chl_0_100_m_scaled +
                           offset(log(volume_best_m3_both_sides)),
                         family = nbinom2,
                         data = ammodytidae_df)
summary(ammodytidae_lm)
visreg(ammodytidae_lm)
visreg(ammodytidae_lm, "depth_mean_scaled", by = "time_of_day",
       ylab = "Ammodytidae individuals in tow", xlab = "scaled mean depth")
res_ammodytidae <- simulateResiduals(ammodytidae_lm, n = 1000)
plot(res_ammodytidae)
testResiduals(res_ammodytidae)
testDispersion(res_ammodytidae)
testZeroInflation(res_ammodytidae)

