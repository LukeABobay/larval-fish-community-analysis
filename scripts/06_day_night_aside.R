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

#Filter to keep only 2019 rows
mocness_major_taxa_19 <- filter(mocness_major_taxa, collection_date > "2019-01-01" & collection_date < "2019-12-31") %>%
  #add time of day column
  mutate(time_of_day = substr(replicate, 3, 3)) %>%
  mutate(time_of_day = recode(time_of_day, "D" = "Day", "N" = "Night"))


# Classify taxa by habitat affinity and create color vectors ---------------
# Categorize taxa by habitat affinity
nearshore_species <- c("Ammodytidae", "Cottidae", "Gadidae", "Glyptocephalus_zachirus", "Hemilepidotus_spp", 
                       "Hexagrammidae", "Psychrolutidae", "Scorpaenichthys_marmoratus")
nearshore_colors <- colorRampPalette(brewer.pal(9, "Greens")[2:9])(length(nearshore_species))

coastal_species <- c("Agonidae", "Cyclopsettidae", "Isopsetta_isolepis", "Liparis_spp", "Lyopsetta_exilis", "Osmeridae", 
                     "Parophrys_vetulus", "Psettichthys_melanostictus", "Sebastes_spp")
coastal_colors <- colorRampPalette(brewer.pal(10, "Blues")[1:9])(length(coastal_species))

oceanic_species <- c("Bathylagus_ochotensis", "Lestidiops_ringens", "Protomyctophum_spp", "Stenobrachius_leucopsarus", 
                     "Tarletonbeania_crenularis")
oceanic_colors <- colorRampPalette(brewer.pal(5, "Purples")[2:6])(length(oceanic_species))

# Named species color vector
species_colors <- c(setNames(nearshore_colors, nearshore_species),
                    setNames(coastal_colors, coastal_species),
                    setNames(oceanic_colors, oceanic_species))

# Vector of taxa ordered alphabetically within categories to order bars and figure legends
ordered_taxa_19 <- c(nearshore_species, coastal_species, oceanic_species)

mocness_major_taxa_19 <- mocness_major_taxa_19 %>%
  #Reorder taxa
  mutate(taxon = factor(taxon, levels = ordered_taxa_19)) %>%
  #Reorder stations
  mutate(station = factor(station, levels = rev(sort(unique(station))))) %>%
  mutate(adult_habitat_affinity = case_when(taxon %in% nearshore_species ~ "Nearshore",
                                            taxon %in% coastal_species ~ "Coastal",
                                            taxon %in% oceanic_species ~ "Oceanic",
                                            TRUE ~ "Other"))

model_data <- mocness_major_taxa_19 %>%
  select(collection_date, start_latitude_dd, start_longitude_dd, transect_station_rep_year_net,
         taxon, depth_mean_m, time_of_day, volume_best_m3_both_sides, individuals_in_tow) %>%
  complete(nesting(collection_date, start_latitude_dd, start_longitude_dd,
                   transect_station_rep_year_net, depth_mean_m, time_of_day, volume_best_m3_both_sides),
           taxon, fill = list(individuals_in_tow = 0)) %>%
  mutate(time_of_day = factor(time_of_day, levels = c("Day", "Night")),
         depth_mean_scaled = scale(depth_mean_m)[, 1])

depth_mean_center <- mean(model_data$depth_mean_m, na.rm = TRUE)
depth_mean_scale <- sd(model_data$depth_mean_m, na.rm = TRUE)


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
ggplot(avgd_mocness_major_taxa_19, aes(x = depth_mean_m, y = log(avg_taxa_concentration), color = taxon)) +
  geom_point() +
  geom_smooth(method = "lm",se = FALSE) +
  facet_wrap(~ time_of_day, nrow = 2) +
  labs(title = "Day-night comparison of taxa concentrations by mean depths",
       x = "Mean depth (m)", y = "log(average individuals per m3)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
full_model_nb_effects <- expand_grid(taxon = factor(ordered_taxa_19, levels = ordered_taxa_19),
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
  rows <- tibble(
    taxon = factor(current_taxon, levels = levels(full_model_nb_effects$taxon)),
    time_of_day = factor(c("Day", "Day", "Night", "Night"),
                         levels = levels(full_model_nb_effects$time_of_day)),
    depth_mean_scaled = c(1, 0, 1, 0)
  )
  
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

windows()
ggplot(full_model_nb_effects,
       aes(x = fit, y = depth_mean_m, color = time_of_day, fill = time_of_day)) +
  geom_ribbon(aes(xmin = lwr, xmax = upr), alpha = 0.2, color = NA, orientation = "y") +
  geom_line(linewidth = 1) +
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


#Scatterplot of only 4 species of interest
ggplot(avgd_mocness_major_taxa_19 %>%
         filter(taxon %in% c("Sebastes_spp", "Parophrys_vetulus", "Stenobrachius_leucopsarus", "Isopsetta_isolepis")),
       aes(x = depth_mean_m, y = log(avg_taxa_concentration), color = taxon)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ time_of_day, nrow = 2) +
  labs(title = "Day-night comparison of taxa concentrations by mean depths",
       x = "Mean depth (m)", y = "log(average individuals per m3)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Linear regression on specific taxa --------------------------------------

#Cluster 1: Sebastes
seb_df <- mocness_major_taxa_19 %>%
  filter(taxon == "Sebastes_spp")
seb_lm <- lm(log(individuals_per_m3) ~ depth_mean_m + time_of_day + depth_mean_m:time_of_day, 
             data = seb_df)
summary(seb_lm)
visreg(seb_lm, "depth_mean_m", by = "time_of_day", ylab = "log(Sebastes individuals per m3)", xlab = "mean depth (m)")

#Cluster 2: P. vetulus
p_vetulus_df <- mocness_major_taxa_19 %>%
  filter(taxon == "Parophrys_vetulus")
p_vetulus_lm <- lm(log(individuals_per_m3) ~ depth_mean_m + time_of_day + depth_mean_m:time_of_day, 
             data = p_vetulus_df)
summary(p_vetulus_lm)
visreg(p_vetulus_lm, "depth_mean_m", by = "time_of_day", ylab = "log(P. vetulus individuals per m3)", xlab = "mean depth (m)")

#Cluster 3: S. leucopsarus 
s_leucopsarus_df <- mocness_major_taxa_19 %>%
  filter(taxon == "Stenobrachius_leucopsarus")
s_leucopsarus_lm <- lm(log(individuals_per_m3) ~ depth_mean_m + time_of_day + depth_mean_m:time_of_day, 
                   data = s_leucopsarus_df)
summary(s_leucopsarus_lm)
visreg(s_leucopsarus_lm, "depth_mean_m", by = "time_of_day", ylab = "log(S. leucopsarus individuals per m3)", xlab = "mean depth (m)")

#Cluster 5: I. isolepis
i_isolepis_df <- mocness_major_taxa_19 %>%
  filter(taxon == "Isopsetta_isolepis")
i_isolepis_lm <- lm(log(individuals_per_m3) ~ depth_mean_m + time_of_day + depth_mean_m:time_of_day, 
                       data = i_isolepis_df)
summary(i_isolepis_lm)
visreg(i_isolepis_lm, "depth_mean_m", by = "time_of_day", ylab = "log(I. isolepis individuals per m3)", xlab = "mean depth (m)")
