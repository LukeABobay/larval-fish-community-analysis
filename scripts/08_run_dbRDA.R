# Description -------------------------------------------------------------

# Conduct distance-based redundancy analysis (db-RDA) to evaluate support for
# environmental variables as predictors of larval fish assemblage composition


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(vegan)
library(ggrepel)


# Source code -------------------------------------------------------------

source(here("scripts/02_prepare_community_data.R"))


# Create wide dataframe ---------------------------------------------------

dbRDA_major_taxa_wide <- wide_major_taxa_nets


# Choose db-RDA covariates ------------------------------------------------

spatiotemporal_covariates <- c("year", "solar_dayness_scaled",
                               "start_latitude_dd_scaled",
                               "depth_mean_m_scaled",
                               "seafloor_depth_m_scaled")

environmental_covariates <- c("mean_temperature_c_scaled",
                              "mean_salinity_psu_scaled",
                              "dissolved_oxygen_ml_l_scaled",
                              "mean_chl_0_100_m_mgm3_scaled")

dbRDA_covariates <- c(spatiotemporal_covariates, environmental_covariates)

dbRDA_metadata_cols <- AHC_metadata_cols
dbRDA_taxa_cols <- taxa_cols


# Prepare environmental data ---------------------------------------------

dbRDA_env <- dbRDA_major_taxa_wide %>%
  mutate(year = factor(year)) %>%
  mutate(total_concentration = rowSums(across(all_of(dbRDA_taxa_cols)))) %>%
  # Keep only complete cases in order to make the base and full models comparable
  # Actually, looks like we aren't losing any incomplete cases, but I'll keep this in place
  filter(total_concentration > 0) %>%
  drop_na(all_of(dbRDA_covariates))


# Create community matrix -------------------------------------------------

# Use square-root transformation, as in 03 script
dbRDA_comm_matrix <- dbRDA_env %>%
  select(all_of(dbRDA_taxa_cols)) %>%
  mutate(across(everything(), sqrt)) %>%
  as.data.frame()

row.names(dbRDA_comm_matrix) <- dbRDA_env$transect_station_rep_year_net

dbRDA_env_model <- dbRDA_env %>%
  select(transect_station_rep_year_net, all_of(dbRDA_covariates), shelf_position,
         collection_date, cruise, net) %>%
  mutate(year = factor(year)) %>%
  as.data.frame()

row.names(dbRDA_env_model) <- dbRDA_env_model$transect_station_rep_year_net


# Use NMDS cluster assignments from shared community prep -----------------

dbRDA_clusters <- clusters %>%
  mutate(cluster = factor(as.character(cluster), levels = cluster_levels))


# Fit db-RDA models -------------------------------------------------------

set.seed(123)

# Using Bray-Curtis dissimilarity because these are community
# composition data with many zeros, matches NMDS/cluster analyses
dbRDA_base_model <- capscale(dbRDA_comm_matrix ~ year + solar_dayness_scaled +
                               start_latitude_dd_scaled + depth_mean_m_scaled +
                               seafloor_depth_m_scaled,
                             data = dbRDA_env_model,
                             distance = "bray",
                             # add = "lingoes" applies a correction for negative
                             # eigenvalues that can arise with Bray-Curtis dissimilarity
                             add = "lingoes")

dbRDA_full_model <- capscale(dbRDA_comm_matrix ~ year + solar_dayness_scaled +
                               start_latitude_dd_scaled + depth_mean_m_scaled +
                               seafloor_depth_m_scaled + mean_temperature_c_scaled +
                               mean_salinity_psu_scaled + dissolved_oxygen_ml_l_scaled +
                               mean_chl_0_100_m_mgm3_scaled,
                             data = dbRDA_env_model,
                             distance = "bray",
                             add = "lingoes")

# This partial model tests environmental variables after conditioning on the
# spatiotemporal variables in the base model
dbRDA_env_partial_model <- capscale(dbRDA_comm_matrix ~ mean_temperature_c_scaled +
                                      mean_salinity_psu_scaled + dissolved_oxygen_ml_l_scaled +
                                      mean_chl_0_100_m_mgm3_scaled +
                                      Condition(year + solar_dayness_scaled +
                                                  start_latitude_dd_scaled +
                                                  depth_mean_m_scaled +
                                                  seafloor_depth_m_scaled),
                                    data = dbRDA_env_model,
                                    distance = "bray",
                                    add = "lingoes")

# Partition variation between spatiotemporal and environmental covariates
dbRDA_bray_dist <- vegdist(dbRDA_comm_matrix, method = "bray")

dbRDA_varpart <- varpart(dbRDA_bray_dist,
                         ~ year + solar_dayness_scaled + start_latitude_dd_scaled +
                           depth_mean_m_scaled + seafloor_depth_m_scaled,
                         ~ mean_temperature_c_scaled + mean_salinity_psu_scaled +
                           dissolved_oxygen_ml_l_scaled +
                           mean_chl_0_100_m_mgm3_scaled,
                         data = dbRDA_env_model,
                         add = "lingoes")

# Summary allocates shared fractions equally to each covariate set
dbRDA_varpart_summary <- summary(dbRDA_varpart)

# Testable unique fractions from the two-set variation partitioning
dbRDA_spatiotemporal_unique_model <- dbrda(dbRDA_bray_dist ~ year + solar_dayness_scaled +
                                             start_latitude_dd_scaled + depth_mean_m_scaled +
                                             seafloor_depth_m_scaled +
                                             Condition(mean_temperature_c_scaled + mean_salinity_psu_scaled +
                                                         dissolved_oxygen_ml_l_scaled +
                                                         mean_chl_0_100_m_mgm3_scaled),
                                           data = dbRDA_env_model,
                                           add = "lingoes")

dbRDA_environmental_unique_model <- dbrda(dbRDA_bray_dist ~ mean_temperature_c_scaled +
                                            mean_salinity_psu_scaled +
                                            dissolved_oxygen_ml_l_scaled +
                                            mean_chl_0_100_m_mgm3_scaled +
                                            Condition(year + solar_dayness_scaled +
                                                        start_latitude_dd_scaled +
                                                        depth_mean_m_scaled +
                                                        seafloor_depth_m_scaled),
                                          data = dbRDA_env_model,
                                          add = "lingoes")


# Evaluate model support --------------------------------------------------

# Can increase number of permutations once the model structure is final
dbRDA_base_overall_test <- anova(dbRDA_base_model, permutations = 999)
dbRDA_full_overall_test <- anova(dbRDA_full_model, permutations = 999)

# Evaluate whether adding environmental variables improves model support
# over spatiotemporal variables alone
dbRDA_base_vs_full_test <- anova(dbRDA_base_model, dbRDA_full_model,
                                 permutations = 999)
dbRDA_base_r2 <- RsquareAdj(dbRDA_base_model)
dbRDA_full_r2 <- RsquareAdj(dbRDA_full_model)

# Tests for individual terms in the full model
dbRDA_full_term_tests <- anova(dbRDA_full_model, by = "margin",
                               permutations = 999)

# Tests for individual environmental variables while conditioning on the base
# spatiotemporal model
dbRDA_env_partial_tests <- anova(dbRDA_env_partial_model, by = "margin",
                                 permutations = 999)

# Check whether any model terms are strongly collinear
dbRDA_full_vif <- vif.cca(dbRDA_full_model)

# Permutation tests for the unique fractions in the variation partitioning
dbRDA_spatiotemporal_unique_test <- anova(dbRDA_spatiotemporal_unique_model,
                                          permutations = 999)
dbRDA_environmental_unique_test <- anova(dbRDA_environmental_unique_model,
                                         permutations = 999)

# Save a simple variation partitioning diagram
png(filename = here("output/dbRDA_variance_partitioning.png"),
    width = 8,
    height = 8,
    units = "in",
    res = 300)
plot(dbRDA_varpart, bg = c("#A6CEE3", "#B2DF8A"), cutoff = 0,
     Xnames = c("Spatiotemporal", "Environmental"))
dev.off()


# Plot constrained ordination --------------------------------------------
dbRDA_site_scores <- scores(dbRDA_full_model, display = "sites", choices = 1:2) %>%
  as.data.frame() %>%
  rownames_to_column("transect_station_rep_year_net") %>%
  mutate(CAP1 = -CAP1,
         CAP2 = -CAP2) %>%
  left_join(dbRDA_env_model, by = "transect_station_rep_year_net") %>%
  left_join(dbRDA_clusters, by = "transect_station_rep_year_net")

dbRDA_hulls <- dbRDA_site_scores %>%
  group_by(cluster) %>%
  slice(chull(CAP1, CAP2)) %>%
  ungroup()

windows()
dbRDA_plot <- ggplot(dbRDA_site_scores, aes(x = CAP1, y = CAP2, color = cluster)) +
  geom_polygon(data = dbRDA_hulls,
               aes(x = CAP1, y = CAP2, fill = cluster, group = cluster),
               alpha = 0.25,
               color = NA,
               inherit.aes = FALSE) +
  geom_point(size = 1, alpha = 0.9) +
  scale_fill_manual(values = cluster_colors,
                    limits = cluster_levels,
                    breaks = cluster_levels,
                    drop = FALSE) +
  scale_color_manual(values = cluster_colors,
                     limits = cluster_levels,
                     breaks = cluster_levels,
                     drop = FALSE) +
  theme_classic() +
  labs(title = "db-RDA of larval fish assemblage composition",
       x = "CAP1", y = "CAP2", color = "Cluster", fill = "Cluster")
print(dbRDA_plot)
ggsave("dbRDA_cluster_ordination_no_overlays.png", plot = dbRDA_plot, path = here("output"),
       width = 7, height = 5, units = "in", dpi = 300)


dbRDA_vector_scores <- scores(dbRDA_full_model, display = "bp", choices = 1:2) %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  mutate(
    CAP1 = -CAP1,
    CAP2 = -CAP2,
    plot_label = recode(
      variable,
      "mean_temperature_c_scaled" = "Temperature",
      "mean_salinity_psu_scaled" = "Salinity",
      "dissolved_oxygen_ml_l_scaled" = "Oxygen",
      "mean_chl_0_100_m_mgm3_scaled" = "Chl a",
      "depth_mean_m_scaled" = "Mean depth",
      "seafloor_depth_m_scaled" = "Seafloor depth",
      "start_latitude_dd_scaled" = "Latitude",
      "solar_dayness_scaled" = "Time of day",
      "year2022" = "2022",
      "year2019" = "2019",
      "year2023" = "2023",
      .default = variable
    ),
    base_label_x = CAP1 + if_else(CAP1 >= 0, 0.14, -0.14),
    base_label_y = CAP2 + if_else(CAP2 >= 0, 0.10, -0.10),
    label_x = case_when(
      variable == "year2019" ~ 0.25,
      variable == "solar_dayness_scaled" ~ -0.15,
      variable == "dissolved_oxygen_ml_l_scaled" ~ -0.15,
      variable == "year2018" ~ 0.5,
      variable == "year2023" ~ -0.65,
      variable == "mean_chl_0_100_m_mgm3_scaled" ~ -0.55,
      TRUE ~ base_label_x
    ),
    label_y = case_when(
      variable == "year2019" ~ -1,
      variable == "solar_dayness_scaled" ~ 0.7,
      variable == "dissolved_oxygen_ml_l_scaled" ~ -0.64,
      variable == "year2018" ~ 0.1,
      variable == "year2023" ~ 0.05,
      variable == "mean_chl_0_100_m_mgm3_scaled" ~ -0.5,
      TRUE ~ base_label_y
    ),
    label_hjust = case_when(
      variable %in% c("year2019", "year2018") ~ 0,
      variable %in% c("dissolved_oxygen_ml_l_scaled", "solar_dayness_scaled", 
                      "year2023", "mean_chl_0_100_m_mgm3_scaled") ~ 1,
      CAP1 >= 0 ~ 0,
      TRUE ~ 1
    )
  )

windows()
dbRDA_overlays_plot <- ggplot(dbRDA_site_scores, aes(x = CAP1, y = CAP2, color = cluster)) +
  geom_polygon(data = dbRDA_hulls,
               aes(x = CAP1, y = CAP2, fill = cluster, group = cluster),
               alpha = 0.25,
               color = NA,
               inherit.aes = FALSE) +
  geom_point(size = 1, alpha = 0.9) +
  scale_fill_manual(values = cluster_colors,
                    limits = cluster_levels,
                    breaks = cluster_levels,
                    drop = FALSE) +
  scale_color_manual(values = cluster_colors,
                     limits = cluster_levels,
                     breaks = cluster_levels,
                     drop = FALSE) +
  geom_segment(data = dbRDA_vector_scores,
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.15, "cm")),
               color = "black", linewidth = 0.5) +
  geom_segment(data = dbRDA_vector_scores,
               aes(x = CAP1, y = CAP2, xend = label_x, yend = label_y),
               inherit.aes = FALSE,
               color = "grey35",
               linewidth = 0.25) +
  geom_text(data = dbRDA_vector_scores,
            aes(x = label_x, y = label_y, label = plot_label,
                hjust = label_hjust),
            inherit.aes = FALSE,
            color = "black",
            size = 2) +
  theme_classic() +
  labs(x = "CAP1", y = "CAP2", color = "Cluster", fill = "Cluster")
print(dbRDA_overlays_plot)

ggsave("dbRDA_cluster_ordination_with_overlays.png", plot = dbRDA_overlays_plot, path = here("output"),
       width = 7, height = 5, units = "in", dpi = 300)
