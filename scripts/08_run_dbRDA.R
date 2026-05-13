# Description -------------------------------------------------------------

# Conduct distance-based redundancy analysis (db-RDA) to evaluate support for
# environmental variables as predictors of larval fish assemblage composition


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(vegan)
library(ggrepel)
library(suncalc)


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Create wide dataframe ---------------------------------------------------

# Copied from 03 script
dbRDA_major_taxa_wide <- mocness_major_taxa_nets %>%
  # Removing NAs for now, but there shouldn't be any to begin with
  filter(!is.na(individuals_in_tow)) %>%
  filter(!is.na(individuals_per_m3)) %>%
  select(project, year, cruise, collection_date, transect, replicate, station, net,
         transect_station_rep_year_net, transect_station_rep_year, start_time_pt,
         start_longitude_dd, start_latitude_dd, maximum_depth_m, minimum_depth_m,
         depth_mean_m, depth_diff_m, volume_best_m3_both_sides,
         mean_temperature_c, mean_salinity_psu, mean_density_kgm3, seafloor_depth_m,
         distance_to_shore_km, shelf_position, prey_zooplankton_abundance_ind_m3,
         dissolved_oxygen_ml_l, mean_chl_0_100_m_mgm3, mlotst,
         taxon, individuals_per_m3) %>%
  # For some reason, MOC 1 and MOC 4 have different values of mean_temperature_c,
  # mean_salinity_psu, and mean_density_kgm3 in 6 cases. To eliminate differences,
  # calculate mean, as in scripts 03 and 07.
  group_by(transect_station_rep_year_net) %>%
  mutate(mean_temperature_c = mean(mean_temperature_c),
         mean_salinity_psu = mean(mean_salinity_psu),
         mean_density_kgm3 = mean(mean_density_kgm3)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_per_m3, values_fill = 0)


# Choose db-RDA covariates ------------------------------------------------

spatiotemporal_covariates <- c("year", "time_of_day", "start_latitude_dd",
                               "depth_mean_m", "seafloor_depth_m")

environmental_covariates <- c("mean_temperature_c", "mean_salinity_psu",
                              "dissolved_oxygen_ml_l", "mean_chl_0_100_m_mgm3")

dbRDA_covariates <- c(spatiotemporal_covariates, environmental_covariates)

dbRDA_metadata_cols <- c("project", "year", "cruise", "collection_date", "transect",
                         "replicate", "station", "net", "transect_station_rep_year_net",
                         "transect_station_rep_year", "start_time_pt",
                         "start_longitude_dd", "start_latitude_dd",
                         "maximum_depth_m", "minimum_depth_m", "depth_mean_m",
                         "depth_diff_m", "volume_best_m3_both_sides",
                         "mean_temperature_c", "mean_salinity_psu", "mean_density_kgm3",
                         "seafloor_depth_m", "distance_to_shore_km", "shelf_position",
                         "prey_zooplankton_abundance_ind_m3", "dissolved_oxygen_ml_l",
                         "mean_chl_0_100_m_mgm3", "mlotst")

dbRDA_taxa_cols <- names(dbRDA_major_taxa_wide) %>%
  setdiff(dbRDA_metadata_cols)


# Prepare environmental data ---------------------------------------------

dbRDA_env <- dbRDA_major_taxa_wide %>%
  mutate(time_of_day = substr(replicate, 3, 3),
         time_of_day = recode(time_of_day, "D" = "Day", "N" = "Night", .default = NA_character_)) %>%
  group_by(transect_station_rep_year_net) %>%
  # Fill missing day/night labels using sunrise/sunset, as in script 03, 
  # to avoid dropping samples where replicate labels do not indicate day/night
  mutate(sunrise = getSunlightTimes(date = as.Date(collection_date),
                                    lat  = first(start_latitude_dd),
                                    lon  = first(start_longitude_dd),
                                    keep = c("sunrise", "sunset"))$sunrise,
         sunset  = getSunlightTimes(
           date = as.Date(collection_date),
           lat  = first(start_latitude_dd),
           lon  = first(start_longitude_dd),
           keep = c("sunrise", "sunset"))$sunset,
         time_of_day = case_when(!is.na(time_of_day) ~ time_of_day,
                                 start_time_pt >= sunrise & start_time_pt < sunset ~ "Day",
                                 TRUE ~ "Night"),
         year = factor(year),
         time_of_day = factor(time_of_day, levels = c("Day", "Night"))) %>%
  ungroup() %>%
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
  as.data.frame()

row.names(dbRDA_env_model) <- dbRDA_env_model$transect_station_rep_year_net


# Recreate NMDS cluster assignments from script 03 ------------------------

dbRDA_cluster_matrix <- dbRDA_major_taxa_wide %>%
  select(transect_station_rep_year_net, depth_mean_m, all_of(dbRDA_taxa_cols))

dbRDA_cluster_taxa <- dbRDA_cluster_matrix %>%
  select(all_of(dbRDA_taxa_cols)) %>%
  mutate(across(everything(), sqrt))

row.names(dbRDA_cluster_taxa) <- dbRDA_cluster_matrix$transect_station_rep_year_net

dbRDA_cluster_dissimilarity <- vegdist(dbRDA_cluster_taxa, method = "bray")
dbRDA_cluster_result <- hclust(dbRDA_cluster_dissimilarity, method = "average")

cluster_levels <- paste("Cluster", 1:10)
cluster_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                    "#D55E00", "#CC79A7", "#8A2BE2", "#00CED1", "#FF1493")
names(cluster_colors) <- cluster_levels

dbRDA_clusters <- tibble(
  transect_station_rep_year_net = names(cutree(dbRDA_cluster_result, k = 10)),
  cluster = cutree(dbRDA_cluster_result, k = 10)
) %>%
  mutate(cluster = factor(cluster, levels = 1:10, labels = cluster_levels))


# Fit db-RDA models -------------------------------------------------------

set.seed(123)

# Using Bray-Curtis dissimilarity because these are community
# composition data with many zeros, matches NMDS/cluster analyses
dbRDA_base_model <- capscale(dbRDA_comm_matrix ~ year + time_of_day +
                               start_latitude_dd + depth_mean_m +
                               seafloor_depth_m,
                             data = dbRDA_env_model,
                             distance = "bray",
                             # add = "lingoes" applies a correction for negative
                             # eigenvalues that can arise with Bray-Curtis dissimilarity
                             add = "lingoes")

dbRDA_full_model <- capscale(dbRDA_comm_matrix ~ year + time_of_day +
                               start_latitude_dd + depth_mean_m +
                               seafloor_depth_m + mean_temperature_c +
                               mean_salinity_psu + dissolved_oxygen_ml_l +
                               mean_chl_0_100_m_mgm3,
                             data = dbRDA_env_model,
                             distance = "bray",
                             add = "lingoes")

# This partial model tests environmental variables after conditioning on the
# spatiotemporal variables in the base model
dbRDA_env_partial_model <- capscale(dbRDA_comm_matrix ~ mean_temperature_c +
                                      mean_salinity_psu + dissolved_oxygen_ml_l +
                                      mean_chl_0_100_m_mgm3 +
                                      Condition(year + time_of_day +
                                                  start_latitude_dd +
                                                  depth_mean_m +
                                                  seafloor_depth_m),
                                    data = dbRDA_env_model,
                                    distance = "bray",
                                    add = "lingoes")


# Evaluate model support --------------------------------------------------

# Can increase number of permutations once the model structure is final
dbRDA_base_overall_test <- anova(dbRDA_base_model, permutations = 999)
dbRDA_full_overall_test <- anova(dbRDA_full_model, permutations = 999)

# Evaluate whether adding environmental variables improves model support
# over spatiotemporal variables alone
dbRDA_base_vs_full_test <- anova(dbRDA_base_model, dbRDA_full_model,
                                 permutations = 999)
RsquareAdj(dbRDA_base_model)
RsquareAdj(dbRDA_full_model)

# Tests for individual terms in the full model
dbRDA_full_term_tests <- anova(dbRDA_full_model, by = "margin",
                               permutations = 999)

# Tests for individual environmental variables while conditioning on the base
# spatiotemporal model
dbRDA_env_partial_tests <- anova(dbRDA_env_partial_model, by = "margin",
                                 permutations = 999)

# Check whether any model terms are strongly collinear
dbRDA_full_vif <- vif.cca(dbRDA_full_model)


# Plot constrained ordination --------------------------------------------

dbRDA_site_scores <- scores(dbRDA_full_model, display = "sites", choices = 1:2) %>%
  as.data.frame() %>%
  rownames_to_column("transect_station_rep_year_net") %>%
  left_join(dbRDA_env_model, by = "transect_station_rep_year_net") %>%
  left_join(dbRDA_clusters, by = "transect_station_rep_year_net")

dbRDA_hulls <- dbRDA_site_scores %>%
  group_by(cluster) %>%
  filter(n() >= 3) %>%
  slice(chull(CAP1, CAP2)) %>%
  ungroup()

dbRDA_vector_scores <- scores(dbRDA_full_model, display = "bp", choices = 1:2) %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  mutate(
    plot_label = recode(
      variable,
      "mean_temperature_c" = "Temperature",
      "mean_salinity_psu" = "Salinity",
      "dissolved_oxygen_ml_l" = "Oxygen",
      "mean_chl_0_100_m_mgm3" = "Chl a (0-100 m)",
      "depth_mean_m" = "Mean depth",
      "seafloor_depth_m" = "Seafloor depth",
      "start_latitude_dd" = "Latitude",
      "time_of_dayNight" = "Night vs day",
      "year2018" = "Year 2018",
      "year2019" = "Year 2019",
      "year2023" = "Year 2023",
      .default = variable
    ),
    base_label_x = CAP1 + if_else(CAP1 >= 0, 0.14, -0.14),
    base_label_y = CAP2 + if_else(CAP2 >= 0, 0.10, -0.10),
    label_x = case_when(
      variable == "year2019" ~ -0.25,
      variable == "time_of_dayNight" ~ -0.18,
      variable == "dissolved_oxygen_ml_l" ~ 0.15,
      variable == "year2018" ~ -0.30,
      variable == "year2023" ~ 0.32,
      variable == "mean_chl_0_100_m_mgm3" ~ 0.62,
      TRUE ~ base_label_x
    ),
    label_y = case_when(
      variable == "year2019" ~ 0.72,
      variable == "time_of_dayNight" ~ 0.45,
      variable == "dissolved_oxygen_ml_l" ~ 0.64,
      variable == "year2018" ~ -0.16,
      variable == "year2023" ~ 0.16,
      variable == "mean_chl_0_100_m_mgm3" ~ 0.06,
      TRUE ~ base_label_y
    ),
    label_hjust = case_when(
      variable %in% c("year2019", "time_of_dayNight", "year2018") ~ 1,
      variable %in% c("dissolved_oxygen_ml_l", "year2023",
                      "mean_chl_0_100_m_mgm3") ~ 0,
      CAP1 >= 0 ~ 0,
      TRUE ~ 1
    )
  )

windows()
dbRDA_plot <- ggplot(dbRDA_site_scores, aes(x = CAP1, y = CAP2, color = cluster)) +
  geom_polygon(data = dbRDA_hulls,
               aes(x = CAP1, y = CAP2, fill = cluster, group = cluster),
               alpha = 0.2,
               color = NA,
               inherit.aes = FALSE) +
  geom_point(size = 3, alpha = 0.9) +
  stat_ellipse(data = dbRDA_site_scores,
               aes(x = CAP1, y = CAP2, linetype = time_of_day),
               color = "grey20",
               linewidth = 0.9,
               type = "norm",
               level = 0.68,
               show.legend = TRUE,
               inherit.aes = FALSE) +
  scale_fill_manual(values = cluster_colors, guide = "none") +
  scale_color_manual(values = cluster_colors) +
  scale_linetype_manual(values = c("Day" = "solid", "Night" = "dashed")) +
  geom_segment(data = dbRDA_vector_scores,
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", linewidth = 0.85) +
  geom_segment(data = dbRDA_vector_scores,
               aes(x = CAP1, y = CAP2, xend = label_x, yend = label_y),
               inherit.aes = FALSE,
               color = "grey35",
               linewidth = 0.4) +
  geom_label(data = dbRDA_vector_scores,
             aes(x = label_x, y = label_y, label = plot_label,
                 hjust = label_hjust),
             inherit.aes = FALSE,
             color = "black",
             fill = "white",
             size = 3,
             linewidth = 0.2,
             label.padding = unit(0.12, "lines")) +
  theme_classic() +
  labs(title = "db-RDA of larval fish assemblage composition",
       x = "CAP1", y = "CAP2")

print(dbRDA_plot)

ggsave("dbRDA_cluster_ordination.png", plot = dbRDA_plot, path = here("output"),
       width = 12, height = 9, units = "in", dpi = 300)
