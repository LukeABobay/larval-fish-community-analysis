# Description -------------------------------------------------------------

#Run objective 2 analyses and plots with only 4 main clusters

# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(ggrepel)
library(vegan)
library(suncalc)


# Source code -------------------------------------------------------------

source(here("scripts/03_run_cluster_and_NMDS.R"))


# Filter data frames to main clusters only --------------------------------------------

main_clust_samples <- clusters %>% filter(cluster %in% main_clusters)

main_clust_wide_major_taxa_nets <- wide_major_taxa_nets %>% 
  semi_join(main_clust_samples, by = "transect_station_rep_year_net")

main_clust_env_wide <- main_clust_wide_major_taxa_nets %>%
  mutate(time_of_day = substr(replicate, 3, 3),
         time_of_day = recode(time_of_day, "D" = "Day", "N" = "Night", .default = NA_character_)) %>%
  group_by(transect_station_rep_year_net) %>%   # or collection_date, or station, etc.
  mutate(
    # compute sunrise/sunset at that station/date
    sunrise = getSunlightTimes(date = as.Date(collection_date),
                               lat  = first(start_latitude_dd),
                               lon  = first(start_longitude_dd),
                               keep = c("sunrise", "sunset"))$sunrise,
    sunset  = getSunlightTimes(date = as.Date(collection_date),
                               lat  = first(start_latitude_dd),
                               lon  = first(start_longitude_dd),
                               keep = c("sunrise", "sunset"))$sunset,
    time_of_day = case_when(!is.na(time_of_day) ~ time_of_day,
                            start_time_pt >= sunrise & start_time_pt < sunset ~ "Day",
                            TRUE                                              ~ "Night"),
    time_of_day = factor(time_of_day, levels = c("Day", "Night"))) %>%
  ungroup() %>%
  select(-sunrise, -sunset)


# Recompute matrices ------------------------------------------------------

main_clust_AHC_comm_matrix <- main_clust_wide_major_taxa_nets %>%
  select(transect_station_rep_year_net, chrono_sample_ID, depth_mean_m, 29:50)

main_clust_taxa_cols <- names(main_clust_AHC_comm_matrix)[4:ncol(main_clust_AHC_comm_matrix)]

main_clust_transform_taxa_concentrations <- main_clust_AHC_comm_matrix[, main_clust_taxa_cols] %>% sqrt()

# Add rownames
row.names(main_clust_transform_taxa_concentrations) <- main_clust_AHC_comm_matrix$transect_station_rep_year_net

main_clust_AHC_comm_matrix_transformed <- main_clust_AHC_comm_matrix[1:2] %>%
  bind_cols(.,main_clust_transform_taxa_concentrations)

main_clust_dissim_matrix <- vegdist(main_clust_transform_taxa_concentrations, method = "bray")

# Count matrix for Dexter et al. (2018) NMDS stress null model
# The quasiswap_count null model preserves row totals, taxon totals, and zeros
main_clust_wide_major_taxa_counts_nets <- mocness_major_taxa_nets %>%
  semi_join(main_clust_samples, by = "transect_station_rep_year_net") %>%
  filter(!is.na(individuals_in_tow)) %>%
  mutate(individuals_in_tow = as.integer(round(individuals_in_tow))) %>%
  select(transect_station_rep_year_net, taxon, individuals_in_tow) %>%
  pivot_wider(names_from = taxon,
              values_from = individuals_in_tow,
              values_fill = 0,
              values_fn = sum)

main_clust_AHC_count_abundances <- main_clust_wide_major_taxa_counts_nets %>%
  select(transect_station_rep_year_net, all_of(taxa_cols))


# Reorder count matrix to match original community matrix
main_clust_AHC_count_abundances <- main_clust_AHC_count_abundances[match(main_clust_AHC_comm_matrix$transect_station_rep_year_net,
                                                                         main_clust_AHC_count_abundances$transect_station_rep_year_net),]

stopifnot(all(main_clust_AHC_count_abundances$transect_station_rep_year_net ==
                main_clust_AHC_comm_matrix$transect_station_rep_year_net))

main_clust_AHC_sample_volumes <- main_clust_wide_major_taxa_nets %>%
  select(transect_station_rep_year_net, volume_best_m3_both_sides) %>%
  distinct()

main_clust_AHC_sample_volumes <- main_clust_AHC_sample_volumes[match(main_clust_AHC_comm_matrix$transect_station_rep_year_net,
                                                                     main_clust_AHC_sample_volumes$transect_station_rep_year_net),]

stopifnot(all(main_clust_AHC_sample_volumes$transect_station_rep_year_net ==
                main_clust_AHC_comm_matrix$transect_station_rep_year_net))
stopifnot(all(!is.na(main_clust_AHC_sample_volumes$volume_best_m3_both_sides)))
stopifnot(all(main_clust_AHC_sample_volumes$volume_best_m3_both_sides > 0))

main_clust_AHC_sample_volumes <- main_clust_AHC_sample_volumes$volume_best_m3_both_sides

main_clust_AHC_count_abundances <- main_clust_AHC_count_abundances %>%
  select(all_of(main_clust_taxa_cols)) %>%
  as.matrix()
rownames(main_clust_AHC_count_abundances) <- main_clust_AHC_comm_matrix$transect_station_rep_year_net
storage.mode(main_clust_AHC_count_abundances) <- "integer"


# Perform agglomerative hierarchical clustering ---------------------------

main_clust_AHC_result <- hclust(main_clust_dissim_matrix, method = "average")

main_clust_cluster_colors <- c("1" = "#1F77B4", "2" = "#FF7F0E", "3" = "#8C564B", "4" = "#D62728")
main_clust_cluster_levels <- as.character(seq_len(4))
main_clust_dendrogram_clusters <- cutree(main_clust_AHC_result, k = 4)
main_clust_dendrogram_cluster_order <- unique(main_clust_dendrogram_clusters[main_clust_AHC_result$order])
main_clust_dendrogram_cluster_colors <- main_clust_cluster_colors[as.character(main_clust_dendrogram_cluster_order)]


# Plot dendrogram
png(filename = here("output/main_clusters_AHC_sampling_events_dendrogram.png"),
    width = 12,
    height = 6,
    units = "in",
    res = 300)
plot(main_clust_AHC_result, labels = main_clust_AHC_comm_matrix_transformed$chrono_sample_ID,
     xlab = "Net tows", main = "Clusters of Net Tows Within Main 4 Clusters", cex = 0.4)
rect.hclust(main_clust_AHC_result, k = 4, border = main_clust_dendrogram_cluster_colors)
dev.off()


# Extract list of sampling events belonging to each cluster
main_clust_new_clusters <- data.frame(transect_station_rep_year_net = names(main_clust_dendrogram_clusters),
                                          cluster = main_clust_dendrogram_clusters)


# Indicator Species Analysis ----------------------------------------------

main_clust_comm_for_isa <- main_clust_AHC_comm_matrix_transformed %>%
  select(3:24) %>% as.data.frame()

main_clust_new_clusters_for_isa <- as.factor(main_clust_new_clusters$cluster)

main_clust_new_isa_result <- multipatt(main_clust_comm_for_isa, main_clust_new_clusters_for_isa, func = "IndVal.g", max.order = 2)
summary(main_clust_new_isa_result)


# Map points in space by cluster and net ----------------------------------

main_clust_mapping_df <- main_clust_wide_major_taxa_nets %>%
  left_join(main_clust_new_clusters, by = "transect_station_rep_year_net") %>%
  select(transect_station_rep_year_net, chrono_sample_ID, start_longitude_dd, 
         start_latitude_dd, cluster, net, cruise, depth_mean_m,
         transect_station_rep_year) %>%
  distinct(transect_station_rep_year_net, chrono_sample_ID, start_longitude_dd, 
           start_latitude_dd, cluster, net, cruise, depth_mean_m,
           transect_station_rep_year, .keep_all = TRUE)

main_clust_mapping_df$cluster <- factor(main_clust_mapping_df$cluster, levels = main_clust_cluster_levels)
main_clust_mapping_df$net <- factor(main_clust_mapping_df$net, levels = 0:4)

net0_coordinates <- mocness_clean %>%
  filter(net == 0) %>%
  group_by(transect_station_rep_year) %>%
  summarize(
    net0_longitude_dd = first(start_longitude_dd[!is.na(start_longitude_dd)], default = NA_real_),
    net0_latitude_dd = first(start_latitude_dd[!is.na(start_latitude_dd)], default = NA_real_),
    .groups = "drop")

excluded_df <- excluded_tows %>%
  mutate(transect_station_rep_year = str_replace(transect_station_rep_year_net, "_[^_]+$", "")) %>%
  left_join(mocness_major_taxa %>% 
              distinct(transect_station_rep_year_net, cruise), by = "transect_station_rep_year_net")


# Find mapping area and create coastline, state boundaries, and isobaths
map_xlim <- c(-126.8, -123.2)
map_ylim <- c(40.2, 47.8)
map_bbox <- st_bbox(c(xmin = map_xlim[1], xmax = map_xlim[2],
                      ymin = map_ylim[1], ymax = map_ylim[2]),
                    crs = st_crs(4326))

land <- tryCatch(
  ne_download(scale = "large", type = "land", category = "physical", returnclass = "sf"),
  error = function(e) ne_download(scale = "medium", type = "land", category = "physical", returnclass = "sf")) %>%
  st_crop(map_bbox)

coast <- tryCatch(
  ne_download(scale = "large", type = "coastline", category = "physical", returnclass = "sf"),
  error = function(e) ne_download(scale = "medium", type = "coastline", category = "physical", returnclass = "sf")) %>%
  st_crop(map_bbox)

admin1 <- ne_download(scale = "medium",
                      type = "admin_1_states_provinces_lines",
                      category = "cultural",
                      returnclass = "sf") %>%
  st_crop(map_bbox)

space <- land
bathy <- getNOAA.bathy(lon1 = map_xlim[1], lon2 = map_xlim[2],
                       lat1 = map_ylim[1], lat2 = map_ylim[2],
                       resolution = 2)
bathy_df <- fortify.bathy(bathy) %>% as_tibble()
isobath_levels <- -seq(250, 3000, by = 250)


# Create net layout
offsets <- tibble(net = factor(0:4),
                  dx = 0,
                  dy = c(-0.06, -0.03, 0, 0.03, 0.06))
main_clust_mapping_df2 <- main_clust_mapping_df %>% 
  left_join(net0_coordinates, by = "transect_station_rep_year") %>%
  left_join(offsets, by = "net") %>%
  mutate(plot_longitude_dd = coalesce(net0_longitude_dd, start_longitude_dd),
         plot_latitude_dd = coalesce(net0_latitude_dd, start_latitude_dd)) %>%
  mutate(year = case_when(cruise == "W18" ~ 2018, cruise == "W19" ~ 2019, cruise == "W22" ~ 2022, cruise == "W23" ~ 2023),
         rep = str_split(transect_station_rep_year_net, "_", simplify = TRUE)[,3],
         facet_group = case_when(cruise == "W18" ~ paste0("18", rep), cruise == "W19" ~ paste0("19", rep),
                                 cruise == "W22" ~ "22", cruise == "W23" ~ "23")) %>%
  arrange(net)
stopifnot(all(as.character(main_clust_mapping_df2$cluster) %in% names(main_clust_cluster_colors)))

excluded_df <- excluded_df %>%
  mutate(net = factor(net, levels = 0:4)) %>%
  left_join(net0_coordinates, by = "transect_station_rep_year") %>%
  left_join(offsets, by = "net") %>%
  mutate(plot_longitude_dd = coalesce(net0_longitude_dd, start_longitude_dd),
         plot_latitude_dd = coalesce(net0_latitude_dd, start_latitude_dd)) %>%
  mutate(year = case_when(cruise == "W18" ~ 2018, cruise == "W19" ~ 2019, cruise == "W22" ~ 2022, cruise == "W23" ~ 2023),
         rep = str_split(transect_station_rep_year_net, "_", simplify = TRUE)[,3],
         facet_group = case_when(cruise == "W18" ~ paste0("18", rep), cruise == "W19" ~ paste0("19", rep),
                                 cruise == "W22" ~ "22", cruise == "W23" ~ "23")) %>%
  arrange(net)


# Assign lightness/color value to nets
net_lightness <- c("0" = 1.00, "1" = 0.85, "2" = 0.70, "3" = 0.55, "4" = 0.40)

map_layers <- list(
  geom_sf(data = land, fill = "grey55", color = NA),
  geom_sf(data = coast, color = "black", linewidth = 0.4),
  geom_sf(data = admin1, color = "black", linewidth = 0.25),
  geom_contour(data = bathy_df, aes(x = x, y = y, z = z),
               breaks = isobath_levels, color = "grey80", linewidth = 0.25),
  scale_color_manual(values = main_clust_cluster_colors,
                     limits = main_clust_cluster_levels,
                     breaks = main_clust_cluster_levels,
                     drop = FALSE,
                     guide = "none"),
  scale_alpha_manual(values = net_lightness,
                     breaks = rev(names(net_lightness)),
                     guide = "none"),
  coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE),
  theme_classic(base_size = 12),
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5)))

main_clust_cluster_map_legend <- get_legend(
  ggplot() +
    geom_point(
      data = tibble(cluster = factor(main_clust_cluster_levels, levels = main_clust_cluster_levels),
                    x = 1, y = seq_along(main_clust_cluster_levels)),
      aes(x, y, color = cluster),
      size = 2) +
    geom_point(
      data = tibble(net = factor(names(net_lightness), levels = 0:4),
                    x = 1, y = seq_along(net_lightness)),
      aes(x, y, alpha = net),
      color = "black",
      size = 2) +
    scale_color_manual(values = main_clust_cluster_colors,
                       limits = main_clust_cluster_levels,
                       breaks = main_clust_cluster_levels,
                       drop = FALSE,
                       name = "Cluster",
                       guide = guide_legend(order = 1,
                                            override.aes = list(alpha = 1, size = 2))) +
    scale_alpha_manual(values = net_lightness,
                       breaks = rev(names(net_lightness)),
                       name = "Net",
                       guide = guide_legend(order = 2)) +
    theme_void() +
    theme(legend.position = "right"))

main_clust_p18a <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "18MaN"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(main_clust_mapping_df2, facet_group == "18MaN"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "18MaN", x = NULL, y = NULL)

main_clust_p18b <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "18MaD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(main_clust_mapping_df2, facet_group == "18MaD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "18MaD", x = NULL, y = NULL)

main_clust_p18c <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "18MbD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(main_clust_mapping_df2, facet_group == "18MbD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "18MbD", x = NULL, y = NULL)

main_clust_p19a <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "19MaN"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(main_clust_mapping_df2, facet_group == "19MaN"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "19MaN", x = NULL, y = NULL)

main_clust_p19b <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "19MaD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(main_clust_mapping_df2, facet_group == "19MaD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "19MaD", x = NULL, y = NULL)

main_clust_p19c <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "19MbN"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(main_clust_mapping_df2, facet_group == "19MbN"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "19MbN", x = NULL, y = NULL)

main_clust_p19d <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "19MbD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(main_clust_mapping_df2, facet_group == "19MbD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "19MbD", x = NULL, y = NULL)

main_clust_p22 <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "22"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(main_clust_mapping_df2, facet_group == "22"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "22", x = NULL, y = NULL)

main_clust_p23 <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "23"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(main_clust_mapping_df2, facet_group == "23"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "23", x = NULL, y = NULL)


# Make layout panels for 2018 and 2019
main_clust_p2018 <- (main_clust_p18a | main_clust_p18b) / main_clust_p18c +
  plot_layout(heights = c(1, 1))
main_clust_p2019 <- ((main_clust_p19a | main_clust_p19b) /
            (main_clust_p19c | main_clust_p19d))


# Assemble custom layout
main_clust_final_cluster_map <- (main_clust_p2018 | main_clust_p2019 | main_clust_p22 | main_clust_p23 | wrap_elements(main_clust_cluster_map_legend)) +
  plot_layout(widths = c(1, 1, 1.4, 1.4, 0.35))
main_clust_final_cluster_map
## Save
ggsave("main_clust_cluster_map.png", plot = get_last_plot(), path = here("output"), 
       width = 15, height = 10, units = "in", dpi = 300)


# Plot abundance of each taxon, grouped by cluster ------------------------

# Add cluster identities and chronological sample IDs
main_clust_AHC_comm_matrix_transformed_long <- main_clust_AHC_comm_matrix_transformed %>%
  pivot_longer(cols = 3:24, names_to = "taxon", values_to = "sqrt_concentration") %>%
  merge(., main_clust_new_clusters, by = "transect_station_rep_year_net") %>%
  arrange(cluster) %>%
  mutate(chrono_sample_ID = factor(chrono_sample_ID, levels = unique(chrono_sample_ID)))


# Compute cluster bounds to use as vertical separators on barplot
main_clust_cluster_bounds <- main_clust_AHC_comm_matrix_transformed_long %>%
  distinct(cluster, chrono_sample_ID) %>%
  mutate(chrono_sample_ID = as.numeric(chrono_sample_ID)) %>%
  group_by(cluster) %>%
  summarize(start = min(chrono_sample_ID), end   = max(chrono_sample_ID), .groups = "drop")

main_clust_bar_heights <- main_clust_AHC_comm_matrix_transformed_long %>%
  group_by(chrono_sample_ID) %>%
  summarize(total_height = sum(sqrt_concentration), .groups = "drop")

main_clust_max_height <- max(main_clust_bar_heights$total_height)


# Plot by transect_station_rep_year, sorted by cluster
main_clust_abun_bar_plot<- ggplot(main_clust_AHC_comm_matrix_transformed_long, aes(x = chrono_sample_ID, y = sqrt_concentration, fill = factor(taxon, levels = ordered_taxa))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors, breaks = ordered_taxa, name = "Taxonomic group") +
  geom_vline(data = main_clust_cluster_bounds[-1,],
             aes(xintercept = start - 0.5), linetype = "dashed", color = "gray40", linewidth = 0.5, inherit.aes = FALSE) +
  annotate("text", x = mean(range(as.numeric(main_clust_AHC_comm_matrix_transformed_long$chrono_sample_ID))), y = Inf,
           label = "Cluster", vjust = -2, size = 4) +
  annotate("text", x = (main_clust_cluster_bounds$start + main_clust_cluster_bounds$end) / 2, y = Inf,
           label = paste(main_clust_cluster_bounds$cluster), vjust = -1, size = 3) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Sample ID", y = "Concentration (ind./m^3)") +
  guides(fill = guide_legend(ncol = 1)) +
  theme_light() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 35, r = 30, b = 5, l = 5),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 5))
## Extract legend
main_clust_legend_only <- cowplot::get_legend(clust_abun_bar_plot)
### Wrap legend in a ggplot so ggsave works
main_clust_legend_plot <- cowplot::ggdraw(main_clust_legend_only)
ggsave(filename = "main_clust_barplot_taxa_legend.png",
       plot = main_clust_legend_plot,
       path = here("output"),
       width = 2, height = 6, dpi = 300)
## Plot without legend
main_clust_abun_bar_plot_no_legend <- main_clust_abun_bar_plot + theme(legend.position = "none")
ggsave("clusters_abundance_bar_plot.png", plot = get_last_plot(), path = here("output"),
       width = 10, height = 5, units = "in", dpi = 300)
ggsave("main_clusters_clusters_abundance_bar_plot.png", plot = get_last_plot(), path = here("output"),
       width = 10, height = 5, units = "in", dpi = 300)
#RM note: "main_cluster_cluster_abundance_bar_plot.png" isn't working I think because I tried to save without ggsave. disregard this output.

# Check NMDS Stress -------------------------------------------------------

set.seed(123)
main_clust_NMDS_result <- metaMDS(main_clust_dissim_matrix, distance = "bray", k = 2, try = 20, trymax = 20, engine = "monoMDS")
main_clust_NMDS_result$stress  ##check stress

windows()
stressplot(main_clust_NMDS_result)   ##Shepard diagram


# Test NMDS stress against a Dexter et al. (2018) null model --------------

main_clust_n_stress_permutations <- 1000
main_clust_stress_nmds_try <- 20
main_clust_stress_nmds_trymax <- 20
main_clust_stress_progress_every <- 10
main_clust_stress_fit_counter <- 0

main_clust_nmds_stress_statistic <- function(comm, main_clust_sample_volumes = main_clust_AHC_sample_volumes) {
  main_clust_stress_fit_counter <<- main_clust_stress_fit_counter + 1
  
  if (main_clust_stress_fit_counter == 1) {
    message("Fitting observed NMDS stress for the null-model pipeline")} 
  else if ((main_clust_stress_fit_counter - 1) %% main_clust_stress_progress_every == 0 ||
           (main_clust_stress_fit_counter - 1) == main_clust_n_stress_permutations) {
    message("Completed ", main_clust_stress_fit_counter - 1, " of ",
            main_clust_n_stress_permutations, " null NMDS stress fits")}
  
  main_clust_comm_concentrations <- sweep(as.matrix(comm), 1, main_clust_sample_volumes, "/")
  main_clust_comm_transformed <- sqrt(main_clust_comm_concentrations)
  
  list(statistic = c(stress = metaMDS(
    main_clust_comm_transformed,
    distance = "bray",
    k = 2,
    try = main_clust_stress_nmds_try,
    trymax = main_clust_stress_nmds_trymax,
    engine = "monoMDS",
    autotransform = FALSE,
    trace = FALSE)$stress))}

set.seed(123)
main_clust_NMDS_stress_null_test <- oecosimu(main_clust_AHC_count_abundances,
                                             main_clust_nmds_stress_statistic,
                                  method = "quasiswap_count",
                                  nsimul = main_clust_n_stress_permutations,
                                  alternative = "two.sided")

main_clust_stress_null_values <- as.numeric(main_clust_NMDS_stress_null_test$oecosimu$simulated)
main_clust_stress_observed <- as.numeric(main_clust_NMDS_stress_null_test$oecosimu$statistic)
main_clust_stress_null_z <- (main_clust_stress_observed - mean(main_clust_stress_null_values, na.rm = TRUE)) /
  sd(main_clust_stress_null_values, na.rm = TRUE)
main_clust_stress_null_p <- 2 * pnorm(-abs(main_clust_stress_null_z))

main_clust_stress_null_distribution <- tibble(iteration = seq_along(main_clust_stress_null_values),
                                              main_clust_null_stress = main_clust_stress_null_values)

main_clust_stress_null_summary <- tibble(main_clust_observed_stress_null_pipeline = main_clust_stress_observed,
                              main_clust_observed_stress_sqrt_concentration_nmds = main_clust_NMDS_result$stress,
                              main_clust_null_mean_stress = mean(main_clust_stress_null_values, na.rm = TRUE),
                              main_clust_null_sd_stress = sd(main_clust_stress_null_values, na.rm = TRUE),
                              main_clust_null_stress_q025 = quantile(main_clust_stress_null_values, 0.025, na.rm = TRUE),
                              main_clust_null_stress_q975 = quantile(main_clust_stress_null_values, 0.975, na.rm = TRUE),
                              main_clust_z = main_clust_stress_null_z,
                              main_clust_p_value_two_tailed = main_clust_stress_null_p,
                              main_clust_n_permutations = length(main_clust_stress_null_values),
                              main_clust_null_model = "quasiswap_count")

write.csv(main_clust_stress_null_summary,
          here("output/main_clust_NMDS_stress_null_test_summary.csv"),
          row.names = FALSE)

write.csv(main_clust_stress_null_distribution,
          here("output/main_clust_NMDS_stress_null_distribution.csv"),
          row.names = FALSE)

ggplot(main_clust_stress_null_distribution, aes(x = main_clust_null_stress)) +
  geom_histogram(bins = 30, fill = "grey70", color = "white") +
  geom_vline(xintercept = main_clust_stress_observed, linetype = 2, linewidth = 1,
             color = "red3") +
  theme_classic() +
  labs(
    title = "NMDS stress compared with constrained null communities for main clusters",
    subtitle = paste0("Dexter et al. null model: z = ",
                      round(main_clust_stress_null_z, 2),
                      ", p = ",
                      signif(main_clust_stress_null_p, 3)),
    x = "Null stress", y = "Permuted communities")

ggsave("main_clust_NMDS_stress_null_distribution.png",
  plot = get_last_plot(),
  path = here("output"),
  width = 7,
  height = 5,
  units = "in",
  dpi = 300)


# Plot NMDS ordination ---------------------------------------------------

main_clust_site_scores <- as.data.frame(scores(main_clust_NMDS_result, display = "sites"))
main_clust_cluster_groups <- cutree(main_clust_AHC_result, k = 4)
main_clust_station_scores <- mutate(main_clust_site_scores, transect_station_rep_year_net = main_clust_AHC_comm_matrix_transformed$transect_station_rep_year_net)
main_clust_stations_clustered <- mutate(main_clust_station_scores, cluster = main_clust_cluster_groups)
main_clust_stations_clustered$cluster <- as.numeric(as.character(main_clust_stations_clustered$cluster))
main_clust_stations_clustered$cluster <- factor(main_clust_stations_clustered$cluster, levels = c(1,2,3,4))

main_clust_hulls <- main_clust_stations_clustered %>%
  group_by(cluster) %>%
  slice(chull(NMDS1, NMDS2))

ggplot(main_clust_stations_clustered, aes(x = NMDS1, y = NMDS2)) +
  geom_polygon(data = main_clust_hulls, aes(fill = cluster, group = cluster), alpha = 0.25, color = NA) +
  geom_point(aes(color = cluster), size = 1) +
  scale_fill_manual(values = main_clust_cluster_colors) +
  scale_color_manual(values = main_clust_cluster_colors) +
  theme_classic() +
  labs(title = "NMDS Ordination of sampling events by LFC for main clusters", x = "NMDS1", y = "NMDS2")
ggsave("NMDS_main_clusters.png", plot = get_last_plot(), path = here("output"),
       width = 6, height = 5, units = "in", dpi = 300)

# Overlays for NMDS plots -------------------------------------------------

# Vectors
main_clust_env_wide_aligned <- env_wide[match(rownames(scores(main_clust_NMDS_result, display = "sites")),
                                              main_clust_env_wide$transect_station_rep_year_net), ]
main_clust_year_mat <- model.matrix(~ factor(year) - 1, data = main_clust_env_wide_aligned)
colnames(main_clust_year_mat) <- paste0("year_", levels(factor(main_clust_env_wide_aligned$year)))
main_clust_env_numeric <- main_clust_env_wide_aligned[, sapply(main_clust_env_wide_aligned, is.numeric)]
main_clust_env_numeric2 <- cbind(main_clust_env_numeric[, !names(main_clust_env_numeric) %in% "year"], main_clust_year_mat)
main_clust_fit_vectors <- envfit(main_clust_NMDS_result, main_clust_env_numeric2, permutations = 1000, na.rm = TRUE)

## Extract vector scores for plotting
main_clust_vector_scores <- scores(main_clust_fit_vectors, display = "vectors")
main_clust_vector_df <- as.data.frame(main_clust_vector_scores) %>% 
  mutate(variable = rownames(main_clust_vector_scores)) %>% 
  filter(grepl("^year_", variable) | variable %in% c("start_longitude_dd", "depth_mean_m", "seafloor_depth_m"))


# Ellipses
main_clust_time_groups <- main_clust_env_wide_aligned$time_of_day
main_clust_ell_time <- ordiellipse(main_clust_NMDS_result, main_clust_time_groups, kind = "sd", 
                        conf = 0.95,  draw = "none")

##  Convert output to dataframe
main_clust_ell_time_df <- purrr::map_dfr(names(main_clust_ell_time), ~ {
  e     <- main_clust_ell_time[[.x]]
  theta <- seq(0, 2 * pi, length.out = 200)
  circle <- cbind(cos(theta), sin(theta))
  xy <- circle %*% chol(e$cov)
  xy <- sweep(xy * e$scale, 2, e$center, "+")
  tibble(
    NMDS1 = xy[, 1],
    NMDS2 = xy[, 2],
    group = .x)})


# Plot NMDS with overlays
windows()
ggplot(main_clust_stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  #1 Polygons (cluster color)
  geom_polygon(data = main_clust_hulls, aes(x = NMDS1, y = NMDS2, fill = cluster, group = cluster), 
               alpha = 0.25, color = NA, inherit.aes = FALSE) +
  scale_fill_manual(values = main_clust_cluster_colors) +
  #2 Points (cluster colors)
  geom_point(size = 1) +
  scale_color_manual(values = main_clust_cluster_colors) +
  #3 Ellipses - Day/Night
  geom_path(data = main_clust_ell_time_df, aes(x = NMDS1, y = NMDS2, linetype = group), 
            size = 0.5, color = "black", inherit.aes = FALSE) +
  scale_linetype_manual(name = "Time of Day", values = c("Day" = "solid", "Night" = "dashed")) +
  #4 Vectors
  geom_segment(data = main_clust_vector_df, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  geom_text(data = main_clust_vector_df, aes(x = NMDS1, y = NMDS2, label = variable), 
            color = "black", size = 2, vjust = -0.5,inherit.aes = FALSE) +
  labs(title = "NMDS ordination with clustered points and covariate overlays for main clusters", x = "NMDS1", y = "NMDS2",
       color = "Cluster", fill = "Cluster", linetype = "Time of Day") + 
  theme_classic()
ggsave("NMDS_overlays_main_clusters.png", plot = get_last_plot(), path = here("output"),
       width = 7, height = 5, units = "in", dpi = 300)


# Conduct db-RDA ----------------------------------------------------------

# Create db-RDA data frame
main_clust_mocness <- mocness_major_taxa_nets %>%
  semi_join(main_clust_samples, by = "transect_station_rep_year_net")

main_clust_dbRDA_major_taxa_wide <- main_clust_mocness %>%
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
  #RM note: Is this still true and a concern??
  group_by(transect_station_rep_year_net) %>%
  mutate(mean_temperature_c = mean(mean_temperature_c),
         mean_salinity_psu = mean(mean_salinity_psu),
         mean_density_kgm3 = mean(mean_density_kgm3)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_per_m3, values_fill = 0)


# Choose db-RDA covariates
main_clust_spatiotemporal_covariates <- c("year", "time_of_day", "start_latitude_dd",
                               "depth_mean_m", "seafloor_depth_m")

main_clust_environmental_covariates <- c("mean_temperature_c", "mean_salinity_psu",
                              "dissolved_oxygen_ml_l", "mean_chl_0_100_m_mgm3")

main_clust_dbRDA_covariates <- c(main_clust_spatiotemporal_covariates, main_clust_environmental_covariates)

main_clust_dbRDA_metadata_cols <- c("project", "year", "cruise", "collection_date", "transect",
                         "replicate", "station", "net", "transect_station_rep_year_net",
                         "transect_station_rep_year", "start_time_pt",
                         "start_longitude_dd", "start_latitude_dd",
                         "maximum_depth_m", "minimum_depth_m", "depth_mean_m",
                         "depth_diff_m", "volume_best_m3_both_sides",
                         "mean_temperature_c", "mean_salinity_psu", "mean_density_kgm3",
                         "seafloor_depth_m", "distance_to_shore_km", "shelf_position",
                         "prey_zooplankton_abundance_ind_m3", "dissolved_oxygen_ml_l",
                         "mean_chl_0_100_m_mgm3", "mlotst")

main_clust_dbRDA_taxa_cols <- names(main_clust_dbRDA_major_taxa_wide) %>%
  setdiff(main_clust_dbRDA_metadata_cols)


# Prepare environmental data
main_clust_dbRDA_env <-  main_clust_dbRDA_major_taxa_wide %>%
  mutate(year = factor(year)) %>%
  ungroup() %>%
  mutate(total_concentration = rowSums(across(all_of(main_clust_dbRDA_taxa_cols))))


# Build community matrix
main_clust_comm_matrix <- main_clust_wide_major_taxa_nets %>%
  select(all_of(main_clust_taxa_cols)) %>%
  mutate(across(everything(), sqrt)) %>%
  as.data.frame()

row.names(main_clust_comm_matrix) <- main_clust_wide_major_taxa_nets$transect_station_rep_year_net


# Build environmental model
main_clust_env_model <- main_clust_env_wide %>%
  mutate(year = factor(year)) %>%   # important!
  select(transect_station_rep_year_net,
         all_of(main_clust_dbRDA_covariates),
         shelf_position,
         collection_date,
         cruise,
         net) %>%
  as.data.frame()

row.names(main_clust_env_model) <- main_clust_env_model$transect_station_rep_year_net


# Pull AHC assignments from above
main_clust_clusters <- main_clust_new_clusters %>%
  mutate(cluster = factor(cluster,
                          levels = main_clust_cluster_levels))


# Fit db-RDA models
set.seed(123)

main_clust_base_model <- capscale(main_clust_comm_matrix ~ year + time_of_day +
                                    start_latitude_dd + depth_mean_m +
                                    seafloor_depth_m,
                                  data = main_clust_env_model,
                                  distance = "bray",
                                  add = "lingoes")

main_clust_full_model <- capscale(main_clust_comm_matrix ~ year + time_of_day +
                                    start_latitude_dd + depth_mean_m +
                                    seafloor_depth_m + mean_temperature_c +
                                    mean_salinity_psu + dissolved_oxygen_ml_l +
                                    mean_chl_0_100_m_mgm3,
                                  data = main_clust_env_model,
                                  distance = "bray",
                                  add = "lingoes")


# Partial model testing env var after conditioning on spatiotemp var in base model
main_clust_dbRDA_env_partial_model <- capscale(main_clust_comm_matrix ~ mean_temperature_c +
                                      mean_salinity_psu + dissolved_oxygen_ml_l +
                                      mean_chl_0_100_m_mgm3 +
                                      Condition(year + time_of_day +
                                                  start_latitude_dd +
                                                  depth_mean_m +
                                                  seafloor_depth_m),
                                    data = main_clust_env_model,
                                    distance = "bray",
                                    add = "lingoes")


# Partition variation between spatiotemporal and environmental covariates
main_clust_dbRDA_bray_dist <- vegdist(main_clust_comm_matrix, method = "bray")

main_clust_dbRDA_varpart <- varpart(main_clust_dbRDA_bray_dist,
                         ~ year + time_of_day + start_latitude_dd + depth_mean_m + seafloor_depth_m,
                         ~ mean_temperature_c + mean_salinity_psu + dissolved_oxygen_ml_l +
                           mean_chl_0_100_m_mgm3,
                         data = main_clust_env_model,
                         add = "lingoes")


# Summary allocates shared fractions equally to each covariate set
main_clust_dbRDA_varpart_summary <- summary(main_clust_dbRDA_varpart)


# Testable unique fractions from the two-set variation partitioning
main_clust_dbRDA_spatiotemporal_unique_model <- dbrda(main_clust_dbRDA_bray_dist ~ year + time_of_day + start_latitude_dd + depth_mean_m +
                                             seafloor_depth_m +
                                             Condition(mean_temperature_c + mean_salinity_psu + dissolved_oxygen_ml_l +
                                                         mean_chl_0_100_m_mgm3),
                                           data = main_clust_env_model,
                                           add = "lingoes")

main_clust_dbRDA_environmental_unique_model <- dbrda(main_clust_dbRDA_bray_dist ~ mean_temperature_c + mean_salinity_psu +
                                            dissolved_oxygen_ml_l + mean_chl_0_100_m_mgm3 +
                                            Condition(year + time_of_day + start_latitude_dd + depth_mean_m +
                                                        seafloor_depth_m),
                                          data = main_clust_env_model,
                                          add = "lingoes")


# Evaluate model support
## Can increase number of permutations once the model structure is final
main_clust_dbRDA_base_overall_test <- anova(main_clust_base_model, permutations = 999)
main_clust_dbRDA_full_overall_test <- anova(main_clust_full_model, permutations = 999)

## Evaluate whether adding environmental variables improves model support over spatiotemporal variables alone
main_clust_dbRDA_base_vs_full_test <- anova(main_clust_base_model, main_clust_full_model,
                                 permutations = 999)
main_clust_dbRDA_base_r2 <- RsquareAdj(main_clust_base_model)
main_clust_dbRDA_full_r2 <- RsquareAdj(main_clust_full_model)

## Tests for individual terms in the full model
main_clust_dbRDA_full_term_tests <- anova(main_clust_full_model, by = "margin",
                               permutations = 999)

## Tests for individual environmental variables while conditioning on the base spatiotemporal model
main_clust_dbRDA_env_partial_tests <- anova(main_clust_dbRDA_env_partial_model, by = "margin",
                                 permutations = 999)

## Check whether any model terms are strongly collinear
main_clust_dbRDA_full_vif <- vif.cca(main_clust_full_model)

## Permutation tests for the unique fractions in the variation partitioning
main_clust_dbRDA_spatiotemporal_unique_test <- anova(main_clust_dbRDA_spatiotemporal_unique_model,
                                          permutations = 999)
main_clust_dbRDA_environmental_unique_test <- anova(main_clust_dbRDA_environmental_unique_model,
                                         permutations = 999)


# Save a simple variation partitioning diagram
png(filename = here("output/main_clust_dbRDA_variance_partitioning.png"),
    width = 8,
    height = 8,
    units = "in",
    res = 300)
plot(main_clust_dbRDA_varpart, bg = c("#A6CEE3", "#B2DF8A"), cutoff = 0,
     Xnames = c("Spatiotemporal", "Environmental"))
dev.off()


# Plot constrained ordination
main_clust_dbRDA_site_scores <- scores(main_clust_full_model, display = "sites", choices = 1:2) %>%
  as.data.frame() %>%
  rownames_to_column("transect_station_rep_year_net") %>%
  left_join(main_clust_env_model, by = "transect_station_rep_year_net") %>%
  left_join(main_clust_clusters, by = "transect_station_rep_year_net")

main_clust_dbRDA_hulls <- main_clust_dbRDA_site_scores %>%
  group_by(cluster) %>%
  slice(chull(CAP1, CAP2)) %>%
  ungroup()

main_clust_dbRDA_vector_scores <- scores(main_clust_full_model, display = "bp", choices = 1:2) %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  mutate(plot_label = recode(variable,
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
      .default = variable),
    base_label_x = CAP1 + if_else(CAP1 >= 0, 0.14, -0.14),
    base_label_y = CAP2 + if_else(CAP2 >= 0, 0.10, -0.10),
    label_x = case_when(
      variable == "year2019" ~ -0.25,
      variable == "time_of_dayNight" ~ -0.18,
      variable == "dissolved_oxygen_ml_l" ~ 0.15,
      variable == "year2018" ~ -0.30,
      variable == "year2023" ~ 0.32,
      variable == "mean_chl_0_100_m_mgm3" ~ 0.62,
      TRUE ~ base_label_x),
    label_y = case_when(
      variable == "year2019" ~ 0.72,
      variable == "time_of_dayNight" ~ 0.45,
      variable == "dissolved_oxygen_ml_l" ~ 0.64,
      variable == "year2018" ~ -0.16,
      variable == "year2023" ~ 0.16,
      variable == "mean_chl_0_100_m_mgm3" ~ 0.06,
      TRUE ~ base_label_y),
    label_hjust = case_when(
      variable %in% c("year2019", "time_of_dayNight", "year2018") ~ 1,
      variable %in% c("dissolved_oxygen_ml_l", "year2023",
                      "mean_chl_0_100_m_mgm3") ~ 0,
      CAP1 >= 0 ~ 0,
      TRUE ~ 1))

windows()
main_clust_dbRDA_plot <- ggplot(main_clust_dbRDA_site_scores, aes(x = CAP1, y = CAP2, color = cluster)) +
  geom_polygon(data = main_clust_dbRDA_hulls,
               aes(x = CAP1, y = CAP2, fill = cluster, group = cluster),
               alpha = 0.25,
               color = NA,
               inherit.aes = FALSE) +
  geom_point(size = 1, alpha = 0.9) +
  stat_ellipse(data = main_clust_dbRDA_site_scores,
               aes(x = CAP1, y = CAP2, linetype = time_of_day),
               color = "grey20",
               linewidth = 0.5,
               type = "norm",
               level = 0.68,
               show.legend = c(linetype = TRUE, color = FALSE),
               inherit.aes = FALSE) +
  scale_fill_manual(values = main_clust_cluster_colors,
                    limits = main_clust_cluster_levels,
                    breaks = main_clust_cluster_levels,
                    drop = FALSE) +
  scale_color_manual(values = main_clust_cluster_colors,
                     limits = main_clust_cluster_levels,
                     breaks = main_clust_cluster_levels,
                     drop = FALSE) +
  scale_linetype_manual(values = c("Day" = "solid", "Night" = "dashed")) +
  geom_segment(data = main_clust_dbRDA_vector_scores,
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", linewidth = 0.85) +
  geom_segment(data = main_clust_dbRDA_vector_scores,
               aes(x = CAP1, y = CAP2, xend = label_x, yend = label_y),
               inherit.aes = FALSE,
               color = "grey35",
               linewidth = 0.5) +
  geom_text(data = main_clust_dbRDA_vector_scores,
            aes(x = label_x, y = label_y, label = plot_label,
                hjust = label_hjust),
            inherit.aes = FALSE,
            color = "black",
            size = 2) +
  theme_classic() +
  labs(title = "db-RDA of larval fish assemblage composition for main clusters",
       x = "CAP1", y = "CAP2", color = "Cluster", fill = "Cluster", linetype = "Time of Day")

print(main_clust_dbRDA_plot)

ggsave("main_clust_dbRDA_cluster_ordination.png", plot = main_clust_dbRDA_plot, path = here("output"),
       width = 7, height = 5, units = "in", dpi = 300)
