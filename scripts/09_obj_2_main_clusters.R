# Description -------------------------------------------------------------

#Run objective 2 analyses and plots with only 4 main clusters

# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)


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

#RM: did not recompute count matrix for Dexter et al. (2018) NMDS stress null model, will come back to this
##Note to self: do this!

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

#Plot dendrogram
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


## Find mapping area and create coastline, state boundaries, and isobaths
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

## Create net layout
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

## Assign lightness/color value to nets
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
        plot.title = element_text(face = "bold", hjust = 0.5))
)

main_clust_cluster_map_legend <- get_legend(
  ggplot() +
    geom_point(
      data = tibble(cluster = factor(main_clust_cluster_levels, levels = main_clust_cluster_levels),
                    x = 1, y = seq_along(main_clust_cluster_levels)),
      aes(x, y, color = cluster),
      size = 2
    ) +
    geom_point(
      data = tibble(net = factor(names(net_lightness), levels = 0:4),
                    x = 1, y = seq_along(net_lightness)),
      aes(x, y, alpha = net),
      color = "black",
      size = 2
    ) +
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
    theme(legend.position = "right")
)

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

## Make layout panels for 2018 and 2019
main_clust_p2018 <- (main_clust_p18a | main_clust_p18b) / main_clust_p18c +
  plot_layout(heights = c(1, 1))
main_clust_p2019 <- ((main_clust_p19a | main_clust_p19b) /
            (main_clust_p19c | main_clust_p19d))

## Assemble custom layout
main_clust_final_cluster_map <- (main_clust_p2018 | main_clust_p2019 | main_clust_p22 | main_clust_p23 | wrap_elements(main_clust_cluster_map_legend)) +
  plot_layout(widths = c(1, 1, 1.4, 1.4, 0.35))
main_clust_final_cluster_map
#save
ggsave("main_clust_cluster_map.png", plot = get_last_plot(), path = here("output"), 
       width = 15, height = 10, units = "in", dpi = 300)


# Plot abundance of each taxon, grouped by cluster ------------------------




# Plot NMDS ordination ---------------------------------------------------





# overlays for NMDS plots -------------------------------------------------


