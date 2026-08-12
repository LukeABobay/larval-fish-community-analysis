
# Description -------------------------------------------------------------

# Conduct a cluster analysis of sampling events by LFC, plot dendrograms to decide number
#  of clusters. Plot sampling events by cluster on a map of the coast, jittering by depth. 
#  Run an NMDS, and plot the NMDS ordination with overlays for covariates.
#  Perform an indicator taxa analysis on the clustered community matrix.


# Load packages -----------------------------------------------------------

library(here)
library(vegan)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(dplyr)
library(purrr)
library(suncalc)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(indicspecies)
library(ggnewscale)
library(plotly)
library(marmap)
library(rnaturalearthhires)
library(patchwork)
library(cowplot)
library(gtable)
library(grid)

# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))

# Create wide environmental dataframe ---------------------------------------------------

wide_major_taxa_nets <- mocness_major_taxa_nets %>%
  # Removing NAs for now, but there shouldn't be any to begin with
  # Removing NAs for now, but there shouldn't be any to begin with
  filter(!is.na(individuals_in_tow)) %>%
  filter(!is.na(individuals_per_m3)) %>%
  select(project, year, cruise, collection_date, start_time_pt, transect, replicate, station, net,
         transect_station_rep_year_net, transect_station_rep_year, start_time_pt,
         start_longitude_dd, start_latitude_dd, maximum_depth_m, minimum_depth_m, 
         depth_mean_m, depth_diff_m, volume_best_m3_both_sides,
         mean_temperature_c, mean_salinity_psu, mean_density_kgm3, seafloor_depth_m,
         distance_to_shore_km, shelf_position, prey_zooplankton_abundance_ind_m3,
         dissolved_oxygen_ml_l, mean_chl_0_100_m_mgm3,
         taxon, individuals_per_m3) %>%
  # For some reason, MOC 1 and MOC 4 have different values of mean_temperature_c, mean_salinity_psu, and mean_density_kgm3 in 6 cases. To eliminate differences, calculate mean
  group_by(transect_station_rep_year_net) %>%
  mutate(mean_temperature_c = mean(mean_temperature_c),
         mean_salinity_psu = mean(mean_salinity_psu),
         mean_density_kgm3 = mean(mean_density_kgm3)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_per_m3, values_fill = 0) %>%
  # Assign net tows unique sample IDs chronologically
  arrange(start_time_pt) %>%
  mutate(chrono_sample_ID = row_number())

env_wide <- wide_major_taxa_nets %>%
  mutate(
    time_of_day = substr(replicate, 3, 3),
    time_of_day = recode(time_of_day, "D" = "Day", "N" = "Night", .default = NA_character_)
  ) %>%
  group_by(transect_station_rep_year_net) %>%   # or collection_date, or station, etc.
  mutate(
    # compute sunrise/sunset at that station/date
    sunrise = getSunlightTimes(
      date = as.Date(collection_date),
      lat  = first(start_latitude_dd),
      lon  = first(start_longitude_dd),
      keep = c("sunrise", "sunset")
    )$sunrise,
    sunset  = getSunlightTimes(
      date = as.Date(collection_date),
      lat  = first(start_latitude_dd),
      lon  = first(start_longitude_dd),
      keep = c("sunrise", "sunset")
    )$sunset,
    
    time_of_day = case_when(
      !is.na(time_of_day) ~ time_of_day,
      start_time_pt >= sunrise & start_time_pt < sunset ~ "Day",
      TRUE                                              ~ "Night"
    ),
    time_of_day = factor(time_of_day, levels = c("Day", "Night"))
  ) %>%
  ungroup() %>%
  select(-sunrise, -sunset)

# Create community matrix -------------------------------------------------

AHC_metadata_cols <- c(
  "project", "year", "cruise", "collection_date", "start_time_pt",
  "transect", "replicate", "station", "net",
  "transect_station_rep_year_net", "transect_station_rep_year",
  "start_longitude_dd", "start_latitude_dd",
  "maximum_depth_m", "minimum_depth_m", "depth_mean_m", "depth_diff_m",
  "volume_best_m3_both_sides",
  "mean_temperature_c", "mean_salinity_psu", "mean_density_kgm3",
  "seafloor_depth_m", "distance_to_shore_km", "shelf_position",
  "prey_zooplankton_abundance_ind_m3", "dissolved_oxygen_ml_l",
  "mean_chl_0_100_m_mgm3", "chrono_sample_ID"
)

taxa_cols <- setdiff(names(wide_major_taxa_nets), AHC_metadata_cols)

AHC_comm_matrix <- wide_major_taxa_nets %>%
  select(transect_station_rep_year_net, chrono_sample_ID, depth_mean_m, all_of(taxa_cols))

transform_taxa_concentrations <- AHC_comm_matrix[, taxa_cols] %>%
  sqrt()

empty_comm_rows <- rowSums(AHC_comm_matrix[, taxa_cols], na.rm = TRUE) == 0
if (any(empty_comm_rows)) {
  stop(
    "Community matrix has zero-abundance rows after taxon selection: ",
    paste(AHC_comm_matrix$transect_station_rep_year_net[empty_comm_rows], collapse = ", ")
  )
}

# Add rownames
row.names(transform_taxa_concentrations) <- AHC_comm_matrix$transect_station_rep_year_net

AHC_comm_matrix_transformed <- AHC_comm_matrix[1:2] %>%
  bind_cols(.,transform_taxa_concentrations)

# Count matrix for Dexter et al. (2018) NMDS stress null model
# The quasiswap_count null model preserves row totals, taxon totals, and zeros
wide_major_taxa_counts_nets <- mocness_major_taxa_nets %>%
  filter(!is.na(individuals_in_tow)) %>%
  mutate(individuals_in_tow = as.integer(round(individuals_in_tow))) %>%
  select(transect_station_rep_year_net, taxon, individuals_in_tow) %>%
  pivot_wider(names_from = taxon,
              values_from = individuals_in_tow,
              values_fill = 0,
              values_fn = sum)

AHC_count_abundances <- wide_major_taxa_counts_nets %>%
  select(transect_station_rep_year_net, all_of(taxa_cols))

# Reorder count matrix to match original community matrix
AHC_count_abundances <- AHC_count_abundances[match(AHC_comm_matrix$transect_station_rep_year_net,
                                                   AHC_count_abundances$transect_station_rep_year_net),]

stopifnot(all(AHC_count_abundances$transect_station_rep_year_net ==
                AHC_comm_matrix$transect_station_rep_year_net))

AHC_sample_volumes <- wide_major_taxa_nets %>%
  select(transect_station_rep_year_net, volume_best_m3_both_sides) %>%
  distinct()

AHC_sample_volumes <- AHC_sample_volumes[match(AHC_comm_matrix$transect_station_rep_year_net,
                                               AHC_sample_volumes$transect_station_rep_year_net),]

stopifnot(all(AHC_sample_volumes$transect_station_rep_year_net ==
                AHC_comm_matrix$transect_station_rep_year_net))
stopifnot(all(!is.na(AHC_sample_volumes$volume_best_m3_both_sides)))
stopifnot(all(AHC_sample_volumes$volume_best_m3_both_sides > 0))

AHC_sample_volumes <- AHC_sample_volumes$volume_best_m3_both_sides

AHC_count_abundances <- AHC_count_abundances %>%
  select(all_of(taxa_cols)) %>%
  as.matrix()
rownames(AHC_count_abundances) <- AHC_comm_matrix$transect_station_rep_year_net
storage.mode(AHC_count_abundances) <- "integer"


# Calculate dissimilarity matrix ------------------------------------------

dissim_matrix <- vegdist(transform_taxa_concentrations, method = "bray")


# Perform agglomerative hierarchical clustering ---------------------------

AHC_result <- hclust(dissim_matrix, method = "average")

## Assign cluster colors
cluster_colors <- c("1" = "#e6ab02", "2" = "#1b9e77", "3" = "#e7298a", "4" = "#d95f02", "5" = "#66a61e",
                    "6" = "#a6761d", "7" = "#7570b3")
cluster_levels <- as.character(seq_len(7))
dendrogram_clusters <- cutree(AHC_result, k = 7)
dendrogram_cluster_order <- unique(dendrogram_clusters[AHC_result$order])
dendrogram_cluster_colors <- cluster_colors[as.character(dendrogram_cluster_order)]

# Plot the dendrograms -----------------------------------------------------

##plot k clusters/rectangles
windows()
plot(AHC_result, labels = AHC_comm_matrix_transformed$chrono_sample_ID,
     xlab = "Net tows", main = "Clusters of Net Tows", cex = 0.4)
rect.hclust(AHC_result, k = 7, border = dendrogram_cluster_colors)

png(filename = here("output/AHC_sampling_events_dendrogram.png"),
    width = 12,
    height = 6,
    units = "in",
    res = 300)
plot(AHC_result, labels = AHC_comm_matrix_transformed$chrono_sample_ID,
     xlab = "Net tows", main = "Clusters of Net Tows", cex = 0.4)
rect.hclust(AHC_result, k = 7, border = dendrogram_cluster_colors)
dev.off()

# Extract list of sampling events belonging to each cluster
clusters <- data.frame(transect_station_rep_year_net = names(dendrogram_clusters),
                       cluster = dendrogram_clusters)

main_clusters <- clusters %>%
  count(cluster, name = "n_net_tows") %>%
  arrange(desc(n_net_tows), cluster) %>%
  slice_head(n = 4) %>%
  pull(cluster)

# Indicator Species Analysis ----------------------------------------------

comm_for_isa <- AHC_comm_matrix_transformed %>%
  select(all_of(taxa_cols)) %>%
  as.data.frame()

clusters_for_isa <- as.factor(clusters$cluster)

isa_result <- multipatt(comm_for_isa, clusters_for_isa, func = "IndVal.g", max.order = 3)
summary(isa_result)

# Map points in space by cluster and net ----------------------------------

## Make data frame
mapping_df <- wide_major_taxa_nets %>%
  left_join(clusters, by = "transect_station_rep_year_net") %>%
  select(transect_station_rep_year_net, chrono_sample_ID, start_longitude_dd, 
         start_latitude_dd, cluster, net, cruise, depth_mean_m,
         transect_station_rep_year) %>%
  distinct(transect_station_rep_year_net, chrono_sample_ID, start_longitude_dd, 
         start_latitude_dd, cluster, net, cruise, depth_mean_m,
         transect_station_rep_year, .keep_all = TRUE)

mapping_df$cluster <- factor(mapping_df$cluster, levels = cluster_levels)
mapping_df$net <- factor(mapping_df$net, levels = 0:4)

net0_coordinates <- mocness_clean %>%
  filter(net == 0) %>%
  group_by(transect_station_rep_year) %>%
  summarize(
    net0_longitude_dd = first(start_longitude_dd[!is.na(start_longitude_dd)], default = NA_real_),
    net0_latitude_dd = first(start_latitude_dd[!is.na(start_latitude_dd)], default = NA_real_),
    .groups = "drop"
  )

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
  error = function(e) ne_download(scale = "medium", type = "land", category = "physical", returnclass = "sf")
) %>%
  st_crop(map_bbox)

coast <- tryCatch(
  ne_download(scale = "large", type = "coastline", category = "physical", returnclass = "sf"),
  error = function(e) ne_download(scale = "medium", type = "coastline", category = "physical", returnclass = "sf")
) %>%
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
mapping_df2 <- mapping_df %>% 
  left_join(net0_coordinates, by = "transect_station_rep_year") %>%
  left_join(offsets, by = "net") %>%
  mutate(plot_longitude_dd = coalesce(net0_longitude_dd, start_longitude_dd),
         plot_latitude_dd = coalesce(net0_latitude_dd, start_latitude_dd)) %>%
  mutate(year = case_when(cruise == "W18" ~ 2018, cruise == "W19" ~ 2019, cruise == "W22" ~ 2022, cruise == "W23" ~ 2023),
         rep = str_split(transect_station_rep_year_net, "_", simplify = TRUE)[,3],
         facet_group = case_when(cruise == "W18" ~ paste0("18", rep), cruise == "W19" ~ paste0("19", rep),
                                 cruise == "W22" ~ "22", cruise == "W23" ~ "23")) %>%
  arrange(net)
stopifnot(all(as.character(mapping_df2$cluster) %in% names(cluster_colors)))

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
  scale_color_manual(values = cluster_colors,
                     limits = cluster_levels,
                     breaks = cluster_levels,
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

cluster_map_legend <- get_legend(
  ggplot() +
    geom_point(
      data = tibble(cluster = factor(cluster_levels, levels = cluster_levels),
                    x = 1, y = seq_along(cluster_levels)),
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
    scale_color_manual(values = cluster_colors,
                       limits = cluster_levels,
                       breaks = cluster_levels,
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

p18a <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "18MaN"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(mapping_df2, facet_group == "18MaN"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "18MaN", x = NULL, y = NULL)

p18b <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "18MaD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(mapping_df2, facet_group == "18MaD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "18MaD", x = NULL, y = NULL)

p18c <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "18MbD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(mapping_df2, facet_group == "18MbD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "18MbD", x = NULL, y = NULL)

p19a <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "19MaN"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(mapping_df2, facet_group == "19MaN"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "19MaN", x = NULL, y = NULL)

p19b <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "19MaD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(mapping_df2, facet_group == "19MaD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "19MaD", x = NULL, y = NULL)

p19c <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "19MbN"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(mapping_df2, facet_group == "19MbN"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "19MbN", x = NULL, y = NULL)

p19d <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "19MbD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(mapping_df2, facet_group == "19MbD"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "19MbD", x = NULL, y = NULL)

p22 <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "22"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(mapping_df2, facet_group == "22"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "22", x = NULL, y = NULL)

p23 <- ggplot() + map_layers +
  geom_point(data = filter(excluded_df, facet_group == "23"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
             shape = 4, color = "black", size = 2, stroke = 0.7) +
  geom_point(data = filter(mapping_df2, facet_group == "23"),
             aes(plot_longitude_dd + dx, plot_latitude_dd + dy, color = cluster, alpha = net),
             size = 1.2) +
  labs(title = "23", x = NULL, y = NULL)

## Make layout panels for 2018 and 2019
p2018 <- (p18a | p18b) / p18c +
  plot_layout(heights = c(1, 1))
p2019 <- ((p19a | p19b) /
          (p19c | p19d))

## Assemble custom layout
final_cluster_map <- (p2018 | p2019 | p22 | p23 | wrap_elements(cluster_map_legend)) +
  plot_layout(widths = c(1, 1, 1.4, 1.4, 0.35))
final_cluster_map
  #save
ggsave("cluster_map.png", plot = get_last_plot(), path = here("output"), 
       width = 15, height = 10, units = "in", dpi = 300)

# # Adjust the values of width and height to change the size of the saved figure
# ggsave("cluster_map.tif", plot = get_last_plot(), path = here("output"),
#   width = 6.5, height = 4, units = "in", dpi = 300)


#try 3D mapping for nets by depth instead of jittering (code from Copilot so may need adjustment)
#extract coastline
west_coast_bbox <- st_bbox(c(xmin = -127,
                             xmax = -123,
                             ymin = 40,
                             ymax = 48), crs = st_crs(space))

west_coast <- st_crop(space, west_coast_bbox)

coast_list <- west_coast %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  st_coordinates() %>%
  as.data.frame() %>%
  group_split(L1, L2)   # L1 = polygon, L2 = ring

#prepare data
depth_scale <- 0.05   # compress depth to 5% of original range

mapping_df <- mapping_df %>%
  mutate(z_depth = depth_mean_m * depth_scale + as.numeric(net) * 0.1)


# Map net numbers to shapes
net_shapes <- c("circle", "square", "diamond", "triangle-up", "triangle-down")

p <- plot_ly()

# Coastline (black, no legend)
for (seg in coast_list) {p <- p %>%
  add_trace(x = c(seg$X, NA),
            y = c(seg$Y, NA),
            z = c(rep(0, nrow(seg)), NA),
            type = "scatter3d",
            mode = "lines",
            line = list(color = "black", width = 3),
            showlegend = FALSE,
            hoverinfo = "none")}

# Sampling points (this is the ONLY trace that needs legend info)
p <- p %>%
  add_trace(data = mapping_df,
            x = ~start_longitude_dd,
            y = ~start_latitude_dd,
            z = ~z_depth,
            color = ~cluster,
            colors = cluster_colors,
            symbol = ~factor(net),
            symbols = net_shapes,
            type = "scatter3d",
            mode = "markers",
            marker = list(size = 4, opacity = 0.95, color = "rgba(0,0,0,0)", line = list(width = 2, color = cluster_colors)),
            legendgroup = "samples",
            showlegend = TRUE) %>%
  layout(scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Scaled Depth", autorange = "reversed"),
                      aspectmode = "data",
                      camera = list(eye = list(x = -3.0, y = 0.2, z = 0.6))),
         legend = list(orientation = "v", x = 1.05, y = 1))

p


# Plot abundance of each taxon, grouped by cluster ------------------------

# Add cluster identities and chronological sample IDs
AHC_comm_matrix_transformed_long <- AHC_comm_matrix_transformed %>%
  pivot_longer(cols = all_of(taxa_cols), names_to = "taxon", values_to = "sqrt_concentration") %>%
  merge(., clusters, by = "transect_station_rep_year_net") %>%
  arrange(cluster) %>%
  mutate(chrono_sample_ID = factor(chrono_sample_ID, levels = unique(chrono_sample_ID)))

# Compute cluster bounds to use as vertical separators on barplot
cluster_bounds <- AHC_comm_matrix_transformed_long %>%
  distinct(cluster, chrono_sample_ID) %>%
  mutate(chrono_sample_ID = as.numeric(chrono_sample_ID)) %>%
  group_by(cluster) %>%
  summarize(start = min(chrono_sample_ID), end   = max(chrono_sample_ID), .groups = "drop")

bar_heights <- AHC_comm_matrix_transformed_long %>%
  group_by(chrono_sample_ID) %>%
  summarize(total_height = sum(sqrt_concentration), .groups = "drop")

max_height <- max(bar_heights$total_height)

# Plot by transect_station_rep_year, sorted by cluster
windows()
clust_abun_bar_plot <- ggplot(AHC_comm_matrix_transformed_long, aes(x = chrono_sample_ID, y = sqrt_concentration, fill = factor(taxon, levels = ordered_taxa))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors, breaks = ordered_taxa, name = "Taxonomic group") +
  geom_vline(data = cluster_bounds[-1,],
             aes(xintercept = start - 0.5), linetype = "dashed", color = "gray40", linewidth = 0.5, inherit.aes = FALSE) +
  annotate("text", x = mean(range(as.numeric(AHC_comm_matrix_transformed_long$chrono_sample_ID))), y = Inf,
           label = "Cluster", vjust = -2, size = 4) +
  annotate("text", x = (cluster_bounds$start + cluster_bounds$end) / 2, y = Inf,
           label = paste(cluster_bounds$cluster), vjust = -1, size = 3) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Sample ID", y = "Concentration (ind./m^3)") +
  guides(fill = guide_legend(ncol = 1)) +
  theme_light() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 35, r = 30, b = 5, l = 5),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 5))
## Extract legend
legend_only <- cowplot::get_legend(clust_abun_bar_plot)
### Wrap legend in a ggplot so ggsave works
legend_plot <- cowplot::ggdraw(legend_only)
ggsave(filename = "barplot_taxa_legend.png",
  plot = legend_plot,
  path = here("output"),
  width = 2, height = 6, dpi = 300)
## Plot without legend
clust_abun_bar_plot_no_legend <- clust_abun_bar_plot + theme(legend.position = "none")
ggsave("clusters_abundance_bar_plot.png", plot = get_last_plot(), path = here("output"),
       width = 10, height = 5, units = "in", dpi = 300)


# Plot same but only for the four clusters with the most net tows
major_clusters_plot_df <- AHC_comm_matrix_transformed_long %>%
  filter(cluster %in% main_clusters) %>%
  mutate(chrono_sample_ID = factor(chrono_sample_ID, levels = unique(chrono_sample_ID)))

major_clusters_bounds <- major_clusters_plot_df %>% 
  distinct(cluster, chrono_sample_ID) %>%
  mutate(chrono_sample_ID = as.numeric(chrono_sample_ID)) %>%
  group_by(cluster) %>%
  summarize(start = min(chrono_sample_ID), end = max(chrono_sample_ID), .groups = "drop")

major_clusters_max_height <- major_clusters_plot_df %>%
  group_by(chrono_sample_ID) %>%
  summarize(total_height = sum(sqrt_concentration), .groups = "drop") %>%
  pull(total_height) %>%
  max()

windows()
ggplot(major_clusters_plot_df, aes(x = chrono_sample_ID, y = sqrt_concentration, fill = factor(taxon, levels = ordered_taxa))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = species_colors, breaks = ordered_taxa, name = "Taxonomic group") +
  geom_vline(data = major_clusters_bounds[-1,],
    aes(xintercept = start - 0.5), linetype = "dashed", color = "gray40", linewidth = 0.5, inherit.aes = FALSE) +
  annotate("text", x = mean(range(as.numeric(major_clusters_plot_df$chrono_sample_ID))), y = Inf, 
           label = "Cluster", vjust = -2, size = 4) +
  annotate("text", x = (major_clusters_bounds$start + major_clusters_bounds$end) / 2, y = Inf,
           label = major_clusters_bounds$cluster, vjust = -1, size = 3) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Sample ID", y = "Concentration (ind./m^3)") +
  theme_light() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 35, r = 30, b = 0, l = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6))



# ggplot(AHC_comm_matrix_transformed_long %>%
#          dplyr::filter(cluster %in% main_clusters),
#        aes(x = chrono_sample_ID, y = sqrt_concentration, fill = factor(taxon, levels = ordered_taxa))) +
# geom_bar(stat = "identity", position = "stack") +
# scale_fill_manual(values = species_colors, breaks = ordered_taxa) +
# facet_grid(rows = vars(cluster)) +
# labs(x = "Depth sampled (m)", y = "individuals/m3") +
# theme_light() +
# theme(axis.text.x = element_text(angle = 45, hjust = 0), legend.position = "none")

# Plot NMDS ordination ---------------------------------------------------

set.seed(123)
NMDS_result <- metaMDS(dissim_matrix, distance = "bray", k = 2, try = 20, trymax = 20, engine = "monoMDS")
NMDS_result$stress  ##check stress

stressplot(NMDS_result)   ##Shepard diagram


# Test NMDS stress against a Dexter et al. (2018) null model --------------

n_stress_permutations <- 1000
stress_nmds_try <- 20
stress_nmds_trymax <- 20
stress_progress_every <- 10
stress_fit_counter <- 0

nmds_stress_statistic <- function(comm, sample_volumes = AHC_sample_volumes) {
  stress_fit_counter <<- stress_fit_counter + 1

  if (stress_fit_counter == 1) {
    message("Fitting observed NMDS stress for the null-model pipeline")
  } else if ((stress_fit_counter - 1) %% stress_progress_every == 0 ||
             (stress_fit_counter - 1) == n_stress_permutations) {
    message("Completed ", stress_fit_counter - 1, " of ",
            n_stress_permutations, " null NMDS stress fits")
  }

  comm_concentrations <- sweep(as.matrix(comm), 1, sample_volumes, "/")
  comm_transformed <- sqrt(comm_concentrations)

  list(statistic = c(stress = metaMDS(
    comm_transformed,
    distance = "bray",
    k = 2,
    try = stress_nmds_try,
    trymax = stress_nmds_trymax,
    engine = "monoMDS",
    autotransform = FALSE,
    trace = FALSE)$stress)
  )
}

set.seed(123)
NMDS_stress_null_test <- oecosimu(AHC_count_abundances,
                                  nmds_stress_statistic,
                                  method = "quasiswap_count",
                                  nsimul = n_stress_permutations,
                                  alternative = "two.sided")

stress_null_values <- as.numeric(NMDS_stress_null_test$oecosimu$simulated)
stress_observed <- as.numeric(NMDS_stress_null_test$oecosimu$statistic)
stress_null_z <- (stress_observed - mean(stress_null_values, na.rm = TRUE)) /
  sd(stress_null_values, na.rm = TRUE)
stress_null_p <- 2 * pnorm(-abs(stress_null_z))

stress_null_distribution <- tibble(iteration = seq_along(stress_null_values),
                                   null_stress = stress_null_values)

stress_null_summary <- tibble(observed_stress_null_pipeline = stress_observed,
                              observed_stress_sqrt_concentration_nmds = NMDS_result$stress,
                              null_mean_stress = mean(stress_null_values, na.rm = TRUE),
                              null_sd_stress = sd(stress_null_values, na.rm = TRUE),
                              null_stress_q025 = quantile(stress_null_values, 0.025, na.rm = TRUE),
                              null_stress_q975 = quantile(stress_null_values, 0.975, na.rm = TRUE),
                              z = stress_null_z,
                              p_value_two_tailed = stress_null_p,
                              n_permutations = length(stress_null_values),
                              null_model = "quasiswap_count")

write.csv(stress_null_summary,
          here("output/NMDS_stress_null_test_summary.csv"),
          row.names = FALSE)

write.csv(stress_null_distribution,
          here("output/NMDS_stress_null_distribution.csv"),
          row.names = FALSE)

ggplot(stress_null_distribution, aes(x = null_stress)) +
  geom_histogram(bins = 30, fill = "grey70", color = "white") +
  geom_vline(xintercept = stress_observed, linetype = 2, linewidth = 1,
             color = "red3") +
  theme_classic() +
  labs(
    title = "NMDS stress compared with constrained null communities",
    subtitle = paste0(
      "Dexter et al. null model: z = ",
      round(stress_null_z, 2),
      ", p = ",
      signif(stress_null_p, 3)
    ),
    x = "Null stress",
    y = "Permuted communities"
  )

ggsave(
  "NMDS_stress_null_distribution.png",
  plot = get_last_plot(),
  path = here("output"),
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

site_scores <- as.data.frame(scores(NMDS_result, display = "sites"))
cluster_groups <- cutree(AHC_result, k = 9)
station_scores <- mutate(site_scores, transect_station_rep_year_net = AHC_comm_matrix_transformed$transect_station_rep_year_net)
stations_clustered <- mutate(station_scores, cluster = cluster_groups)
stations_clustered$cluster <- as.numeric(as.character(stations_clustered$cluster))
stations_clustered$cluster <- factor(stations_clustered$cluster, levels = c(1,2,3,4,5,6,7,8,9))

hulls <- stations_clustered %>%
  group_by(cluster) %>%
  slice(chull(NMDS1, NMDS2))

ggplot(stations_clustered, aes(x = NMDS1, y = NMDS2)) +
  geom_polygon(data = hulls, aes(fill = cluster, group = cluster), alpha = 0.25, color = NA) +
  geom_point(aes(color = cluster), size = 1) +
  scale_fill_manual(values = cluster_colors) +
  scale_color_manual(values = cluster_colors) +
  theme_classic() +
  labs(title = "NMDS Ordination of sampling events by LFC", x = "NMDS1", y = "NMDS2")
ggsave("NMDS_all_clusters.png", plot = get_last_plot(), path = here("output"),
       width = 6, height = 5, units = "in", dpi = 300)

# overlays for NMDS plots -------------------------------------------------

#Vectors
env_wide_aligned <- env_wide[match(rownames(scores(NMDS_result, display = "sites")),
                                   env_wide$transect_station_rep_year_net), ]
year_mat <- model.matrix(~ factor(year) - 1, data = env_wide_aligned)
colnames(year_mat) <- paste0("year_", levels(factor(env_wide_aligned$year)))
env_numeric <- env_wide_aligned[, sapply(env_wide_aligned, is.numeric)]
env_numeric2 <- cbind(env_numeric[, !names(env_numeric) %in% "year"], year_mat)
fit_vectors <- envfit(NMDS_result, env_numeric2, permutations = 1000, na.rm = TRUE)

##Extract vector scores for plotting
vector_scores <- scores(fit_vectors, display = "vectors")
vector_df <- as.data.frame(vector_scores) %>% 
  mutate(variable = rownames(vector_scores)) %>% 
  filter(grepl("^year_", variable) | variable %in% c("start_latitude_dd", "depth_mean_m", "seafloor_depth_m")
  )

#Ellipses
##fit ellipses
# ell_shelf <- ordiellipse(NMDS_result, env_wide_aligned$shelf_position,
#                          kind = "sd", conf = 0.95, draw = "none") 

time_groups <- env_wide_aligned$time_of_day
ell_time <- ordiellipse(NMDS_result, time_groups, kind = "sd", 
                        conf = 0.95,  draw = "none")

##convert outputs to data frames
# ell_shelf_df <- purrr::map_dfr(names(ell_shelf), ~ {
#   e     <- ell_shelf[[.x]]
#   theta <- seq(0, 2 * pi, length.out = 200)
#   circle <- cbind(cos(theta), sin(theta))
#   # one ellipse per group: center + scale * chol(cov) %*% circle
#   xy <- circle %*% chol(e$cov)
#   xy <- sweep(xy * e$scale, 2, e$center, "+")
#   dplyr::tibble(
#     NMDS1 = xy[, 1],
#     NMDS2 = xy[, 2],
#     group = .x)})

ell_time_df <- purrr::map_dfr(names(ell_time), ~ {
  e     <- ell_time[[.x]]
  theta <- seq(0, 2 * pi, length.out = 200)
  circle <- cbind(cos(theta), sin(theta))
  xy <- circle %*% chol(e$cov)
  xy <- sweep(xy * e$scale, 2, e$center, "+")
  tibble(
    NMDS1 = xy[, 1],
    NMDS2 = xy[, 2],
    group = .x)})


#Plot NMDS with overlays
windows()
ggplot(stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  #1 Polygons (cluster color)
  geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, fill = cluster, group = cluster), 
               alpha = 0.25, color = NA, inherit.aes = FALSE) +
  scale_fill_manual(values = cluster_colors) +
  #2 Points (cluster colors)
  geom_point(size = 1) +
  scale_color_manual(values = cluster_colors) +
  # #3 Ellipses - Shelf Position
  # geom_path(data = ell_shelf_df, aes(x = NMDS1, y = NMDS2, color = group), 
  #           size = 1, inherit.aes = FALSE) +
  # scale_color_manual(name = "Shelf position", values = c("shelf" = "#1f78b4", "offshore" = "#e31a1c")) +
  # new_scale_color() +
  #4 Ellipses - Day/Night
  geom_path(data = ell_time_df, aes(x = NMDS1, y = NMDS2, linetype = group), 
            size = 0.5, color = "black", inherit.aes = FALSE) +
  scale_linetype_manual(name = "Time of Day", values = c("Day" = "solid", "Night" = "dashed")) +
  #5 Vectors
  geom_segment(data = vector_df, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  geom_text(data = vector_df, aes(x = NMDS1, y = NMDS2, label = variable), 
            color = "black", size = 2, vjust = -0.5,inherit.aes = FALSE) +
  labs(title = "NMDS ordination with clustered points and covariate overlays", x = "NMDS1", y = "NMDS2",
       color = "Cluster", fill = "Cluster", linetype = "Time of Day") + 
theme_classic()
ggsave("NMDS_overlays_all_clusters.png", plot = get_last_plot(), path = here("output"),
       width = 7, height = 5, units = "in", dpi = 300)


