
# Description -------------------------------------------------------------

# Conduct a cluster analysis of sampling events by LFC, plot dendrograms to decide number
#  of clusters. Plot sampling events by cluster on a map of the coast, jittering by depth. 
#  Run an NMDS, and plot the NMDS ordination with overlays for covariates.
#  Perform an indicator taxa analysis on the clustered community matrix.


# Load packages -----------------------------------------------------------

library(here)
library(vegan)
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


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Create wide environmental dataframe ---------------------------------------------------

wide_major_taxa_nets <- mocness_major_taxa_nets %>%
  # Removing NAs for now, but there shouldn't be any to begin with
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
  # For some reason, MOC 1 and MOC 4 have different values of mean_temperature_c, mean_salinity_psu, and mean_density_kgm3 in 6 cases. To eliminate differences, calculate mean
  group_by(transect_station_rep_year_net) %>%
  mutate(mean_temperature_c = mean(mean_temperature_c),
         mean_salinity_psu = mean(mean_salinity_psu),
         mean_density_kgm3 = mean(mean_density_kgm3)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_per_m3, values_fill = 0)

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
# removed mlotst for right now because all are NAs at the moment and I don't want this to cause errors down the line
# also excluded redundant information like transect, transect_station, transect_station_rep, and so on
## 04/13 RM : added mlotst back in now. kept redundant information out 

# Create community matrix -------------------------------------------------

AHC_comm_matrix <- wide_major_taxa_nets %>%
  select(transect_station_rep_year_net, depth_mean_m, 29:50)

transform_taxa_concentrations <- AHC_comm_matrix[, 3:24] %>%
  sqrt()

# Add rownames
row.names(transform_taxa_concentrations) <- AHC_comm_matrix$transect_station_rep_year_net

AHC_comm_matrix_transformed <- AHC_comm_matrix[,1] %>%
  bind_cols(.,transform_taxa_concentrations)


# Calculate dissimilarity matrix ------------------------------------------

dissim_matrix <- vegdist(transform_taxa_concentrations, method = "bray")


# Perform agglomerative hierarchical clustering ---------------------------

AHC_result <- hclust(dissim_matrix, method = "average")


# Plot the dendrograms -----------------------------------------------------

# plot 2 clusters/rectangles
#plot(AHC_result, labels = AHC_comm_matrix_transformed$transect_station_rep_year_net, main = "average linkage AHC of sampling events by LFC")
#rect.hclust(AHC_result, k = 2, border = c(2, 4))
## 04/21 RM : I don't think rhis plot is needed anymore 

##plot k clusters/rectangles
windows()
plot(AHC_result, labels = AHC_comm_matrix_transformed$transect_station_rep_year_net, main = "average linkage AHC of sampling events by LFC")
rect.hclust(AHC_result, k = 10, border = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11))

# Extract list of sampling events belonging to each cluster
clusters <- data.frame(transect_station_rep_year_net = names(cutree(AHC_result, k = 10)),
                       cluster = cutree(AHC_result, k = 10))


# Map points in space by cluster and net ----------------------------------

mapping_df <- wide_major_taxa_nets %>%
  left_join(clusters, by = "transect_station_rep_year_net") %>%
  select(transect_station_rep_year_net, start_longitude_dd, start_latitude_dd, cluster, net, cruise, depth_mean_m) %>%
distinct(transect_station_rep_year_net, start_longitude_dd, start_latitude_dd, cluster, net, cruise, depth_mean_m, .keep_all = TRUE)
mapping_df$cluster <- factor(mapping_df$cluster)

cluster_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#8A2BE2", "#00CED1", "#FF1493")

space <- ne_countries(scale = "medium", returnclass = "sf")

offsets <- tibble(net = factor(0:4),
                  dx = c(0, 0.06, -0.06, 0, 0),
                  dy = c(0, 0, 0, 0.07, -0.07))

mapping_df2 <- mapping_df %>%
  left_join(offsets, by = "net")

ggplot() +
  geom_sf(data = space, fill = "grey90", color = "grey40") +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) + 
  geom_point(data = mapping_df2,
    aes(x = start_longitude_dd+dx, y = start_latitude_dd+dy, 
        color = cluster, shape = factor(net)),
    size = 1, alpha = 0.95) +
  coord_sf(xlim = c(-127, -123), ylim = c(40, 48), expand = FALSE) +
  scale_color_manual(values = cluster_colors) +
  facet_grid(cols = vars(cruise))
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

# Add cluster identities to long version of AHC_comm_matrix_transformed
AHC_comm_matrix_transformed_long <- AHC_comm_matrix_transformed %>%
  pivot_longer(cols = 2:23, names_to = "taxon", values_to = "sqrt_concentration") %>%
  merge(., clusters, by = "transect_station_rep_year_net")

# Plot by transect_station_rep_year, sorted by cluster
windows()
ggplot(AHC_comm_matrix_transformed_long, aes(x = transect_station_rep_year_net, y = sqrt_concentration, fill = factor(taxon, levels = ordered_taxa))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors, breaks = ordered_taxa) +
  facet_grid(rows = vars(cluster)) +
  labs(x = "Depth sampled (m)", y = "individuals/m3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##not sure if my adjustments here to account for standardizing counts by volume were correct and/or needed

# Plot same but only for clusters 1, 2, 3, and 5
windows()
ggplot(AHC_comm_matrix_transformed_long %>% 
         dplyr::filter(cluster %in% c(1, 2, 3, 5)),
       aes(x = transect_station_rep_year_net, y = sqrt_concentration, fill = factor(taxon, levels = ordered_taxa))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors, breaks = ordered_taxa) +
  facet_grid(rows = vars(cluster)) +
  labs(x = "Depth sampled (m)", y = "individuals/m3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Plot NMDS ordination ---------------------------------------------------

NMDS_result <- metaMDS(dissim_matrix, distance = "bray", k = 2, try = 20, trymax = 20, engine = "monoMDS")
NMDS_result$stress  ##check stress

stressplot(NMDS_result)   ##Shepard diagram

site_scores <- as.data.frame(scores(NMDS_result, display = "sites"))
cluster_groups <- cutree(AHC_result, k = 10)
station_scores <- mutate(site_scores, transect_station_rep_year_net = AHC_comm_matrix_transformed$transect_station_rep_year_net)
stations_clustered <- mutate(station_scores, cluster = cluster_groups)
stations_clustered$cluster <- as.numeric(as.character(stations_clustered$cluster))
stations_clustered$cluster <- factor(stations_clustered$cluster, levels = c(1,2,3,4,5,6,7,8,9,10), 
                                     labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9", "Cluster 10"))

hulls <- stations_clustered %>%
  group_by(cluster) %>%
  slice(chull(NMDS1, NMDS2))

ggplot(stations_clustered, aes(x = NMDS1, y = NMDS2)) +
  geom_polygon(data = hulls, aes(fill = cluster, group = cluster), alpha = 0.25, color = NA) +
  geom_point(aes(color = cluster), size = 3) +
  scale_fill_manual(values = cluster_colors) +
  scale_color_manual(values = cluster_colors) +
  theme_classic() +
  labs(title = "NMDS Ordination of sampling events by LFC", x = "NMDS1", y = "NMDS2")


# overlays for NMDS plots -------------------------------------------------

#Vectors
env_wide_aligned <- env_wide[match(rownames(scores(NMDS_result, display = "sites")),
                                   env_wide$transect_station_rep_year_net), ]
env_numeric <- env_wide_aligned[, sapply(env_wide_aligned, is.numeric)]
fit_vectors<- envfit(NMDS_result, env_numeric, permutations = 1000, na.rm = TRUE)

##Extract vector scores for plotting
vector_scores <- scores(fit_vectors, display = "vectors")
vector_df <- as.data.frame(vector_scores) %>% 
  mutate(variable = rownames(vector_scores)) %>% 
  filter(variable %in% c("year", "start_longitude_dd", "start_latitude_dd", "depth_mean_m", "seafloor_depth_m", "distance_to_shore_km"))

#Ellipses
##fit ellipses
ell_shelf <- ordiellipse(NMDS_result, env_wide_aligned$shelf_position,
                         kind = "sd", conf = 0.95, draw = "none") 

time_groups <- env_wide_aligned$time_of_day
ell_time <- ordiellipse(NMDS_result, time_groups, kind = "sd", 
                        conf = 0.95,  draw = "none")

##convert outputs to data frames
ell_shelf_df <- purrr::map_dfr(names(ell_shelf), ~ {
  e     <- ell_shelf[[.x]]
  theta <- seq(0, 2 * pi, length.out = 200)
  circle <- cbind(cos(theta), sin(theta))
  # one ellipse per group: center + scale * chol(cov) %*% circle
  xy <- circle %*% chol(e$cov)
  xy <- sweep(xy * e$scale, 2, e$center, "+")
  dplyr::tibble(
    NMDS1 = xy[, 1],
    NMDS2 = xy[, 2],
    group = .x)})

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
  geom_point(size = 3) +
  scale_color_manual(values = cluster_colors) +
  new_scale_color() +
  #3 Ellipses - Shelf Position
  geom_path(data = ell_shelf_df, aes(x = NMDS1, y = NMDS2, color = group), 
            size = 1, inherit.aes = FALSE) +
  scale_color_manual(name = "Shelf position", values = c("shelf" = "#1f78b4", "offshore" = "#e31a1c")) +
  new_scale_color() +
  #4 Ellipses - Day/Night
  geom_path(data = ell_time_df, aes(x = NMDS1, y = NMDS2, color = group), 
            size = 1, linetype = 2, inherit.aes = FALSE) +
  scale_color_manual(name = "Day/Night", values = c("Day" = "#33a02c", "Night" = "#ff7f00")) +
  #5 Vectors
  geom_segment(data = vector_df, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "black", linewidth = 1, inherit.aes = FALSE) +
  geom_text(data = vector_df, aes(x = NMDS1, y = NMDS2, label = variable), 
            color = "black", size = 3, vjust = -0.5,inherit.aes = FALSE) +
  theme_classic() +
  labs(title = "NMDS ordination with clustered points and covariate overlays", x = "NMDS1", y = "NMDS2")


# Indicator Species Analysis ----------------------------------------------

comm_for_isa <- AHC_comm_matrix_transformed %>%
  select(where(is.numeric)) %>%
  as.data.frame()

clusters_for_isa <- as.factor(clusters$cluster)

isa_result <- multipatt(comm_for_isa, clusters_for_isa, func = "IndVal.g", max.order = 2)
summary(isa_result)
