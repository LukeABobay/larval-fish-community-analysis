
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
library(ggpubr)

# Source code -------------------------------------------------------------

source(here("scripts/02_prepare_community_data.R"))

# Plot the dendrograms -----------------------------------------------------

##plot k clusters/rectangles
windows()
plot(AHC_result, labels = AHC_comm_matrix_transformed$chrono_sample_ID,
     xlab = "Sample", cex = 0.4)
rect.hclust(AHC_result, k = 7, border = dendrogram_cluster_colors)

png(filename = here("output/AHC_sampling_events_dendrogram.png"),
    width = 12,
    height = 6,
    units = "in",
    res = 300)
plot(AHC_result, labels = AHC_comm_matrix_transformed$chrono_sample_ID,
     xlab = "Sample", cex = 0.4)
rect.hclust(AHC_result, k = 7, border = dendrogram_cluster_colors)
dev.off()

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
         transect_station_rep_year, .keep_all = TRUE) %>%
  mutate(mid_tow_depth_m = depth_mean_m)

mapping_df$cluster <- factor(mapping_df$cluster, levels = cluster_levels)
mapping_df$net <- factor(mapping_df$net, levels = 0:4)

net0_coordinates <- mocness_clean %>%
  filter(net == 4) %>%
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
  filter(net != 0) %>%
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
  filter(net != 0) %>%
  arrange(net)

## Assign point translucence by mid-tow depth, while keeping position offsets by net
depth_alpha_range <- c(0.3, 0.95)
depth_alpha_limits <- range(mapping_df2$mid_tow_depth_m, na.rm = TRUE)
mapping_df2 <- mapping_df2 %>%
  mutate(mid_tow_depth_alpha = scales::rescale(
    mid_tow_depth_m,
    to = depth_alpha_range,
    from = depth_alpha_limits
  ))

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
  scale_alpha_identity(guide = "none"),
  coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE),
  theme_classic(base_size = 12),
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
)

net4_sampling_locations_df <- mocness_clean %>%
  filter(net == 4) %>%
  distinct(transect_station_rep_year_net, year, start_longitude_dd,
           start_latitude_dd) %>%
  mutate(year = factor(year, levels = c(2018, 2019, 2022, 2023))) %>%
  filter(!is.na(start_longitude_dd), !is.na(start_latitude_dd), !is.na(year))

year_shape_values <- c("2018" = 16, "2019" = 17, "2022" = 15, "2023" = 18)

net4_sampling_locations_map <- ggplot() +
  map_layers +
  geom_point(
    data = net4_sampling_locations_df,
    aes(start_longitude_dd, start_latitude_dd, shape = year),
    color = "black",
    size = 0.5,
    stroke = 0.7
  ) +
  scale_shape_manual(
    values = year_shape_values,
    name = "Year",
    drop = FALSE
  ) +
  guides(shape = guide_legend(override.aes = list(color = "black", size = 1))) +
  labs(x = NULL, y = NULL) +
  theme(aspect.ratio = 3.35, legend.position = "right")

ggsave("net4_sampling_locations_map.png",
       plot = net4_sampling_locations_map,
       path = here("output"),
       width = 6, height = 9, units = "in", dpi = 600)

environment_covariates_df <- env_wide %>%
  select(transect_station_rep_year_net, year, net, depth_mean_m,
         mean_temperature_c, mean_salinity_psu, dissolved_oxygen_ml_l,
         mean_chl_0_100_m_mgm3, seafloor_depth_m) %>%
  distinct() %>%
  filter(net != 0) %>%
  mutate(year = factor(year, levels = c(2018, 2019, 2022, 2023)),
         seafloor_depth_plot_m = seafloor_depth_m)

seafloor_depth_color_limits <- range(environment_covariates_df$seafloor_depth_plot_m,
                                     na.rm = TRUE)

environment_depth_plot_df <- environment_covariates_df %>%
  pivot_longer(cols = c(mean_temperature_c, mean_salinity_psu,
                        dissolved_oxygen_ml_l),
               names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(
    variable,
    mean_temperature_c = "Temperature (C)",
    mean_salinity_psu = "Salinity (PSU)",
    dissolved_oxygen_ml_l = "Dissolved oxygen (mL L-1)"
  ),
  variable = factor(variable,
                    levels = c("Temperature (C)", "Salinity (PSU)",
                               "Dissolved oxygen (mL L-1)"))) %>%
  filter(!is.na(value), !is.na(depth_mean_m),
         !is.na(seafloor_depth_plot_m), !is.na(year))

make_environment_depth_plot <- function(plot_variable, plot_title, show_y_title = FALSE) {
  ggplot(filter(environment_depth_plot_df, variable == plot_variable),
         aes(x = value, y = depth_mean_m,
             color = seafloor_depth_plot_m, shape = year)) +
    geom_smooth(aes(x = value, y = depth_mean_m,
                    group = year, linetype = year),
                method = "loess", formula = y ~ x, se = FALSE,
                orientation = "y",
                color = "black", linewidth = 0.4,
                inherit.aes = FALSE) +
    geom_point(size = 1, alpha = 0.85) +
    scale_y_reverse() +
    scale_color_viridis_c(name = "Seafloor depth (m)",
                          limits = seafloor_depth_color_limits) +
    scale_shape_manual(values = year_shape_values, drop = FALSE) +
    scale_linetype_manual(
      values = c("2018" = "solid",
                 "2019" = "dotted",
                 "2022" = "dashed",
                 "2023" = "11"),
      drop = FALSE
    ) +
    guides(shape = guide_legend(title = "Year"),
           linetype = guide_legend(title = "Year")) +
    labs(x = plot_title,
         y = "Mean tow depth (m)") +
    theme_classic(base_size = 10) +
    theme(aspect.ratio = 2,
          legend.position = "right",
          axis.title.y = if (show_y_title) {
            element_text()
          } else {
            element_text(color = "transparent")
          },
          plot.margin = margin(t = 4, r = 2, b = 4, l = 2))
}

temperature_depth_plot <- make_environment_depth_plot(
  "Temperature (C)", "Temperature (°C)", show_y_title = TRUE
)
salinity_depth_plot <- make_environment_depth_plot(
  "Salinity (PSU)", "Salinity (PSU)"
)
dissolved_oxygen_depth_plot <- make_environment_depth_plot(
  "Dissolved oxygen (mL L-1)",
  expression(paste("Dissolved oxygen (mL ", plain(L)^{-1}, ")"))
)

chlorophyll_plot_df <- environment_covariates_df %>%
  filter(!is.na(mean_chl_0_100_m_mgm3), !is.na(seafloor_depth_plot_m),
         !is.na(year))

chlorophyll_box_plot <- ggplot(chlorophyll_plot_df,
                               aes(x = year, y = mean_chl_0_100_m_mgm3)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85", color = "black",
               linewidth = 0.3) +
  geom_jitter(aes(color = seafloor_depth_plot_m, shape = year),
              width = 0.15, height = 0, size = 1, alpha = 0.85) +
  scale_color_viridis_c(name = "Seafloor depth (m)",
                        limits = seafloor_depth_color_limits) +
  scale_shape_manual(values = year_shape_values, drop = FALSE) +
  guides(shape = "none") +
  labs(x = "Year",
       y = expression("Mean chlorophyll 0-100 m"~(mg~m^{-3}))) +
  theme_classic(base_size = 10) +
  theme(aspect.ratio = 0.485, legend.position = "right")

environmental_covariates_legend <- cowplot::get_legend(
  temperature_depth_plot + theme(legend.position = "right")
)

environment_depth_profiles <- cowplot::plot_grid(
  temperature_depth_plot + theme(legend.position = "none"),
  salinity_depth_plot + theme(legend.position = "none"),
  dissolved_oxygen_depth_plot + theme(legend.position = "none"),
  ncol = 3,
  labels = c("B", "C", "D"),
  label_size = 12,
  label_fontface = "bold",
  align = "hv",
  axis = "tblr"
)

environment_depth_profiles_for_layout <- cowplot::ggdraw() +
  cowplot::draw_plot(environment_depth_profiles,
                     x = -0.031, y = -0.01,
                     width = 1, height = 1)

net4_sampling_locations_map_for_layout <- cowplot::ggdraw() +
  cowplot::draw_plot(net4_sampling_locations_map + theme(legend.position = "none"),
                     x = 0.045, y = -0.01,
                     width = 1, height = 1)

environmental_covariates_legend_for_layout <- cowplot::ggdraw() +
  cowplot::draw_plot(environmental_covariates_legend,
                     x = -0.25, y = 0,
                     width = 1, height = 1)

environmental_covariates <- ggarrange(
  environment_depth_profiles_for_layout,
  chlorophyll_box_plot + theme(legend.position = "none"),
  ncol = 1,
  labels = c("", "E"),
  font.label = list(size = 12, face = "bold"),
  heights = c(1, 0.8),
  align = "v"
)

net4_sampling_locations_environmental_covariates_no_legend <- ggarrange(
  net4_sampling_locations_map_for_layout,
  environmental_covariates,
  ncol = 2,
  labels = c("A", ""),
  font.label = list(size = 12, face = "bold"),
  widths = c(0.77, 2),
  align = "hv"
)

net4_sampling_locations_environmental_covariates <- ggarrange(
  net4_sampling_locations_environmental_covariates_no_legend,
  environmental_covariates_legend_for_layout,
  ncol = 2,
  widths = c(1, 0.16)
)

ggsave("net4_sampling_locations_environmental_covariates.png",
       plot = net4_sampling_locations_environmental_covariates,
       path = here("output"),
       width = 12.5, height = 8.5, units = "in", dpi = 600)

cluster_map_cluster_legend <- ggplot(
  tibble(cluster = factor(cluster_levels, levels = cluster_levels),
         x = 0.14,
         label_x = 0.22,
         y = rev(seq_along(cluster_levels)))
) +
  geom_point(aes(x, y, color = cluster), size = 2) +
  geom_text(aes(label_x, y, label = cluster), hjust = 0, size = 3) +
  scale_color_manual(values = cluster_colors,
                     limits = cluster_levels,
                     breaks = cluster_levels,
                     drop = FALSE,
                     guide = "none") +
  scale_x_continuous(NULL, limits = c(0, 1), breaks = NULL,
                     expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(NULL, limits = c(0.5, length(cluster_levels) + 0.5),
                     breaks = NULL, expand = expansion(mult = c(0, 0))) +
  labs(title = "Cluster") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(hjust = 0, size = 10),
        plot.margin = margin(t = 4, r = 0, b = 4, l = 0))

depth_alpha_values <- seq(depth_alpha_limits[1], depth_alpha_limits[2], length.out = 400)
depth_alpha_values_scaled <- scales::rescale(
  depth_alpha_values,
  to = depth_alpha_range,
  from = depth_alpha_limits
)
depth_alpha_step <- diff(depth_alpha_values)[1]
depth_alpha_scale_df <- tibble(
  depth = depth_alpha_values,
  alpha = depth_alpha_values_scaled,
  ymin = depth_alpha_values - depth_alpha_step / 2,
  ymax = depth_alpha_values + depth_alpha_step / 2
) %>%
  mutate(ymin = pmax(ymin, depth_alpha_limits[1]),
         ymax = pmin(ymax, depth_alpha_limits[2]))
depth_alpha_scale_breaks <- scales::breaks_pretty(n = 4)(depth_alpha_limits)
depth_alpha_scale_breaks <- depth_alpha_scale_breaks[
  depth_alpha_scale_breaks >= depth_alpha_limits[1] &
    depth_alpha_scale_breaks <= depth_alpha_limits[2]
]
depth_alpha_breaks <- tibble(
  depth = depth_alpha_scale_breaks,
  label = scales::number(depth_alpha_scale_breaks, accuracy = 1)
)

depth_alpha_legend <- ggplot() +
  geom_rect(
    data = depth_alpha_scale_df,
    aes(xmin = 0.10, xmax = 0.18, ymin = ymin, ymax = ymax, alpha = alpha),
    fill = "black"
  ) +
  geom_text(
    data = depth_alpha_breaks,
    aes(x = 0.22, y = depth, label = label),
    hjust = 0, size = 3
  ) +
  scale_alpha_identity(guide = "none") +
  scale_y_reverse(name = NULL,
                  limits = rev(depth_alpha_limits),
                  breaks = NULL,
                  expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(NULL, limits = c(0, 0.42), breaks = NULL,
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Mean sampling\ndepth (m)") +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(hjust = 0, size = 10),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y.left = element_blank(),
        axis.line.y.right = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y.left = element_blank(),
        axis.text.y.right = element_blank(),
        plot.margin = margin(t = 4, r = 0, b = 4, l = 0))

cluster_map_legend <- ggarrange(
  cluster_map_cluster_legend,
  depth_alpha_legend,
  ncol = 1,
  heights = c(0.25, 0.8)
)

make_cluster_map_panel <- function(facet, title) {
  is_recent_year_panel <- facet %in% c("22", "23")
  x_size <- if (is_recent_year_panel) 1 else 0.5
  x_stroke <- if (is_recent_year_panel) 0.35 else 0.175
  point_size <- if (is_recent_year_panel) 1.2 else 0.6

  ggplot() +
    map_layers +
    geom_point(data = filter(excluded_df, facet_group == facet),
               aes(plot_longitude_dd + dx, plot_latitude_dd + dy),
               shape = 4, color = "black", size = x_size, stroke = x_stroke) +
    geom_point(data = filter(mapping_df2, facet_group == facet),
               aes(plot_longitude_dd + dx, plot_latitude_dd + dy,
                   color = cluster, alpha = mid_tow_depth_alpha),
               size = point_size) +
    labs(title = title, x = NULL, y = NULL)
}

p18a <- make_cluster_map_panel("18MaN", "2018N1")
p18c <- make_cluster_map_panel("18MbD", "2018D1")

p19a <- make_cluster_map_panel("19MaN", "2019N1")
p19b <- make_cluster_map_panel("19MaD", "2019D1")
p19c <- make_cluster_map_panel("19MbN", "2019N2")
p19d <- make_cluster_map_panel("19MbD", "2019D2")

p22 <- make_cluster_map_panel("22", "2022")
p23 <- make_cluster_map_panel("23", "2023")

## Make layout panels for 2018 and 2019
p2018_2019_top <- (p18c | p18a | p19b) +
  plot_layout(widths = c(1, 1, 1))
p2018_2019_bottom <- (p19a | p19d | p19c) +
  plot_layout(widths = c(1, 1, 1))
p2018_2019 <- (p2018_2019_top / p2018_2019_bottom) +
  plot_layout(heights = c(1, 1))

## Assemble custom layout
final_cluster_map <- (p2018_2019 | p22 | p23 | wrap_elements(cluster_map_legend)) +
  plot_layout(widths = c(2, 1, 1, 0.45))
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
  left_join(clusters, by = "transect_station_rep_year_net") %>%
  left_join(
    wide_major_taxa_nets %>%
      select(transect_station_rep_year_net, year) %>%
      distinct(),
    by = "transect_station_rep_year_net"
  ) %>%
  arrange(year, cluster, chrono_sample_ID) %>%
  mutate(chrono_sample_numeric = chrono_sample_ID,
         chrono_sample_ID = factor(chrono_sample_ID, levels = unique(chrono_sample_ID)))

# Compute cluster bounds to use as vertical separators on barplot
cluster_bounds <- AHC_comm_matrix_transformed_long %>%
  distinct(year, cluster, chrono_sample_ID) %>%
  arrange(year, cluster, chrono_sample_ID) %>%
  group_by(year) %>%
  mutate(year_x_position = row_number()) %>%
  group_by(year, cluster) %>%
  summarize(start = min(year_x_position), end = max(year_x_position), .groups = "drop")

cluster_separators <- cluster_bounds %>%
  group_by(year) %>%
  filter(start > min(start)) %>%
  ungroup()

bar_heights <- AHC_comm_matrix_transformed_long %>%
  group_by(year, chrono_sample_ID) %>%
  summarize(total_height = sum(sqrt_concentration), .groups = "drop")

max_height <- max(bar_heights$total_height)

AHC_comm_matrix_log_transformed_long <- AHC_comm_matrix_transformed_long %>%
  filter(sqrt_concentration > 0) %>%
  mutate(log_concentration = log(sqrt_concentration),
         log_concentration_shifted = log_concentration - min(log_concentration, na.rm = TRUE))

# Plot proportional taxonomic composition across all net tows, sorted by cluster
cluster_proportion_bounds <- AHC_comm_matrix_transformed_long %>%
  distinct(cluster, chrono_sample_ID, chrono_sample_numeric) %>%
  arrange(cluster, chrono_sample_numeric) %>%
  mutate(x_position = row_number()) %>%
  group_by(cluster) %>%
  summarize(start = min(x_position), end = max(x_position), .groups = "drop")

cluster_proportion_separators <- cluster_proportion_bounds %>%
  filter(start > min(start))

cluster_proportion_label_positions <- cluster_proportion_bounds %>%
  mutate(x = (start + end) / 2)

clusters_proportion_bar_plot <- ggplot(
  AHC_comm_matrix_transformed_long %>%
    arrange(cluster, chrono_sample_numeric) %>%
    mutate(chrono_sample_ID = factor(chrono_sample_ID, levels = unique(chrono_sample_ID))),
  aes(x = chrono_sample_ID,
      y = sqrt_concentration,
      fill = factor(taxon, levels = ordered_taxa))
) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(
    values = species_colors,
    breaks = ordered_taxa,
    labels = parse(text = taxon_labels[ordered_taxa]),
    name = "Taxon"
  ) +
  geom_vline(data = cluster_proportion_separators,
             aes(xintercept = start - 0.5),
             linetype = "11", color = "black", linewidth = 0.5,
             inherit.aes = FALSE) +
  geom_text(data = cluster_proportion_label_positions,
            aes(x = x, y = Inf, label = cluster),
            vjust = -0.6, size = 3, inherit.aes = FALSE) +
  coord_cartesian(clip = "off") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Sample", y = "Proportion", title = "Cluster") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(t = 20, r = 5, b = 5, l = 5),
        axis.text.x = element_text(size = 4.5),
        legend.position = "none")

ggsave("clusters_proportion_bar_plot.png",
       plot = clusters_proportion_bar_plot,
       path = here("output"),
       width = 10, height = 5, units = "in", dpi = 300)

# Plot by transect_station_rep_year, sorted by cluster
make_cluster_abundance_year_plot <- function(plot_data, separator_data, plot_year,
                                             abundance_column = "sqrt_concentration",
                                             y_limit = NULL,
                                             y_label = expression(paste("Concentration (ind. ", m^-3, ")")),
                                             show_y_axis = FALSE) {
  year_plot_data <- plot_data %>%
    filter(year == plot_year) %>%
    arrange(cluster, chrono_sample_numeric) %>%
    mutate(chrono_sample_ID = factor(chrono_sample_ID, levels = unique(chrono_sample_ID)))

  year_top_axis_data <- year_plot_data %>%
    distinct(cluster, chrono_sample_ID, chrono_sample_numeric) %>%
    arrange(cluster, chrono_sample_numeric) %>%
    mutate(year_x_position = row_number())
  year_cluster_label_positions <- year_top_axis_data %>%
    group_by(cluster) %>%
    filter(abs(year_x_position - mean(range(year_x_position))) ==
             min(abs(year_x_position - mean(range(year_x_position))))) %>%
    slice(1) %>%
    ungroup()
  year_top_axis_labels <- rep("", nrow(year_top_axis_data))
  year_top_axis_labels[year_cluster_label_positions$year_x_position] <-
    as.character(year_cluster_label_positions$cluster)

  ggplot(year_plot_data, aes(x = chrono_sample_ID,
                             y = .data[[abundance_column]],
                             fill = factor(taxon, levels = ordered_taxa))) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
      values = species_colors,
      breaks = ordered_taxa,
      labels = parse(text = taxon_labels[ordered_taxa]),
      name = "Taxon"
    ) +
    geom_vline(data = filter(separator_data, year == plot_year),
               aes(xintercept = start - 0.5),
               linetype = "dotted", color = "black", linewidth = 0.25,
               inherit.aes = FALSE) +
    coord_cartesian(clip = "off") +
    scale_x_discrete(
      guide = guide_axis(angle = 90),
      sec.axis = dup_axis(
        name = NULL,
        labels = function(x) {
          labels <- year_top_axis_labels[seq_along(x)]
          labels[is.na(labels)] <- ""
          unname(labels)
        },
        guide = guide_axis(angle = 0)
      )
    ) +
    scale_y_continuous(limits = if (is.null(y_limit)) NULL else c(0, y_limit * 1.05),
                       expand = expansion(mult = c(0, 0))) +
    labs(title = NULL,
         x = NULL,
         y = y_label) +
    guides(fill = guide_legend(ncol = 1)) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(t = 10, r = 4, b = 5, l = 4),
          axis.text.x.top = element_text(size = 7),
          axis.title.x.top = element_blank(),
          axis.ticks.x.top = element_blank(),
          axis.text.x = element_text(size = 3.5),
          axis.title.y = if (show_y_axis) element_text() else element_blank(),
          axis.text.y = if (show_y_axis) element_text() else element_blank(),
          axis.ticks.y = if (show_y_axis) element_line() else element_blank(),
          axis.line.y = if (show_y_axis) element_line() else element_blank())
}

cluster_years <- sort(unique(AHC_comm_matrix_transformed_long$year))
cluster_year_widths <- c(24, 72, 60, 35)
cluster_year_plots <- map2(cluster_years, seq_along(cluster_years),
                           ~make_cluster_abundance_year_plot(
                             AHC_comm_matrix_transformed_long,
                             cluster_separators,
                             .x,
                             abundance_column = "sqrt_concentration",
                             y_limit = max_height,
                             show_y_axis = .y == 1
                           ))
cluster_abun_header <- ggarrange(
  ggarrange(
    plotlist = map(cluster_years, ~text_grob(as.character(.x), size = 12)),
    ncol = length(cluster_years),
    nrow = 1,
    widths = cluster_year_widths
  ),
  text_grob("Cluster", size = 12),
  ncol = 1,
  heights = c(1, 0.8)
)

## Extract legend
legend_only <- cowplot::get_legend(cluster_year_plots[[1]] + theme(legend.position = "right"))

clust_abun_panel_row <- ggarrange(
  plotlist = map(cluster_year_plots, ~.x + theme(legend.position = "none")),
  ncol = length(cluster_year_plots),
  nrow = 1,
  widths = cluster_year_widths,
  align = "hv"
)

clust_abun_bar_plot_no_legend <- ggarrange(
  cluster_abun_header,
  clust_abun_panel_row,
  text_grob("Sample", size = 12),
  ncol = 1,
  heights = c(0.14, 1, 0.06)
)

clust_abun_bar_plot <- ggarrange(
  clust_abun_bar_plot_no_legend,
  legend_only,
  ncol = 2,
  widths = c(1, 0.18)
)

### Wrap legend in a ggplot so ggsave works
legend_plot <- cowplot::ggdraw(legend_only)
ggsave(filename = "barplot_taxa_legend.png",
  plot = legend_plot,
  path = here("output"),
  width = 2, height = 6, dpi = 300)

ggsave("clusters_abundance_bar_plot.png", plot = clust_abun_bar_plot_no_legend, path = here("output"),
       width = 10, height = 5, units = "in", dpi = 300)

cluster_year_log_plots <- map2(cluster_years, seq_along(cluster_years),
                               ~make_cluster_abundance_year_plot(
                                 AHC_comm_matrix_log_transformed_long,
                                 cluster_separators,
                                 .x,
                                 abundance_column = "log_concentration_shifted",
                                 y_label = expression(paste("log Concentration (ind. ", m^-3, ")")),
                                 show_y_axis = .y == 1
                               ))

log_clust_abun_panel_row <- ggarrange(
  plotlist = map(cluster_year_log_plots, ~.x + theme(legend.position = "none")),
  ncol = length(cluster_year_log_plots),
  nrow = 1,
  widths = cluster_year_widths,
  align = "hv"
)

log_clust_abun_bar_plot_no_legend <- ggarrange(
  cluster_abun_header,
  log_clust_abun_panel_row,
  text_grob("Sample", size = 12),
  ncol = 1,
  heights = c(0.14, 1, 0.06)
)

ggsave("clusters_abundance_bar_plot_log_transformed.png",
       plot = log_clust_abun_bar_plot_no_legend,
       path = here("output"),
       width = 10, height = 5, units = "in", dpi = 300)


# Plot same but only for the four clusters with the most net tows
major_clusters_plot_df <- AHC_comm_matrix_transformed_long %>%
  filter(cluster %in% main_clusters) %>%
  arrange(year, cluster, chrono_sample_numeric) %>%
  mutate(chrono_sample_ID = factor(chrono_sample_ID, levels = unique(chrono_sample_ID)))

major_clusters_bounds <- major_clusters_plot_df %>%
  distinct(year, cluster, chrono_sample_ID) %>%
  arrange(year, cluster, chrono_sample_ID) %>%
  group_by(year) %>%
  mutate(year_x_position = row_number()) %>%
  group_by(year, cluster) %>%
  summarize(start = min(year_x_position), end = max(year_x_position), .groups = "drop")

major_clusters_separators <- major_clusters_bounds %>%
  group_by(year) %>%
  filter(start > min(start)) %>%
  ungroup()

make_major_cluster_abundance_year_plot <- function(plot_data, separator_data, plot_year,
                                                   show_y_axis = FALSE) {
  year_plot_data <- plot_data %>%
    filter(year == plot_year) %>%
    arrange(cluster, chrono_sample_numeric) %>%
    mutate(chrono_sample_ID = factor(chrono_sample_ID, levels = unique(chrono_sample_ID)))

  year_top_axis_data <- year_plot_data %>%
    distinct(cluster, chrono_sample_ID, chrono_sample_numeric) %>%
    arrange(cluster, chrono_sample_numeric) %>%
    mutate(year_x_position = row_number())
  year_cluster_label_positions <- year_top_axis_data %>%
    group_by(cluster) %>%
    filter(abs(year_x_position - mean(range(year_x_position))) ==
             min(abs(year_x_position - mean(range(year_x_position))))) %>%
    slice(1) %>%
    ungroup()
  year_top_axis_labels <- rep("", nrow(year_top_axis_data))
  year_top_axis_labels[year_cluster_label_positions$year_x_position] <-
    as.character(year_cluster_label_positions$cluster)

  ggplot(year_plot_data, aes(x = chrono_sample_ID,
                             y = sqrt_concentration,
                             fill = factor(taxon, levels = ordered_taxa))) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(
      values = species_colors,
      breaks = ordered_taxa,
      labels = parse(text = taxon_labels[ordered_taxa]),
      name = "Taxon"
    ) +
    geom_vline(data = filter(separator_data, year == plot_year),
               aes(xintercept = start - 0.5),
               linetype = "dashed", color = "gray40", linewidth = 0.5,
               inherit.aes = FALSE) +
    coord_cartesian(clip = "off") +
    scale_x_discrete(
      guide = guide_axis(angle = 45),
      sec.axis = dup_axis(
        name = NULL,
        labels = function(x) {
          labels <- year_top_axis_labels[seq_along(x)]
          labels[is.na(labels)] <- ""
          unname(labels)
        },
        guide = guide_axis(angle = 0)
      )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = NULL, x = NULL, y = "Concentration (ind./m^3)") +
    theme_light() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(t = 10, r = 4, b = 0, l = 4),
          axis.text.x.top = element_text(size = 7),
          axis.title.x.top = element_blank(),
          axis.ticks.x.top = element_blank(),
          axis.text.x = element_text(size = 6),
          axis.title.y = if (show_y_axis) element_text() else element_blank(),
          axis.text.y = if (show_y_axis) element_text() else element_blank(),
          axis.ticks.y = if (show_y_axis) element_line() else element_blank(),
          axis.line.y = if (show_y_axis) element_line() else element_blank())
}

major_cluster_years <- sort(unique(major_clusters_plot_df$year))
major_cluster_year_widths <- major_clusters_plot_df %>%
  distinct(year, chrono_sample_ID) %>%
  count(year) %>%
  arrange(match(year, major_cluster_years)) %>%
  pull(n)
major_cluster_year_plots <- map2(major_cluster_years, seq_along(major_cluster_years),
                                 ~make_major_cluster_abundance_year_plot(
                                   major_clusters_plot_df,
                                   major_clusters_separators,
                                   .x,
                                   show_y_axis = .y == 1
                                 ))
major_cluster_abun_header <- ggarrange(
  ggarrange(
    plotlist = map(major_cluster_years, ~text_grob(as.character(.x), size = 12)),
    ncol = length(major_cluster_years),
    nrow = 1,
    widths = major_cluster_year_widths
  ),
  text_grob("Cluster", size = 12),
  ncol = 1,
  heights = c(1, 0.8)
)

windows()
major_clusters_abun_panel_row <- ggarrange(
  plotlist = map(major_cluster_year_plots, ~.x + theme(legend.position = "none")),
  ncol = length(major_cluster_year_plots),
  nrow = 1,
  widths = major_cluster_year_widths,
  align = "hv"
)

major_clusters_abun_bar_plot_no_legend <- ggarrange(
  major_cluster_abun_header,
  major_clusters_abun_panel_row,
  text_grob("Sample ID", size = 12),
  ncol = 1,
  heights = c(0.14, 1, 0.06)
)

major_clusters_legend_only <- cowplot::get_legend(major_cluster_year_plots[[1]] + theme(legend.position = "right"))
major_clusters_abun_bar_plot <- ggarrange(
  major_clusters_abun_bar_plot_no_legend,
  major_clusters_legend_only,
  ncol = 2,
  widths = c(1, 0.18)
)
major_clusters_abun_bar_plot



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
station_scores <- mutate(site_scores, transect_station_rep_year_net = AHC_comm_matrix_transformed$transect_station_rep_year_net)
stations_clustered <- station_scores %>%
  left_join(clusters, by = "transect_station_rep_year_net") %>%
  mutate(cluster = factor(cluster, levels = cluster_levels))

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
envfit_covariates <- c("solar_dayness_scaled", "start_latitude_dd_scaled",
                       "depth_mean_m_scaled", "seafloor_depth_m_scaled")
env_numeric2 <- bind_cols(env_wide_aligned %>% select(all_of(envfit_covariates)),
                          as_tibble(year_mat)) %>%
  as.data.frame()
fit_vectors <- envfit(NMDS_result, env_numeric2, permutations = 1000, na.rm = TRUE)

##Extract vector scores for plotting
vector_scores <- scores(fit_vectors, display = "vectors")
vector_df <- as.data.frame(vector_scores) %>% 
  mutate(variable = rownames(vector_scores)) %>% 
  filter(grepl("^year_", variable) |
           variable %in% c("solar_dayness_scaled", "start_latitude_dd_scaled",
                           "depth_mean_m_scaled", "seafloor_depth_m_scaled")
  ) %>%
  mutate(
    plot_label = recode(
      variable,
      "depth_mean_m_scaled" = "Mean depth",
      "seafloor_depth_m_scaled" = "Seafloor depth",
      "start_latitude_dd_scaled" = "Latitude",
      "solar_dayness_scaled" = "Time of day",
      "year_2018" = "2018",
      "year_2019" = "2019",
      "year_2022" = "2022",
      "year_2023" = "2023",
      .default = variable
    ),
    base_label_x = NMDS1 + if_else(NMDS1 >= 0, 0.14, -0.14),
    base_label_y = NMDS2 + if_else(NMDS2 >= 0, 0.10, -0.10),
    label_x = case_when(
      variable == "seafloor_depth_m_scaled" ~ 0.75,
      variable == "start_latitude_dd_scaled" ~ -0.65,
      variable == "depth_mean_m_scaled" ~ 0.3,
      variable == "solar_dayness_scaled" ~ -0.1,
      variable == "year_2018" ~ 0.08,
      variable == "year_2019" ~ 0.3,
      variable == "year_2022" ~ 0.05,
      variable == "year_2023" ~ -0.5,
      TRUE ~ base_label_x
    ),
    label_y = case_when(
      variable == "seafloor_depth_m_scaled" ~ 0.1,
      variable == "start_latitude_dd_scaled" ~ -0.5,
      variable == "depth_mean_m_scaled" ~ 0.4,
      variable == "solar_dayness_scaled" ~ 0.25,
      variable == "year_2018" ~ -0.25,
      variable == "year_2019" ~ -0.15,
      variable == "year_2022" ~ 0.3,
      variable == "year_2023" ~ 0,
      TRUE ~ base_label_y
    ),
    label_hjust = case_when(
      variable %in% c("start_latitude_dd_scaled",
                      "solar_dayness_scaled", "year_2023") ~ 1,
      TRUE ~ 0
    )
  )

# Ellipses
## fit ellipses
# ell_shelf <- ordiellipse(NMDS_result, env_wide_aligned$shelf_position,
#                          kind = "sd", conf = 0.95, draw = "none") 

## convert outputs to data frames
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

#Plot NMDS with overlays
windows()
ggplot(stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  #1 Polygons (cluster color)
  geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, fill = cluster, group = cluster), 
               alpha = 0.25, color = NA, inherit.aes = FALSE) +
  scale_fill_manual(name = "Cluster", values = cluster_colors) +
  #2 Points (cluster colors)
  geom_point(size = 1) +
  scale_color_manual(name = "Cluster", values = cluster_colors) +
  # #3 Ellipses - Shelf Position
  # geom_path(data = ell_shelf_df, aes(x = NMDS1, y = NMDS2, color = group), 
  #           size = 1, inherit.aes = FALSE) +
  # scale_color_manual(name = "Shelf position", values = c("shelf" = "#1f78b4", "offshore" = "#e31a1c")) +
  # new_scale_color() +
  #4 Vectors
  geom_segment(data = vector_df, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.15, "cm")), 
               color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  geom_segment(data = vector_df,
               aes(x = NMDS1, y = NMDS2, xend = label_x, yend = label_y),
               color = "grey35", linewidth = 0.25, inherit.aes = FALSE) +
  geom_text(data = vector_df,
            aes(x = label_x, y = label_y, label = plot_label, hjust = label_hjust),
            color = "black", size = 2, inherit.aes = FALSE) +
  labs(x = "NMDS1", y = "NMDS2",
       color = "Cluster", fill = "Cluster") + 
theme_classic()
ggsave("NMDS_overlays_all_clusters.png", plot = get_last_plot(), path = here("output"),
       width = 7, height = 5, units = "in", dpi = 300)


