
# Description -------------------------------------------------------------

# Conduct a cluster analysis of sampling events by LFC, plot clusters of 
#sampling events in dendrograms, run an NMDS, and plot the NMDS ordination


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


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Create wide environmental dataframe ---------------------------------------------------

wide_major_taxa_nets <- mocness_major_taxa_nets %>%
  # Removing NAs for now, but there shouldn't be any to begin with
  filter(!is.na(individuals_in_tow)) %>%
  filter(!is.na(individuals_per_m3)) %>%
  group_by(collection_date, replicate, transect, station, taxon) %>%
  mutate(individuals_per_m3 = sum(individuals_per_m3)) %>%
  ungroup() %>%
  distinct(collection_date, replicate, transect, station, taxon, .keep_all = TRUE) %>%
  pivot_wider(names_from = taxon, values_from = individuals_per_m3, values_fill = 0)

env_wide <- wide_major_taxa_nets %>%
  select(project, collection_date, transect_station_rep_year_net, replicate, net, start_time_pt,
         start_latitude_dd, start_longitude_dd, depth_range, shelf_position,
         seafloor_depth_m, prey_zooplankton_abundance_ind_m3, dissolved_oxygen_ml_l,
         mean_temperature_c, mean_salinity_psu, depth_mean_m, depth_diff_m,
         mean_density_kgm3, mlotst, mean_chl_0_100_m_mgm3) %>%
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
#removed mlotst for right now because all are NAs at the moment and I don't want this to cause errors down the line
#also excluded redundant information like transect, transect_station, transect_station_rep, and so on
## 04/13 RM : added mlotst back in now. kept redundant information out 

# Create community matrix -------------------------------------------------

AHC_comm_matrix <- mocness_major_taxa_nets %>%
  filter(!is.na(individuals_in_tow)) %>%
  filter(!is.na(individuals_per_m3)) %>%
  group_by(transect_station_rep_year_net, taxon) %>%
  summarize(individuals_per_m3 = sum(individuals_per_m3, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_per_m3, values_fill = 0)

transform_taxa_concentrations <- AHC_comm_matrix[, 2:23] %>%
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

## plot 2 clusters/rectangles
plot(AHC_result, labels = AHC_comm_matrix_transformed$transect_station_rep_year_net, main = "average linkage AHC of sampling events by LFC")
rect.hclust(AHC_result, k = 2, border = c(2, 4))

##plot 3 clusters/rectangles
windows()
plot(AHC_result, labels = AHC_comm_matrix_transformed$transect_station_rep_year_net, main = "average linkage AHC of sampling events by LFC")
rect.hclust(AHC_result, k = 5, border = c(2, 3, 4, 5, 6))

# Extract list of sampling events belonging to each cluster
clusters <- data.frame(transect_station_rep_year_net = names(cutree(AHC_result, k = 5)),
                       cluster = cutree(AHC_result, k = 5))


# Map points in space by cluster and net ----------------------------------

mapping_df <- wide_major_taxa_nets %>%
  left_join(clusters, by = "transect_station_rep_year_net") %>%
  select(transect_station_rep_year_net, start_longitude_dd, start_latitude_dd, cluster, net)
mapping_df$cluster <- factor(mapping_df$cluster)

space <- ne_countries(scale = "medium", returnclass = "sf")

windows()
ggplot() +
  geom_sf(data = space, fill = "grey90", color = "grey40") +
  geom_point(
    data = mapping_df,
    aes(x = start_longitude_dd, y = start_latitude_dd, 
        color = cluster, shape = factor(net)),
    size = 2, alpha = 0.95, position = position_jitter(width = 0.15, height = 0.15)) +
  coord_sf(xlim = c(-127, -123), ylim = c(40, 48), expand = FALSE) +
  scale_color_brewer(palette = "Dark2")

# Plot abundance of each taxon, grouped by cluster ------------------------

# Add cluster identities to long version of AHC_comm_matrix_transformed
AHC_comm_matrix_transformed_long <- AHC_comm_matrix_transformed %>%
  pivot_longer(cols = 2:23, names_to = "taxon", values_to = "sqrt_concentration") %>%
  merge(., clusters, by = "transect_station_rep_year")

# Categories of taxa in AHC_comm_matrix_transformed
coastal_species <- c("Agonidae", "Artedius", "Cottidae", "Hexagrammidae", "Liparis", "Paralichthyidae", "Parophrys_vetulus", "Pholidae", "Pleuronectidae", "Sebastes", "Stichaeidae", "Ammodytidae", "Gadidae", "Osmeridae", "Pleuronectidae_other")
coastal_colors <- colorRampPalette(brewer.pal(9, "Greens")[2:9])(length(coastal_species))

coastal_oceanic_species <- c("Engraulis_mordax", "Sardinops_sagax")
coastal_oceanic_colors <- colorRampPalette(brewer.pal(3, "Blues")[2:3])(length(coastal_oceanic_species))

oceanic_species <- c("Bathylagidae", "Chauliodus_macouni", "Lestidiops_ringens", "Lipolagus_ochotensis", "Macrouridae", "Myctophidae", "Paralepididae")
oceanic_colors <- colorRampPalette(brewer.pal(9, "Purples")[2:9])(length(oceanic_species))

# Named species color vector
species_colors <- c(setNames(coastal_colors, coastal_species),
                    setNames(coastal_oceanic_colors, coastal_oceanic_species),
                    setNames(oceanic_colors, oceanic_species))

# Vector of taxa ordered alphabetically within categories to order bars and figure legends
ordered_taxa <- c(coastal_species, coastal_oceanic_species, oceanic_species)

# Plot by transect_station_rep_year, sorted by cluster
windows()
ggplot(AHC_comm_matrix_transformed_long, aes(x = transect_station_rep_year, y = sqrt_concentration, fill = factor(taxon, levels = ordered_taxa))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors, breaks = ordered_taxa) +
  facet_grid(rows = vars(cluster)) +
  labs(x = "Depth sampled (m)", y = "individuals/m3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##not sure if my adjustments here to account for standardizing counts by volume were correct and/or needed


# Plot NMDS ordinations ---------------------------------------------------

NMDS_result <- metaMDS(dissim_matrix, distance = "bray", k = 2, try = 20, trymax = 20, engine = "monoMDS")
NMDS_result$stress  ##check stress

stressplot(NMDS_result)   ##Shepard diagram

site_scores <- as.data.frame(scores(NMDS_result, display = "sites"))
cluster_groups <- cutree(AHC_result, k = 5)
station_scores <- mutate(site_scores, transect_station_rep_year = AHC_comm_matrix_transformed$transect_station_rep_year)
stations_clustered <- mutate(station_scores, cluster = cluster_groups)
stations_clustered$cluster <- as.numeric(as.character(stations_clustered$cluster))
stations_clustered$cluster <- factor(stations_clustered$cluster, levels = c(1,2,3,4,5), labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))

ggplot(stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  scale_color_manual(values = c("red", "blue", "black", "green", "orange")) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = transect_station_rep_year), size = 3, max.overlaps = 10) +
  theme_classic() +
  labs(title = "NMDS Ordination of sampling events by LFC", x = "NMDS1", y = "NMDS2")   ##NMDS plot


# overlays for NMDS plots -------------------------------------------------

#Vectors for environmental variables
env_wide_aligned <- env_wide[match(rownames(scores(NMDS_result, display = "sites")),
                                   env_wide$transect_station_rep_year), ]
env_numeric <- env_wide_aligned[, sapply(env_wide_aligned, is.numeric)]
fit_vectors<- envfit(NMDS_result, env_numeric, permutations = 1000, na.rm = TRUE)

##Extract vector scores for plotting
vector_scores <- scores(fit_vectors, display = "vectors")
vector_df <- as.data.frame(vector_scores)
vector_df$variable <- rownames(vector_df)

##Plot NMDS with vector overlays
windows()
ggplot(stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  scale_color_manual(values = c("red", "blue", "black", "green", "orange")) +
  geom_point(size = 3) +
  #geom_text_repel(aes(label = transect_station_rep_year), size = 3, max.overlaps = 10) +
  theme_classic() +
  labs(title = "NMDS Ordination of sampling events by LFC", x = "NMDS1", y = "NMDS2") +
  geom_segment(data = vector_df,
             aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
             arrow = arrow(length = unit(0.3, "cm")),
             color = "darkgreen", linewidth = 1) +
  geom_text(data = vector_df,
          aes(x = NMDS1, y = NMDS2, label = variable),
          color = "darkgreen", size = 3, vjust = -0.5)

#Ellipses for categorical variables

##shelf_position
###fit ellipses
ell_shelf <- ordiellipse(NMDS_result, env_wide_aligned$shelf_position,
                        kind = "sd", conf = 0.95, draw = "none") 

###convert ellipse output to data frames
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
    group = .x
  )
})

###overlay ellipses on NMDS plot
ggplot(stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  geom_point(size = 3) +
  geom_path(data = ell_shelf_df, aes(x = NMDS1, y = NMDS2, color = group),
            size = 1) +
  scale_color_manual(values = c("red", "blue", "black", "green", "orange", "darkgreen", "darkorange")) +
  theme_classic() +
  labs(title = "NMDS Ordination with Clustered Points and Shelf Position Ellipses",
       x = "NMDS1", y = "NMDS2")

##time_of_day
###fit ellipses
time_groups <- env_wide_aligned$time_of_day

ell_time <- ordiellipse(
  NMDS_result,
  time_groups,
  kind = "sd",
  conf = 0.95, 
  draw = "none"
)

### convert ellipse output to data frame
ell_time_df <- purrr::map_dfr(names(ell_time), ~ {
  e     <- ell_time[[.x]]
  theta <- seq(0, 2 * pi, length.out = 200)
  circle <- cbind(cos(theta), sin(theta))
  
  xy <- circle %*% chol(e$cov)
  xy <- sweep(xy * e$scale, 2, e$center, "+")
  
  tibble(
    NMDS1 = xy[, 1],
    NMDS2 = xy[, 2],
    group = .x
  )
})

###overlay ellipses on NMDS plot
ggplot(stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  geom_point(size = 3) +
  geom_path(data = ell_time_df, aes(x = NMDS1, y = NMDS2, color = group),
            size = 1, linetype = 2) +
  scale_color_manual(values = c("red", "blue", "black", "green", "orange", "darkgrey", "darkblue")) +
  theme_classic() +
  labs(title = "NMDS Ordination with Clustered Points and Time of Day Ellipses",
       x = "NMDS1", y = "NMDS2")
