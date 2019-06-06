# Combine species occurence and environmental data
#   And calculating derived variables
# Cape vs SWA
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

# Load packages and functions
library(here)
source(here("analyses-May-2019/setup.R"))

# Import processed environmental data
var_names <- c(
  # Environmental variable names in nice order
  "Elevation",
  "MAP",
  "PDQ",
  "Surface T",
  "NDVI",
  "CEC",
  "Clay",
  "Soil C",
  "pH"
)
data_dir <- here("data/derived-data/May-2019")
GCFR_variables_masked2 <- map(var_names,
  ~raster(glue("{data_dir}/GCFR_{.x}_masked2.tif"))
)
names(GCFR_variables_masked2) <- var_names
SWAFR_variables_masked2 <- map(var_names,
  ~raster(glue("{data_dir}/SWAFR_{.x}_masked2.tif"))
)
names(SWAFR_variables_masked2) <- var_names

import_region_polygons()

# Calculate roughness layers across scales -------------------------------------

# Define roughness **here** as the mean absolute difference of a focal cell's
# value (in a 3 x 3 neighbourhood) to it's ca. 8 neighbours
body(roughness)

# Test
foo <- roughness(GCFR_variables_masked2$pH)
gplot(foo) +
  geom_tile(aes(fill = value))
foo <- roughness(GCFR_variables_masked2$Elevation)
gplot(foo) +
  geom_tile(aes(fill = value))
# Good!

# Run on all data, aggregating to a higher scale **before** calculating
# roughness (b/c: I know why!)
# (Eighth Degree Square = EDS, Quarter Degree Square       = QDS,
#  Half Degree Square   = HDS, Three-Quarter Degree Square = 3QDS)
scales <- list(base = 0.05, EDS = 0.125, QDS = 0.25, HDS = 0.50, `3QDS` = 0.75)
GCFR_roughness <- map(scales, function(scale) {
  map(GCFR_variables_masked2, function(layer) {
    layer %>%
      aggregate(fact = scale / 0.05) %>%
      roughness()
  })
})
SWAFR_roughness <- map(scales, function(scale) {
  map(SWAFR_variables_masked2, function(layer) {
    layer %>%
      aggregate(fact = scale / 0.05) %>%
      roughness()
  })
})

# Plot to check
par(mfrow = c(2, 3))
iwalk(GCFR_roughness, ~plot(.x$pH, main = glue("GCFR {.y} pH")))
par(op)
par(mfrow = c(2, 3))
iwalk(SWAFR_roughness, ~plot(.x$pH, main = glue("SWAFR {.y} pH")))
par(op)

# Tidy up
GCFR_roughness_matrices <- map(GCFR_roughness,
  ~ .x %>%
    stack() %>%
    as.data.frame() %>%
    na.exclude() %>%
    {cbind(region = "GCFR", .)}
)
SWAFR_roughness_matrices <- map(SWAFR_roughness,
  ~ .x %>%
    stack() %>%
    as.data.frame() %>%
    na.exclude() %>%
    {cbind(region = "SWAFR", .)}
)
roughness_matrices <- map2(
  GCFR_roughness_matrices, SWAFR_roughness_matrices,
  rbind
)

# Calculate roughness in Larsen grid cells -------------------------------------

# The goal, as I have learnt is the easiest approach, is to construct a
# SpatialPixelsDataFrame to collate environmental and species data.

# First, get the centroids of each QDS polygon
GCFR_QDS_centroids <- map_df(GCFR_QDS@polygons,
  ~ data.frame(
    x = .x@labpt[[1]],
    y = .x@labpt[[2]]
  )
)
SWAFR_QDS_centroids <- map_df(SWAFR_QDS@polygons,
  ~ data.frame(
    x = .x@labpt[[1]],
    y = .x@labpt[[2]]
  )
)

# Put those in a Spatial**Points**DataFrame
GCFR_QDS_centroids_pts <- SpatialPointsDataFrame(
  coords = GCFR_QDS_centroids,
  data   = GCFR_QDS@data["qdgc"]
)
SWAFR_QDS_centroids_pts <- SpatialPointsDataFrame(
  coords = SWAFR_QDS_centroids,
  data   = SWAFR_QDS@data["qdgc"]
)

# Plot to inspect
plot(GCFR_QDS_centroids_pts, col = "red")
plot(GCFR_QDS, add = TRUE)
plot(SWAFR_QDS_centroids_pts, col = "red")
plot(SWAFR_QDS, add = TRUE)
# Good, they line up exactly as they should: the centroids (red) fall
# in between the the "grid" of polygons (black)

# Use those points to make a SpatialPixelsDataFrame
GCFR_QDS_pixels <- SpatialPixelsDataFrame(
  points      = GCFR_QDS_centroids_pts,
  proj4string = crs(GCFR_QDS),
  data        = GCFR_QDS@data["qdgc"]
)
SWAFR_QDS_pixels <- SpatialPixelsDataFrame(
  points      = SWAFR_QDS_centroids_pts,
  proj4string = crs(SWAFR_QDS),
  data        = SWAFR_QDS@data["qdgc"]
)

# Coerce QDS codes from factor to character
GCFR_QDS_pixels@data$qdgc  %<>% as.character()
SWAFR_QDS_pixels@data$qdgc %<>% as.character()

# Now, extract all environmental data in these pixels
GCFR_QDS_pixels@data %<>% cbind(
  extract(stack(GCFR_variables_masked2),  GCFR_QDS_pixels)
)
SWAFR_QDS_pixels@data %<>% cbind(
  extract(stack(SWAFR_variables_masked2), SWAFR_QDS_pixels)
)

# Tidy into a dataframe
GCFR_QDS_variables_cells <-
  cbind(GCFR_QDS_pixels@coords, GCFR_QDS_pixels@data) %>%
  na.exclude() %>%
  as_tibble() %>%
  rename(lon = x, lat = y)
SWAFR_QDS_variables_cells <-
  cbind(SWAFR_QDS_pixels@coords, SWAFR_QDS_pixels@data) %>%
  na.exclude() %>%
  as_tibble() %>%
  rename(lon = x, lat = y)
GCFR_QDS_variables_cells
SWAFR_QDS_variables_cells
# Yay!

# Inspect visually
ggplot(GCFR_QDS_variables_cells, aes(lon, lat)) +
  geom_tile(aes(fill = MAP)) +
  geom_path(data = GCFR_QDS,    aes(long, lat, group = group), col = "grey") +
  geom_path(data = GCFR_border, aes(long, lat, group = group))
ggplot(SWAFR_QDS_variables_cells, aes(lon, lat)) +
  geom_tile(aes(fill = MAP)) +
  geom_path(data = SWAFR_QDS,    aes(long, lat, group = group), col = "grey") +
  geom_path(data = SWAFR_border, aes(long, lat, group = group))
# Seems good!

# Now to calculate roughness again, but differently from before,
# in order to align with the species turnover analysis to come.

# Define roughness **here** as the mean of sub-cells' mean absolute differences
# from their con-cellulars (i.e. & e.g.: other QDS in an HDS)
roughness_cells <- function(x) {
  out <- vector(length = length(x))
  for (i in seq_along(x)) {
    out[[i]] <- mean(abs(x[i] - x[-i]))
  }
  mean(out)
}
# Test
roughness_cells(1:4)
# Good.

# Tidy further and calculate roughness in cells sensu stricto
QDS_variables_cells <- rbind(
  cbind(region = "GCFR",  GCFR_QDS_variables_cells),
  cbind(region = "SWAFR", SWAFR_QDS_variables_cells)
)
QDS_roughness_cells <- QDS_variables_cells %>%
  gather(variable, value, Elevation:pH) %>%
  mutate(hdgc = str_remove(qdgc, ".$")) %>%
  group_by(region, hdgc, variable) %>%
  summarise(
    lon = mean(lon),
    lat = mean(lat),
    n_QDS = n(),
    roughness = map_dbl(list(value), roughness_cells)
  ) %>%
  ungroup() %>%
  filter(n_QDS >= 2)

# Plot to check
ggplot(QDS_roughness_cells, aes(region, roughness)) +
  geom_boxplot() +
  facet_grid(variable ~ ., scales = "free_y")

# Now to PCA **these** data
QDS_roughness_cells_prepped <- QDS_roughness_cells %>%
  dplyr::select(region, hdgc, variable, roughness) %>%
  spread(variable, roughness) %>%
  #dplyr::select(-hdgc) %>%
  as.data.frame()
QDS_roughness_cells_PCA <- prcomp(
  QDS_roughness_cells_prepped[, -c(1, 2)],
  scale. = TRUE
)

# Force PC1 scores to be positive if all vars rotations are negative
if (all(QDS_roughness_cells_PCA$rotation[, 1] <= 0)) {
  QDS_roughness_cells_PCA$rotation[, 1] %<>% multiply_by(-1)
  QDS_roughness_cells_PCA$x[, 1]        %<>% multiply_by(-1)
}

# Plot to inspect
autoplot(
  QDS_roughness_cells_PCA,
  data   = QDS_roughness_cells_prepped,
  colour = "region"
)

# Store PC1--2 in the original data, for later modelling etc.
#QDS_roughness_cells$PC1 <- QDS_roughness_cells_PCA$x[, 1]
#QDS_roughness_cells$PC2 <- QDS_roughness_cells_PCA$x[, 2]

# Inspect rotations/loadings too
QDS_roughness_cells_PCA %$%
  rotation %>%
  {tibble(
    var = rownames(.),
    PC1 = .[, 1],
    PC2 = .[, 2]
  )} %>%
  gather(PC, rotation, -var) %>%
  ggplot(aes(var, rotation)) +
    geom_col() +
    facet_wrap(~PC, scales = "free") +
    theme(axis.text.x = element_text(angle = 45))

# And again but QDS vs EDS a.o.t. HDS vs QDS -----------------------------------

# Import EDS polygons
ZA_EDS <- readOGR(here("data/raw-data/QDGC/qdgc_zaf"), layer = "qdgc_03_zaf")
AU_EDS <- readOGR(here("data/raw-data/QDGC/qdgc_aus"), layer = "qdgc_03_aus")

# Crop to regions
GCFR_EDS  <- crop(ZA_EDS, GCFR_box)
SWAFR_EDS <- crop(AU_EDS, SWAFR_box)

# First, get the centroids of each EDS polygon
GCFR_EDS_centroids <- map_df(GCFR_EDS@polygons,
  ~ data.frame(
    x = .x@labpt[[1]],
    y = .x@labpt[[2]]
  )
)
SWAFR_EDS_centroids <- map_df(SWAFR_EDS@polygons,
  ~ data.frame(
    x = .x@labpt[[1]],
    y = .x@labpt[[2]]
  )
)

# Put those in a Spatial**Points**DataFrame
GCFR_EDS_centroids_pts <- SpatialPointsDataFrame(
  coords = GCFR_EDS_centroids,
  data   = GCFR_EDS@data["qdgc"]
)
SWAFR_EDS_centroids_pts <- SpatialPointsDataFrame(
  coords = SWAFR_EDS_centroids,
  data   = SWAFR_EDS@data["qdgc"]
)

# Plot to inspect
plot(GCFR_EDS_centroids_pts, col = "red")
plot(GCFR_EDS, add = TRUE)
plot(SWAFR_EDS_centroids_pts, col = "red")
plot(SWAFR_EDS, add = TRUE)
# Good, they line up exactly as they should: the centroids (red) fall
# in between the the "grid" of polygons (black)

# Use those points to make a SpatialPixelsDataFrame
GCFR_EDS_pixels <- SpatialPixelsDataFrame(
  points      = GCFR_EDS_centroids_pts,
  proj4string = crs(GCFR_EDS),
  data        = GCFR_EDS@data["qdgc"]
)
SWAFR_EDS_pixels <- SpatialPixelsDataFrame(
  points      = SWAFR_EDS_centroids_pts,
  proj4string = crs(SWAFR_EDS),
  data        = SWAFR_EDS@data["qdgc"]
)

# Coerce QDS codes from factor to character
GCFR_EDS_pixels@data$qdgc  %<>% as.character()
SWAFR_EDS_pixels@data$qdgc %<>% as.character()

# Now, extract all environmental data in these pixels
GCFR_EDS_pixels@data %<>% cbind(
  extract(stack(GCFR_variables_masked2),  GCFR_EDS_pixels)
)
SWAFR_EDS_pixels@data %<>% cbind(
  extract(stack(SWAFR_variables_masked2), SWAFR_EDS_pixels)
)

# Tidy into a dataframe
GCFR_EDS_variables_cells <-
  cbind(GCFR_EDS_pixels@coords, GCFR_EDS_pixels@data) %>%
  na.exclude() %>%
  as_tibble() %>%
  rename(lon = x, lat = y)
SWAFR_EDS_variables_cells <-
  cbind(SWAFR_EDS_pixels@coords, SWAFR_EDS_pixels@data) %>%
  na.exclude() %>%
  as_tibble() %>%
  rename(lon = x, lat = y)
GCFR_EDS_variables_cells
SWAFR_EDS_variables_cells
# Yay!

# Inspect visually
ggplot(GCFR_EDS_variables_cells, aes(lon, lat)) +
  geom_tile(aes(fill = MAP)) +
  geom_path(data = GCFR_EDS,    aes(long, lat, group = group), col = "grey") +
  geom_path(data = GCFR_border, aes(long, lat, group = group))
ggplot(SWAFR_EDS_variables_cells, aes(lon, lat)) +
  geom_tile(aes(fill = MAP)) +
  geom_path(data = SWAFR_EDS,    aes(long, lat, group = group), col = "grey") +
  geom_path(data = SWAFR_border, aes(long, lat, group = group))
# Seems good!

# Now to calculate roughness again, but differently from before,
# in order to align with the species turnover analysis to come.

# Tidy further and calculate roughness in cells sensu stricto
EDS_variables_cells <- rbind(
  cbind(region = "GCFR",  GCFR_EDS_variables_cells),
  cbind(region = "SWAFR", SWAFR_EDS_variables_cells)
)
EDS_roughness_cells <- EDS_variables_cells %>%
  rename(edgc = qdgc) %>%  # although called "qdgc", it is actually for EDS
  gather(variable, value, Elevation:pH) %>%
  mutate(qdgc = str_remove(edgc, ".$")) %>%  # now "qdgc" sensu stricto
  group_by(region, qdgc, variable) %>%
  summarise(
    lon = mean(lon),
    lat = mean(lat),
    n_EDS = n(),
    roughness = map_dbl(list(value), roughness_cells)
  ) %>%
  ungroup() %>%
  filter(n_EDS >= 2)

# Plot to check
ggplot(EDS_roughness_cells, aes(region, roughness)) +
  geom_boxplot() +
  facet_grid(variable ~ ., scales = "free_y")

# Now to PCA **these** data
EDS_roughness_cells_prepped <- EDS_roughness_cells %>%
  dplyr::select(region, qdgc, variable, roughness) %>%
  spread(variable, roughness) %>%
  #dplyr::select(-qdgc) %>%
  as.data.frame()
EDS_roughness_cells_PCA <- prcomp(
  EDS_roughness_cells_prepped[, -c(1, 2)],
  scale. = TRUE
)

# Force PC1 scores to be positive if all vars rotations are negative
if (all(EDS_roughness_cells_PCA$rotation[, 1] <= 0)) {
  EDS_roughness_cells_PCA$rotation[, 1] %<>% multiply_by(-1)
  EDS_roughness_cells_PCA$x[, 1]        %<>% multiply_by(-1)
}

# Plot to inspect
autoplot(
  EDS_roughness_cells_PCA,
  data   = EDS_roughness_cells_prepped,
  colour = "region"
)

# Store PC1--2 in the original data, for later modelling etc.
#EDS_roughness_cells$PC1 <- EDS_roughness_cells_PCA$x[, 1]
#EDS_roughness_cells$PC2 <- EDS_roughness_cells_PCA$x[, 2]

# Inspect rotations/loadings too
EDS_roughness_cells_PCA %$%
  rotation %>%
  {tibble(
    var = rownames(.),
    PC1 = .[, 1],
    PC2 = .[, 2]
  )} %>%
  gather(PC, rotation, -var) %>%
  ggplot(aes(var, rotation)) +
    geom_col() +
    facet_wrap(~PC, scales = "free") +
    theme(axis.text.x = element_text(angle = 45))

# Species turnover -------------------------------------------------------------

GCFR_species <- read_rds(here(
  "data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_species"
))
SWAFR_species <- read_rds(here(
  "data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_species"
))

# Put QDS geocodes in species SpatialPointsDataFrame
qdgc2hdgc <- function(x) {
  # QDS -> HDS by dropping the last letter
  substr(x, 1, nchar(x) - 1)
}
get_geocodes <- function(flora_points, QDS_polygon) {
  flora_points@data$qdgc <- over(flora_points, QDS_polygon)[[1]]
  flora_points@data$hdgc <- map_chr(flora_points@data$qdgc, ~
    .x %>%
      as.character() %>%
      qdgc2hdgc()
  )
  flora_points
}

# Calculate turnover between QDS within HDS
calc_richness_turnover <- function(flora_points, QDS_polygon, output_path,
                                   region_name = NULL, date = NULL) {
  # Master function to calculate richness and turnover metrics in grid cells
  # using a SpatialPointsDataFrame of species occurrences
  stopifnot(exprs = {
    class(flora_points) == "SpatialPointsDataFrame"
    class(QDS_polygon)  == "SpatialPolygonsDataFrame"
  })

  # Get the QDS and HDS geocodes -----------------------------------------------

  flora_points %<>% get_geocodes(QDS_polygon[, "qdgc"])

  # Calculate average Jaccard distance betw QDS in each HDS --------------------

  # Init empty columns for data to come
  flora_points@data$HDS_richness <- NA
  flora_points@data$n_QDS <- NA
  flora_points@data$mean_QDS_richness <- NA
  flora_points@data$mean_QDS_turnover <- NA

  #flora_points@data$lat <- NA
  #flora_points@data$lon <- NA

  HDS_cells <- levels(factor(flora_points$hdgc))
  pb <- txtProgressBar(0, length(HDS_cells))
  for (i in seq_along(HDS_cells)) {

    # Current HDS geocode name
    current_HDS <- HDS_cells[[i]]

    # Filter to only those QDS w/i the current HDS
    spp_in_hdgc_by_qdgc <- flora_points@data %>%
      filter(hdgc == current_HDS) %>%
      dplyr::select(species, qdgc)

    # Construct a community matrix containing the 4 QDS
    # Make an empty matrix:
    community_matrix <- matrix(
      nrow = length(unique(spp_in_hdgc_by_qdgc$qdgc)),
      ncol = length(unique(spp_in_hdgc_by_qdgc$species)),
      dimnames = list(
        unique(spp_in_hdgc_by_qdgc$qdgc),
        unique(spp_in_hdgc_by_qdgc$species)
      )
    )
    # Fill it:
    for (j in seq(nrow(community_matrix))) {
      for (k in seq(ncol(community_matrix))) {
        community_matrix[j, k] <-
          spp_in_hdgc_by_qdgc %>%
          filter(
            qdgc    == rownames(community_matrix)[[j]],
            species == colnames(community_matrix)[[k]]
          ) %>%
          magrittr::extract2("species") %>%
          unique() %>%
          is_empty() %>%
          not()
      }
    }

    # Output mean Jaccard + richness measures ----------------------------------

    # Output the summary data to the right rows in the spatial points dataframe
    flora_points$HDS_richness[flora_points$hdgc == current_HDS] <-
      community_matrix %>%
      colnames() %>%
      length()
    flora_points$mean_QDS_turnover[flora_points$hdgc == current_HDS] <-
      community_matrix %>%
      vegdist(method = "jaccard") %>%
      mean(na.rm = TRUE)
    flora_points$n_QDS[flora_points$hdgc == current_HDS] <-
      community_matrix %>%
      nrow()
    flora_points$mean_QDS_richness[flora_points$hdgc == current_HDS] <-
      community_matrix %>%
      t() %>%
      as.data.frame() %>%
      summarise_if(is.logical, function(x) length(x[x])) %>%
      t() %>%
      mean(na.rm = TRUE)

    #flora_points$lat[flora_points$hdgc == current_HDS] <-
    #flora_points$lon[flora_points$hdgc == current_HDS] <-

    setTxtProgressBar(pb, i)

  }
  close(pb)

  # Save to disc ---------------------------------------------------------------

  if (is.null(date)) {
    date <- Sys.Date()
  }
  if (is.null(region_name)) {
    region_name <- "region"
  }
  for (layer in names(flora_points)) {
    writeOGR(
      flora_points,
      dsn    = glue("{output_path}/{region_name}_species_{date}"),
      layer  = layer,
      driver = "ESRI Shapefile"
    )
  }

  flora_points
}

output_path <- here("outputs/turnover")

GCFR_species_path  <- glue("{output_path}/GCFR_species_2019-06-05/")
SWAFR_species_path <- glue("{output_path}/SWAFR_species_2019-06-05/")

if (!file.exists(GCFR_species_path)) {
  # Read in processed GBIF occurence data as SpatialPointsDataFrames
  trimmed_GCFR_clean_flora_spdf_species <- read_rds(here(
    "data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_species"
  ))
  GCFR_species <- calc_richness_turnover(
    flora_points = trimmed_GCFR_clean_flora_spdf_species,
    QDS_polygon  = GCFR_QDS,
    output_path  = output_path,
    region_name  = "GCFR"
  )
} else {
  GCFR_species <- readOGR(GCFR_species_path)
}

if (!file.exists(SWAFR_species_path)) {
  trimmed_SWAFR_clean_flora_spdf_species <- read_rds(here(
    "data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_species"
  ))
  SWAFR_species <- calc_richness_turnover(
    flora_points = trimmed_SWAFR_clean_flora_spdf_species,
    QDS_polygon  = SWAFR_QDS,
    output_path  = output_path,
    region_name  = "SWAFR"
  )
} else {
  SWAFR_species <- readOGR(SWAFR_species_path)
}

# Combine species turnover and richness (cellular) datasets --------------------

names(GCFR_species) <- names(SWAFR_species) <- c(
  "species", "cell_nos", "qdgc", "hdgc",
  "HDS_richness", "n_QDS", "mean_QDS_richness", "mean_QDS_turnover"
)
GCFR_species_data <- GCFR_species@data %>%
  dplyr::select(-species, -cell_nos, -qdgc) %>%
  distinct()
SWAFR_species_data <- SWAFR_species@data %>%
  dplyr::select(-species, -cell_nos, -qdgc) %>%
  distinct()
QDS_species_data <-
  rbind(
    cbind(region = "GCFR",  GCFR_species_data),
    cbind(region = "SWAFR", SWAFR_species_data)
  ) %>%
  as_tibble() %>%
  filter(n_QDS >= 2) %>%
  mutate(
    add_turnover      = HDS_richness - mean_QDS_richness,
    add_turnover_prop = add_turnover / HDS_richness,
    mul_turnover      = HDS_richness / mean_QDS_richness
  )

QDS_species_data

QDS_roughness_cells_prepped$PC1 <- QDS_roughness_cells_PCA$x[, 1]
QDS_roughness_cells_prepped$PC2 <- QDS_roughness_cells_PCA$x[, 2]

QDS_data_cells <- QDS_roughness_cells %>%
  spread(variable, roughness) %>%
  full_join(QDS_roughness_cells_prepped[,c("region", "hdgc", "PC1", "PC2")]) %>%
  full_join(QDS_species_data) %>%
  na.exclude()

ggplot(QDS_data_cells, aes(lon, lat, colour = HDS_richness)) +
  geom_point(size = 3) +
  facet_grid(~region, scales = "free")
ggplot(QDS_data_cells, aes(lon, lat, colour = mul_turnover)) +
  geom_point(size = 3) +
  facet_grid(~region, scales = "free")
ggplot(QDS_data_cells, aes(lon, lat, colour = PC1)) +
  geom_point(size = 3) +
  facet_grid(~region, scales = "free")
ggplot(QDS_data_cells, aes(lon, lat, colour = PC2)) +
  geom_point(size = 3) +
  facet_grid(~region, scales = "free")

ggplot(QDS_data_cells) +
  aes(
    PC1, HDS_richness,
    colour = region
  ) +
  geom_point()
ggplot(QDS_data_cells) +
  aes(
    PC1/n_QDS, HDS_richness/n_QDS,
    colour = region
  ) +
  geom_point()
ggplot(QDS_data_cells) +
  aes(
    PC1, mul_turnover,
    colour = region
  ) +
  geom_point()
ggplot(QDS_data_cells) +
  aes(
    PC1/n_QDS, mul_turnover/n_QDS,
    colour = region
  ) +
  geom_point()
ggplot(QDS_data_cells) +
  aes(
    PC1, mean_QDS_turnover,
    colour = region
  ) +
  geom_point()
ggplot(QDS_data_cells) +
  aes(
    PC1/n_QDS, mean_QDS_turnover/n_QDS,
    colour = region
  ) +
  geom_point()
ggplot(QDS_data_cells) +
  aes(
    add_turnover_prop, HDS_richness,
    colour = region
  ) +
  geom_point()
ggplot(QDS_data_cells) +
  aes(
    add_turnover_prop/n_QDS, HDS_richness/n_QDS,
    colour = region
  ) +
  geom_point()

write_csv(QDS_data_cells, here("outputs/QDS_data_cells.csv"))

# Now again for QDS-EDS --------------------------------------------------------

GCFR_species %<>% get_geocodes(GCFR_EDS[, "qdgc"])
GCFR_species_data_EDS <- GCFR_species@data %>%
  rename(edgc = qdgc, qdgc = hdgc) %>%
  #as_tibble() %>%
  #dplyr::select(edgc, qdgc) %>%
  group_by(qdgc) %>%
  summarise(
    QDS_richness = length(unique(species)),
    n_EDS        = length(unique(edgc))
  )
SWAFR_species %<>% get_geocodes(SWAFR_EDS[, "qdgc"])
SWAFR_species_data_EDS <- SWAFR_species@data %>%
  rename(edgc = qdgc, qdgc = hdgc) %>%
  #as_tibble() %>%
  #dplyr::select(edgc, qdgc) %>%
  group_by(qdgc) %>%
  summarise(
    QDS_richness = length(unique(species)),
    n_EDS        = length(unique(edgc))
  )
EDS_species_data <-
  rbind(
    cbind(region = "GCFR",  GCFR_species_data_EDS),
    cbind(region = "SWAFR", SWAFR_species_data_EDS)
  ) %>%
  filter(n_EDS >= 2)

EDS_species_data

EDS_roughness_cells_prepped$PC1 <- EDS_roughness_cells_PCA$x[, 1]
EDS_roughness_cells_prepped$PC2 <- EDS_roughness_cells_PCA$x[, 2]

EDS_data_cells <- EDS_roughness_cells %>%
  spread(variable, roughness) %>%
  full_join(EDS_roughness_cells_prepped[,c("region", "qdgc", "PC1", "PC2")]) %>%
  full_join(EDS_species_data) %>%
  na.exclude()

EDS_data_cells

ggplot(EDS_data_cells, aes(lon, lat, colour = QDS_richness)) +
  geom_point(size = 3) +
  facet_grid(~region, scales = "free")

ggplot(EDS_data_cells) +
  aes(PC1, QDS_richness, colour = region) +
  geom_point()
ggplot(EDS_data_cells) +
  aes(PC1/n_EDS, QDS_richness/n_EDS, colour = region) +
  geom_point()
ggplot(EDS_data_cells) +
  aes(PC2, QDS_richness, colour = region) +
  geom_point()
ggplot(EDS_data_cells) +
  aes(PC2/n_EDS, QDS_richness/n_EDS, colour = region) +
  geom_point()

write_csv(EDS_data_cells, here("outputs/EDS_data_cells.csv"))
