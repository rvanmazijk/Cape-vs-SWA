# Heterogeneity and species richness: Larsen et al. 2009 grids
# R. van Mazijk
# CC-BY-4.0 2019

# Import region polygons -------------------------------------------------------

GCFR_border_buffered <-
  readOGR(here("data/derived-data/borders/GCFR_border_buffered/"))
SWAFR_border_buffered <-
  readOGR(here("data/derived-data/borders/SWAFR_border_buffered/"))

GCFR_box  <- readOGR(here("data/derived-data/borders/GCFR_box"))
SWAFR_box <- readOGR(here("data/derived-data/borders/SWAFR_box"))

# Import Larsen grid polygons --------------------------------------------------

ZA_EDS <- readOGR(here("data/raw-data/QDGC/qdgc_zaf"), layer = "qdgc_03_zaf")
AU_EDS <- readOGR(here("data/raw-data/QDGC/qdgc_aus"), layer = "qdgc_03_aus")
ZA_QDS <- readOGR(here("data/raw-data/QDGC/qdgc_zaf"), layer = "qdgc_02_zaf")
AU_QDS <- readOGR(here("data/raw-data/QDGC/qdgc_aus"), layer = "qdgc_02_aus")
ZA_HDS <- readOGR(here("data/raw-data/QDGC/qdgc_zaf"), layer = "qdgc_01_zaf")
AU_HDS <- readOGR(here("data/raw-data/QDGC/qdgc_aus"), layer = "qdgc_01_aus")

# Generate polygons I need -----------------------------------------------------

# Merge regions' borders
borders_buffered <- rbind(GCFR_border_buffered, SWAFR_border_buffered)

# Crop each Larsen grid to regions' extents
GCFR_EDS  <- crop(ZA_EDS, GCFR_box)
SWAFR_EDS <- crop(AU_EDS, SWAFR_box)
GCFR_QDS  <- crop(ZA_QDS, GCFR_box)
SWAFR_QDS <- crop(AU_QDS, SWAFR_box)
GCFR_HDS  <- crop(ZA_HDS, GCFR_box)
SWAFR_HDS <- crop(AU_HDS, SWAFR_box)

# Merge regions' Larsen grids
Larsen_grid_EDS <- rbind(GCFR_EDS, SWAFR_EDS)
Larsen_grid_QDS <- rbind(GCFR_QDS, SWAFR_QDS)
Larsen_grid_HDS <- rbind(GCFR_HDS, SWAFR_HDS)

# Tidy up Larsen grid data and note QDS, HDS and DS
Larsen_grid_EDS$edgc <- as.character(Larsen_grid_EDS$qdgc)
Larsen_grid_EDS$qdgc <- str_remove(Larsen_grid_EDS$edgc, ".$")
Larsen_grid_EDS$hdgc <- str_remove(Larsen_grid_EDS$qdgc, ".$")
Larsen_grid_EDS$dgc  <- str_remove(Larsen_grid_EDS$hdgc, ".$")

Larsen_grid_QDS$qdgc <- as.character(Larsen_grid_QDS$qdgc)
Larsen_grid_QDS$hdgc <- str_remove(Larsen_grid_QDS$qdgc, ".$")
Larsen_grid_QDS$dgc  <- str_remove(Larsen_grid_QDS$hdgc, ".$")

Larsen_grid_HDS$hdgc <- as.character(Larsen_grid_HDS$qdgc)
Larsen_grid_HDS$dgc  <- str_remove(Larsen_grid_HDS$hdgc, ".$")

# Make numeric data properly numeric instead of factors
Larsen_grid_EDS@data[, c("lon", "lat", "areakm2")] %<>%
  map(as.character) %>%
  map(as.numeric)
Larsen_grid_QDS@data[, c("lon", "lat", "areakm2")] %<>%
  map(as.character) %>%
  map(as.numeric)
Larsen_grid_HDS@data[, c("lon", "lat", "areakm2")] %<>%
  map(as.character) %>%
  map(as.numeric)
# NOTE: lon, lat data for grid polygons are the midpoints

# Detemine which DS, HDS & QDS have all 4 of their HDS, QDS & EDS --------------
# (within the regions' borders)

# Query the border polygon and store results in Larsen grid
Larsen_grid_EDS@data <- cbind(
  Larsen_grid_EDS@data,
  Larsen_grid_EDS %over% borders_buffered
)
# For larger cells, just use longitude for region classification,
# because later I filter the cells based on whether their constituent cells
# are in the regions (from EDS above)
Larsen_grid_QDS@data$region <-
  ifelse(Larsen_grid_QDS@data$lon > 90,
    "SWAFR", "GCFR"
  )
Larsen_grid_HDS@data$region <-
  ifelse(Larsen_grid_HDS@data$lon > 90,
    "SWAFR", "GCFR"
  )

# Filter to EDS that are within the regions' borders
Larsen_grid_EDS <- Larsen_grid_EDS[!is.na(Larsen_grid_EDS$region), ]

# Make grids tibbles for easier wrangling
Larsen_grid_EDS_data <- as_tibble(Larsen_grid_EDS@data)
Larsen_grid_QDS_data <- as_tibble(Larsen_grid_QDS@data)
Larsen_grid_HDS_data <- as_tibble(Larsen_grid_HDS@data)

# Pull out QDS-codes of QDS with all 4 EDS (within borders)
QDS_w_all_EDS <- Larsen_grid_EDS_data %>%
  group_by(qdgc, region) %>%
  dplyr::select(edgc) %>%
  distinct() %>%  # just in case
  summarise(n_EDS = n()) %>%
  filter(n_EDS == 4) %>%
  pull(qdgc)

# Pull out HDS-codes of HDS with all 4 HDS (within borders)
HDS_w_all_QDS <- Larsen_grid_QDS_data %>%
  group_by(hdgc, region) %>%
  dplyr::select(qdgc) %>%
  distinct() %>%
  filter(qdgc %in% QDS_w_all_EDS) %>%
  summarise(n_QDS = n()) %>%
  filter(n_QDS == 4) %>%
  pull(hdgc)

# Pull out DS-codes of DS with all 4 DS (within borders)
DS_w_all_HDS <- Larsen_grid_HDS_data %>%
  group_by(dgc, region) %>%
  dplyr::select(hdgc) %>%
  distinct() %>%
  filter(hdgc %in% HDS_w_all_QDS) %>%
  summarise(n_HDS = n()) %>%
  filter(n_HDS == 4) %>%
  pull(dgc)

# Plot grids and cell midpoints to check
# NOTE:
#   Plots EDS that belong to a QDS with all 4 EDS in region,
#   **not** the QDS themselves,
#   etc. for other scales
if (FALSE) {
  # GCFR:
  plot(border = "green", Larsen_grid_EDS[
    Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS &
    Larsen_grid_EDS$region == "GCFR",
  ])
  points(col = "green",
    Larsen_grid_EDS$lon[Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS],
    Larsen_grid_EDS$lat[Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS]
  )
  plot(border = "red", add = TRUE, Larsen_grid_QDS[
    Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS &
    Larsen_grid_QDS$region == "GCFR",
  ])
  points(col = "red",
    Larsen_grid_QDS$lon[Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS],
    Larsen_grid_QDS$lat[Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS]
  )
  plot(border = "blue", add = TRUE, Larsen_grid_HDS[
    Larsen_grid_HDS$dgc %in% DS_w_all_HDS &
    Larsen_grid_HDS$region == "GCFR",
  ])
  points(col = "blue",
    Larsen_grid_HDS$lon[Larsen_grid_HDS$dgc %in% DS_w_all_HDS],
    Larsen_grid_HDS$lat[Larsen_grid_HDS$dgc %in% DS_w_all_HDS]
  )

  # SWAFR:
  plot(border = "green", Larsen_grid_EDS[
    Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS &
    Larsen_grid_EDS$region == "SWAFR",
  ])
  plot(border = "red", add = TRUE, Larsen_grid_QDS[
    Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS &
    Larsen_grid_QDS$region == "SWAFR",
  ])
  plot(border = "blue", add = TRUE, Larsen_grid_HDS[
    Larsen_grid_HDS$dgc %in% DS_w_all_HDS &
    Larsen_grid_HDS$region == "SWAFR",
  ])
}

# Create my own blank rasters of/from the Larsen grids -------------------------

# (Remake un-filtered EDS grid)
Larsen_grid_EDS_raw <- rbind(GCFR_EDS, SWAFR_EDS)

Larsen_grid_EDS_ras <- grid2raster(Larsen_grid_EDS_raw, 0.125)
Larsen_grid_QDS_ras <- grid2raster(Larsen_grid_QDS,     0.25)
Larsen_grid_HDS_ras <- grid2raster(Larsen_grid_HDS,     0.5)

# Plots to check
if (FALSE) {
  # Create dummy data in rasters for plottign
  Larsen_grid_EDS_ras2 <- Larsen_grid_EDS_ras
  Larsen_grid_QDS_ras2 <- Larsen_grid_QDS_ras
  Larsen_grid_HDS_ras2 <- Larsen_grid_HDS_ras
  Larsen_grid_EDS_ras2[] <- 1:ncell(Larsen_grid_EDS_ras2)
  Larsen_grid_QDS_ras2[] <- 1:ncell(Larsen_grid_QDS_ras2)
  Larsen_grid_HDS_ras2[] <- 1:ncell(Larsen_grid_HDS_ras2)

  # Plot rasters and region polygons to check extents
  plot(Larsen_grid_EDS_ras2)
  plot(borders_buffered, add = TRUE)

  plot(Larsen_grid_QDS_ras2)
  plot(borders_buffered, add = TRUE)

  plot(Larsen_grid_HDS_ras2)
  plot(borders_buffered, add = TRUE)

  # Plot rasters and cells midpoints check
  Larsen_grid_EDS_ras2 %>%
    crop(GCFR_border_buffered) %>%
    {
      plot(.)
      points(xyFromCell(., 1:ncell(.)))
    }
  Larsen_grid_QDS_ras2 %>%
    crop(GCFR_border_buffered) %>%
    {
      plot(.)
      points(xyFromCell(., 1:ncell(.)))
    }
  Larsen_grid_HDS_ras2 %>%
    crop(GCFR_border_buffered) %>%
    {
      plot(.)
      points(xyFromCell(., 1:ncell(.)))
    }
}

# Test putting arbitrary data into raster via lon-lat lookup from tibble -------

Larsen_grid_EDS_ras2 <- Larsen_grid_EDS_ras
cells_to_fill <- cellFromXY(
  Larsen_grid_EDS_ras2,
  as.data.frame(Larsen_grid_EDS_data[, c("lon", "lat")])
)
Larsen_grid_EDS_ras2[cells_to_fill] <- Larsen_grid_EDS_data$areakm2
plot(Larsen_grid_EDS_ras2)

# Save these blank template rasters --------------------------------------------

Larsen_grid_EDS_ras[] <- 0
writeRaster(
  Larsen_grid_EDS_ras,
  here("data/derived-data/May-2019/Larsen_grid_EDS_ras.tif"),
  overwrite = TRUE
)

Larsen_grid_QDS_ras[] <- 0
writeRaster(
  Larsen_grid_QDS_ras,
  here("data/derived-data/May-2019/Larsen_grid_QDS_ras.tif"),
  overwrite = TRUE
)

Larsen_grid_HDS_ras[] <- 0
writeRaster(
  Larsen_grid_HDS_ras,
  here("data/derived-data/May-2019/Larsen_grid_HDS_ras.tif"),
  overwrite = TRUE
)

# Collate richness data into grids ---------------------------------------------

make_SpatialPointsDataFrame <- function(df) {
  SpatialPointsDataFrame(
    coords      = df[, c("decimallongitude", "decimallatitude")],
    data        = df[, "species"],
    proj4string = crs(borders_buffered)
  )
}
GCFR_species_occ <- make_SpatialPointsDataFrame(read_csv(here(
  "data/derived-data/flora",
  "GCFR_clean_flora_2017-09-14.csv"
)))
SWAFR_species_occ <- make_SpatialPointsDataFrame(read_csv(here(
  "data/derived-data/flora",
  "SWAFR_clean_flora_2017-09-14.csv"
)))
species_occ <- rbind(GCFR_species_occ, SWAFR_species_occ)

# Add grid codes to species data
species_occ$EDS <- species_occ %over%
  Larsen_grid_EDS %>%
  pull(edgc) %>%
  as.character()
species_occ$QDS <- str_remove(species_occ$EDS, ".$")
species_occ$HDS <- str_remove(species_occ$QDS, ".$")
species_occ$DS  <- str_remove(species_occ$HDS, ".$")

# Export final lists of species
GCFR_species_occ@data %>%
  group_by(species)  %>%
  summarise(n_collections = n()) %>%
  arrange(desc(n_collections)) %>%
  write_csv(here("for-Dryad/GCFR-species.csv"))
SWAFR_species_occ@data %>%
  group_by(species) %>%
  summarise(n_collections = n()) %>%
  arrange(desc(n_collections)) %>%
  write_csv(here("for-Dryad/SWAFR-species.csv"))

# Flag species w/ < 5 collections total in each region
GCFR_bad_species <-
  read_csv(here("for-Dryad/GCFR-species.csv")) %>%
  filter(n_collections < 5) %>%
  pull(species)
SWAFR_bad_species <-
  read_csv(here("for-Dryad/SWAFR-species.csv")) %>%
  filter(n_collections < 5) %>%
  pull(species)
# Filter them out
species_occ_data <- species_occ@data %>%
  filter(!(species %in% c(GCFR_bad_species, SWAFR_bad_species))) %>%
  na.exclude()

# Check species counts now:
species_occ_data %>%
  mutate(region = EDS %>%
    str_extract("E[0-9]{3}") %>%
    str_remove("E") %>%
    as.numeric() %>%
    is_greater_than(60) %>%
    ifelse("SWAFR", "GCFR")
  ) %>%
  group_by(region) %>%
  summarise(n_spp = length(unique(species)))
## # A tibble: 2 x 2
##   region n_spp
##   <chr>  <int>
## 1 GCFR    9489
## 2 SWAFR   6757

# Collate richness measures at each scale
# At QDS-scale:
QDS_richness <- species_occ_data %>%
  filter(QDS %in% QDS_w_all_EDS) %>%
  distinct() %>%  # just in case
  group_by(QDS) %>%
  summarise(
    n_collections   = length(species),
    QDS_richness    = length(unique(species))
  )
# At HDS-scale:
mean_QDS_richness <- QDS_richness %>%
  mutate(HDS = str_remove(QDS, ".$")) %>%
  group_by(HDS) %>%
  summarise(mean_QDS_richness = mean(QDS_richness, na.rm = TRUE))
HDS_richness <- species_occ_data %>%
  group_by(HDS) %>%
  filter(HDS %in% HDS_w_all_QDS) %>%
  summarise(
    n_collections   = length(species),
    HDS_richness    = length(unique(species))
  ) %>%
  full_join(mean_QDS_richness)
# At DS-scale:
mean_HDS_richness <- HDS_richness %>%
  mutate(DS = str_remove(HDS, ".$")) %>%
  group_by(DS) %>%
  summarise(mean_HDS_richness = mean(HDS_richness, na.rm = TRUE))
DS_richness <- species_occ_data %>%
  group_by(DS) %>%
  filter(DS %in% DS_w_all_HDS) %>%
  summarise(
    n_collections   = length(species),
    DS_richness     = length(unique(species))
  ) %>%
  full_join(mean_HDS_richness)

# Check
ggplot(DS_richness, aes(mean_HDS_richness, DS_richness - mean_HDS_richness)) +
  geom_point() +
  lims(x = c(0, 3000), y = c(0, 3000))

ggplot(HDS_richness, aes(mean_QDS_richness, HDS_richness - mean_QDS_richness)) +
  geom_point() +
  lims(x = c(0, 3000), y = c(0, 3000))

# Import environmental data ----------------------------------------------------

GCFR_file_names  <- glue("{data_dir}/GCFR_{var_names}_masked2.tif")
SWAFR_file_names <- glue("{data_dir}/SWAFR_{var_names}_masked2.tif")

GCFR_variables  <- stack(GCFR_file_names)
SWAFR_variables <- stack(SWAFR_file_names)

enviro_data <- raster::merge(GCFR_variables, SWAFR_variables)
names(enviro_data)  <- str_replace_all(var_names, " ", "_")

# Check
plot(GCFR_variables[[1]])
plot(border = "green", add = TRUE, Larsen_grid_EDS[
  Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS &
  Larsen_grid_EDS$region == "GCFR",
])
plot(border = "red", add = TRUE, Larsen_grid_QDS[
  Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS &
  Larsen_grid_QDS$region == "GCFR",
])
plot(border = "blue", add = TRUE, Larsen_grid_HDS[
  Larsen_grid_HDS$dgc %in% DS_w_all_HDS &
  Larsen_grid_HDS$region == "GCFR",
])

# Resample environmental data to EDS -------------------------------------------

Larsen_grid_EDS2 <- Larsen_grid_EDS
for (variable in names(enviro_data)) {
  Larsen_grid_EDS2[[variable]] <- extract(
    enviro_data[[variable]],
    Larsen_grid_EDS2,
    fun = mean, na.rm = TRUE
  )
  message(variable, " done")
}
save(Larsen_grid_EDS2, file = "Larsen_grid_EDS2")

#####

# 2020-02-10

GCFR_heterogeneity_QDS1 <- var_names %>%
  str_replace(" ", "_") %>%
  {glue("for-Dryad/raster-layers/GCFR_heterogeneity_{.}_QDS.tif")} %>%
  map(raster) %>%
  stack()

load(file = "Larsen_grid_EDS2")
GCFR_heterogeneity_QDS2 <- Larsen_grid_EDS2@data %>%
  as_tibble() %>%
  filter(region == "GCFR") %>%
  group_by(qdgc) %>%
  dplyr::select(Elevation:pH) %>%
  summarise_if(is.numeric, var) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_if(is.numeric, log10) %>%
  mutate_if(is.numeric, scale)

GCFR_heterogeneity_QDS1 %>%
  as.data.frame() %>%
  map_dfc(log10) %>%
  map_dfc(scale) %>%
  gather(var, val) %>%
  filter(!is.na(val)) %>%
  ggplot() +
    aes(var, val) +
    geom_boxplot() +
    coord_flip()

GCFR_heterogeneity_QDS2 %>%
  gather(var, val, -qdgc) %>%
  ggplot() +
    aes(var, val) +
    geom_boxplot() +
    coord_flip()

#####

heterogeneity_QDS2 <- Larsen_grid_EDS2@data %>%
  as_tibble() %>%
  group_by(region, qdgc) %>%
  dplyr::select(Elevation:pH) %>%
  summarise_if(is.numeric, var) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_if(is.numeric, log10) %>%
  mutate_if(is.numeric, scale)

force_positive_PC1 <- function(PCA) {
  if (all(PCA$rotation[, 1] <= 0)) {
    message("Multiplying this one by -1")
    PCA$rotation[, 1] %<>% multiply_by(-1)
    PCA$x[, 1]        %<>% multiply_by(-1)
  }
  PCA
}

heterogeneity_PCA <- heterogeneity_QDS2 %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

summary(heterogeneity_PCA)

heterogeneity_QDS2$PC1 <- heterogeneity_PCA$x[, 1]
heterogeneity_QDS2$PC2 <- heterogeneity_PCA$x[, 2]

ggplot(heterogeneity_QDS2) +
  aes(PC1, PC2, colour = region) +
  geom_point()

#####

plot(GCFR_heterogeneity_QDS1$GCFR_heterogeneity_Elevation_QDS)
points(xyFromCell(GCFR_heterogeneity_QDS1, 1:1232), pch = 3)
plot(border = "red", add = TRUE, Larsen_grid_QDS[
  Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS &
  Larsen_grid_QDS$region == "GCFR",
])

#####

code2midpt <- function(x, type = c("lon", "lat"), scale = c("QDS", "HDS")) {
  if (scale == "QDS") {
    if (type == "lon") {
      case_when(
        x %in% c("AA", "AC", "CA", "CC") ~ 0.125,
        x %in% c("AB", "AD", "CB", "CD") ~ 0.375,
        x %in% c("BA", "BC", "DA", "DC") ~ 0.625,
        x %in% c("BB", "BD", "DB", "DD") ~ 0.875
      )
    } else if (type == "lat") {
      case_when(
        x %in% c("AA", "AB", "BA", "BB") ~ 0.125,
        x %in% c("AC", "AD", "BC", "BD") ~ 0.375,
        x %in% c("CA", "CB", "DA", "DB") ~ 0.625,
        x %in% c("CC", "CD", "DC", "DD") ~ 0.875
      )
    }
  } else if (scale == "HDS") {
    if (type == "lon") {
      case_when(
        x %in% c("A", "C") ~ 0.25,
        x %in% c("B", "D") ~ 0.75
      )
    } else if (type == "lat") {
      case_when(
        x %in% c("A", "B") ~ 0.25,
        x %in% c("C", "D") ~ 0.75
      )
    }
  }
}

GCFR_heterogeneity_QDS2

outliers2 <- outliers %>%
  mutate(
    lon1 = case_when(
      scale == "QDS" ~ QDS %>%
        str_extract("E\\d{3}") %>%
        str_remove("E") %>%
        as.numeric(),
      scale == "HDS" ~ HDS %>%
        str_extract("E\\d{3}") %>%
        str_remove("E") %>%
        as.numeric(),
      scale == "DS" ~ DS %>%
        str_extract("E\\d{3}") %>%
        str_remove("E") %>%
        as.numeric()
    ),
    lon2 = case_when(
      scale == "QDS" ~ QDS %>%
        str_extract("[ABCD]{2}") %>%
        code2midpt("lon", "QDS"),
      scale == "HDS" ~ HDS %>%
        str_extract("[ABCD]{1}") %>%
        code2midpt("lon", "HDS"),
      scale == "DS" ~ 0.5
    ),
    lat1 = case_when(
      scale == "QDS" ~ QDS %>%
        str_extract("S\\d{2}") %>%
        str_remove("S") %>%
        as.numeric(),
      scale == "HDS" ~ HDS %>%
        str_extract("S\\d{2}") %>%
        str_remove("S") %>%
        as.numeric(),
      scale == "DS" ~ DS %>%
        str_extract("S\\d{2}") %>%
        str_remove("S") %>%
        as.numeric()
    ),
    lat2 = case_when(
      scale == "QDS" ~ QDS %>%
        str_extract("[ABCD]{2}") %>%
        code2midpt("lat", "QDS"),
      scale == "HDS" ~ HDS %>%
        str_extract("[ABCD]{1}") %>%
        code2midpt("lat", "HDS"),
      scale == "DS" ~ 0.5
    ),
  ) %>%
  mutate(
    lon = lon1 + lon2,
    lat = -(lat1 + lat2)  # bc all southern hemisphere
  ) %>%
  dplyr::select(-lon1, -lon2, -lat1, -lat2) %>%
  as.data.frame()
