# Import data ------------------------------------------------------------------

# .... My Larsen-type grid polygons and rasters --------------------------------

Larsen_grid_EDS <- readOGR(
  here("data/derived-data/May-2019/Larsen_grid_EDS"),
  layer = "Larsen_grid_EDS"
)
Larsen_grid_QDS <- readOGR(
  here("data/derived-data/May-2019/Larsen_grid_QDS"),
  layer = "Larsen_grid_QDS"
)
Larsen_grid_HDS <- readOGR(
  here("data/derived-data/May-2019/Larsen_grid_HDS"),
  layer = "Larsen_grid_HDS"
)

Larsen_grid_EDS_ras <- raster(
  here("data/derived-data/May-2019/Larsen_grid_EDS_ras.tif")
)
Larsen_grid_QDS_ras <- raster(
  here("data/derived-data/May-2019/Larsen_grid_QDS_ras.tif")
)
Larsen_grid_HDS_ras <- raster(
  here("data/derived-data/May-2019/Larsen_grid_HDS_ras.tif")
)
Larsen_grid_DS_ras <- raster(
  here("data/derived-data/May-2019/Larsen_grid_DS_ras.tif")
)

# .... Region polygons ---------------------------------------------------------

GCFR_border_buffered <- readOGR(
  here("data/derived-data/borders/GCFR_border_buffered/")
)
SWAFR_border_buffered <- readOGR(
  here("data/derived-data/borders/SWAFR_border_buffered/")
)

# Merge regions' borders
borders_buffered <- rbind(GCFR_border_buffered, SWAFR_border_buffered)

# .... Environmental data ------------------------------------------------------

GCFR_file_names  <- glue("{data_dir}/GCFR_{var_names}_masked2.tif")
SWAFR_file_names <- glue("{data_dir}/SWAFR_{var_names}_masked2.tif")

GCFR_variables  <- stack(GCFR_file_names)
SWAFR_variables <- stack(SWAFR_file_names)

enviro_data <- raster::merge(GCFR_variables, SWAFR_variables)
names(enviro_data)  <- str_replace_all(var_names, " ", "_")

# Resample environmental data to EDS -------------------------------------------

enviro_data_EDS <- resample(
  enviro_data, Larsen_grid_EDS_ras,
  method = "bilinear"
)
enviro_data_QDS <- resample(
  enviro_data, Larsen_grid_QDS_ras,
  method = "bilinear"
)
enviro_data_HDS <- resample(
  enviro_data, Larsen_grid_HDS_ras,
  method = "bilinear"
)

# Merge environmental data from rasters into dataframes ------------------------

enviro_data_EDS_df <- raster2df(enviro_data_EDS, Larsen_grid_EDS_data)
enviro_data_QDS_df <- raster2df(enviro_data_QDS, Larsen_grid_QDS_data)
enviro_data_HDS_df <- raster2df(enviro_data_HDS, Larsen_grid_HDS_data)

# Calculate heterogeneity within QDS, HDS, DS ----------------------------------

heterogeneity_QDS_df <- enviro_data_EDS_df %>%
  filter(qdgc %in% QDS_w_all_EDS) %>%
  filter(!is.na(region)) %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  group_by(region, qdgc) %>%
  summarise_at(
    vars(str_replace(var_names, " ", "_")),
    ~var(., na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_at(vars(str_replace(var_names, " ", "_")), log10) %>%
  mutate_at(vars(str_replace(var_names, " ", "_")), scale)

heterogeneity_HDS_df <- enviro_data_QDS_df %>%
  filter(hdgc %in% HDS_w_all_QDS) %>%
  filter(!is.na(region)) %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  group_by(region, hdgc) %>%
  summarise_at(
    vars(str_replace(var_names, " ", "_")),
    ~var(., na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_at(vars(str_replace(var_names, " ", "_")), log10) %>%
  mutate_at(vars(str_replace(var_names, " ", "_")), scale)

heterogeneity_DS_df <- enviro_data_HDS_df %>%
  filter(dgc %in% DS_w_all_HDS) %>%
  filter(!is.na(region)) %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  group_by(region, dgc) %>%
  summarise_at(
    vars(str_replace(var_names, " ", "_")),
    ~var(., na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_at(vars(str_replace(var_names, " ", "_")), log10) %>%
  mutate_at(vars(str_replace(var_names, " ", "_")), scale)

# Run PCA of heterogeneity -----------------------------------------------------

heterogeneity_QDS_PCA <- heterogeneity_QDS_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

heterogeneity_HDS_PCA <- heterogeneity_HDS_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

heterogeneity_DS_PCA <- heterogeneity_DS_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

summary(heterogeneity_QDS_PCA)
summary(heterogeneity_HDS_PCA)
summary(heterogeneity_DS_PCA)

# Save PC1 and PC2 into dataframes ---------------------------------------------

heterogeneity_QDS_df$PC1 <- heterogeneity_QDS_PCA$x[, 1]
heterogeneity_QDS_df$PC2 <- heterogeneity_QDS_PCA$x[, 2]

heterogeneity_HDS_df$PC1 <- heterogeneity_HDS_PCA$x[, 1]
heterogeneity_HDS_df$PC2 <- heterogeneity_HDS_PCA$x[, 2]

heterogeneity_DS_df$PC1 <- heterogeneity_DS_PCA$x[, 1]
heterogeneity_DS_df$PC2 <- heterogeneity_DS_PCA$x[, 2]

# Merge richness data into dataframes ------------------------------------------

# Plot to check
if (FALSE) {
  QDS_richness %>%
    rename(qdgc = QDS) %>%
    full_join(heterogeneity_QDS_df) %>%
    filter_all(~ (!is.nan(.)) & (!is.na(.))) %>%
    ggplot() +
    aes(PC1, QDS_richness, colour = region) +
    geom_point()
  HDS_richness %>%
    rename(hdgc = HDS) %>%
    full_join(heterogeneity_HDS_df) %>%
    filter_all(~ (!is.nan(.)) & (!is.na(.))) %>%
    ggplot() +
    aes(PC1, HDS_richness, colour = region) +
    geom_point()
  DS_richness %>%
    rename(dgc = DS) %>%
    full_join(heterogeneity_DS_df) %>%
    filter_all(~ (!is.nan(.)) & (!is.na(.))) %>%
    ggplot() +
    aes(PC1, DS_richness, colour = region) +
    geom_point()
  # YAY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
}

#####

# Plot PC-biplots
PC_biplots <- pmap(list(heterogeneity_PCAs, heterogeneity, names(heterogeneity)),
  ~ autoplot(..1, data = ..2, colour = "region",
      loadings = TRUE,
      loadings.colour = "black",
      loadings.label = TRUE,
      loadings.label.colour = "black",
      loadings.label.hjust = -0.25,
      loadings.label.size = 3
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    lims(
      x = case_when(
        ..3 == "point1" ~ c(-0.10, 0.10),
        ..3 == "QDS"    ~ c(-0.20, 0.20),
        ..3 == "HDS"    ~ c(-0.25, 0.25),
        ..3 == "DS"     ~ c(-0.25, 0.25)
      ),
      y = case_when(
        ..3 == "point1" ~ c(-0.10, 0.10),
        ..3 == "QDS"    ~ c(-0.20, 0.20),
        ..3 == "HDS"    ~ c(-0.25, 0.25),
        ..3 == "DS"     ~ c(-0.25, 0.25)
      )
    ) +
    scale_colour_manual(name = "Region", values = c("grey25", "grey75")) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
)
my_legend <- get_legend(PC_biplots$point1)
PC_biplots %<>% map(~ . + theme(legend.position = "none"))
PC_biplots <- plot_grid(
  plotlist = PC_biplots,
  nrow = 2,
  labels         = c("(a) 0.10°×0.10°", "(b) QDS", "(c) HDS", "(d) DS"),
  label_fontface = "plain",
  label_x        = 0.150,
  label_y        = 0.975,
  hjust          = 0
)
PC_biplots <- plot_grid(
  PC_biplots, my_legend,
  nrow = 1, rel_widths = c(1, 0.2)
)
PC_biplots

# Save for SI
ggsave(
  here("figures/plot-PC-biplots.pdf"),
  PC_biplots,
  width = 8, height = 6
)
ggsave(
  here("figures/plot-PC-biplots.png"),
  PC_biplots,
  width = 8, height = 6
)

# Collate heterogeneity data into grids ----------------------------------------

# Include lon/lat when converting from Raster* to data.frame
raster2df <- function(r) {
  lon_lat <- xyFromCell(r, 1:ncell(r))
  colnames(lon_lat) <- c("lon", "lat")
  df <- as.data.frame(log10(r))
  df <- cbind(lon_lat, df)
  df
}
heterogeneity_w_coords <- map2(GCFR_heterogeneity, SWAFR_heterogeneity,
  ~ na.exclude(rbind(
    cbind(region = "GCFR",  raster2df(.x)),
    cbind(region = "SWAFR", raster2df(.y))
  ))
)
heterogeneity_w_coords %<>%
  map(mutate_at, vars(str_replace_all(var_names, " ", "_")), scale) %>%
  map(as_tibble)
heterogeneity <- map2(heterogeneity, heterogeneity_w_coords,
  full_join
)

heterogeneity$QDS$QDS <- heterogeneity %$%
  QDS %$%
  SpatialPoints(
    coords      = data.frame(x = lon, y = lat),
    proj4string = crs(Larsen_grid)
  ) %over%
  Larsen_grid %>%
  pull(qdgc)
heterogeneity$QDS %<>%
  filter(QDS %in% c(GCFR_QDS_w_all_EDS, SWAFR_QDS_w_all_EDS))

heterogeneity$HDS$HDS <- heterogeneity %$%
  HDS %$%
  SpatialPoints(
    coords      = data.frame(x = lon, y = lat),
    proj4string = crs(Larsen_grid)
  ) %over%
  Larsen_grid %>%
  pull(hdgc)
heterogeneity$HDS %<>%
  filter(HDS %in% c(GCFR_HDS_w_all_QDS, SWAFR_HDS_w_all_QDS))

heterogeneity$DS$DS <- heterogeneity %$%
  DS %$%
  SpatialPoints(
    coords      = data.frame(x = lon, y = lat),
    proj4string = crs(Larsen_grid)
  ) %over%
  Larsen_grid %>%
  pull(dgc)
heterogeneity$DS %<>%
  filter(DS %in% c(GCFR_DS_w_all_HDS, SWAFR_DS_w_all_HDS))

# Check
map(heterogeneity[2:4],
  ~ ggplot(.x, aes(lon, lat)) +
    geom_point() +
    facet_grid(~region, scales = "free_x")
)
# Works!

# Collate richness data into grids ---------------------------------------------

make_SpatialPointsDataFrame <- function(df) {
  SpatialPointsDataFrame(
    coords      = df[, c("decimallongitude", "decimallatitude")],
    data        = df[, "species"],
    proj4string = crs(Larsen_grid)
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
  Larsen_grid %>%
  pull(edgc)
species_occ@data$EDS %<>% as.character()
species_occ$QDS <- str_remove(species_occ$EDS, ".$")
species_occ$HDS <- str_remove(species_occ$QDS, ".$")
species_occ$DS  <- str_remove(species_occ$HDS, ".$")

# Export final lists of species
GCFR_species_occ@data %>%
  group_by(species)  %>%
  summarise(n_collections = n()) %>%
  arrange(desc(n_collections)) %>%
  write_csv(glue("{data_dir}/GCFR-species.csv"))
SWAFR_species_occ@data %>%
  group_by(species) %>%
  summarise(n_collections = n()) %>%
  arrange(desc(n_collections)) %>%
  write_csv(glue("{data_dir}/SWAFR-species.csv"))

# Flag species w/ < 5 collections total in each region
GCFR_bad_species <-
  read_csv(glue("{data_dir}/GCFR-species.csv")) %>%
  filter(n_collections < 5) %>%
  pull(species)
SWAFR_bad_species <-
  read_csv(glue("{data_dir}/SWAFR-species.csv")) %>%
  filter(n_collections < 5) %>%
  pull(species)
# Filter them out
species_occ_data <- species_occ@data %>%
  filter(!(species %in% c(GCFR_bad_species, SWAFR_bad_species)))

species_occ_data %<>% na.exclude()

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
## # A tibble: 3 x 2
##   region n_spp
##   <chr>  <int>
## 1 GCFR    9494
## 2 SWAFR   6793
## 3 NA        14

# Collate richness measures at each scale
# At QDS-scale:
QDS_richness <- species_occ_data %>%
  full_join(tibble(EDS = Larsen_grid$edgc)) %>%
  mutate(EDS_in_region = EDS %in% Larsen_grid$edgc[Larsen_grid$in_region]) %>%
  group_by(QDS) %>%
  filter(QDS %in% c(GCFR_QDS_w_all_EDS, SWAFR_QDS_w_all_EDS)) %>%
  summarise(
    n_EDS_in_region = length(unique(EDS[EDS_in_region])),
    n_collections   = length(species),
    QDS_richness    = length(unique(species))
  )
# At HDS-scale:
mean_QDS_richness <- QDS_richness %>%
  mutate(HDS = str_remove(QDS, ".$")) %>%
  group_by(HDS) %>%
  summarise(mean_QDS_richness = mean(QDS_richness, na.rm = TRUE))
HDS_richness <- species_occ_data %>%
  full_join(tibble(QDS = Larsen_grid$qdgc[Larsen_grid$in_region])) %>%
  mutate(QDS_in_region = QDS %in% Larsen_grid$qdgc[Larsen_grid$in_region]) %>%
  group_by(HDS) %>%
  filter(HDS %in% c(GCFR_HDS_w_all_QDS, SWAFR_HDS_w_all_QDS)) %>%
  summarise(
    n_QDS_in_region = length(unique(QDS[QDS_in_region])),
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
  full_join(tibble(HDS = Larsen_grid$hdgc[Larsen_grid$in_region])) %>%
  mutate(HDS_in_region = HDS %in% Larsen_grid$hdgc[Larsen_grid$in_region]) %>%
  group_by(DS) %>%
  filter(DS %in% c(GCFR_DS_w_all_HDS, SWAFR_DS_w_all_HDS)) %>%
  summarise(
    n_HDS_in_region = length(unique(HDS[HDS_in_region])),
    n_collections   = length(species),
    DS_richness     = length(unique(species))
  ) %>%
  full_join(mean_HDS_richness)

# Collate gridded heterogeneity & richness data --------------------------------

data <- heterogeneity[-1]  # b/c can't use 0.10 x 0.10 for richness data
data$QDS %<>%
  full_join(QDS_richness) %>%
  na.exclude()
data$HDS %<>%
  full_join(HDS_richness) %>%
  na.exclude() %>%
  mutate(
    QDS_turnover      = HDS_richness - mean_QDS_richness,
    QDS_turnover_prop = QDS_turnover / HDS_richness
  )
data$DS %<>%
  full_join(DS_richness) %>%
  na.exclude() %>%
  mutate(
    HDS_turnover      = DS_richness - mean_HDS_richness,
    HDS_turnover_prop = HDS_turnover/DS_richness,
  )

# Check
map(data, visdat::vis_dat)
map(data,
  ~ ggplot(.x, aes(lon, lat)) +
    geom_point() +
    facet_grid(~region, scales = "free_x")
)
# Good!

# Check species counts used to create the data frames above
# (i.e. excluding the last few QDS):
species_occ_data %>%
  mutate(
    region = EDS %>%
      str_extract("E[0-9]{3}") %>%
      str_remove("E") %>%
      as.numeric() %>%
      is_greater_than(60) %>%
      ifelse("SWAFR", "GCFR"),
    QDS_in_region = QDS %in% data$QDS$QDS,
    HDS_in_region = HDS %in% data$HDS$HDS,
    DS_in_region  = DS  %in% data$DS$DS,
  ) %>%
  filter(QDS_in_region) %>% #filter(HDS_in_region); #filter(DS_in_region)
  group_by(region) %>%
  summarise(n_spp = length(unique(species)))
## # A tibble: 2 x 2
##   region n_spp
##   <chr>  <int>
## 1 GCFR    9419
## 2 SWAFR   6696

# Save to disc
iwalk(data, ~write_csv(.x, glue("{data_dir}/data-{.y}.csv")))

# Rasterise richness & PC1 data ------------------------------------------------

# .... QDS-scale ---------------------------------------------------------------

# GCFR:
GCFR_QDS_richness   <- GCFR_QDS_PC1   <- GCFR_heterogeneity$QDS$Elevation
GCFR_QDS_richness[] <- GCFR_QDS_PC1[] <- NA
names(GCFR_QDS_richness) <- "QDS_richness"
names(GCFR_QDS_PC1)      <- "QDS_PC1"
cell_nos <- cellFromXY(
  GCFR_QDS_richness,
  data %$%
    QDS %>%
    filter(region == "GCFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
GCFR_QDS_richness[cell_nos] <- data %$%
  QDS %>%
  filter(region == "GCFR") %>%
  pull(QDS_richness)
GCFR_QDS_PC1[cell_nos] <- data %$%
  QDS %>%
  filter(region == "GCFR") %>%
  pull(PC1)
# Check
plot(GCFR_QDS_richness)
plot(GCFR_border, add = TRUE)
plot(GCFR_QDS_PC1)
plot(GCFR_border, add = TRUE)
plot(GCFR_QDS_richness[] ~ GCFR_QDS_PC1[])
# Works!
writeRaster(
  GCFR_QDS_richness,
  glue("{data_dir}/GCFR_QDS_richness.tif"),
  overwrite = TRUE
)
writeRaster(
  GCFR_QDS_PC1,
  glue("{data_dir}/GCFR_QDS_PC1.tif"),
  overwrite = TRUE
)

# SWAFR:
SWAFR_QDS_richness   <- SWAFR_QDS_PC1   <- SWAFR_heterogeneity$QDS$Elevation
SWAFR_QDS_richness[] <- SWAFR_QDS_PC1[] <- NA
names(SWAFR_QDS_richness) <- "QDS_richness"
names(SWAFR_QDS_PC1)      <- "QDS_PC1"
cell_nos <- cellFromXY(
  SWAFR_QDS_richness,
  data %$%
    QDS %>%
    filter(region == "SWAFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
SWAFR_QDS_richness[cell_nos] <- data %$%
  QDS %>%
  filter(region == "SWAFR") %>%
  pull(QDS_richness)
SWAFR_QDS_PC1[cell_nos] <- data %$%
  QDS %>%
  filter(region == "SWAFR") %>%
  pull(PC1)
plot(SWAFR_QDS_richness)
plot(SWAFR_border, add = TRUE)
plot(SWAFR_QDS_PC1)
plot(SWAFR_border, add = TRUE)
plot(SWAFR_QDS_richness[] ~ SWAFR_QDS_PC1[])
writeRaster(
  SWAFR_QDS_richness,
  glue("{data_dir}/SWAFR_QDS_richness.tif"),
  overwrite = TRUE
)
writeRaster(
  SWAFR_QDS_PC1,
  glue("{data_dir}/SWAFR_QDS_PC1.tif"),
  overwrite = TRUE
)

# .... HDS-scale ---------------------------------------------------------------

# GCFR:
GCFR_HDS_richness   <- GCFR_HDS_PC1   <- GCFR_heterogeneity$HDS$Elevation
GCFR_HDS_richness[] <- GCFR_HDS_PC1[] <- NA
names(GCFR_HDS_richness) <- "HDS_richness"
names(GCFR_HDS_PC1)      <- "HDS_PC1"
cell_nos <- cellFromXY(
  GCFR_HDS_richness,
  data %$%
    HDS %>%
    filter(region == "GCFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
GCFR_HDS_richness[cell_nos] <- data %$%
  HDS %>%
  filter(region == "GCFR") %>%
  pull(HDS_richness)
GCFR_HDS_PC1[cell_nos] <- data %$%
  HDS %>%
  filter(region == "GCFR") %>%
  pull(PC1)
plot(GCFR_HDS_richness)
plot(GCFR_border, add = TRUE)
plot(GCFR_HDS_PC1)
plot(GCFR_border, add = TRUE)
plot(GCFR_HDS_richness[] ~ GCFR_HDS_PC1[])
writeRaster(
  GCFR_HDS_richness,
  glue("{data_dir}/GCFR_HDS_richness.tif"),
  overwrite = TRUE
)
writeRaster(
  GCFR_HDS_PC1,
  glue("{data_dir}/GCFR_HDS_PC1.tif"),
  overwrite = TRUE
)

# SWAFR:
SWAFR_HDS_richness   <- SWAFR_HDS_PC1   <- SWAFR_heterogeneity$HDS$Elevation
SWAFR_HDS_richness[] <- SWAFR_HDS_PC1[] <- NA
names(SWAFR_HDS_richness) <- "HDS_richness"
names(SWAFR_HDS_PC1)      <- "HDS_PC1"
cell_nos <- cellFromXY(
  SWAFR_HDS_richness,
  data %$%
    HDS %>%
    filter(region == "SWAFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
SWAFR_HDS_richness[cell_nos] <- data %$%
  HDS %>%
  filter(region == "SWAFR") %>%
  pull(HDS_richness)
SWAFR_HDS_PC1[cell_nos] <- data %$%
  HDS %>%
  filter(region == "SWAFR") %>%
  pull(PC1)
plot(SWAFR_HDS_richness)
plot(SWAFR_border, add = TRUE)
plot(SWAFR_HDS_PC1)
plot(SWAFR_border, add = TRUE)
plot(SWAFR_HDS_richness[] ~ SWAFR_HDS_PC1[])
writeRaster(
  SWAFR_HDS_richness,
  glue("{data_dir}/SWAFR_HDS_richness.tif"),
  overwrite = TRUE
)
writeRaster(
  SWAFR_HDS_PC1,
  glue("{data_dir}/SWAFR_HDS_PC1.tif"),
  overwrite = TRUE
)

# .... DS-scale ----------------------------------------------------------------

# GCFR:
GCFR_DS_richness   <- GCFR_DS_PC1   <- GCFR_heterogeneity$DS$Elevation
GCFR_DS_richness[] <- GCFR_DS_PC1[] <- NA
names(GCFR_DS_richness) <- "DS_richness"
names(GCFR_DS_PC1)      <- "DS_PC1"
cell_nos <- cellFromXY(
  GCFR_DS_richness,
  data %$%
    DS %>%
    filter(region == "GCFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
GCFR_DS_richness[cell_nos] <- data %$%
  DS %>%
  filter(region == "GCFR") %>%
  pull(DS_richness)
GCFR_DS_PC1[cell_nos] <- data %$%
  DS %>%
  filter(region == "GCFR") %>%
  pull(PC1)
plot(GCFR_DS_richness)
plot(GCFR_border, add = TRUE)
plot(GCFR_DS_PC1)
plot(GCFR_border, add = TRUE)
plot(GCFR_DS_richness[] ~ GCFR_DS_PC1[])
writeRaster(
  GCFR_DS_richness,
  glue("{data_dir}/GCFR_DS_richness.tif"),
  overwrite = TRUE
)
writeRaster(
  GCFR_DS_PC1,
  glue("{data_dir}/GCFR_DS_PC1.tif"),
  overwrite = TRUE
)

# SWAFR:
SWAFR_DS_richness   <- SWAFR_DS_PC1   <- SWAFR_heterogeneity$DS$Elevation
SWAFR_DS_richness[] <- SWAFR_DS_PC1[] <- NA
names(SWAFR_DS_richness) <- "DS_richness"
names(SWAFR_DS_PC1)      <- "DS_PC1"
cell_nos <- cellFromXY(
  SWAFR_DS_richness,
  data %$%
    DS %>%
    filter(region == "SWAFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
SWAFR_DS_richness[cell_nos] <- data %$%
  DS %>%
  filter(region == "SWAFR") %>%
  pull(DS_richness)
SWAFR_DS_PC1[cell_nos] <- data %$%
  DS %>%
  filter(region == "SWAFR") %>%
  pull(PC1)
plot(SWAFR_DS_richness)
plot(SWAFR_border, add = TRUE)
plot(SWAFR_DS_PC1)
plot(SWAFR_border, add = TRUE)
plot(SWAFR_DS_richness[] ~ SWAFR_DS_PC1[])
writeRaster(
  SWAFR_DS_richness,
  glue("{data_dir}/SWAFR_DS_richness.tif"),
  overwrite = TRUE
)
writeRaster(
  SWAFR_DS_PC1,
  glue("{data_dir}/SWAFR_DS_PC1.tif"),
  overwrite = TRUE
)
