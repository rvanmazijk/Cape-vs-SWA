# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 1: Setup and data collation
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

output_path <- here(
  "R/analyses/",
  "analyse-species-environment-relationships/run-on-UCT-HPC"
)

# Combine all data at QDS-scale ------------------------------------------------

# Import/get data
source(here("R/analyses/generate-roughness.R"))
import_region_polygons()

# Aggregate enviro data to QDS-scale
GCFR_variables_QDS <- map(GCFR_variables, aggregate, 0.25 / 0.05)
SWAFR_variables_QDS <- map(SWAFR_variables, aggregate, 0.25 / 0.05)

# Import species data (don't use generate-turnover.R, that is for HDS-scale)
GCFR_species <- read_rds(here(
  "data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_species"
))
SWAFR_species <- read_rds(here(
  "data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_species"
))

# .... Get species richness in each QDS ----------------------------------------

# Put QDS geocodes in species SpatialPointsDataFrame
GCFR_species %<>% get_geocodes(GCFR_QDS[, "qdgc"])
SWAFR_species %<>% get_geocodes(SWAFR_QDS[, "qdgc"])

# Now count no. species by QDS
GCFR_richness_values <- GCFR_species@data %>%
  group_by(qdgc) %>%
  summarise(QDS_richness = length(unique(species)))
SWAFR_richness_values <- SWAFR_species@data %>%
  group_by(qdgc) %>%
  summarise(QDS_richness = length(unique(species)))

# Put those values in the QDS SpatialPolygonsDataFrame
GCFR_QDS@data %<>% left_join(GCFR_richness_values)
SWAFR_QDS@data %<>% left_join(SWAFR_richness_values)
GCFR_QDS <- GCFR_QDS[!is.na(GCFR_QDS$QDS_richness), ]
SWAFR_QDS <- SWAFR_QDS[!is.na(SWAFR_QDS$QDS_richness), ]
GCFR_richness_QDS <- rasterize(
  GCFR_QDS,
  GCFR_variables_QDS$Elevation,
  field = "QDS_richness"
)
SWAFR_richness_QDS <- rasterize(
  SWAFR_QDS,
  SWAFR_variables_QDS$Elevation,
  field = "QDS_richness"
)
names(GCFR_richness_QDS) <- "QDS_richness"
names(SWAFR_richness_QDS) <- "QDS_richness"

plot(GCFR_richness_QDS)
plot(SWAFR_richness_QDS)

# .... Collate richness and environmental data ---------------------------------

names(GCFR_roughness_QDS) %<>% paste0("rough_", .)
names(SWAFR_roughness_QDS) %<>% paste0("rough_", .)

GCFR_data_QDS_stack <- stack(
  GCFR_richness_QDS,
  stack(
    stack(GCFR_variables_QDS),
    stack(GCFR_roughness_QDS)
  )
)
SWAFR_data_QDS_stack <- stack(
  SWAFR_richness_QDS,
  stack(
    stack(SWAFR_variables_QDS),
    stack(SWAFR_roughness_QDS)
  )
)
GCFR_data_QDS <- GCFR_data_QDS_stack %>%
  as.data.frame() %>%
  na.exclude() %>%
  mutate(log_QDS_richness = log(QDS_richness))
SWAFR_data_QDS <- SWAFR_data_QDS_stack %>%
  as.data.frame() %>%
  na.exclude() %>%
  mutate(log_QDS_richness = log(QDS_richness))

names(GCFR_data_QDS)
names(SWAFR_data_QDS)

# .... Output data for bare-minimum BRT work on UCT HPC ------------------------

write.csv(GCFR_data_QDS, glue("{output_path}/GCFR_variables_QDS.csv"))
write.csv(SWAFR_data_QDS, glue("{output_path}/SWAFR_variables_QDS.csv"))

# Combine all data at HDS-scale ------------------------------------------------

source(here("R/analyses/generate-turnover.R"))
# Reset output path after runnng above R-script
output_path <- here(
  "R/analyses/",
  "analyse-species-environment-relationships/run-on-UCT-HPC"
)

# Tidy names(<region>_species) after SpatialPointsDataFrame import
names(GCFR_species) <- c(
  #"family", "genus", "species",
  "species", "cell_nos",
  "qdgc", "hdgc",
  "HDS_richness", "n_QDS", "mean_QDS_richness", "mean_QDS_turnover"
)
names(SWAFR_species) <- c(
  #"family", "genus", "species",
  "species", "cell_nos",
  "qdgc", "hdgc",
  "HDS_richness", "n_QDS", "mean_QDS_richness", "mean_QDS_turnover"
)

variables_HDS_stacks <- pmap(
  # For each region:
  .l = list(
    vars = list(GCFR_variables, SWAFR_variables),
    rough_vars = list(GCFR_roughness_HDS, SWAFR_roughness_HDS),
    species = list(GCFR_species, SWAFR_species)
  ),
  .f = function(vars, rough_vars, species) {

    # Generate absolute environmental values at HDS-scale
    vars %<>% map(aggregate, fact = 0.50 / 0.05)

    # Add roughness layers to data list
    names(rough_vars) %<>% paste0("rough_", .)
    vars %<>% c(rough_vars)

    # Convert NA-turnover to nonsensicle value (rasterize() can't handles NAs)
    species$mean_QDS_turnover[is.na(species$mean_QDS_turnover)] <- 9999

    # Add raster of HDS richness and mean QDS turnover to data list
    species_rasters <- list(
      HDS_richness = rasterize(
        species,
        vars$Elevation,
        field = "HDS_richness"
      ),
      mean_QDS_turnover = rasterize(
        species,
        vars$Elevation,
        field = "mean_QDS_turnover"
      )
    )

    # Convert nonsense back to NAs
    nonsense <- species_rasters$mean_QDS_turnover == 9999
    species_rasters$mean_QDS_turnover[nonsense] <- NA

    # Add richness and turnover to data list
    vars <- c(species_rasters, vars)

    # And make it all a RasterStack
    stack(vars)
  }
)

GCFR_variables_HDS_stack <- variables_HDS_stacks[[1]]
SWAFR_variables_HDS_stack <- variables_HDS_stacks[[2]]

# .... Output data for bare-minimum BRT work on UCT HPC ------------------------

GCFR_variables_HDS_stack %>%
  as.data.frame() %>%
  na.exclude() %>%
  write.csv(glue("{output_path}/GCFR_variables_HDS.csv"))
SWAFR_variables_HDS_stack %>%
  as.data.frame() %>%
  na.exclude() %>%
  write.csv(glue("{output_path}/SWAFR_variables_HDS.csv"))

# Combined-region datasets -----------------------------------------------------

GCFR_data_QDS <- read_csv(glue("{output_path}/GCFR_variables_QDS.csv"))[, -1]
SWAFR_data_QDS <- read_csv(glue("{output_path}/SWAFR_variables_QDS.csv"))[, -1]
GCFR_data_HDS <- read_csv(glue("{output_path}/GCFR_variables_HDS.csv"))[, -1]
SWAFR_data_HDS <- read_csv(glue("{output_path}/SWAFR_variables_HDS.csv"))[, -1]

BOTH_data_QDS <- rbind(
  cbind(region = "GCFR", GCFR_data_QDS),
  cbind(region = "SWAFR", SWAFR_data_QDS)
)
write_csv(
  BOTH_data_QDS,
  glue("{output_path}/BOTH_variables_QDS.csv")
)
BOTH_data_HDS <- rbind(
  cbind(region = "GCFR", GCFR_data_HDS),
  cbind(region = "SWAFR", SWAFR_data_HDS)
)
write_csv(
  BOTH_data_HDS,
  glue("{output_path}/BOTH_variables_HDS.csv")
)
