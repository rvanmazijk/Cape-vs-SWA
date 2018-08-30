# Analyse value of environmental & heterogeneity variables for predicting
# vascular plant species richness and turnover---using BRTs
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))
source(here("R/02_analyses/generate-roughness.R"))
source(here("R/02_analyses/generate-turnover.R"))

output_path <- here("outputs/species-environment-relationships")

library(dismo)
library(virtualspecies)

# HDS-scale for now:

# Combine all data -------------------------------------------------------------

# .... GCFR --------------------------------------------------------------------

# Generate absolute environmental values at HDS-scale
GCFR_variables_HDS <- map(GCFR_variables, aggregate, fact = 0.50 / 0.05)

# Add roughnes layers to data list
names(GCFR_roughness_HDS) %<>% paste0("rough_", .)
GCFR_variables_HDS2 <- c(
  GCFR_variables_HDS,
  GCFR_roughness_HDS
)

# Convert NA-turnover to nonsensicle value (rasterize() can't handles NAs)
GCFR_species$mn_QDS_t[is.na(GCFR_species$mn_QDS_t)] <- 9999

# Add raster of HDS richness and mean QDS turnover to data list
GCFR_species_rasters <- list(
  HDS_richness = rasterize(
    GCFR_species,
    GCFR_variables_HDS$Elevation,
    field = "HDS_rch"
  ),
  mean_QDS_turnover = rasterize(
    GCFR_species,
    GCFR_variables_HDS$Elevation,
    field = "mn_QDS_t"
  )
)

# Convert nonsense back to NAs
nonsense <- GCFR_species_rasters$mean_QDS_turnover == 9999
GCFR_species_rasters$mean_QDS_turnover[nonsense] <- NA

GCFR_variables_HDS2 <- c(
  GCFR_species_rasters,
  GCFR_variables_HDS2
)

GCFR_variables_HDS_df <- GCFR_variables_HDS2 %>%
  map_df(getValues) %>%
  na.exclude()
GCFR_variables_HDS_stack <- stack(GCFR_variables_HDS2)

# .... SWAFR --------------------------------------------------------------------

# Generate absolute environmental values at HDS-scale
SWAFR_variables_HDS <- map(SWAFR_variables, aggregate, fact = 0.50 / 0.05)

# Add roughnes layers to data list
names(SWAFR_roughness_HDS) %<>% paste0("rough_", .)
SWAFR_variables_HDS2 <- c(
  SWAFR_variables_HDS,
  SWAFR_roughness_HDS
)

# Convert NA-turnover to nonsensicle value (rasterize() can't handles NAs)
SWAFR_species$mn_QDS_t[is.na(SWAFR_species$mn_QDS_t)] <- 9999

# Add raster of HDS richness and mean QDS turnover to data list
SWAFR_species_rasters <- list(
  HDS_richness = rasterize(
    SWAFR_species,
    SWAFR_variables_HDS$Elevation,
    field = "HDS_rch"
  ),
  mean_QDS_turnover = rasterize(
    SWAFR_species,
    SWAFR_variables_HDS$Elevation,
    field = "mn_QDS_t"
  )
)

# Convert nonsense back to NAs
nonsense <- SWAFR_species_rasters$mean_QDS_turnover == 9999
SWAFR_species_rasters$mean_QDS_turnover[nonsense] <- NA

SWAFR_variables_HDS2 <- c(
  SWAFR_species_rasters,
  SWAFR_variables_HDS2
)

SWAFR_variables_HDS_df <- SWAFR_variables_HDS2 %>%
  map_df(getValues) %>%
  na.exclude()
SWAFR_variables_HDS_stack <- stack(SWAFR_variables_HDS2)


# Collinearity checks ----------------------------------------------------------

png(
  filename = glue("{output_path}/Cape_collinearity-check_0.8-cutoff_HDS.png"),
  width = 17, height = 15, units = "cm", res = 100
)
removeCollinearity(
  raster.stack = GCFR_variables_HDS_stack[[-c(1, 2)]],  # without richness (the response)
  select.variables = FALSE,  # To do manually
  multicollinearity.cutoff = 0.8,  # My thumb-suck
  plot = TRUE
)
dev.off()

png(
  filename = glue("{output_path}/SWA_collinearity-check_0.8-cutoff_HDS.png"),
  width = 17, height = 15, units = "cm", res = 100
)
removeCollinearity(
  raster.stack = SWAFR_variables_HDS_stack[[-c(1, 2)]],  # without richness (the response)
  select.variables = FALSE,  # To do manually
  multicollinearity.cutoff = 0.8,  # My thumb-suck
  plot = TRUE
)
dev.off()


plot(GCFR_variables_HDS_stack)

# TODO: cont. from here w/ BRTs
