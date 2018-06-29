# Maps!

# Setup ------------------------------------------------------------------------

source(here::here("analyses/01_setup.R"))
source(here::here("analyses/02_import-region-polygons.R"))
source(here::here("analyses/04_import-environmental-data.R"))
source(here::here("analyses/06_import-floral-data.R"))
op <- par()

# Additional data setup --------------------------------------------------------

var_names <- c(
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
GCFR_variables <- list(
  GCFR_elev,
  GCFR_MAP,
  GCFR_PDQ,
  GCFR_MLST,
  GCFR_NDVI,
  GCFR_soils$GCFR_CECSOL_M_250m_std_CRS_0.05,
  GCFR_soils$GCFR_CLYPPT_M_250m_std_CRS_0.05,
  GCFR_soils$GCFR_OCDENS_M_250m_std_CRS_0.05,
  GCFR_soils$GCFR_PHIKCL_M_250m_std_CRS_0.05
)
SWAFR_variables <- list(
  SWAFR_elev,
  SWAFR_MAP,
  SWAFR_PDQ,
  SWAFR_MLST,
  SWAFR_NDVI,
  SWAFR_soils$SWAFR_CECSOL_M_250m_std_CRS_0.05,
  SWAFR_soils$SWAFR_CLYPPT_M_250m_std_CRS_0.05,
  SWAFR_soils$SWAFR_OCDENS_M_250m_std_CRS_0.05,
  SWAFR_soils$SWAFR_PHIKCL_M_250m_std_CRS_0.05
)
GCFR_variables %<>% map(crop, GCFR_variables[[4]])
SWAFR_variables %<>% map(crop, SWAFR_variables[[4]])
names(GCFR_variables) <- var_names
names(SWAFR_variables) <- var_names

# Function setup ---------------------------------------------------------------

focal_sd <- function(x, ...) {
  focal(
    x = x,
    w = matrix(1, nrow = 3, ncol = 3),
    fun = function(x, ...) {
      diffs <- vector(length = 8)
      diffs[1] <- (x[5] - x[1]) ^ 2
      diffs[2] <- (x[5] - x[2]) ^ 2
      diffs[3] <- (x[5] - x[3]) ^ 2
      diffs[4] <- (x[5] - x[4]) ^ 2
      diffs[5] <- (x[5] - x[6]) ^ 2
      diffs[6] <- (x[5] - x[7]) ^ 2
      diffs[7] <- (x[5] - x[8]) ^ 2
      diffs[8] <- (x[5] - x[9]) ^ 2
      return(
        sqrt(mean(diffs) / 7)  # 7 = 8 comparisons - 1
      )
    }
  )
}

# Elevation + elevational roughness --------------------------------------------

par(mfrow = c(2, 2))

plot(
  GCFR_variables$Elevation,
  main = "Elevation (0.05deg x 0.05deg)"
)
plot(GCFR_border, add = TRUE)

plot(
  GCFR_variables$Elevation %>%
    aggregate(fact = 0.25 / 0.05),
  main = "Elevation (QDS)"
)
plot(GCFR_border, add = TRUE)

plot(
  GCFR_variables$Elevation %>%
    focal_sd(),
  main = "Elevation roughness (0.05deg x 0.05deg)"
)
plot(GCFR_border, add = TRUE)

plot(
  GCFR_variables$Elevation %>%
    aggregate(fact = 0.25 / 0.05) %>%
    focal_sd(),
  main = "Elevation roughness (QDS)"
)
plot(GCFR_border, add = TRUE)

par(op)

# TODO: SWAFR

# <other environmental variables + roughness> ----------------------------------

# TODO

# Species richness -------------------------------------------------------------

par(mfrow = c(1, 3))

plot(GCFR_richness_QDS, title = )
plot(GCFR_border, add = TRUE)

plot(GCFR_richness_HDS)
plot(GCFR_border, add = TRUE)

plot(GCFR_richness_3QDS)
plot(GCFR_border, add = TRUE)

par(op)

# TODO: SWAFR

# Species richness @scale i w/ avg turnover & richness @scale j ----------------

source(here::here("analyses/09_analyse-species-turnover-and-richness.R"))  # FIXME

# TODO: put turnover and richness @scale j back into a raster for plotting
