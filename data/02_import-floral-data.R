# Import floral occurrence data
# Cape vs SWA publication
# Ruan van Mazijk

flora_dir <- here::here("data/derived-data/flora")

# Richness rasters -------------------------------------------------------------

GCFR_richness_QDS <-
  raster(glue("{flora_dir}/GCFR_richness_QDS_2017-09-16.tif"))
stopifnot(proj4string(GCFR_richness_QDS) == std_CRS)
GCFR_richness_HDS <-
  raster(glue("{flora_dir}/GCFR_richness_HDS_2017-09-16.tif"))
GCFR_richness_3QDS <-
  raster(glue("{flora_dir}/GCFR_richness_3QDS_2017-09-16.tif"))

SWAFR_richness_QDS <-
  raster(glue("{flora_dir}/SWAFR_richness_QDS_2017-09-16.tif"))
stopifnot(proj4string(SWAFR_richness_QDS) == std_CRS)
SWAFR_richness_HDS <-
  raster(glue("{flora_dir}/SWAFR_richness_HDS_2017-09-16.tif"))
SWAFR_richness_3QDS <-
  raster(glue("{flora_dir}/SWAFR_richness_3QDS_2017-09-16.tif"))

# Floral data sets as SpatialPointsDataFrames ----------------------------------

GCFR_spp <- readOGR(glue(
  "{flora_dir}/GCFR_spp_2018-08-14"
))
SWAFR_spp <- readOGR(glue(
  "{flora_dir}/SWAFR_spp_2018-08-14"
))

vars_to_keep <- c(
  "hdgc",
  "HDS_richness",
  "n_QDS",
  "mean_QDS_richness",
  "mean_QDS_jaccard"
)
names(GCFR_spp)[5:9] <- vars_to_keep
names(SWAFR_spp)[5:9] <- vars_to_keep

GCFR_spp_data <- GCFR_spp@data %>%
  select(vars_to_keep) %>%
  filter(n_QDS > 1) %>%
  distinct()
SWAFR_spp_data <- SWAFR_spp@data %>%
  select(vars_to_keep) %>%
  filter(n_QDS > 1) %>%
  distinct()

richness_turnover_data <-
  rbind(
    cbind(region = "Cape", GCFR_spp_data),
    cbind(region = "SWA", SWAFR_spp_data)
  ) %>%
  as_tibble() %>%
  filter(n_QDS > 1) %>%  # turnover is non-sensicle for 1 QDS)
  mutate(
    add_residual_turnover = HDS_richness - mean_QDS_richness,
    add_residual_turnover_prop = add_residual_turnover / HDS_richness,
    mul_residual_turnover = HDS_richness / mean_QDS_richness
  )
