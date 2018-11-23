# Trying BRTs of richness at QDS-scale
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

# Extra packages
library(dismo)
library(gbm)
library(foreach)
library(doParallel)
library(virtualspecies)

# Import/get data
source(here("R/02_analyses/generate-roughness.R"))
import_region_polygons()

# Aggregate enviro data to QDS-scale
GCFR_variables_QDS <- map(GCFR_variables, aggregate, 0.25 / 0.05)
SWAFR_variables_QDS <- map(SWAFR_variables, aggregate, 0.25 / 0.05)

# Import species data
GCFR_species <- read_rds(here(
  "data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_species"
))
SWAFR_species <- read_rds(here(
  "data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_species"
))

# Get species richness in each QDS ---------------------------------------------

# Put QDS geocodes in species SpatialPointsDataFrame
GCFR_species %<>% get_geocodes(GCFR_QDS[, "qdgc"])
SWAFR_species %<>% get_geocodes(SWAFR_QDS[, "qdgc"])

# Now count no. species by QDS
GCFR_richness_values <- GCFR_species@data %>%
  group_by(qdgc) %>%
  summarise(QDS_richness = length(species))
SWAFR_richness_values <- SWAFR_species@data %>%
  group_by(qdgc) %>%
  summarise(QDS_richness = length(species))

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

# Collate richness and environmental data --------------------------------------

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

# Re-check for collinearity at QDS_scale ---------------------------------------

removeCollinearity(
  raster.stack = GCFR_data_QDS_stack[[-1]],  # exclude responses
  select.variables = FALSE,  # To do manually
  multicollinearity.cutoff = 0.8,  # My thumb-suck. TODO: try 0.7?
  plot = TRUE
)
removeCollinearity(
  raster.stack = SWAFR_data_QDS_stack[[-1]],
  select.variables = FALSE,
  multicollinearity.cutoff = 0.8,
  plot = TRUE
)

merge(GCFR_data_QDS_stack$QDS_richness, SWAFR_data_QDS_stack$QDS_richness)
origin(GCFR_data_QDS_stack)
origin(SWAFR_data_QDS_stack)

# Explore data visually --------------------------------------------------------

GCFR_biplots <- foreach(variable_ = GCFR_predictor_names) %do% {
  ggplot(GCFR_data_QDS, aes_string(variable_, "QDS_richness")) +
    geom_point() +
    scale_y_continuous(trans = "log")
}
names(GCFR_biplots) <- GCFR_predictor_names
GCFR_biplots$rough_Elevation

SWAFR_biplots <- foreach(variable_ = SWAFR_predictor_names) %do% {
  ggplot(SWAFR_data_QDS, aes_string(variable_, "QDS_richness")) +
    geom_point() +
    scale_y_continuous(trans = "log")
}
names(SWAFR_biplots) <- SWAFR_predictor_names
SWAFR_biplots$rough_Elevation

# Try BRTs ---------------------------------------------------------------------

set.seed(Sys.time())
GCFR_model <- gbm.step(
  data = GCFR_data_QDS,
  gbm.x = GCFR_predictor_names,
  gbm.y = "log_QDS_richness",
  tree.complexity = 3,
  learning.rate = 0.001,
  max.trees = 10000,
  family = "gaussian",
  plot.main = TRUE
)
SWAFR_model <- gbm.step(
  data = SWAFR_data_QDS,
  gbm.x = SWAFR_predictor_names,
  gbm.y = "log_QDS_richness",
  tree.complexity = 3,
  learning.rate = 0.001,
  max.trees = 10000,
  family = "gaussian",
  plot.main = TRUE
)

pseudo_r2(GCFR_model)
pseudo_r2(SWAFR_model)
pred_obs_r2(GCFR_model)
pred_obs_r2(SWAFR_model)

my_BRT_summary(GCFR_model)$contribs
my_BRT_summary(SWAFR_model)$contribs

GCFR_model %$%
  tibble(fit_log_richness = fitted, obs_log_richness = data$y) %>%
  ggplot(aes(obs_log_richness, fit_log_richness)) +
    geom_smooth(method = lm) +
    geom_abline(intercept = 0, slope = 1, lty = "dashed") +
    geom_point()
SWAFR_model %$% plot(fitted ~ data$y)
abline(0, 1, lty = "dashed")

summary(GCFR_model)
summary(SWAFR_model)


