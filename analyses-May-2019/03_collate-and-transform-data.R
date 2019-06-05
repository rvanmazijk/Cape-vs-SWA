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
  ~raster( glue("{data_dir}/GCFR_{.x}_masked2.tif"))
)
SWAFR_variables_masked2 <- map(var_names,
  ~raster( glue("{data_dir}/SWAFR_{.x}_masked2.tif"))
)

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

# PCAs -------------------------------------------------------------------------

roughness_PCAs <- map(roughness_matrices, ~prcomp(.x[, -1], scale. = TRUE))
map(roughness_PCAs, summary)
# Force PC1 scores to be positive if all vars rotations are negative
roughness_PCAs %<>% map(function(PCA) {
  if (all(PCA$rotation[, 1] <= 0)) {
    PCA$rotation[, 1] %<>% multiply_by(-1)
    PCA$x[, 1]        %<>% multiply_by(-1)
  }
  PCA
})
# Plot
map2(roughness_PCAs, roughness_matrices,
  ~ autoplot(
    .x,
    data = .y,
    colour = "region"
  )
)

# Store PC1 & 2 in matrices for later
roughness_matrices <- map2(roughness_PCAs, roughness_matrices,
  function(PCA, layer) {
    layer$PC1 <- PCA$x[, 1]
    layer$PC2 <- PCA$x[, 2]
    layer
  }
)

# TODO: Interrogate rotations associated with different variables
#PCA_rotations <- roughness_PCAs %$%
#  rotation %>%
#  {tibble(
#    var = rownames(.),
#    PC1 = .[, 1],
#    PC2 = .[, 2]
#  )} %>%
#  gather(PC, rotation, -var)
#ggplot(PCA_rotations, aes(var, rotation)) +
#  geom_col() +
#  facet_wrap(~PC, scales = "free") +
#  theme(axis.text.x = element_text(angle = 45))

# LDAs -------------------------------------------------------------------------
# (Linear Discriminant Analysis)

roughness_matrix_scaled <- roughness_matrix
roughness_matrix_scaled[, -1] %<>% scale()
roughness_LDA <- lda(region ~ ., roughness_matrix_scaled, )
roughness_LDA
roughness_LDA_values <- predict(roughness_LDA)
ldahist(roughness_LDA_values$x[, 1], g = roughness_matrix$region)  # DF1
# FIXME: No DF2?
#plot(
#  roughness_LDA_values$x[, 1],
#  roughness_LDA_values$x[, 2]
#)

# FIXME:
roughness_EDS_LDA <- lda(region ~ ., roughness_EDS_matrix)
roughness_EDS_LDA
roughness_EDS_LDA_values <- predict(roughness_EDS_LDA)
ldahist(roughness_EDS_LDA_values$x[, 1], g = roughness_EDS_matrix$region)
roughness_QDS_LDA <- lda(region ~ ., roughness_QDS_matrix)
roughness_QDS_LDA
roughness_QDS_LDA_values <- predict(roughness_QDS_LDA)
ldahist(roughness_QDS_LDA_values$x[, 1], g = roughness_QDS_matrix$region)
roughness_HDS_LDA <- lda(region ~ ., roughness_HDS_matrix)
roughness_HDS_LDA
roughness_HDS_LDA_values <- predict(roughness_HDS_LDA)
ldahist(roughness_HDS_LDA_values$x[, 1], g = roughness_HDS_matrix$region)
roughness_3QDS_LDA <- lda(region ~ ., roughness_3QDS_matrix)
roughness_3QDS_LDA
roughness_3QDS_LDA_values <- predict(roughness_3QDS_LDA)
ldahist(roughness_3QDS_LDA_values$x[, 1], g = roughness_3QDS_matrix$region)

# CLESs ------------------------------------------------------------------------

# Pseudo-code:
# for each resolution,
#   for each variable,
#     CLES(Cape, SWA)

output_path <- here("outputs/roughness")

# Collate roughness data
roughness_data <- list(
  "0.05º" = roughness_matrix,
  "EDS"   = roughness_EDS_matrix,
  "QDS"   = roughness_QDS_matrix,
  "HDS"   = roughness_HDS_matrix,
  "3QDS"  = roughness_3QDS_matrix
)
GCFR_roughness_data <- roughness_data %>%
  map(filter, region == "GCFR") %>%
  map(dplyr::select, -region)
SWAFR_roughness_data <- roughness_data %>%
  map(filter, region == "SWAFR") %>%
  map(dplyr::select, -region)

# (WARNING: this takes a while...)
if (prompt_continue()) {  # only run if haven't already
  set.seed(1234)
  CLES_results <- map2_df(GCFR_roughness_data, SWAFR_roughness_data,
    .id = "resolution",  # for every spatial resolution,
    ~ map2_df(.x, .y,
      .id = "variable",  # for every variable in each region,
      function(.x, .y) {
        CLES_value <- CLES(.y, .x)
        message("Done")
        tibble(CLES_value)
      }
    )
  )
  # Save results to disc
  write_csv(
    CLES_results,
    glue("{output_path}/CLES_results.csv")
  )
} else {  # or import from disc
  CLES_results <- read_csv(glue("{output_path}/CLES_results.csv"))
}

CLES_results %<>%
  filter(variable != "region") %>%
  mutate(resolution = factor(resolution, levels = c(
    "0.05º", "EDS", "QDS", "HDS", "3QDS"
  ))) %>%
  mutate(resolution = case_when(
    resolution == "0.05º" ~ 0.05,
    resolution == "EDS"   ~ 0.125,
    resolution == "QDS"   ~ 0.25,
    resolution == "HDS"   ~ 0.50,
    resolution == "3QDS"  ~ 0.75
  ))


# Plot & model CLES_value ~ resolution "in general"
m_all <- lm(CLES_value ~ resolution, CLES_results)
summary(m_all)
visreg::visreg(m_all)

# Plot & model all fits for all vars separately
m_seps <- CLES_results %>%
  split(.$variable) %>%
  map(~lm(CLES_value ~ resolution, .x))
ggplot(CLES_results, aes(resolution, CLES_value)) +
  geom_point() +
  geom_smooth(method = lm) +
  geom_hline(yintercept = 0.5, lty = "dashed") +
  facet_wrap(~variable)
# NOTE: PC2 crosses the CLES=0.5 line!!!
# Coefficients' plot:
m_seps %>%
  map_df(.id = "variable", tidy, conf.int = TRUE) %>%
  filter(term == "resolution") %>%
  mutate(sig = case_when(
    p.value <= 0.001 ~ "***",
    p.value <= 0.01  ~ "**",
    p.value <= 0.05  ~ "*",
    p.value <= 0.10  ~ "*",
    TRUE             ~ ""
  )) %>%
  ggplot(aes(variable, estimate, colour = p.value, label = sig)) +
    geom_hline(yintercept = 0, lty = "dashed") +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
    geom_text(colour = "black", size = 8, nudge_y = 0.05) +
    scale_colour_distiller(palette = "Spectral")

# Species turnover -------------------------------------------------------------

GCFR_species <- read_rds(here(
  "data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_species"
))
SWAFR_species <- read_rds(here(
  "data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_species"
))

# Explore
GCFR_species
SWAFR_species

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
# Put QDS geocodes in species SpatialPointsDataFrame
GCFR_species  %<>% get_geocodes(GCFR_QDS[,  "qdgc"])
SWAFR_species %<>% get_geocodes(SWAFR_QDS[, "qdgc"])

# New
GCFR_QDS$Elevation <- extract(GCFR_roughness_QDS$Elevation, GCFR_QDS)


