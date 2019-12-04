# Load packages ----------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(here)
library(glue)

library(raster)
library(rgdal)
library(rgeos)

library(stringr)
library(ggfortify)  # for PCAs

library(canprot)
library(broom)

# Load helper functions --------------------------------------------------------

source(here("draft-02/R/functions/helper-functions.R"))

raster2df <- function(r) {
  lon_lat <- xyFromCell(r, 1:ncell(r))
  colnames(lon_lat) <- c("lon", "lat")
  df <- as.data.frame(r)
  df <- cbind(lon_lat, df)
  df
}

# Import data ------------------------------------------------------------------

import_region_polygons()

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
data_dir <- here("data/derived-data/May-2019")
GCFR_file_names  <- glue("{data_dir}/GCFR_{var_names}_masked2.tif")
SWAFR_file_names <- glue("{data_dir}/SWAFR_{var_names}_masked2.tif")
GCFR_variables  <- stack(GCFR_file_names)
SWAFR_variables <- stack(SWAFR_file_names)
names(GCFR_variables)  <- str_replace_all(var_names, " ", "_")
names(SWAFR_variables) <- str_replace_all(var_names, " ", "_")

# Resample base environmental data to 16th-degree-squares (SDS) ----------------
# (i.e., resolution = 0.0625)

SDS_template_raster <- GCFR_variables$Elevation %>%
  aggregate(fact = 5) %>%  # aggregate up to QDS
  disaggregate(fact = 4)   # disaggregate down to SDS
GCFR_variables_SDS <- resample(
  GCFR_variables, SDS_template_raster,
  method = "bilinear"
)

SDS_template_raster <- SWAFR_variables$Elevation %>%
  aggregate(fact = 5) %>%
  disaggregate(fact = 4)
SWAFR_variables_SDS <- resample(
  SWAFR_variables, SDS_template_raster,
  method = "bilinear"
)

# Generate roughness data ------------------------------------------------------

scales <- 1:8

GCFR_roughness <- map(scales, function(each_scale) {
  map(as.list(GCFR_variables_SDS), function(each_layer) {
    each_layer %>%
      aggregate(fact = each_scale) %>%
      aggregate(fun = sd)
  })
})
names(GCFR_roughness) <- 0.0625 * scales * 2

SWAFR_roughness <- map(scales, function(each_scale) {
  map(as.list(SWAFR_variables_SDS), function(each_layer) {
    each_layer %>%
      aggregate(fact = each_scale) %>%
      aggregate(fun = sd)
  })
})
names(SWAFR_roughness) <- 0.0625 * scales * 2

# Tidy roughness data ----------------------------------------------------------

GCFR_roughness_df <- map_dfr(GCFR_roughness, .id = "scale",
  ~ .x %>%
    stack() %>%
    raster2df() %>%
    na.exclude() %>%
    {cbind(region = "GCFR", .)}
)
SWAFR_roughness_df <- map_dfr(SWAFR_roughness, .id = "scale",
  ~ .x %>%
    stack() %>%
    raster2df() %>%
    na.exclude() %>%
    {cbind(region = "SWAFR", .)}
)
roughness_df <- rbind(GCFR_roughness_df, SWAFR_roughness_df)

# Find first principle component of roughness ----------------------------------

roughness_PCAs <- roughness_df %>%
  split(.$scale) %>%
  map(~ .x[, -(1:4)] %>%
    log1p() %>%
    prcomp(scale. = TRUE)
  )
map(roughness_PCAs, summary)

# Force PC1 scores to be positive if all vars rotations are negative
roughness_PCAs %<>% map(function(PCA) {
  if (all(PCA$rotation[, 1] <= 0)) {
    message("Multiplying this one by -1")
    PCA$rotation[, 1] %<>% multiply_by(-1)
    PCA$x[, 1]        %<>% multiply_by(-1)
  }
  PCA
})
#map2(
#  .x = roughness_PCAs,
#  .y = roughness_df %>%
#    split(.$scale) %>%
#    map(dplyr::select, -lon, -lat),
#  .f =
#    ~ autoplot(.x, data = .y, colour = "region",
#      alpha = 0.25,
#      loadings = TRUE, loadings.colour = "black",
#      loadings.label = TRUE, loadings.label.colour = "black",
#      loadings.label.hjust = -0.25
#    ) +
#    ggtitle(unique(.y$scale)) +
#    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
#    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)
#)

PC1s <- map_dfr(roughness_PCAs, ~tibble(PC1 = .x$x[, 1]))
roughness_df <- cbind(roughness_df, PC1s)
#roughness_df %>%
#  mutate(scale = as.numeric(scale)) %>%
#  gather(variable, sd, -scale, -region, -lon, -lat) %>%
#  ggplot(aes(scale, sd, colour = region)) +
#    geom_jitter() +
#    facet_wrap(~variable, scales = "free_y")

# CLES analysis ----------------------------------------------------------------

CLES_results <- map2_df(
  .x = roughness_df %>%
    filter(region == "GCFR") %>%
    split(.$scale) %>%
    map(dplyr::select, -scale, -region, -lon, -lat),# %>%
    #map(~sample_n(.x, min(c(100, nrow(.x))))),
  .y = roughness_df %>%
    filter(region == "SWAFR") %>%
    split(.$scale) %>%
    map(dplyr::select, -scale, -region, -lon, -lat),# %>%
    #map(~sample_n(.x, min(c(100, nrow(.x))))),
  .id = "scale",  # for every spatial scale,
  ~ map2_df(
    .x = .x,
    .y = .y,
    .id = "variable",  # for every variable in each region,
    ~ tibble(
      CLES_value = CLES(.y, .x),
      U_test = wilcox.test(.x, .y, conf.int = TRUE) %>%
        tidy() %>%
        list()
    )
  )
)
CLES_results %<>% mutate(
  scale = as.numeric(scale),
  diff  = map_dbl(U_test, "estimate"),
  P_U   = map_dbl(U_test, "p.value"),
  U_low = map_dbl(U_test, "conf.low"),
  U_upp = map_dbl(U_test, "conf.high")
)

# Figures
ggplot(CLES_results[CLES_results$variable != "PC1", ]) +
  aes(scale, CLES_value, group = variable) +
  geom_hline(yintercept = 0.5, lty = "dashed") +
  geom_smooth(method = lm, colour = "black") +
  geom_point(aes(shape = P_U < 0.05)) +
  scale_shape_manual(values = c(1, 19)) +
  facet_wrap(~variable)
ggplot(CLES_results) +
  aes(scale, CLES_value, group = variable) +
  geom_hline(yintercept = 0.5, lty = "dashed") +
  geom_smooth(method = lm, colour = "black") +
  geom_point(aes(shape = P_U < 0.05)) +
  scale_shape_manual(values = c(1, 19)) +
  facet_wrap(~variable)
ggplot(CLES_results[CLES_results$variable != "PC1", ]) +
  aes(scale, CLES_value, colour = variable, fill = variable, group = variable) +
  geom_hline(yintercept = 0.5, lty = "dashed") +
  geom_smooth(method = lm, colour = "black") +
  geom_point(aes(shape = P_U < 0.05)) +
  scale_shape_manual(values = c(1, 19))
ggplot(CLES_results) +
  aes(scale, diff, group = variable) +
  geom_hline(yintercept = 0.0, lty = "dashed") +
  geom_smooth(method = lm, colour = "black") +
  geom_errorbar(aes(ymin = U_low, ymax = U_upp), width = 0) +
  geom_point(aes(shape = P_U < 0.05)) +
  scale_shape_manual(values = c(1, 19)) +
  facet_wrap(~variable, scales = "free_y")

# Fit linear models of CLES ~ spatial scale for each variable
CLES_models <- CLES_results %>%
  split(.$variable) %>%
  map(~lm(CLES_value ~ scale, .x))
# Summarise those models
CLES_model_summaries <- CLES_models %>%
  map_df(.id = "variable", tidy) %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = case_when(
    p.value <= 0.05 ~ "*",
    p.value <= 0.10 ~ ".",
    TRUE            ~ " "
  )) %>%
  mutate(variable = factor(variable, levels = var_names %>%
     str_replace_all(" ", "_") %>%
     c("PC1")
  )) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  dplyr::select(variable, estimate, p.value, sig)
CLES_model_summaries
