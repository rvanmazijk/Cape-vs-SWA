# Load packages
library(tidyverse)
library(here)
library(glue)
library(raster)
library(rgdal)
library(rgeos)
library(stringr)
library(ggfortify)  # for PCAs
library(canprot)
library(broom)

# Helper functions
source(here("draft-02/R/functions/helper-functions.R"))
raster2df <- function(r) {
  lon_lat <- xyFromCell(r, 1:ncell(r))
  colnames(lon_lat) <- c("lon", "lat")
  df <- as.data.frame(r)
  df <- cbind(lon_lat, df)
  df
}

# Import borders and Larsen et al. (2009) grids
import_region_polygons()

# Import processed environmental data
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

# ...

scales <- seq(from = 0.05, to = 0.50, by = 0.05)
GCFR_roughness <- map(scales, function(each_scale) {
  map(as.list(GCFR_variables), function(each_layer) {
    each_layer %>%
      aggregate(fact = each_scale / 0.05) %>%
      aggregate(fun = sd)
  })
})
names(GCFR_roughness) <- scales * 2
SWAFR_roughness <- map(scales, function(each_scale) {
  map(as.list(SWAFR_variables), function(each_layer) {
    each_layer %>%
      aggregate(fact = each_scale / 0.05) %>%
      aggregate(fun = sd)
  })
})
names(SWAFR_roughness) <- scales * 2
# NOTE: still doesn't, then, have QDS s.s.

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
roughness_matrices <- rbind(GCFR_roughness_df, SWAFR_roughness_df)
roughness_matrices %>%
  gather(variable, sd, -scale, -region) %>%
  ggplot(aes(scale, sd, colour = region)) +
    geom_boxplot() +
    facet_wrap(~variable, scales = "free_y")

roughness_PCAs <- roughness_matrices %>%
  split(.$scale) %>%
  map(dplyr::select, -scale) %>%
  map(~prcomp(.x[, -1], center = TRUE, scale. = TRUE))
map(roughness_PCAs, summary)
PC1s <- imap(roughness_PCAs, ~tibble(scale = .y, PC1 = .x$x[, 1]))
roughness_matrices <- left_join(roughness_matrices, bind_rows(PC1s))

set.seed(1234)
# FIXME:
CLES_results <- map2_df(
  .x = roughness_matrices %>%
    filter(region == "GCFR") %>%
    split(.$scale) %>%
    map(dplyr::select, -scale, -region) %>%
    map(~sample_n(.x, min(c(100, nrow(.x))))),
  .y = roughness_matrices %>%
    filter(region == "SWAFR") %>%
    split(.$scale) %>%
    map(dplyr::select, -scale, -region) %>%
    map(~sample_n(.x, min(c(100, nrow(.x))))),
  .id = "scale",  # for every spatial scale,
  ~ map2_df(
    .x = .x,
    .y = .y,
    .id = "variable",  # for every variable in each region,
    ~ tibble(
      CLES_value = CLES(.y, .x),
      U_test = wilcox.test(.y, .x, conf.int = TRUE) %>%
        tidy() %>%
        list()
    )
  )
)
CLES_results %>%
  mutate(
    variable = var_names[as.numeric(variable)],
    P_U = map_dbl(U_test, "p.value"),
    low = map_dbl(U_test, "conf.low"),
    upp = map_dbl(U_test, "conf.high")
  ) %>%
  ggplot(aes(scale, CLES_value, group = variable)) +
    geom_hline(yintercept = 0.5, lty = "dashed") +
    geom_smooth(method = lm, colour = "black") +
    #geom_errorbar(aes(ymin = low, ymax = upp)) +
    geom_point(aes(shape = P_U < 0.05)) +
    scale_shape_manual(values = c(1, 19)) +
    facet_wrap(~variable)
