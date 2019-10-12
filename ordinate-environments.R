library(here)
library(tidyverse)
library(glue)
library(magrittr)
library(ggfortify)
library(raster)
library(rgdal)
library(vegan)

# ...

data <- read_csv(here(
  "data/derived-data/May-2019",
  "data-QDS.csv"
))

soil_PCA <- data %>%
  dplyr::select(CEC, Clay, Soil_C, pH) %>%
  prcomp(center = TRUE, scale. = TRUE)

summary(soil_PCA)
if (all(soil_PCA$rotation[, 1] <= 0)) {
  soil_PCA$rotation[, 1] %<>% multiply_by(-1)
  soil_PCA$x[, 1]        %<>% multiply_by(-1)
}

soil_PCA %>%
  autoplot(
    data = data, colour = "region", size = "QDS_richness",# alpha = 0.25,
    loadings = TRUE, loadings.colour = "black",
    loadings.label = TRUE, loadings.label.colour = "black",
    loadings.label.hjust = -0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)

clim_PCA <- data %>%
  dplyr::select(MAP, PDQ, Surface_T) %>%
  prcomp(center = TRUE, scale. = TRUE)

summary(clim_PCA)
if (all(clim_PCA$rotation[, 1] <= 0)) {
  clim_PCA$rotation[, 1] %<>% multiply_by(-1)
  clim_PCA$x[, 1]        %<>% multiply_by(-1)
}

clim_PCA %>%
  autoplot(
    data = data, colour = "region", size = "QDS_richness",# alpha = 0.25,
    loadings = TRUE, loadings.colour = "black",
    loadings.label = TRUE, loadings.label.colour = "black",
    loadings.label.hjust = -0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)

# ...

data_dir <- here("data/derived-data/May-2019")
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

GCFR_file_names  <- glue("{data_dir}/GCFR_{var_names}_masked2.tif")
SWAFR_file_names <- glue("{data_dir}/SWAFR_{var_names}_masked2.tif")

GCFR_variables  <- stack(GCFR_file_names)
SWAFR_variables <- stack(SWAFR_file_names)

names(GCFR_variables) <- names(SWAFR_variables) <-
  str_replace_all(var_names, " ", "_")

data2 <- as_tibble(rbind(
  na.exclude(cbind(region = "GCFR",  as.data.frame(log10(GCFR_variables)))),
  na.exclude(cbind(region = "SWAFR", as.data.frame(log10(SWAFR_variables))))
))
abs_PCA <- prcomp(data2[, -(1:2)], center = TRUE, scale. = TRUE)
summary(abs_PCA)

abs_soil_PCA <- prcomp(data2[, 7:10], center = TRUE, scale. = TRUE)
summary(abs_soil_PCA)

abs_clim_PCA <- prcomp(data2[, 2:6], center = TRUE, scale. = TRUE)
summary(abs_clim_PCA)

abs_PCA %>%
  autoplot(
    data = data2, colour = "region", alpha = 0.25,
    loadings = TRUE, loadings.colour = "black",
    loadings.label = TRUE, loadings.label.colour = "black",
    loadings.label.hjust = -0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)

abs_soil_PCA %>%
  autoplot(
    data = data2, colour = "region", alpha = 0.25,
    loadings = TRUE, loadings.colour = "black",
    loadings.label = TRUE, loadings.label.colour = "black",
    loadings.label.hjust = -0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)

abs_clim_PCA %>%
  autoplot(
    data = data2, colour = "region", alpha = 0.25,
    loadings = TRUE, loadings.colour = "black",
    loadings.label = TRUE, loadings.label.colour = "black",
    loadings.label.hjust = -0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)

