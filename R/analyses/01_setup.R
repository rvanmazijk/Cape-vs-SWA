# Heterogeneity and species richness: analysis setup
# R. van Mazijk
# CC-BY-4.0 2019

# Load packages ----------------------------------------------------------------

# General programming
library(tidyverse)
library(here)  # for more reliable file paths
library(glue)  # better than paste()
library(magrittr)  # for %<>% & %$%

# GIS
library(raster)
library(rgdal)
library(rgeos)

# Analyses
library(canprot)  # for CLES
library(broom)  # to tidy model outputs

# Figures
library(cowplot)  # for panelling
library(ggfortify)  # for autoplot() of PCAs
library(rasterVis)
library(scales)
library(grid)

# Set global variables ---------------------------------------------------------

data_dir <- here("data/derived-data/Feb-2020")

# Environmental variable names in nice order
var_names <- c(
  "Elevation",
  "MAP",
  "PDQ",
  "Surface T",
  "NDVI",
  "CEC",
  "Clay",
  "Soil C",
  "pH",
  "PC1"
)
# And a syntactically valid version
var_names_tidy <- str_replace(var_names, " ", "_")

# Figure things ----------------------------------------------------------------

# Preserve clean plotting environment (for base:: figures)
op <- par()

# Set global theme (for ggplot2:: figures)
my_theme <-
  theme_bw() +
  theme(
    strip.background = element_blank(),
    panel.grid       = element_blank()
  )
theme_set(my_theme)

# Make a blank plot object (useful as filler when arranging panels)
white_rect <- grid.rect(gp = gpar(col = "white"))

# Helper-functions -------------------------------------------------------------

grid_dim <- function(x) {
  # Gets the latitudinal and longitudinal range of a SpatialPolygonsDataFrame
  list(
    width  = extent(x)[2] - extent(x)[1],
    height = extent(x)[4] - extent(x)[3],
    crs    = proj4string(x)
  )
}

grid2raster <- function(x, resol = c(0.125, 0.25, 0.5, 1)) {
  # Creates a raster with the same dimensions and number of cells
  # as a Larsen-type grid SpatialPolygonsDataFrame
  n_gc_wide <- grid_dim(x)$width  / resol
  n_gc_high <- grid_dim(x)$height / resol
  raster(
    nrow = n_gc_high, ncol = n_gc_wide,
    crs = proj4string(x),
    xmn = extent(x)[1], xmx = extent(x)[2],
    ymn = extent(x)[3], ymx = extent(x)[4]
  )
}

raster2df <- function(r, Larsen_grid_data = NULL) {
  # Creates a dataframe of raster layer/stack/brick data
  # with columns for the lon and lat of the midpoint of each cell
  df <- cbind(
    xyFromCell(r, 1:ncell(r)),
    as.data.frame(r)
  )
  names(df)[1:2] <- c("lon", "lat")
  if (!is.null(Larsen_grid_data)) {
    df <- full_join(Larsen_grid_data, df)
  }
  df
}

force_positive_PC1 <- function(PCA) {
  # Ammends all variables' loadings in a PCA to be positive
  # if they are all negative, for simplicity
  if (all(PCA$rotation[, 1] <= 0)) {
    message("Multiplying this one by -1")
    PCA$rotation[, 1] %<>% multiply_by(-1)
    PCA$x[, 1]        %<>% multiply_by(-1)
  }
  PCA
}

make_SpatialPointsDataFrame <- function(df) {
  # Makes a SpatialPointsDataFrame out of the (cleaned) GBIF occurrence data
  # for GCFR and SWAFR vascular plants
  SpatialPointsDataFrame(
    coords      = df[, c("decimallongitude", "decimallatitude")],
    data        = df[, "species"],
    proj4string = crs(borders_buffered)  # depends on this object existing!
  )
}

rasterise_data <- function(df, df_col, r) {
  # TODO: describe this function
  r[cellFromXY(r, as.data.frame(df[, c("lon", "lat")]))] <- df[[df_col]]
  r[r == 0] <- NA
  r
}

test_diff <- function(response, sub_sample = FALSE) {
  # Tests for differences between GCFR and SWAFR richness and turnover
  # using Mann-Whitney U-tests and describes those differences using CLES
  dataset <- data %$% {  # depends on this object existing!
    if      (response ==     "QDS_richness")                       QDS
    else if (response %in% c("HDS_richness", "HDS_turnover_prop")) HDS
    else if (response %in% c("DS_richness",  "DS_turnover_prop"))  DS
  }
  x_GCFR  <- dataset[[response]][dataset$region == "GCFR"]
  x_SWAFR <- dataset[[response]][dataset$region == "SWAFR"]
  U_test <- wilcox.test(x_GCFR, x_SWAFR)
  tibble(
    metric     = response,
    GCFR_mean  = mean(x_GCFR),
    SWAR_mean  = mean(x_SWAFR),
    P_U        = tidy(U_test)$p.value,
    CLES_value = CLES(x_SWAFR, x_GCFR)
  )
}

fit_univariate_models <- function(response) {
  # TODO: explain this univariate-model-fitting helper-function
  dataset <- data %$% {  # depends on this object existing!
    if      (response == "QDS_richness") QDS
    else if (response == "HDS_richness") HDS
    else if (response == "DS_richness")  DS
  }

  univar_models <- map(predictor_names, ~list(
    non_region = lm(glue("{response} ~ {.x}"),          dataset),
    add_region = lm(glue("{response} ~ {.x} + region"), dataset),
    int_region = lm(glue("{response} ~ {.x} * region"), dataset)
  ))
  names(univar_models) <- predictor_names

  univar_model_summary1 <- univar_models %>%
    map_dfr(.id = "variable",
      ~ tibble(
        model_type = names(.x),
        model_rank = 1:3,
        model = .x
      )
    ) %>%
    group_by(variable) %>%
    mutate(
      slope        = map_dbl(model, ~tidy(.x)$estimate[2]),
      P_slope      = map_dbl(model, ~tidy(.x)$p.value[ 2]),
      region_coeff = map2_dbl(model, model_type,
                       ~ ifelse(.y != "non_region",
                         tidy(.x)$estimate[3],
                         NA
                       )
                     ),
      P_region     = map2_dbl(model, model_type,
                       ~ ifelse(.y != "non_region",
                         tidy(.x)$p.value[3],
                         NA
                       )
                     ),
      int_coeff    = map2_dbl(model, model_type,
                       ~ ifelse(.y == "int_region",
                         tidy(.x)$estimate[4],
                         NA
                       )
                     ),
      P_int        = map2_dbl(model, model_type,
                       ~ ifelse(.y == "int_region",
                         tidy(.x)$p.value[4],
                         NA
                       )
                     ),
      slope_sig    = ifelse(P_slope  < 0.05, "*", ""),
      region_sig   = ifelse(P_region < 0.05, "*", ""),
      int_sig      = ifelse(P_int    < 0.05, "*", ""),
      AIC          = map_dbl(model, AIC),
      delta_AIC    = AIC - min(AIC),
      best_model   = (model_rank == min(model_rank[delta_AIC < 2]))
    ) %>%
    filter(best_model) %>%
    ungroup() %>%
    mutate(
      model_type =
        case_when(
          model_type == "non_region"                   ~ "Main effect only",
          model_type == "add_region" & P_slope <  0.05 ~ "Main effect + region",
          model_type == "add_region" & P_slope >= 0.05 ~ "Region only",
          model_type == "int_region"                   ~ "Main effect * region"
        ) %>%
        factor(levels = c(
          "Main effect * region",
          "Main effect + region",
          "Main effect only",
          "Region only"
        )),
      variable    = str_replace_all(variable, "_", " "),
      slope_sign  = ifelse(slope        > 0, "+", "-"),
      region_sign = ifelse(region_coeff > 0, "+", "-"),
      int_sign    = ifelse(int_coeff    > 0, "+", "-")
    ) %>%
    mutate_at(c("P_slope", "P_region", "P_int"),
      ~ case_when(
        .x < 0.001 ~ "***",
        .x < 0.010 ~ "**",
        .x < 0.050 ~ "*",
        .x < 0.100 ~ ".",
        TRUE       ~ " "
      )
    ) %>%
    mutate_if(is.character, ~ ifelse(is.na(.x), " ", .x))

  # Make summary table
  univar_model_summary2 <- univar_model_summary1 %>%
    dplyr::select(
      model_type,  variable,
      slope,        P_slope,
      region_coeff, P_region,
      int_coeff,    P_int
    ) %>%
    mutate_if(is.numeric, ~format(round(., digits = 3), nsmall = 3)) %>%
    arrange(model_type)
  # Remove variable names after first mention in table
  univar_model_summary2$model_type %<>% as.character()
  for (pred in unique(univar_model_summary2$model_type)) {
    to_remove <- which(univar_model_summary2$model_type == pred)[-1]
    univar_model_summary2$model_type[to_remove] <- " "
  }

  # Save (tidy) results to disc
  write_csv(
    univar_model_summary2,
    here("for-Dryad", glue("{response}_univariate_model_results.csv"))
  )

  # Return full summary with models
  return(univar_model_summary1)
}
