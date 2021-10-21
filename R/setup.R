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
  "pH"
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

my_palette <- c(
  "#307aa5",  # GCFR  blue
  "#E69F00"   # SWAFR orange
)

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
    here("results", glue("{response}_univariate_model_results.csv"))
  )

  # Return full summary with models
  return(univar_model_summary1)
}

plot_PC1_models <- function(dataset,
                            keep_outliers = c(TRUE, FALSE),
                            filename      = "plot-PC1-models",
                            ext           = c("pdf", "png")) {

  # Internal functions ---------------------------------------------------------

  choose_cex <- function(scale = c("QDS", "HDS", "DS")) {
    case_when(
      scale == "QDS" ~ 1.00,
      scale == "HDS" ~ 1.25,
      scale == "DS"  ~ 1.50,
    )
  }

  plot_panel <- function(scale = c("QDS", "HDS", "DS"),
                         my_ylab, PC1_explains) {
    response <- glue("{scale}_richness")
    plot(
      dataset[[scale]]$PC1, dataset[[scale]][[response]],
      yaxt = "n",
      xlab = glue("PC1 ({PC1_explains}%)"),
      ylab = my_ylab,
      xlim = my_xlims[[scale]],  # depends on this object existing
      ylim = my_ylim,            # ''
      pch  = 16, cex = choose_cex(scale),
      col  = ifelse(dataset[[scale]]$region == "GCFR", my_palette[[1]], my_palette[[2]])
    )
  }

  plot_outliers <- function(scale = c("QDS", "HDS", "DS")) {
    response <- glue("{scale}_richness")
    points(
      outliers[[scale]]$PC1, outliers[[scale]][[response]],
      pch = 17, cex = choose_cex(scale),
      col = ifelse(outliers[[scale]]$region == "GCFR", my_palette[[1]], my_palette[[2]])
    )
  }

  fit_model_dirty <- function(scale = c("QDS", "HDS", "DS")) {
    response <- glue("{scale}_richness")
    predictors <- "PC1"
    if ((scale == "QDS") |
        (scale == "HDS" & !keep_outliers)) {
      predictors %<>% paste("+ region")
    }
    dataset2 <- dataset[[scale]]
    if (!keep_outliers) {
      dataset2 %<>% filter(!is_PC1_outlier)
    }
    lm(glue("{response} ~ {predictors}"), dataset2)
  }

  R2 <- function(label, x) {
    title(adj = 0, bquote(
      .(label)~"("*italic("R")^2 == .(x)*")"
    ))
  }

  change_col_alpha <- function(x, alpha = 0.5) {
    rgb(
      red   = col2rgb(x)[1, ]/255,
      green = col2rgb(x)[2, ]/255,
      blue  = col2rgb(x)[3, ]/255,
      alpha = alpha
    )
  }

  # Fit models quickly and dirtily ---------------------------------------------
  # (Before data modifications below)

  m_QDS <- fit_model_dirty("QDS")
  m_HDS <- fit_model_dirty("HDS")
  m_DS  <- fit_model_dirty("DS")

  # For plotting model fits:
  PC1_seq <- seq(from = -7, to = 7, by = 0.1)

  # Make changes to filename and data if needed --------------------------------

  if (keep_outliers) {
    # Pull out outlier points to plot as triangles
    outliers <- map(dataset, filter, is_PC1_outlier)
  } else {
    # Otherwise just don't plot the outliers and make the filename clear
    filename %<>% paste0("_refit")
  }

  # Initialise plotting environment on disc ------------------------------------

  filename <- here("figures", glue("{filename}.{ext}"))
  if (ext == "pdf") {
    pdf(filename, width = 8, height = 3)
  } else if (ext == "png") {
    png(filename, width = 8, height = 3, units = "in", res = 600)
  }

  # Define axis limits ---------------------------------------------------------

  my_ylim <- c(0, max(dataset$DS$DS_richness))
  my_xlims <- map(dataset, ~range(.$PC1))

  # Remove outliers from data used to plot dots (not triangles) ----------------

  dataset %<>% map(filter, !is_PC1_outlier)

  # Plot panels ----------------------------------------------------------------

  par(mfrow = c(1, 3))

  # .... (a) QDS ---------------------------------------------------------------

  par(mar = c(4, 4, 3, 0))

  # Plot points
  plot_panel(
    scale        = "QDS",
    my_ylab      = expression(paste(italic("S"))),
    PC1_explains = "40.14"
  )
  if (keep_outliers) {
    plot_outliers("QDS")
  }
  R2("(a) QDS", round(glance(m_QDS)$r.squared, digits = 2))

  # Plot model fits
  fit_GCFR <- predict(
    m_QDS,
    newdata = data.frame(region = "GCFR", PC1 = PC1_seq)
  )
  fit_SWAFR <- predict.lm(
    m_QDS,
    newdata = data.frame(region = "SWAFR", PC1 = PC1_seq)
  )
  lines(PC1_seq, fit_GCFR,  col = my_palette[[1]], lwd = 3)
  lines(PC1_seq, fit_SWAFR, col = my_palette[[2]], lwd = 3)

  # Add y-axis
  axis(2,
    at     = c( 0,  500,  1000,  1500,  2000,  2500,  3000,  3500,  4000),
    labels = c("0",  "", "1000",   "", "2000",   "", "3000",   "", "4000")
  )

  # Legend
  legend(
    x = -6, y = 3800,
    legend  = unique(dataset$QDS$region),
    pch     = 21,
    pt.bg   = c(my_palette[[1]], my_palette[[2]]),
    box.col = NA
  )

  # .... (b) HDS ---------------------------------------------------------------

  par(mar = c(4, 2, 3, 2))

  # Plot points
  plot_panel(
    scale        = "HDS",
    my_ylab      = "",
    PC1_explains = "44.82"
  )
  if (keep_outliers) {
    plot_outliers("HDS")
  }
  R2("(b) HDS", round(glance(m_HDS)$r.squared, digits = 2))

  # Plot model fits
  if (keep_outliers) {
    fit <- predict.lm(m_HDS, newdata = data.frame(PC1 = PC1_seq))
    lines(PC1_seq, fit,  col = "black",  lwd = 3)
  } else {
    fit_GCFR <- predict(
      m_HDS,
      newdata = data.frame(region = "GCFR", PC1 = PC1_seq)
    )
    fit_SWAFR <- predict.lm(
      m_HDS,
      newdata = data.frame(region = "SWAFR", PC1 = PC1_seq)
    )
    lines(PC1_seq, fit_GCFR,  col = my_palette[[1]], lwd = 3, )
    lines(PC1_seq, fit_SWAFR, col = my_palette[[2]], lwd = 3)
  }

  # .... (c) DS ----------------------------------------------------------------

  par(mar = c(4, 0, 3, 4))

  # Plot points
  plot_panel(
    scale        = "DS",
    my_ylab      = "",
    PC1_explains = "49.94"
  )
  if (keep_outliers) {
    plot_outliers("DS")
  }
  R2("(c) DS", round(glance(m_DS)$r.squared, digits = 2))

  # Plot model fits
  fit <- predict.lm(m_DS, newdata = data.frame(PC1 = PC1_seq))
  lines(PC1_seq, fit,  col = "black",  lwd = 3)

  # Close & reset plotting environment -----------------------------------------

  dev.off()
  par(op)

  # Return models for checking -------------------------------------------------

  return(list(
    m_QDS = m_QDS,
    m_HDS = m_HDS,
    m_DS  = m_DS
  ))
}

# Import data ------------------------------------------------------------------

# .... Region polygons ---------------------------------------------------------

GCFR_border_buffered <- readOGR(here(
  "data/derived-data/borders",
  "GCFR_border_buffered/"
))
SWAFR_border_buffered <- readOGR(here(
  "data/derived-data/borders",
  "SWAFR_border_buffered/"
))

# Merge regions' borders
borders_buffered <- rbind(GCFR_border_buffered, SWAFR_border_buffered)

# .... My Larsen-type grid polygons and rasters --------------------------------

# Shapefiles
Larsen_grid_EDS <- readOGR(
  glue("{data_dir}/Larsen_grid_EDS"),
  layer = "Larsen_grid_EDS"
)
Larsen_grid_QDS <- readOGR(
  glue("{data_dir}/Larsen_grid_QDS"),
  layer = "Larsen_grid_QDS"
)
Larsen_grid_HDS <- readOGR(
  glue("{data_dir}/Larsen_grid_HDS"),
  layer = "Larsen_grid_HDS"
)

# Query the border polygon and store results in Larsen grid
Larsen_grid_EDS@data <- cbind(
  Larsen_grid_EDS@data,
  Larsen_grid_EDS %over% borders_buffered
)
# For larger cells, just use longitude for region classification,
# because later I filter the cells based on whether their constituent cells
# are in the regions (from EDS above)
Larsen_grid_QDS@data$region <-
  ifelse(Larsen_grid_QDS@data$lon > 60,
    "SWAFR", "GCFR"
  )
Larsen_grid_HDS@data$region <-
  ifelse(Larsen_grid_HDS@data$lon > 60,
    "SWAFR", "GCFR"
  )

# Filter to EDS that are within the regions' borders
Larsen_grid_EDS <- Larsen_grid_EDS[!is.na(Larsen_grid_EDS$region), ]

# Make grids tibbles for easier wrangling
Larsen_grid_EDS_data <- as_tibble(Larsen_grid_EDS@data)
Larsen_grid_QDS_data <- as_tibble(Larsen_grid_QDS@data)
Larsen_grid_HDS_data <- as_tibble(Larsen_grid_HDS@data)

# Detemine which DS, HDS & QDS have all 4 of their HDS, QDS & EDS --------------
# (within the regions' borders)

# Pull out QDS-codes of QDS with all 4 EDS (within borders)
QDS_w_all_EDS <- Larsen_grid_EDS_data %>%
  group_by(qdgc, region) %>%
  dplyr::select(edgc) %>%
  distinct() %>%  # just in case
  summarise(n_EDS = n()) %>%
  filter(n_EDS == 4) %>%
  pull(qdgc)

# Pull out HDS-codes of HDS with all 4 HDS (within borders)
HDS_w_all_QDS <- Larsen_grid_QDS_data %>%
  group_by(hdgc, region) %>%
  dplyr::select(qdgc) %>%
  distinct() %>%
  filter(qdgc %in% QDS_w_all_EDS) %>%
  summarise(n_QDS = n()) %>%
  filter(n_QDS == 4) %>%
  pull(hdgc)

# Pull out DS-codes of DS with all 4 DS (within borders)
DS_w_all_HDS <- Larsen_grid_HDS_data %>%
  group_by(dgc, region) %>%
  dplyr::select(hdgc) %>%
  distinct() %>%
  filter(hdgc %in% HDS_w_all_QDS) %>%
  summarise(n_HDS = n()) %>%
  filter(n_HDS == 4) %>%
  pull(dgc)

# Raster-layers
Larsen_grid_EDS_ras <- raster(glue(
  "{data_dir}/raster-layers/",
  "Larsen_grid_EDS_ras.tif"
))
Larsen_grid_QDS_ras <- raster(glue(
  "{data_dir}/raster-layers/",
  "Larsen_grid_QDS_ras.tif"
))
Larsen_grid_HDS_ras <- raster(glue(
  "{data_dir}/raster-layers/",
  "Larsen_grid_HDS_ras.tif"
))
Larsen_grid_DS_ras <- raster(glue(
  "{data_dir}/raster-layers/",
  "Larsen_grid_DS_ras.tif"
))

# .... GBIF species occurrence datasets ----------------------------------------

GCFR_species_occ <- make_SpatialPointsDataFrame(read_csv(here(
  "data/derived-data/flora",
  "GCFR_clean_flora_2017-09-14.csv"
)))
SWAFR_species_occ <- make_SpatialPointsDataFrame(read_csv(here(
  "data/derived-data/flora",
  "SWAFR_clean_flora_2017-09-14.csv"
)))

# Merge regions' data
species_occ <- rbind(GCFR_species_occ, SWAFR_species_occ)

# .... Environmental data ------------------------------------------------------

GCFR_file_names  <- glue(
  "{data_dir}/raster-layers/",
  "GCFR_{var_names}_masked2.tif"
)
SWAFR_file_names <- glue(
  "{data_dir}/raster-layers/",
  "SWAFR_{var_names}_masked2.tif"
)

GCFR_variables  <- stack(GCFR_file_names)
SWAFR_variables <- stack(SWAFR_file_names)

enviro_data <- raster::merge(GCFR_variables, SWAFR_variables)
names(enviro_data)  <- var_names_tidy
