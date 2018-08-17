# Geographically weighted regressions of *log* species richness
# as a function of environment and environmental heterogeneity
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
source(here::here("data/import-environmental-data.R"))
#source(here::here("analyses/analyse-turnover.R"))
#GCFR_species
#SWAFR_species
richness_turnover_data <- read_csv(here::here(
  "outputs/turnover/richness_turnover_data.csv"
))

# Make one QDS polygons grid for qdgc name lookups
# First, make each region's QDS polygons have unique IDs
GCFR_QDS2 <- spChFIDs(GCFR_QDS, paste0("Cape_", GCFR_QDS$qdgc))
SWAFR_QDS2 <- spChFIDs(SWAFR_QDS, paste0("SWA_", SWAFR_QDS$qdgc))
# Then merge
QDS_grid <- spRbind(GCFR_QDS2, SWAFR_QDS2)

# Compute roughness data (HDS-scale) as SpatialPointsDataFrames ----------------

# Includes absolute environmental data too

all_variables_HDS <- map(
  .x = list(GCFR_variables, SWAFR_variables),
  .f = function(.x) {

    # Generate roughness + absolute values for each var ------------------------

    .x <- map(
      .x = .x,
      .f = function(.x) {

        # Compute roughness layer ----------------------------------------------

        .x_rough <- .x %>%
          get_roughness(resolution = 0.50) %>%
          rasterToPoints() %>%
          as_tibble()
        colnames(.x_rough)[[3]] <- "roughness"
        .x_rough <- SpatialPointsDataFrame(
          coords = .x_rough[, c("x", "y")],
          data = .x_rough[, "roughness"],
          proj4string = CRS(std_CRS)
        )
        # Name points by HDS
        .x_rough$qdgc <- over(.x_rough, QDS_grid)[, "qdgc"]
        .x_rough$hdgc <- qdgc2hdgc(as.character(.x_rough$qdgc))
        .x_rough$qdgc <- NULL

        # Also get absolute values at same scale -------------------------------

        .x %<>%
          aggregate(fact = 0.50 / 0.05) %>%
          rasterToPoints() %>%
          as_tibble()
        colnames(.x)[[3]] <- "value"
        .x <- SpatialPointsDataFrame(
          coords = .x[, c("x", "y")],
          data = .x[, "value"],
          proj4string = CRS(std_CRS)
        )
        .x$qdgc <- over(.x, QDS_grid)[, "qdgc"]
        .x$hdgc <- qdgc2hdgc(as.character(.x$qdgc))
        .x$qdgc <- NULL

        # Merge absolute and roughness into 1 spatial points data frame --------

        final_hdgcs <- .x_rough$hdgc
        .x <- .x[.x$hdgc %in% final_hdgcs, ]
        .x$roughness <- .x_rough$roughness
        .x

      }
    )

    # Combine all vars into 1 SpatialPointsDataFrame ---------------------------

    # Start with elevation:
    .x_2 <- .x$Elevation
    names(.x_2)[c(1, 3)] <- c("Elevation", "rough_Elevation")

    for (i in 2:length(.x)) {

      # Get the layer and the next layer to the same HDS -----------------------

      # Find the HDS common to a layer and the next layer
      final_hdgcs <- intersect(
        .x_2$hdgc,
        .x[[i]]$hdgc
      )

      # Subset the layer and the next layer to that set of HDS
      .x_2 <- .x_2[
        .x_2$hdgc %in% final_hdgcs,
      ]
      .x[[i]] <- .x[[i]][
        .x[[i]]$hdgc %in% final_hdgcs,
      ]

      # Add the next layer's data ----------------------------------------------

      # Init empty columns in the layer for the next layer's data
      .x_2$new_var <- NULL
      .x_2$new_var_roughness <- NULL

      # Add it
      .x_2$new_var <- .x[[i]]$value
      .x_2$new_roughness <- .x[[i]]$roughness

      # Rename it
      names(.x_2)[names(.x_2) == "new_var"] <-
        var_names[[i]]
      names(.x_2)[names(.x_2) == "new_roughness"] <-
        paste0("rough_", var_names[[i]])

    }

    .x_2

  }
)
names(all_variables_HDS) <- c("Cape", "SWA")

# Add richness and turnover data to SpatialPointsDataFrames --------------------

# Separate Cape and SWA richness-turnover data for map()-ing below
richness_turnover_data <- richness_turnover_data %$% list(
  Cape = filter(richness_turnover_data, region == "Cape"),
  SWA = filter(richness_turnover_data, region == "SWA")
)

all_data_HDS <- map2(
  .x = richness_turnover_data,
  .y = all_variables_HDS,
  .f = function(.x, .y) {

    # Find the HDS common to the richness-turnover and the environmental data
    final_hdgcs <- intersect(.x$hdgc, .y$hdgc)
    .x <- .x[.x$hdgc %in% final_hdgcs, ]
    .y <- .y[.y$hdgc %in% final_hdgcs, ]

    .y@data <- right_join(.y@data, .x)
    .y
    ## Init empty columns in the environmental data
    #.y$region <- NULL
    #.y$HDS_richness <- NULL
    #.y$n_QDS <- NULL
    #.y$mean_QDS_richness <- NULL
    #.y$mean_QDS_jaccard <- NULL
    #.y$add_residual_turnover <- NULL
    #.y$add_residual_turnover_prop <- NULL
    #.y$mul_residual_turnover <- NULL

    ## Add richness-turnover data
    #.y$region <- .x$region
    #.y$HDS_richness <- .x$HDS_richness
    #.y$n_QDS <- .x$n_QDS
    #.y$mean_QDS_richness <- .x$mean_QDS_richness
    #.y$mean_QDS_jaccard <- .x$mean_QDS_jaccard
    #.y$add_residual_turnover <- .x$add_residual_turnover
    #.y$add_residual_turnover_prop <- .x$add_residual_turnover_prop
    #.y$mul_residual_turnover <- .x$mul_residual_turnover

    #.y

  }
)

# Merge the two region's data
all_data_HDS <- spRbind(all_data_HDS$Cape, all_data_HDS$SWA)

# Reorganise columns
all_data_HDS@data <- all_data_HDS@data %$% data.frame(
  # Variable names are tidied automatically (no spaces, etc.)
  region,
  hdgc,
  HDS_richness,
  n_QDS,
  mean_QDS_richness,
  mean_QDS_jaccard,
  add_residual_turnover,
  add_residual_turnover_prop,
  mul_residual_turnover,
  Elevation,
  MAP,
  PDQ,
  `Surface T`,
  NDVI,
  CEC,
  Clay,
  `Soil C`,
  pH,
  rough_Elevation,
  rough_MAP,
  rough_PDQ,
  `rough_Surface T`,
  rough_NDVI,
  rough_CEC,
  rough_Clay,
  `rough_Soil C`,
  rough_pH
)

# Adjust environmental values stored at x10 etc.
all_data_HDS@data %<>% mutate(
  Surface.T  = Surface.T - 273.15, # K -> ºC (no adj needed for rough)
  NDVI       = NDVI / 1e+07,
  rough_NDVI = rough_NDVI / 1e+07,
  pH         = pH / 10,
  rough_pH   = rough_pH / 10
)
# TODO: Check which other vars have been x10
# TODO: Check units for NDVI

# Start modelling --------------------------------------------------------------

gwr_model2 <- function(my_data, response, predictors) {
  formula <- glue(
    "{response} ~ {paste0(predictors, collapse = '+')}"
  )
  print(formula)
  auto_bw <- spgwr::gwr.sel(
    formula, my_data,
    gweight = gwr.Gauss, verbose = TRUE
  )
  model_gwr <- spgwr::gwr(
    formula, my_data,
    gweight = gwr.Gauss, bandwidth = auto_bw, hatmatrix = TRUE
  )
  model_gwr
}
names(all_data_HDS)

# Practice
if (FALSE) {
  gwr_model2(my_data = all_data_HDS, "log(HDS_richness)", 1)
  gwr_model2(
    my_data = all_data_HDS,
    "log(HDS_richness)",
    names(all_data_HDS)[str_detect(names(all_data_HDS), "rough_")]
  )
}
# Works :)

# .... Separate regions' models ------------------------------------------------

# Without the region or HDS-geocode column
Cape_data_HDS <- all_data_HDS[all_data_HDS$region == "Cape", -c(1, 2)]
SWA_data_HDS <- all_data_HDS[all_data_HDS$region == "SWA", -c(1, 2)]

# Describe different sets of predictor variables
abs_var_names <- c(
  "Elevation", "MAP", "PDQ", "Surface.T", "NDVI",
  "CEC", "Clay", "Soil.C", "pH"
)
rough_var_names <- paste0("rough_", abs_var_names)
richness_model_specs <- list(
  null     =  1,
  abs      = abs_var_names,
  rough    = rough_var_names,
  elev     = c("Elevation", "rough_Elevation"),
  non_elev = c(abs_var_names[-1], rough_var_names[-1]),
  soil     = c(abs_var_names[6:9], rough_var_names[6:9]),
  non_soil = c(abs_var_names[1:5], rough_var_names[1:5]),
  full     = c(abs_var_names, rough_var_names)
)

set.seed(1234)
Cape_HDS_richness_models <- map(
  .x = richness_model_specs,
  .f = ~ gwr_model2(
    my_data = Cape_data_HDS,
    response = "log(HDS_richness)",
    predictors = .x
  )
)
Cape_models_summary <- delta_AICc(Cape_HDS_richness_models)
best_Cape_model <- Cape_models_summary$model[Cape_models_summary$delta_AICc == 0]
plot(
  x = log(Cape_data_HDS$HDS_richness),
  y = Cape_HDS_richness_models[[best_model]]$SDF$pred
)
abline(0, 1)

set.seed(1234)
SWA_HDS_richness_models <- map(
  .x = richness_model_specs,
  .f = ~ gwr_model2(
    my_data = SWA_data_HDS,
    response = "log(HDS_richness)",
    predictors = .x
  )
)
SWA_models_summary <- delta_AICc(SWA_HDS_richness_models)
best_SWA_model <- SWA_models_summary$model[SWA_models_summary$delta_AICc == 0]
plot(
  x = log(SWA_data_HDS$HDS_richness),
  y = SWA_HDS_richness_models[[best_SWA_model]]$SDF$pred
)
abline(0, 1)
# Why does ELEVATION matter in SWA??
plot(
  x = SWA_data_HDS$Elevation,
  y = SWA_HDS_richness_models[[best_SWA_model]]$SDF$pred
)
plot(
  x = SWA_data_HDS$rough_Elevation,
  y = SWA_HDS_richness_models[[best_SWA_model]]$SDF$pred
)
plot(
  x = SWA_data_HDS$Elevation,
  y = SWA_data_HDS$HDS_richness
)
plot(
  x = SWA_data_HDS$rough_Elevation,
  y = SWA_data_HDS$HDS_richness
)
