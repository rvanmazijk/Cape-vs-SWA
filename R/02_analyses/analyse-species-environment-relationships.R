# Analyse value of environmental & heterogeneity variables for predicting
# vascular plant species richness and turnover---using BRTs
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))
source(here("R/02_analyses/generate-roughness.R"))
source(here("R/02_analyses/generate-turnover.R"))

output_path <- here("outputs/species-environment-relationships")

library(dismo)
library(virtualspecies)

# HDS-scale for now:

# Combine all data -------------------------------------------------------------

# Tidy names(<region>_species) after SpatialPointsDataFrame import
names(GCFR_species) <- c(
  "family", "genus", "species",
  "qdgc", "hdgc",
  "HDS_richness", "n_QDS", "mean_QDS_richness", "mean_QDS_turnover"
)
names(SWAFR_species) <- c(
  "family", "genus", "species",
  "qdgc", "hdgc",
  "HDS_richness", "n_QDS", "mean_QDS_richness", "mean_QDS_turnover"
)

variables_HDS_stacks <- pmap(
  # For each region:
  .l = list(
    vars = list(GCFR_variables, SWAFR_variables),
    rough_vars = list(GCFR_roughness_HDS, SWAFR_roughness_HDS),
    species = list(GCFR_species, SWAFR_species)
  ),
  .f = function(vars, rough_vars, species) {

    # Generate absolute environmental values at HDS-scale
    vars %<>% map(aggregate, fact = 0.50 / 0.05)

    # Add roughnes layers to data list
    names(rough_vars) %<>% paste0("rough_", .)
    vars %<>% c(rough_vars)

    # Convert NA-turnover to nonsensicle value (rasterize() can't handles NAs)
    species$mean_QDS_turnover[is.na(species$mean_QDS_turnover)] <- 9999

    # Add raster of HDS richness and mean QDS turnover to data list
    species_rasters <- list(
      HDS_richness = rasterize(
        species,
        vars$Elevation,
        field = "HDS_richness"
      ),
      mean_QDS_turnover = rasterize(
        species,
        vars$Elevation,
        field = "mean_QDS_turnover"
      )
    )

    # Convert nonsense back to NAs
    nonsense <- species_rasters$mean_QDS_turnover == 9999
    species_rasters$mean_QDS_turnover[nonsense] <- NA

    # Add richness and turnover to data list
    vars <- c(species_rasters, vars)

    # And make it all a RasterStack
    stack(vars)

  }
)

GCFR_variables_HDS_stack <- variables_HDS_stacks[[1]]
SWAFR_variables_HDS_stack <- variables_HDS_stacks[[2]]

# Collinearity checks ----------------------------------------------------------

# .... GCFR --------------------------------------------------------------------

png(
  filename = glue("{output_path}/Cape_collinearity-check_0.8-cutoff_HDS.png"),
  width = 17, height = 15, units = "cm", res = 100
)
GCFR_collinearity <- removeCollinearity(
  raster.stack = GCFR_variables_HDS_stack[[-c(1, 2)]],  # without richness (the response)
  select.variables = FALSE,  # To do manually
  multicollinearity.cutoff = 0.8,  # My thumb-suck
  plot = TRUE
)
dev.off()

# .... SWAFR -------------------------------------------------------------------

png(
  filename = glue("{output_path}/SWA_collinearity-check_0.8-cutoff_HDS.png"),
  width = 17, height = 15, units = "cm", res = 100
)
SWAFR_collinearity <- removeCollinearity(
  raster.stack = SWAFR_variables_HDS_stack[[-c(1, 2)]],  # without richness (the response)
  select.variables = FALSE,  # To do manually
  multicollinearity.cutoff = 0.8,  # My thumb-suck
  plot = TRUE
)
dev.off()

# Take first variable in each collinear cluster
GCFR_predictor_names <- map_chr(GCFR_collinearity, 1)
SWAFR_predictor_names <- map_chr(SWAFR_collinearity, 1)

# BRTs -------------------------------------------------------------------------

# .... nt, tc, lr --------------------------------------------------------------

# TODO properly

# Thumbsucks for now:
nt <- 10000
tc <- 5  # No more than 5-way interactions
lr <- 0.002

# .... Initial model fitting: gbm.step(richness ~ ...) -------------------------

gbm_steps <- pmap(
  # For each region:
  .l = list(
    variables = list(GCFR_variables_HDS_stack, SWAFR_variables_HDS_stack),
    predictor_names = list(GCFR_predictor_names, SWAFR_predictor_names)
  ),
  .f = function(variables, predictor_names) {
    fit_gbm_step(
      variables, predictor_names,
      response_name = "HDS_richness", log_response = TRUE,
      tc = tc, lr = lr, nt = nt
    )
  }
)

# Explore results

GCFR_gbm_step <- gbm_steps[[1]]
SWAFR_gbm_step <- gbm_steps[[2]]

pseudo_r2(GCFR_gbm_step)
GCFR_gbm_step$n.trees
GCFR_gbm_step$contributions
summary(GCFR_gbm_step)

pseudo_r2(SWAFR_gbm_step)
SWAFR_gbm_step$n.trees
SWAFR_gbm_step$contributions
summary(SWAFR_gbm_step)

# .... Model simplification: gbm.simplify(...) ---------------------------------

GCFR_gbm_simp <- gbm.simplify(GCFR_gbm_step)
optimal_no_drops <- GCFR_gbm_simp$deviance.summary %$%
  which(mean == min(mean))
GCFR_predictor_names_simp <- GCFR_gbm_simp$pred.list[[optimal_no_drops]]

SWAFR_gbm_simp <- gbm.simplify(SWAFR_gbm_step)
optimal_no_drops <- SWAFR_gbm_simp$deviance.summary %$%
  which(mean == min(mean))
SWAFR_predictor_names_simp <- SWAFR_gbm_simp$pred.list[[optimal_no_drops]]

# .... Refitting models with simplified predictor sets -------------------------

gbm_steps_simp <- pmap(
  # For each region:
  .l = list(
    variables = list(GCFR_variables_HDS_stack, SWAFR_variables_HDS_stack),
    predictor_names = list(GCFR_predictor_names_simp, SWAFR_predictor_names_simp)
  ),
  .f = function(variables, predictor_names) {
    fit_gbm_step(
      variables, predictor_names,
      response_name = "HDS_richness", log_response = TRUE,
      tc = tc, lr = lr, nt = nt
    )
  }
)

# Explore results

GCFR_gbm_step_simp <- gbm_steps_simp[[1]]
SWAFR_gbm_step_simp <- gbm_steps_simp[[2]]

pseudo_r2(GCFR_gbm_step_simp)
GCFR_gbm_step_simp$n.trees
GCFR_gbm_step_simp$contributions
summary(GCFR_gbm_step_simp)

pseudo_r2(SWAFR_gbm_step_simp)
SWAFR_gbm_step_simp$n.trees
SWAFR_gbm_step_simp$contributions
summary(SWAFR_gbm_step_simp)

# .... Explore predicted richness vs obs ---------------------------------------

GCFR_pred_vs_obs_data <-
  tibble(
    pred = GCFR_gbm_step_simp$fitted,
    obs = GCFR_gbm_step_simp$data$y
  ) %>%
  mutate(
    exp_pred = exp(pred),
    exp_obs = exp(obs)
  )
SWAFR_pred_vs_obs_data <-
  tibble(
    pred = SWAFR_gbm_step_simp$fitted,
    obs = SWAFR_gbm_step_simp$data$y
  ) %>%
  mutate(
    exp_pred = exp(pred),
    exp_obs = exp(obs)
  )

GCFR_pred_vs_obs_m <- lm(pred ~ obs, data = GCFR_pred_vs_obs_data)
GCFR_pred_vs_obs_m_exp <- lm(exp_pred ~ exp_obs, data = GCFR_pred_vs_obs_data)
SWAFR_pred_vs_obs_m <- lm(pred ~ obs, data = SWAFR_pred_vs_obs_data)
SWAFR_pred_vs_obs_m_exp <- lm(exp_pred ~ exp_obs, data = SWAFR_pred_vs_obs_data)

tidy(GCFR_pred_vs_obs_m)
tidy(GCFR_pred_vs_obs_m_exp)
tidy(SWAFR_pred_vs_obs_m)
tidy(SWAFR_pred_vs_obs_m_exp)

glance(GCFR_pred_vs_obs_m)
glance(GCFR_pred_vs_obs_m_exp)
glance(SWAFR_pred_vs_obs_m)
glance(SWAFR_pred_vs_obs_m_exp)

visreg::visreg(GCFR_pred_vs_obs_m, gg = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")
visreg::visreg(SWAFR_pred_vs_obs_m, gg = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")

visreg::visreg(GCFR_pred_vs_obs_m_exp, gg = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")
visreg::visreg(SWAFR_pred_vs_obs_m_exp, gg = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")
