# Varying CV protocols for BRT construction
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))
setwd(here(
  "R/02_analyses/",
  "analyse-species-environment-relationships/",
  "run-on-UCT-HPC/"
))
source("setup.R")
setwd(here())

# E.g. GCFR richness model -----------------------------------------------------

biplots <- foreach(variable_ = GCFR_predictor_names) %do% {
  ggplot(GCFR_variables_HDS, aes_string(variable_, "HDS_richness")) +
    geom_point()
}
names(biplots) <- GCFR_predictor_names
biplots$rough_Elevation

set.seed(Sys.time())
summary(gbm.step(
  data = GCFR_variables_HDS,
  gbm.x = GCFR_predictor_names,
  gbm.y = "HDS_richness",
  tree.complexity = 3,
  learning.rate = 0.001,
  max.trees = 10000,
  family = "gaussian",
  bag.fraction = 0.75,  # default
  plot.main = TRUE
))
