# Selecting representative BRT-models from the 1000-replicates
#   (especially useuful for predictions)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

output_path <- here("outputs/species-environment-relationships/from-local-machines")

set.seed(1234)

# Helper function(s) -----------------------------------------------------------

filter_reps_by_bound <- function(summary_data, summary_data_medians,
                                 response_ = c("richness", "turnover"),
                                 region_ = c("GCFR", "SWAFR"),
                                 scale_ = c("QDS", "HDS")) {
  reps <- filter(summary_data,
    model_type == "replicates",
    region == region_,
    response == response_,
    scale == scale_
  )
  bounds <- filter(summary_data_medians,
    region == region_,
    response == response_,
    scale == scale_
  )
  filter(reps,
    nt <= bounds$nt_upr,
    nt >= bounds$nt_lwr,
    pseudo_r2 <= bounds$pseudo_r2_upr,
    pseudo_r2 >= bounds$pseudo_r2_lwr,
    pred_obs_r2 <= bounds$pred_obs_r2_upr,
    pred_obs_r2 >= bounds$pred_obs_r2_lwr
  )
}

remove_dir <- function(x, levels_to_keep = 0) {
  stopifnot(is.numeric(levels_to_keep))
  # Remove starting "/" if present
  x %<>% str_remove("^/")
  n_levels <- str_count(x, "/")
  if (levels_to_keep == n_levels) {
    message("Why are you even using this function?")
  } else if (levels_to_keep > n_levels) {
    stop("Cannot remove more levels than path has")
  } else {
    for (i in 1:(n_levels - levels_to_keep)) {
      x %<>% str_remove("[^/]+/")
    }
    if (levels_to_keep == 0) {
      x %<>% str_remove("/")
    }
  }
  x
}

# Import BRT quality results ---------------------------------------------------

summary_dataset_names <- c(
  "QDS-richness-models-summaries.csv",
  "HDS-richness-models-summaries.csv",
  "HDS-turnover-models-summaries.csv"
)
summary_data <- map_df(summary_dataset_names,
  ~ read_csv(glue("{output_path}/{.x}"))
)

# Get median values for each quality metrics for each model --------------------

summary_data_medians <- summary_data %>%
  filter(model_type == "replicates") %>%
  group_by(region, response, scale) %>%
  summarise_at(
    c("nt", "pseudo_r2", "pred_obs_r2", "pred_obs_r2_exp"),
    funs(lwr = quantile(., 0.475), upr = quantile(., 0.525))
  )

# Filter the set of replicate models to those close to median quality ----------

scales <- unique(summary_data_medians$scale)
regions <- unique(summary_data_medians$region)
responses <- unique(summary_data_medians$response)

candidate_rep_models <-
  map_df(scales, function(scale_) {
    map_df(regions, function(region_) {
      map_df(responses, function(response_) {
        filter_reps_by_bound(
          summary_data, summary_data_medians,
          response_, region_, scale_
        )
      })
    })
  })

# Take the first replicate model from each subset ------------------------------

representative_rep_models <- candidate_rep_models %>%
  group_by(scale, region, response) %>%
  arrange(scale, region, response, desc(nt)) %>%
  slice(1) %>%
  select(
    scale, region, response,
    nt, pseudo_r2, pred_obs_r2, pred_obs_r2_exp,
    rep, path
  )
representative_rep_models

# Get those models RDS-files from my external harddrive ------------------------

external_RDS_dir <-
  "/Volumes/RUAN_UCT/Cape-vs-SWA_BRT-outputs-from-local-machines_copy_2018-12-11/"

external_RDS_paths <- representative_rep_models %>%
  ungroup() %>%
  transmute(filename = path %>%
    str_replace("summary", "BRT") %>%
    str_replace("csv", "RDS") %>%
    remove_dir(levels_to_keep = 1)
  ) %$%
  paste0(external_RDS_dir, filename)

file.copy(external_RDS_paths, output_path)
# 2018-12-31 17:37 --- Figured out the problem:
#   Not all the RDS are in the same from-local-machines copy folder

####

ggplot(summary_data, aes(nt, pred_obs_r2, col = region, alpha = model_type)) +
  geom_point() +
  facet_grid(scale ~ response)

####

####

library(gbm)
local_BRT_paths <- list.files(output_path, ".RDS", full.names = TRUE)
BRTs <- map(local_BRT_paths, read_rds)
names(BRTs) <- local_BRT_paths %>%
  str_extract("(GCFR|SWAFR)_(Q|H)DS-(richness|turnover)") %>%
  str_replace("-", "_")

# FIXME
predict(
  BRTs$GCFR_HDS_richness,
  newdata = as.data.frame(BRTs$SWAFR_HDS_richness$data$x.order),
  n.trees = BRTs$GCFR_HDS_richness$n.trees
)

####
