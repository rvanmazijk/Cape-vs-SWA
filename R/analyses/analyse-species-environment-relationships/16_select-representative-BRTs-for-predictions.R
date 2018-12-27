# Choose representative final BRTs (for predictions) from replicate sets
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

output_path <- here("outputs/species-environment-relationships/from-local-machines")

# Import BRT quality results ---------------------------------------------------

summary_dataset_names <- c(
  "QDS-richness-models-summaries.csv",
  "HDS-richness-models-summaries.csv",
  "HDS-turnover-models-summaries.csv"
)
summary_data <- map_df(summary_dataset_names,
  ~ read_csv(glue("{output_path}/{.x}"))
)

####

# Get median values for each quality metrics for each model
summary_data_medians <- summary_data %>%
  filter(model_type == "replicates") %>%
  group_by(region, response, scale) %>%
  summarise_at(
    c("nt", "pseudo_r2", "pred_obs_r2", "pred_obs_r2_exp"),
    funs(lwr = quantile(., 0.475), upr = quantile(., 0.525))
  )

filter_reps_by_bounds <- function(summary_data, summary_data_medians,
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

candidate_rep_models <-
  map_df(scales, function(scale_) {
    map_df(regions, function(region_) {
      map_df(responses, function(response_) {
        filter_reps_by_nt_bound(
          summary_data, summary_data_medians,
          response_, region_, scale_
        )
      })
    })
  })

candidate_rep_models %>%
  group_by(scale, region, response) %>%
  arrange(scale, region, response, desc(nt)) %>%
  slice(1) %>%
  select(
    scale, region, response,
    nt, pseudo_r2, pred_obs_r2, pred_obs_r2_exp,
    rep
  )

####

ggplot(summary_data, aes(nt, pred_obs_r2, col = region, alpha = model_type)) +
  geom_point() +
  facet_grid(scale ~ response)

####
