# Testing for differences in model quality between repeated and permuted BRTs
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

# Helper functions -------------------------------------------------------------

tidy_t_test <- function(summary_data,
                        response_ = c("richness", "turnover"),
                        scale_ = c("QDS", "HDS"),
                        quality_metric_name) {
  summary_data_ <- filter(summary_data,
    model_type == "replicates",
    response == response_,
    scale == scale_
  )
  if (nrow(summary_data_) < 1) {
    out <- data.frame(
      quality_metric = quality_metric_name, response = response_, scale = scale_,
      est_diff = NA, est_GCFR = NA, est_SWAFR = NA,
      statistic = NA, p.value = NA, parameter = NA,
      conf.low = NA, conf.high = NA,
      method = NA, alternative = NA
    )
  } else {
    out <- t.test(
      summary_data_[[quality_metric_name]][
        summary_data_$region == "GCFR"
      ],
      summary_data_[[quality_metric_name]][
        summary_data_$region == "SWAFR"
      ]
    )
    out %<>%
      tidy() %>%
      rename(
        est_diff = estimate,
        est_GCFR = estimate1,
        est_SWAFR = estimate2
      ) %>%
      cbind(
        quality_metric = quality_metric_name,
        response = response_,
        scale = scale_,
        .
      )
  }
  out
}

# Test for differences between Cape and SWA repeated-model quality -------------
# Using t.tests

quality_metric_names <- c(
  "nt",
  "pseudo_r2",
  "pred_obs_r2",
  "pred_obs_r2_exp"
)
responses <- c("richness", "turnover")
scales <- c("QDS", "HDS")
quality_region_data <-
  map_df(quality_metric_names, function(qual_) {
    map_df(scales, function(scale_) {
      map_df(responses, function(response_) {
        tidy_t_test(
          summary_data,
          response_,
          scale_,
          qual_
        )
      })
    })
  })
quality_region_data %<>% filter(
  !(response == "turnover" & scale == "QDS")
)

# Exploratory plot -------------------------------------------------------------
# TODO: move to figure scripts

ggplot(quality_region_data, aes(quality_metric, statistic, col = scale)) +
  geom_point(position = position_dodge(0.1)) +
  geom_hline(yintercept = 0, lty = "dashed", col = "grey50") +
  facet_grid(~ response) +
  labs(x = "Quality statistic", y = "t-value (Cape - SWA)")

# Save to disc for table -------------------------------------------------------

write_csv(
  quality_region_data,
  here("manuscript/quality-region-data.csv")
)
