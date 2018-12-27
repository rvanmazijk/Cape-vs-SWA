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

avg_rank <- function(x, y) {
  x_ranks <- vector(length = length(x))
  for (i in seq_along(x)) {
    ranked_x_y <- percent_rank(c(x[i], y))
    x_ranks[i] <- ranked_x_y[1]
  }
  mean(x_ranks)
}

test_quality_rank <- function(summary_data_, quality_metric_name) {
  reps <- summary_data_[[quality_metric_name]][
    summary_data_$model_type == "replicates"
  ]
  perms <- summary_data_[[quality_metric_name]][
    summary_data_$model_type == "permutations"
  ]
  out <- data.frame(avg_rank(reps, perms))
  colnames(out) <- glue("{quality_metric_name}_rank")
  out
}

test_perms_reps <- function(summary_data,
                            response_ = c("richness", "turnover"),
                            region_ = c("GCFR", "SWAFR"),
                            scale_ = c("QDS", "HDS"),
                            plot = TRUE) {
  summary_data_ <- filter(summary_data,
    response == response_,
    region == region_,
    scale == scale_
  )
  if (plot) {
    summary_data_ %$% {
      hist(nt[model_type == "replicates"])
      hist(nt[model_type == "permutations"])
    }
  }
  quality_metric_names <- c(
    "nt",
    "pseudo_r2",
    "pred_obs_r2",
    "pred_obs_r2_exp"
  )
  quality_metric_names %>%
    map(~ test_quality_rank(summary_data_, .x)) %>%
    bind_cols() %>%
    cbind(
      response = response_,
      region = region_,
      scale = scale_,
      .
    )
}

# Test for differences between permuted and repeated model quality -------------
# Using average percent-rank of each repeat model relative to
#   all of the permuted models

responses <- c("richness", "turnover")
regions <- c("GCFR", "SWAFR")
scales <- c("QDS", "HDS")
quality_rank_data <-
  map_df(scales, function(scale_) {
    map_df(regions, function(region_) {
      map_df(responses, function(response_) {
        test_perms_reps(
          summary_data,
          response_,
          region_,
          scale_,
          plot = FALSE
        )
      })
    })
  })
quality_rank_data %<>% filter(
  !(response == "turnover" & scale == "QDS")
)

quality_rank_data
# All significant i.m.o. :)

# Save to disc for table -------------------------------------------------------

write_csv(
  quality_rank_data,
  here("manuscript/quality-rank-data.csv")
)
