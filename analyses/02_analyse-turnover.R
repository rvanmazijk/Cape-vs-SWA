# Assessing problems in species occurence dataset
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
#source(here::here("data/01_import-region-polygons.R"))
source(here::here("data/02_import-floral-data.R"))

# Compare turnover between GCFR and SWAFR --------------------------------------

# .... Mean Jaccard distance between QDS in HDS --------------------------------

# For any no. QDS >= 2
richness_turnover_data %$% compare_samples(
  mean_QDS_jaccard[region == "Cape"],
  mean_QDS_jaccard[region == "SWA"],
  alternative = "two.sided"
)
richness_turnover_data %$%
  hist(mean_QDS_jaccard[region == "Cape"], breaks = 30)
richness_turnover_data %$%
  hist(mean_QDS_jaccard[region == "SWA"], breaks = 30)

# For when no. QDS = 4 only
richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  compare_samples(
    mean_QDS_jaccard[region == "Cape"],
    mean_QDS_jaccard[region == "SWA"],
    alternative = "two.sided"
  )
richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  hist(mean_QDS_jaccard[region == "Cape"], breaks = 30)
richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  hist(mean_QDS_jaccard[region == "SWA"], breaks = 30)

# .... Additively defined residual turnover in HDS -----------------------------

# For any no. QDS >= 2
richness_turnover_data %$%
  compare_samples(
    add_residual_turnover[region == "Cape"],
    add_residual_turnover[region == "SWA"],
    alternative = "two.sided"
  )
richness_turnover_data %$%
  hist(add_residual_turnover[region == "Cape"], breaks = 30)
richness_turnover_data %$%
  hist(add_residual_turnover[region == "SWA"], breaks = 30)

# For when no. QDS = 4 only
richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  compare_samples(
    add_residual_turnover[region == "Cape"],
    add_residual_turnover[region == "SWA"],
    alternative = "two.sided"
  )
richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  hist(add_residual_turnover[region == "Cape"], breaks = 30)
richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  hist(add_residual_turnover[region == "SWA"], breaks = 30)

# .... Additively defined residual turnover in HDS (proportion) ----------------

# For any no. QDS >= 2
richness_turnover_data %$%
  compare_samples(
    add_residual_turnover_prop[region == "Cape"],
    add_residual_turnover_prop[region == "SWA"],
    alternative = "two.sided"
  )
richness_turnover_data %$%
  hist(add_residual_turnover_prop[region == "Cape"], breaks = 30)
richness_turnover_data %$%
  hist(add_residual_turnover_prop[region == "SWA"], breaks = 30)

# For when no. QDS = 4 only
richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  compare_samples(
    add_residual_turnover_prop[region == "Cape"],
    add_residual_turnover_prop[region == "SWA"],
    alternative = "two.sided"
  )
richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  hist(add_residual_turnover_prop[region == "Cape"], breaks = 30)
richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  hist(add_residual_turnover_prop[region == "SWA"], breaks = 30)
