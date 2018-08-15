# Comparing species turnover between the Cape and SWA
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("data/02_import-floral-data.R"))

# Compare turnover between GCFR and SWAFR --------------------------------------

# .... Mean Jaccard distance between QDS in HDS --------------------------------

# For any no. QDS >= 2
richness_turnover_data %$% compare_samples(
  mean_QDS_jaccard[region == "Cape"],
  mean_QDS_jaccard[region == "SWA"],
  alternative = "two.sided"
)

# For when no. QDS = 4 only
richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  compare_samples(
    mean_QDS_jaccard[region == "Cape"],
    mean_QDS_jaccard[region == "SWA"],
    alternative = "two.sided"
  )

# .... Additively defined residual turnover in HDS -----------------------------

# For any no. QDS >= 2
richness_turnover_data %$%
  compare_samples(
    add_residual_turnover[region == "Cape"],
    add_residual_turnover[region == "SWA"],
    alternative = "two.sided"
  )

# For when no. QDS = 4 only
richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  compare_samples(
    add_residual_turnover[region == "Cape"],
    add_residual_turnover[region == "SWA"],
    alternative = "two.sided"
  )

# .... Additively defined residual turnover in HDS (proportion) ----------------

# For any no. QDS >= 2
richness_turnover_data %$%
  compare_samples(
    add_residual_turnover_prop[region == "Cape"],
    add_residual_turnover_prop[region == "SWA"],
    alternative = "two.sided"
  )

# For when no. QDS = 4 only
richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  compare_samples(
    add_residual_turnover_prop[region == "Cape"],
    add_residual_turnover_prop[region == "SWA"],
    alternative = "two.sided"
  )
