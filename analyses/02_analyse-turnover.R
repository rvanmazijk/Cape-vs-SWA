# Assessing problems in species occurence dataset
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
#source(here::here("data/01_import-region-polygons.R"))
#source(here::here("data/02_import-floral-data.R"))

# Compile data -----------------------------------------------------------------

GCFR_spp <- readOGR(here::here(
  "outputs/species-turnover-and-richness/GCFR_spp_2018-08-14"
))
SWAFR_spp <- readOGR(here::here(
  "outputs/species-turnover-and-richness/SWAFR_spp_2018-08-14"
))

vars_to_keep <- c(
  "HDS_richness",
  "n_QDS",
  "mean_QDS_richness",
  "mean_QDS_jaccard"
)
names(GCFR_spp)[6:9] <- vars_to_keep
names(SWAFR_spp)[6:9] <- vars_to_keep

GCFR_spp_data <- GCFR_spp@data %>%
  select(vars_to_keep) %>%
  distinct()
SWAFR_spp_data <- SWAFR_spp@data %>%
  select(vars_to_keep) %>%
  distinct()

richness_turnover_data <-
  rbind(
    cbind(region = "Cape", GCFR_spp_data),
    cbind(region = "SWA", SWAFR_spp_data)
  ) %>%
  as_tibble() %>%
  filter(n_QDS > 1) %>%  # turnover is non-sensicle for 1 QDS)
  mutate(
    add_residual_turnover = HDS_richness - mean_QDS_richness,
    add_residual_turnover_prop = add_residual_turnover / HDS_richness,
    mul_residual_turnover = HDS_richness / mean_QDS_richness
  )

# Compare data between GCFR and SWAFR ------------------------------------------

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
