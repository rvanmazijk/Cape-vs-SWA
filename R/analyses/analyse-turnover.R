# Comparing species turnover between the Cape and SWA
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))
source(here("R/analyses/generate-turnover.R"))

output_path <- here("outputs/turnover")

# Collate richness and turnover data -------------------------------------------

vars_to_keep <- c(
  "hdgc",
  "HDS_richness",
  "n_QDS",
  "mean_QDS_richness",
  "mean_QDS_jaccard"
)
names(GCFR_species)[4:8] <- vars_to_keep
names(SWAFR_species)[4:8] <- vars_to_keep

GCFR_species_data <- GCFR_species@data %>%
  select(vars_to_keep) %>%
  filter(n_QDS > 1) %>%
  distinct()
SWAFR_species_data <- SWAFR_species@data %>%
  select(vars_to_keep) %>%
  filter(n_QDS > 1) %>%
  distinct()

richness_turnover_data <-
  rbind(
    cbind(region = "Cape", GCFR_species_data),
    cbind(region = "SWA", SWAFR_species_data)
  ) %>%
  as_tibble() %>%
  filter(n_QDS > 1) %>%  # turnover is non-sensicle for 1 QDS)
  mutate(
    avg_no_spp_diff = mean_QDS_jaccard * HDS_richness,
    add_residual_turnover = HDS_richness - mean_QDS_richness,
    add_residual_turnover_prop = add_residual_turnover / HDS_richness,
    mul_residual_turnover = HDS_richness / mean_QDS_richness
  )

# Save data frame to disc for Figure 2
write_csv(
  richness_turnover_data,
  glue("{output_path}/richness_turnover_data.csv")
)

# Compare turnover between GCFR and SWAFR --------------------------------------

# Example with mean Jaccard distances
# For any no. QDS >= 2
jacc <- richness_turnover_data %$% list(
  Cape = mean_QDS_jaccard[region == "Cape"],
  SWA = mean_QDS_jaccard[region == "SWA"]
)
map(jacc, shapiro.test)  # Not ~ N
map(jacc, function(x) shapiro.test(log(x))) # Not log ~ N either
var.test(jacc$Cape, jacc$SWA)  # Variances approx. equal
# Therefore, use Mann-Whitney U-tests and CLES
rm(jacc)

# Compare for real
turnover_results <- richness_turnover_data %>%
  # Doing for any no. QDS >= 2
  # But what about when no. QDS = 4 only?
  #filter(n_QDS == 4) %>%
  select(region, mean_QDS_jaccard, add_residual_turnover_prop) %$%
  list(
    mean_QDS_jaccard = list(
      Cape = mean_QDS_jaccard[region == "Cape"],
      SWA = mean_QDS_jaccard[region == "SWA"]
    ),
    add_residual_turnover_prop = list(
      Cape = add_residual_turnover_prop[region == "Cape"],
      SWA = add_residual_turnover_prop[region == "SWA"]
    )
  ) %>%
  map_df(.id = "test", function(.x) {
    U_p_value <- tidy(wilcox.test(.x$Cape, .x$SWA, alternative = "two.sided"))$p.value
    CLES_value <- CLES(.x$SWA, .x$Cape)  # Order of x and y NB
    message("Done")
    tibble(U_p_value, CLES_value)
  })

# Save results to disc
write_csv(
  turnover_results,
  glue("{output_path}/turnover_results.csv")
)
