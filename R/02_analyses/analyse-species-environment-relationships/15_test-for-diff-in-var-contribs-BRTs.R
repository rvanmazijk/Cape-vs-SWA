# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

output_path <- here("outputs/species-environment-relationships/from-local-machines")

# Import BRT contribution data -------------------------------------------------

contribution_data <- map_df(
  c(
    "QDS-richness-models-contributions.csv",
    "HDS-richness-models-contributions.csv",
    "HDS-turnover-models-contributions.csv"
  ),
  ~ cbind(path = .x, read_csv(glue("{output_path}/{.x}")))
)

# Compute F-statistics for differences in contribs between vars ----------------
# I want the replicates to show signal, and the permutations should not
#   (as they are a pseudo-random null).

contribution_F_tests <- contribution_data %>%
  as_tibble() %>%
  select(-path, -rep) %>%
  select(scale, response, region, model_type, var, rel.inf) %>%
  group_by(scale, response, region, model_type) %>%
  summarise(
    F_test = list(tidy(aov(rel.inf ~ var))),
    n_vars = length(unique(var))
  ) %>%
  mutate(
    F_value = map_dbl(F_test, ~ .x$statistic[1]),
    P_value = map_dbl(F_test, ~ .x$p.value[1]),
    P_Bonferonni = P_value * n_vars
  ) %>%
  select(-F_test)

# Exploratory plot -------------------------------------------------------------
# TODO: move to figure scripts

ggplot(contribution_F_tests,
  aes(model_type, F_value / 1e6, col = paste(scale, response))
) +
  geom_point(position = position_dodge(0.25)) +
  labs(
    x = "",
    y = expression(paste(
      italic("F"), "-value", " (x", 10^6, ")"
    ))
  ) +
  facet_grid(~ region) +
  scale_colour_manual(name = "Scale & response", values = 1:3)
