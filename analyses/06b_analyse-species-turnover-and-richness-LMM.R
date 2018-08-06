# Analyse floral species turnover with richness WITH LINEAR MIXED EFFECTS MODELS
# (Richness (gamma) ~ mean QDS richness (alpha) * mean QDS turnover (beta))
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
p_load(lme4)
import_all_objects_auto(here::here("analyses/06_outputs"))

species_turnover_richness_HDS_LMM1 <- lmer(
  richness ~
    1 + (1 | region),
  na.exclude(gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "species", ])
)
species_turnover_richness_HDS_LMM2 <- lm(
  richness ~
    log(avg_QDS_richness + 1) + avg_QDS_turnover,
  na.exclude(gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "species", ])
)
species_turnover_richness_HDS_LMM3 <- lmer(
  richness ~
    log(avg_QDS_richness + 1) + avg_QDS_turnover +
    (1 | region),
  na.exclude(gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "species", ])
)
species_turnover_richness_HDS_LMM4 <- lmer(
  richness ~
    log(avg_QDS_richness + 1) + avg_QDS_turnover +
    (log(avg_QDS_richness + 1) | region) +
    (avg_QDS_turnover | region),
  na.exclude(gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "species", ])
)

anova(
  test = "Chisq",
  species_turnover_richness_HDS_LMM1,
  species_turnover_richness_HDS_LMM2,
  species_turnover_richness_HDS_LMM3,
  species_turnover_richness_HDS_LMM4
)
