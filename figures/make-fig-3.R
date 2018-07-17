# Make Fig. 2 (Relating taxonomic richness, turnover and environment)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("figures/figure-setup.R"))

# ... --------------------------------------------------------------------------

if (FALSE) {
  ggplot(taxa_enviro_roughness_HDS[, c("region",
                                       "richness",
                                       "rank",
                                       "roughness_Elevation")],
         aes(roughness_Elevation, richness,
         col = region)) +
  geom_point() +
  facet_wrap(~ rank)
}
