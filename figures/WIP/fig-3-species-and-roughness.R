# Make Fig. 3 (Relating species richness (and turnover?) and environment)
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("figures/figure-setup.R"))

all_GWR_models_outputs %<>% mutate(region = case_when(
  region == "GCFR"  ~ "Cape",
  region == "SWAFR" ~ "SWA",
  TRUE              ~ region
))

model_specs <- c(
  "null",
  "abs",
  "rough",
  "elev",
  "non_elev",
  "soil",
  "non_soil",
  "full"
)

all_GWR_models_outputs %>%
  filter(model %in% model_specs[c(2, 3, 8)]) %>%
  select(
    model_region, model, region,
    Elevation:rough_pH
  ) %>%
  gather(
    term, est,
    -model_region, -model, -region
  ) %>%
  ggplot(aes(est, col = region, fill = region)) +
    geom_histogram(alpha = 0.5) +
    scale_colour_manual(values = my_palette) +
    scale_fill_manual(values = my_palette) +
    facet_wrap(model ~ term, ncol = 3, dir = "v", scales = "free")

# Compare coefficients of absolute variables between regions
full_coeff %>%
  select(region, Elevation:pH) %>%
  gather(term, est, -region) %>%
  ggplot(aes(est, col = region, fill = region)) +
    geom_histogram(alpha = 0.5) +
    scale_colour_manual(values = my_palette) +
    scale_fill_manual(values = my_palette) +
    facet_wrap(~ term, scales = "free")

# Compare coefficients of roughness variables between regions
full_coeff %>%
  select(region, rough_Elevation:rough_pH) %>%
  gather(term, est, -region) %>%
  ggplot(aes(est, col = region, fill = region)) +
    geom_histogram(alpha = 0.5) +
    scale_colour_manual(values = my_palette) +
    scale_fill_manual(values = my_palette) +
    facet_wrap(~ term, scales = "free")

# Compare coefficients of spatial position between regions
full_coeff %>%
  select(region, x:y) %>%
  gather(term, est, -region) %>%
  ggplot(aes(est, col = region, fill = region)) +
    geom_histogram(alpha = 0.5) +
    scale_colour_manual(values = my_palette) +
    scale_fill_manual(values = my_palette) +
    facet_wrap(~ term, scales = "free")

# Compare observed and predicted log-richness
# Log-richness
ggplot(full_coeff, aes(obs_log_richness, pred)) +
  geom_point(col = "grey50", alpha = 0.5) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", col = "black"
  ) +
  lims(
    x = c(0, 8.5),
    y = c(0, 8.5)
  ) +
  labs(
    x = "Observed QDS richness (log)",
    y = "Predicted QDS richness (log)"
  )
log_obs_pred_model <- lm(pred ~ obs_log_richness, full_coeff)
glance(log_obs_pred_model)$r.squared

# Linear richness
ggplot(full_coeff, aes(obs_richness, pred_richness)) +
  geom_point(col = "grey50", alpha = 0.5) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", col = "black"
  ) +
  lims(
    x = c(0, 4500),
    y = c(0, 4500)
  ) +
  labs(
    x = "Observed QDS richness",
    y = "Predicted QDS richness"
  )
obs_pred_model <- lm(pred_richness ~ obs_richness, full_coeff)
glance(obs_pred_model)$r.squared

# Log-richness separated by region (from combined model)
ggplot(full_coeff, aes(obs_log_richness, pred, col = region)) +
  geom_point(alpha = 0.5) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", col = "black"
  ) +
  lims(
    x = c(0, 8.5),
    y = c(0, 8.5)
  ) +
  labs(
    x = "Observed QDS richness",
    y = "Predicted QDS richness"
  ) +
  facet_grid(~ region) +
  scale_colour_manual(name = "Region", values = my_palette) +
  theme(strip.text = element_blank())
log_obs_pred_GCFR_model <- full_coeff %>%
  filter(region == "Cape") %>%
  lm(pred ~ obs_log_richness, .)
log_obs_pred_SWAFR_model <- full_coeff %>%
  filter(region == "SWA") %>%
  lm(pred ~ obs_log_richness, .)
glance(log_obs_pred_GCFR_model)$r.squared
glance(log_obs_pred_SWAFR_model)$r.squared

# Log-richness separated by region (from separate models)
full_sep_coeff <- rbind(
  cbind(region = "GCFR", GCFR_models$full$SDF@data),
  cbind(region = "SWAFR", SWAFR_models$full$SDF@data)
)
full_sep_coeff %<>% cbind(obs_log_richness = c(
  GCFR_all_QDS_pts@data$richness,
  SWAFR_all_QDS_pts@data$richness
))
names(full_sep_coeff)[[70]] <- "pred.se2"
full_sep_coeff %<>%
  as_tibble() %>%
  mutate(
    region = case_when(
      region == "GCFR" ~ "Cape",
      region == "SWAFR" ~ "SWA"
    ),
    obs_richness = exp(obs_log_richness),
    pred_richness = exp(pred)
  )
ggplot(full_sep_coeff, aes(obs_log_richness, pred, col = region)) +
  geom_point(alpha = 0.5) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", col = "black"
  ) +
  lims(
    x = c(0, 8.5),
    y = c(0, 8.5)
  ) +
  labs(
    x = "Observed QDS richness",
    y = "Predicted QDS richness"
  ) +
  facet_grid(~ region) +
  scale_colour_manual(name = "Region", values = my_palette) +
  theme(strip.text = element_blank())
log_obs_pred_sep_GCFR_model <- full_sep_coeff %>%
  filter(region == "Cape") %>%
  lm(pred ~ obs_log_richness, .)
log_obs_pred_sep_SWAFR_model <- full_sep_coeff %>%
  filter(region == "SWA") %>%
  lm(pred ~ obs_log_richness, .)
glance(log_obs_pred_sep_GCFR_model)$r.squared
glance(log_obs_pred_sep_SWAFR_model)$r.squared

# TODO: make these sort of graphical comparison between
#   the separate models' estimates

# TODO: what do all the *other* columns in the GWR output mean?
#   --> DO SOME READING

#### WIP

# TODO: interpretation of this?
map(GCFR_models, anova)
map(SWAFR_models, anova)
map(combined_models, anova)

#### WIP
