# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 4: Explore BRT models' predicted vs observed richness
# Cape vs SWA publication
# Ruan van Mazijk

GCFR_pred_vs_obs_data <-
  tibble(
    pred = GCFR_gbm_step_simp$fitted,
    obs = GCFR_gbm_step_simp$data$y
  ) %>%
  mutate(
    exp_pred = exp(pred),
    exp_obs = exp(obs)
  )
SWAFR_pred_vs_obs_data <-
  tibble(
    pred = SWAFR_gbm_step_simp$fitted,
    obs = SWAFR_gbm_step_simp$data$y
  ) %>%
  mutate(
    exp_pred = exp(pred),
    exp_obs = exp(obs)
  )

GCFR_pred_vs_obs_m <- lm(pred ~ obs, data = GCFR_pred_vs_obs_data)
GCFR_pred_vs_obs_m_exp <- lm(exp_pred ~ exp_obs, data = GCFR_pred_vs_obs_data)
SWAFR_pred_vs_obs_m <- lm(pred ~ obs, data = SWAFR_pred_vs_obs_data)
SWAFR_pred_vs_obs_m_exp <- lm(exp_pred ~ exp_obs, data = SWAFR_pred_vs_obs_data)

tidy(GCFR_pred_vs_obs_m)
tidy(GCFR_pred_vs_obs_m_exp)
tidy(SWAFR_pred_vs_obs_m)
tidy(SWAFR_pred_vs_obs_m_exp)

glance(GCFR_pred_vs_obs_m)
glance(GCFR_pred_vs_obs_m_exp)
glance(SWAFR_pred_vs_obs_m)
glance(SWAFR_pred_vs_obs_m_exp)

visreg::visreg(GCFR_pred_vs_obs_m, gg = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")
visreg::visreg(SWAFR_pred_vs_obs_m, gg = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")

visreg::visreg(GCFR_pred_vs_obs_m_exp, gg = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")
visreg::visreg(SWAFR_pred_vs_obs_m_exp, gg = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")
