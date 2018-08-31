# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 4: Explore BRT models' predicted vs observed
# Cape vs SWA publication
# Ruan van Mazijk

library(visreg)

# Extract predicted and observed richness and turnover from BRTs ---------------

BRT_pred_vs_obs_models <- map(
  # For each response:
  .x = gbm_steps_simp,
  .f = ~ map(
    # For each region:
    .x = .x,
    .f = function(.x) {
      pred_obs_data <- .x %$%
        tibble(
          pred = fitted,
          obs = data$y
        ) %>%
        mutate(
          exp_pred = exp(pred),
          exp_obs = exp(obs)
        )
      pred_obs_m <- lm(pred ~ obs, data = pred_obs_data)
      pred_obs_m_exp <- lm(exp_pred ~ exp_obs, data = pred_obs_data)
      list(
        pred_obs_m = pred_obs_m,
        pred_obs_m_exp = pred_obs_m_exp
      )
    }
  )
)

# Get all R^2 values -----------------------------------------------------------

BRT_pred_vs_obs_r2 <- map(
  # For each response:
  .x = BRT_pred_vs_obs_models,
  .f = ~ map(
    # For each region:
    .x = .x,
    .f = ~ map(
      # For the log and linear models:
      .x = .x,
      .f = ~ glance(.x) %>%
        select(r.squared)
    )
  )
)

# Generate visreg ggplots ------------------------------------------------------

BRT_pred_vs_obs_plots <- map(
  # For each response:
  .x = BRT_pred_vs_obs_models,
  .f = ~ map(
    # For each region:
    .x = .x,
    .f = ~ map(
      # For the log and linear models:
      .x = .x,
      .f = function(.x) {
        visreg(.x, gg = TRUE) +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed")# +
          annotate(
            "text",
            x = 0.25 * max(.x$model[[2]]),
            y = 0.75 * max(.x$model[[1]]),
            label = .x %>%
              glance() %>%
              select(r.squared)
          )
      }
    )
  )
)

