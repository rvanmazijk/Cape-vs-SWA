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

choose_label_pos <- function(x, prop) {
  stopifnot(exprs = {
    is.numeric(x)
    is.numeric(prop)
  })
  print(glue(
    "Variable extends to {max(x)} \
    Thus choosing co-ord = {min(x) + (prop * (max(x) - min(x)))}"
  ))
  min(x) + (prop * (max(x) - min(x)))
}

get_r2_label <- function(x) {
  stopifnot(class(x) == "lm")
  x %>%
    glance() %>%
    select(r.squared) %>%
    #as_vector() %>%
    round(digits = 4) %>%
    format(nsmall = 2) %>%
    paste0("italic(R)^2 ==", .)
}

get_slope_label <- function(x) {
  stopifnot(class(x) == "lm")
  x %>%
    tidy() %>%
    select(estimate) %>%
    slice(2) %>%
    round(digits = 4) %>%
    format(nsmall = 2) %>%
    paste0("italic(beta)[1] ==", .)
}

plot_pred_obs <- function(x) {
  stopifnot(class(x) == "lm")
  visreg(x, gg = TRUE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    annotate("text",
      x = choose_label_pos(x$model[[2]], 0.15),
      y = choose_label_pos(x$model[[1]], 0.85),
      label = get_r2_label(x),
      parse = TRUE
    ) +
    annotate("text",
      x = choose_label_pos(x$model[[2]], 0.15),
      y = choose_label_pos(x$model[[1]], 0.95),
      label = get_slope_label(x),
      parse = TRUE
    )
}

# For each response, for each region, for both the log and linear models:
BRT_pred_vs_obs_plots <- map(BRT_pred_vs_obs_models, map, map, plot_pred_obs)

