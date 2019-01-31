# ...
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))
library(callr)

output_path <- here(
  "outputs/species-environment-relationships/from-local-machines/"
)

# Import representative BRT-models ---------------------------------------------

# FIXME:
#source(here(
#  "R/analyses/analyse-species-environment-relationships",
#  "18-pre_copy-representative-BRTs-to-Ubuntu-machine.R"
#))
# (Run manually for now)

representative_models
models <- map(representative_models, read_rds)
names(models) <- c(  # double check against representative_models manually
  "GCFR_QDS_richness",
  "SWAFR_QDS_richness",
  "GCFR_HDS_richness",
  "SWAFR_HDS_richness",
  "SWAFR_HDS_turnover",
  "GCFR_HDS_turnover"
)

# Make plots -------------------------------------------------------------------

# v1
fit_plots <- map(models, function(x) {
  top_vars <- x$contributions %>%
    filter(rel.inf >= 10) %>%
    pull(var)
  fits <- map(top_vars, ~
    plot.gbm(
      x,
      as.character(.),
      return.grid = TRUE
    )
  )
  fits %<>%
    bind_rows() %>%
    arrange(y) %>%
    gather(predictor, predictor_val, -y) %>%
    mutate(predictor = factor(predictor, levels = top_vars))
  ggplot(fits, aes(predictor_val, y)) +
    geom_line() +
    facet_wrap(~predictor, scales = "free_x", nrow = 1) +
    labs(
      x = "Environmental variable",
      y = "Marginal effect on response"
    )
})
n_panels <- function(gg) {
  nlevels(gg$data$predictor)
}
max_n_panels <- fit_plots %>%
  map_int(n_panels)
  max()
fit_plots_w_padding <- map(fit_plots, function(x) {
  n_white_rects_needed <- max_n_panels - n_panels(x)
  plot_grid(
    x, white_rect,
    rel_widths = c(n_panels(x), 0.95 * n_white_rects_needed)
  )
})
plot_grid(plotlist = fit_plots_w_padding, ncol = 1)

# v2
fit_plots <- map(models, function(x) {
  top_vars <- x$contributions %>%
    filter(rel.inf >= 10) %>%
    pull(var)
  fits <- map(top_vars, ~
    plot.gbm(
      x,
      as.character(.),
      return.grid = TRUE
    )
  )
  for (i in seq_along(fits)) {
    fits[[i]][[1]] %<>% scale()
  }
  fits %<>%
    bind_rows() %>%
    arrange(y) %>%
    gather(predictor, predictor_val, -y) %>%
    mutate(predictor = factor(predictor, levels = top_vars))
  ggplot(fits, aes(predictor_val, y, colour = predictor)) +
    geom_line(size = 1.5) +
    labs(
      x = "Environmental variable",
      y = "Marginal effect on response"
    )
})
plot_grid(plotlist = fit_plots, ncol = 3)

# v3
fits <- imap_dfr(models, function(x, y) {
  top_vars <- x$contributions %>%
    filter(rel.inf >= 10) %>%
    pull(var)
  fits <- map(top_vars, ~
    plot.gbm(
      x,
      as.character(.),
      return.grid = TRUE
    )
  )
  for (i in seq_along(fits)) {
    fits[[i]][[1]] %<>% scale()
  }
  fits %<>%
    bind_rows() %>%
    arrange(y) %>%
    gather(predictor, predictor_val, -y) %>%
    mutate(predictor = factor(predictor, levels = top_vars))
  cbind(model = y, fits)
})
var_names2 <- c(
  "Elevation", "rough_Elevation",
  "MAP",       "rough_MAP",
  "PDQ",
               "rough_Surface.T",
  "CEC",       "rough_CEC",
               "rough_Clay",
  "pH"
)
# darken() & lighten() based on this gist:
#   <https://gist.github.com/Jfortin1/72ef064469d1703c6b30>
darken <- function(col, factor = 1.33){
  col %>%
    col2rgb() %>%
    multiply_by(factor) %>%
    apply(1, function(x) if (x > 255) 255 else x) %>%
    as.matrix() %>%
    t() %>%
    rgb(maxColorValue = 255)
}
lighten <- function(col, factor = 1.33) {
  darken(col, 1 / factor)
}
var_colours2 <- c(
  # grey50
  lighten(var_colours[[1]]),      darken(var_colours[[1]]),
  # blue
  lighten(var_colours[[2]], 1.5), lighten(var_colours[[2]]),
  var_colours[[2]],               darken(var_colours[[2]]),
  # brown
  lighten(var_colours[[4]], 1.5), lighten(var_colours[[4]]),
  var_colours[[4]],               darken(var_colours[[4]])
)
model_names <- c(
  "(a) Cape QDS richness", "(b) Cape HDS richness", "(c) Cape HDS turnover",
  "(d) SWA QDS richness",  "(e) SWA HDS richness",  "(f) SWA HDS turnover"
)
fits %<>%
  mutate(model = factor(levels = model_names, case_when(
    model == "GCFR_QDS_richness"  ~ model_names[[1]],
    model == "GCFR_HDS_richness"  ~ model_names[[2]],
    model == "GCFR_HDS_turnover"  ~ model_names[[3]],
    model == "SWAFR_QDS_richness" ~ model_names[[4]],
    model == "SWAFR_HDS_richness" ~ model_names[[5]],
    model == "SWAFR_HDS_turnover" ~ model_names[[6]]
  ))) %>%
  mutate(predictor = factor(predictor, levels = var_names2)) %>%
  mutate(var_type = ifelse(str_detect(predictor, "rough"),
    "rough",
    "absolute"
  ))
ggplot(fits, aes(predictor_val, y)) +
  geom_line(
    aes(colour = predictor, linetype = var_type),
    size = 1
  ) +
  facet_wrap(~model, nrow = 2, scales = "free") +
  labs(
    x = bquote(italic("Z")*"(Environmental value)"),
    y = "Marginal effect on response"
  ) +
  scale_colour_manual(values = var_colours2) +
  scale_linetype_manual(values = c("solid", "1111"), guide = FALSE) +
  guides(col = guide_legend(
    title = "Environmental variables",
    nrow = 6, ncol = 2,
    direction = "vertical",
    override.aes = list(
      linetype = c(
        "solid", "1111",
        "solid", "1111",
        "solid", "1111",
        "solid", "1111",
        "1111", "solid"
      ),
      size = 1
    )
  ))

