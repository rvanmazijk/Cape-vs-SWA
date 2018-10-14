# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 4: Exploring the 4x final BRT-models
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

output_path <- here("outputs/species-environment-relationships/from-UCT-HPC/final-BRTs")

# Import final BRTs ------------------------------------------------------------

models <-
  list(
    cape_richness = "{output_path}/final-BRT_GCFR_richness_BRTs.RDS",
    swa_richness  = "{output_path}/final-BRT_SWAFR_richness_BRTs.RDS",
    cape_turnover = "{output_path}/final-BRT_GCFR_turnover_BRTs.RDS",
    swa_turnover  = "{output_path}/final-BRT_SWAFR_turnover_BRTs.RDS"
  ) %>%
  map(glue) %>%
  map(read_rds)

# Wrangle data -----------------------------------------------------------------

model_quality <- imap_dfr(models, ~ tibble(
  model_name = .y,
  pseudo_r2 = pseudo_r2(.x),
  pred_obs_r2 = ifelse(str_detect(.y, "richness"),
    pred_obs_r2(.x)$pred_obs_m_exp,
    pred_obs_r2(.x)$pred_obs_m
  )
))

model_contributions <-
  map_dfr(models, .id = "model_name", "contributions") %>%
  as_tibble() %>%
  mutate(
    region = model_name %>%
      str_split("_") %>%
      map(1) %$%
      case_when(
        . == "cape" ~ "Cape",
        . == "swa" ~ "SWA"
      ),
    response = model_name %>%
      str_split("_") %>%
      map(2) %>%
      as_vector()
  )
zero_contrib_vars <- map_dfr(
  .x = list(
    list(region = "Cape", response = "richness"),
    list(region = "Cape", response = "turnover"),
    list(region = "SWA", response = "richness"),
    list(region = "SWA", response = "turnover")
  ), ~
    tibble(
      region = .x$region,
      response = .x$response,
      model_name = tolower(glue("{.x$region}_{.x$response}")),
      var = get_zero_contrib_vars(.x$region, .x$response),
      rel.inf = 0
    )
)
model_contributions %<>% full_join(zero_contrib_vars)
any(model_contributions$rel.inf == 0)
model_contributions %<>% mutate(
  var_type = ifelse(str_detect(var, "rough_"),
    "Rough",
    "Absolute"
  ),
  var_class =
    case_when(
      str_detect(var, "Elevation")              ~ "Elevation",
      str_detect(var, "(MAP|PDQ|Surface\\.T)")  ~ "Climate",
      str_detect(var, "NDVI")                   ~ "NDVI",
      str_detect(var, "(CEC|Clay|Soil\\.C|pH)") ~ "Soil"
    ) %>%
    factor(levels = c("Elevation", "Climate", "NDVI", "Soil")),
  var = var %>%
    str_replace_all("(_|\\.)", " ") %>%
    str_replace("rough", "Rough") %>%
    factor()
)

# Define screeplot dimensions (for later)
screeplot_width <- length(unique(model_contributions$var)) + 2
screeplot_height <- 62

# Plot screeplots of variable class contributions ------------------------------

screeplots <- foreach(model_name_ = names(models)) %do% {
  # Make the actual screeplot
  screeplot_ <- model_contributions %>%
    filter(model_name == model_name_) %>%
    mutate(var = reorder(var, desc(rel.inf))) %>%
    ggplot(aes(var, rel.inf, fill = var_class), drop = FALSE) +
      geom_col() +
      ylim(0, 60) +
      labs(
        x = "Environmental variable",
        y = "Relative influence (%)"
      ) +
      scale_fill_manual(values = var_colours) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank(),
        legend.position = ifelse(str_detect(model_name_, "cape_richness"),
          "left",
          "none"
        )
      )

  # Add annotations + statistics
  panel_number <- annotate("text",
  x = 1, y = 60, hjust = 0,
  label = case_when(
    model_name_ == "cape_richness" ~ "(a) Cape richness",
    model_name_ == "swa_richness"  ~ "(b) SWA richness",
    model_name_ == "cape_turnover" ~ "(c) Cape turnover",
    model_name_ == "swa_turnover"  ~ "(d) SWA turnover"
  )
  )
  panel_pseudo_r2 <- model_quality %>%
    filter(model_name == model_name_) %>%
    select(pseudo_r2) %>%
    as_vector() %>%
    round(digits = 2) %>%
    format(nsmall = 2) %>%
    as.character() %>%
    glue("italic(R)[Ps]^2 == {.}") %>%
    annotate("text",
      x = 0.9 * screeplot_width,
      y = 0.5 * screeplot_height,
      hjust = 1, size = 3,
      label = .,
      parse = TRUE
    )
  panel_pred_obs_r2 <- model_quality %>%
    filter(model_name == model_name_) %>%
    select(pred_obs_r2) %>%
    as_vector() %>%
    round(digits = 2) %>%
    format(nsmall = 2) %>%
    as.character() %>%
    glue("italic(R)[PO]^2 == {.}")
    annotate("text",
      x = 0.9 * screeplot_width,
      y = 0.4 * screeplot_height,
      hjust = 1, size = 3,
      label = .,
      parse = TRUE
    )
  screeplot_ <- screeplot_ +
    panel_number +
    panel_pseudo_r2 +
    panel_pred_obs_r2

  # Remove x-axis title if making a panel on top row
  if (str_detect(model_name_, "richness")) {
    screeplot_ <- screeplot_ + theme(axis.title.x = element_blank())
  }

  # Remove y-axis title + numbers if making a panel on right
  if (str_detect(model_name_, "swa")) {
    screeplot_ <- screeplot_ + theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )
  }

  screeplot_
}
var_class_legend <- get_legend(screeplots[[1]])
screeplots[[1]] <- screeplots[[1]] + theme(legend.position = "none")

# Plot piecharts of roughness vs absolute contributions ------------------------
# (variable _type_)

transparent <- element_rect(colour = "transparent", fill = "transparent")

piecharts <- foreach(model_name_ = names(models)) %do% {
  model_contributions %>%
    filter(model_name == model_name_) %>%
    mutate(var = reorder(var_type, desc(rel.inf))) %>%
    ggplot(aes("", rel.inf, fill = var_type)) +
      geom_col(col = "black", size = 0.25) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("white", "grey75")) +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = transparent,
        plot.background =  transparent,
        legend.title = element_blank(),
        legend.position = ifelse(str_detect(model_name_, "cape_richness"),
          "left",
          "none"
        )
      )
}

var_type_legend <- get_legend(piecharts[[1]])
piecharts[[1]] <- piecharts[[1]] + theme(legend.position = "none")

# Inset the piecharts in the screeplots ----------------------------------------

screepieplots <- foreach(screeplot_ = screeplots, piechart_ = piecharts) %do% {
  screeplot_ + annotation_custom(
    ggplotGrob(piechart_),
    xmin = 0.5 * screeplot_width,  xmax = screeplot_width,
    ymin = 0.5 * screeplot_height, ymax = screeplot_height
  )
}
screepieplots <- plot_grid(
  plotlist = screepieplots,
  nrow = 2, ncol = 2,
  rel_heights = c(0.95, 1), rel_widths = c(1, 0.9)
)

# Add legends x2 ---------------------------------------------------------------

var_legend <- plot_grid(
  var_type_legend, var_class_legend, white_rect,
  nrow = 3,
  rel_heights = c(1, 1, 0.25)
)
screepieplots <- plot_grid(
  screepieplots, var_legend,
  nrow = 1,
  rel_widths = c(1, 0.2)
)

# Save to disc -----------------------------------------------------------------

ggsave(
  here("figures/fig-3-species-environment-relationships.png"),
  screepieplots,
  width = 6, height = 6,
  dpi = 300
)
