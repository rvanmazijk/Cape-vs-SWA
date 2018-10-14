# Make Fig. 3 (Relating species richness (and turnover) and environment)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

# Import BRT summary statistics ------------------------------------------------

summary_output_path <- here(
  "outputs/species-environment-relationships/",
  "from-local-machines"
)

model_quality <- read_csv(glue(
  "{summary_output_path}/model_quality.csv"
))

model_contributions <- read_csv(glue(
  "{summary_output_path}/model_contributions.csv"
))
model_contributions$var_class %<>% factor(levels = c(
  "Elevation",
  "Climate",
  "NDVI",
  "Soil"
))

# Plot screeplots of variable class contributions ------------------------------

# Define screeplot dimensions (for later)
screeplot_width <- length(unique(model_contributions$var)) + 2
screeplot_height <- 62

screeplots <- foreach(model_name_ = unique(model_contributions$model_name)) %do% {
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
    x = 1, y = 0.95 * screeplot_height, hjust = 0,
    label = case_when(
      model_name_ == "cape_richness" ~ "(a) Cape richness",
      model_name_ == "swa_richness"  ~ "(b) SWA richness",
      model_name_ == "cape_turnover" ~ "(c) Cape turnover",
      model_name_ == "swa_turnover"  ~ "(d) SWA turnover"
    )
  )
  panel_pseudo_r2 <- annotate("text",
    x = 0.85 * screeplot_width,
    y = 0.5 * screeplot_height,
    hjust = 1, size = 3,
    label = model_quality %>%
      quality_label(model_name_, "pseudo_r2") %>%
      paste("italic(R)[pseudo]^2 ==", "'", ., "'"),  # "'" to get trailing zeroes to render
    parse = TRUE
  )
  panel_pred_obs_r2 <- annotate("text",
    x = 0.85 * screeplot_width,
    y = 0.4 * screeplot_height,
    hjust = 1, size = 3,
    label = model_quality %>%
      quality_label(model_name_, "pred_obs_r2") %>%
      paste("italic(R)[O-E]^2 ==", "'", ., "'"),
    parse = TRUE
  )
  panel_nt <- annotate("text",
    x = 0.85 * screeplot_width,
    y = 0.3 * screeplot_height,
    hjust = 1, size = 3,
    label = model_quality %>%
      quality_label(model_name_, "nt") %>%
      paste("nt ==", "'", ., "'"),  # "'" to get trailing zeroes to render
    parse = TRUE
  )
  screeplot_ <- screeplot_ +
    panel_number +
    panel_pseudo_r2 +
    panel_pred_obs_r2 +
    panel_nt

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

piecharts <- foreach(model_name_ = unique(model_contributions$model_name)) %do% {
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
