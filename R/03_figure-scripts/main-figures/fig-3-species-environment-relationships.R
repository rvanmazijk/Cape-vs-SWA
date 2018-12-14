# Make Fig. 3 (Relating species richness (and turnover) and environment---
#   Plot screeplots of variable class contributions)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

output_path <- here(
  "outputs/species-environment-relationships/",
  "from-local-machines"
)

var_names %<>% str_replace_all(" ", ".")
rough_var_names <- glue("rough_{var_names}")

# Import BRT contribution data -------------------------------------------------

contribution_data <- map_df(
  c(
    "QDS-richness-models-contributions.csv",
    "HDS-richness-models-contributions.csv",
    "HDS-turnover-models-contributions.csv"
  ),
  ~ cbind(path = .x, read_csv(glue("{output_path}/{.x}")))
)

# Prepare data for plots -------------------------------------------------------

contribution_data %<>%
  as_tibble() %>%
  mutate(
    region = case_when(
      region == "GCFR" ~ "Cape",
      region == "SWAFR" ~ "SWA"
    ),
    response = str_extract(path, "(richness|turnover)"),
    scale = str_extract(path, "(QDS|HDS)")
  ) %>%
  mutate(model_name = paste(region, response, scale)) %>%
  select(-path, -rep)

zero_contrib_vars <- map_dfr(
  list(
    list(region = "Cape", response = "richness", scale = "QDS"),
    list(region = "Cape", response = "richness", scale = "HDS"),
    list(region = "Cape", response = "turnover", scale = "HDS"),
    list(region = "SWA",  response = "richness", scale = "QDS"),
    list(region = "SWA",  response = "richness", scale = "HDS"),
    list(region = "SWA",  response = "turnover", scale = "HDS")
  ),
  ~ tibble(
    region = .x$region,
    response = .x$response,
    scale = .x$scale,
    model_name = paste(.x$region, .x$response, .x$scale),
    var = get_zero_contrib_vars(.x$region, .x$response, .x$scale),
    rel.inf = 0
  )
)
contribution_data %<>% full_join(zero_contrib_vars)
any(contribution_data$rel.inf == 0)
contribution_data %>% filter(rel.inf == 0)

contribution_data %<>%
  mutate(
    var_class = case_when(
      var %in% c(var_names[[1]], rough_var_names[[1]]) ~ "Elevation",
      var %in% c(var_names[2:4], rough_var_names[2:4]) ~ "Climate",
      var %in% c(var_names[[5]], rough_var_names[[5]]) ~ "NDVI",
      var %in% c(var_names[6:9], rough_var_names[6:9]) ~ "Soil"
    ),
    var_type = str_extract(var, "rough")
  ) %>%
  mutate(var_type = ifelse(is.na(var_type), "absolute", var_type))
contribution_data$var_class %<>% factor(levels = c(
  "Elevation",
  "Climate",
  "NDVI",
  "Soil"
))

contribs_data_summary <- contribution_data %>%
  group_by(model_type, region, var) %>%
  summarise(
    mean_rel.inf = mean(rel.inf, na.rm = TRUE),
    sd_rel.inf = sd(rel.inf, na.rm = TRUE)
  ) %>%
  mutate(
    upper_sd = mean_rel.inf + sd_rel.inf,
    lower_sd = mean_rel.inf - sd_rel.inf
  ) %>%
  split(.$region) %>%
  map(~ split(.x, .x$model_type))

# Plot screeplots of variable class contributions ------------------------------

# Define screeplot dimensions (for later)
screeplot_width <- length(unique(contribution_data$var)) + 2
screeplot_height <- 62

screeplots <- foreach(model_name_ = unique(contribution_data$model_name)) %do% {

  # Make the actual screeplot of mean rep BRT variable contributions
  screeplot_ <- contribution_data %>%
    filter(model_name == model_name_, model_type %in% c("replicates", NA)) %>%
    group_by(var, var_class, var_type) %>%
    summarise(rel.inf = mean(rel.inf)) %>%
    ungroup() %>%
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
        legend.position = ifelse(str_detect(model_name_, "Cape richness QDS"),
          "left",
          "none"
        )
      )

  # Add error bars for permuted BRT variable contributions
  region_ <- str_extract(model_name_, "(Cape|SWA)")
  screeplot_ <- screeplot_ +
    geom_point(
      data = contribs_data_summary[[region_]]$permutations,
      aes(y = mean_rel.inf),
      col = "grey25"
    ) +
    geom_errorbar(
      data = contribs_data_summary[[region_]]$permutations,
      aes(ymin = lower_sd, ymax = upper_sd),
      col = "grey25",
      width = 0
    )

  # Remove x-axis title if making a panel on top row
  if (str_detect(model_name_, "richness")) {
    screeplot_ <- screeplot_ + theme(axis.title.x = element_blank())
  }

  # Remove y-axis title + numbers if making a panel on right
  if (str_detect(model_name_, "SWA")) {
    screeplot_ <- screeplot_ + theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )
  }

  screeplot_
}
screeplots
# FIXME: Error in FUN(X[[i]], ...) : object 'var_class' not found
var_class_legend <- get_legend(screeplots[[1]])
screeplots[[1]] <- screeplots[[1]] + theme(legend.position = "none")

# Plot piecharts of roughness vs absolute contributions ------------------------
# (variable _type_)

transparent <- element_rect(colour = "transparent", fill = "transparent")

piecharts <- foreach(model_name_ = unique(contribution_data$model_name)) %do% {
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
