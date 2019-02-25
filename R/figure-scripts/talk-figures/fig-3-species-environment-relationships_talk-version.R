# Make Fig. 3 (BRT variable contributions)
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

transparent <- element_rect(colour = "transparent", fill = "transparent")

remove_legend <- function(x) {
  x + theme(legend.position = "none")
}
remove_xlab <- function(x) {
  x + xlab("")
}
remove_ylab <- function(x) {
  x + theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )
}

# Import BRT contribution data -------------------------------------------------

contribution_data <- map_df(
  c(
    "QDS-richness-models-contributions.csv",
    "HDS-richness-models-contributions.csv",
    "HDS-turnover-models-contributions.csv"
  ),
  ~ cbind(path = .x, read_csv(glue("{output_path}/{.x}")))
)

contribution_F_tests <- read_csv(here(
  "manuscript/tables/contribution-F-tests.csv"
))
contribution_F_tests %<>%
  mutate(region = case_when(
    region == "GCFR" ~ "Cape",
    region == "SWAFR" ~ "SWA"
  )) %>%
  mutate(model_name = paste(region, response, scale))

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
    rel.inf = 0,
    model_type = "replicates"
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
  group_by(
    model_type, model_name,
    var, var_class, var_type
  ) %>%
  summarise(
    mean_rel.inf = mean(rel.inf, na.rm = TRUE),
    sd_rel.inf = sd(rel.inf, na.rm = TRUE)
  ) %>%
  mutate(
    upper_sd = mean_rel.inf + sd_rel.inf,
    lower_sd = mean_rel.inf - sd_rel.inf
  ) %>%
  mutate(mean_rel.inf = ifelse(mean_rel.inf == 0, NA, mean_rel.inf)) %>%
  ungroup() %>%
  mutate(var = str_replace_all(var, "(\\.|_)", " ")) %>%
  mutate(var = str_replace(var, "rough", "R"))

# Plot screeplots of variable class contributions ------------------------------

model_names <- c("Cape richness QDS", "SWA richness QDS")

# Set panel height for labelling-ease and adding of piecharts below
panel_height <- 40

# Create all 9 screeplots
screeplots <- foreach(model_name_ = model_names) %do% {
  # Extract data for "model_name_"th panel
  reps <- filter(contribs_data_summary,
    model_name == model_name_,
    model_type == "replicates"
  )
  perms <- filter(contribs_data_summary,
    model_name == model_name_,
    model_type == "permutations"
  )
  rep_F_value <- contribution_F_tests %>%
    filter(model_name == model_name_, model_type == "replicates") %>%
    pull("F_value") %>%
    round(0) %>%
    format(big.mark = ",", scientific = FALSE)
  perm_F_value <- contribution_F_tests %>%
    filter(model_name == model_name_, model_type == "permutations") %>%
    pull("F_value") %>%
    round(2) %>%
    format(nsmall = 2)
  # Make the actual screeplot of mean rep BRT variable contributions
  screeplot_ <-
    ggplot(
      data = reps,
      aes(
        reorder(var, desc(mean_rel.inf)), mean_rel.inf,
        col = var_class#, alpha = model_type, shape = model_type
      ),
      drop = FALSE
    ) +
    # Add replicate-BRT variable contributions ± sd
    geom_errorbar(
      aes(ymin = lower_sd, ymax = upper_sd),
      width = 0
    ) +
    geom_point(size = 2) +
    # Add permuted-BRT variable contributions ± sd
    geom_errorbar(
      data = perms,
      aes(ymin = lower_sd, ymax = upper_sd),
      width = 0,
      position = position_nudge(x = 0.25)
    ) +
    geom_point(
      data = perms,
      size = 2, shape = 3,  # plus-sign
      position = position_nudge(x = 0.25)
    ) +
    ylim(min(perms$lower_sd), panel_height)
  # Add better labels etc.
  screeplot_ <- screeplot_ +
    labs(
      x = "Environmental variables",
      y = "Relative influence (%)",
      title = model_name_
    ) +
    scale_colour_manual(values = var_colours) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.title = element_blank()
    )
  screeplot_
}
names(screeplots) <- str_replace_all(model_names, " ", "_")

# Get the legend (for use when combined all 9 plots)
var_class_legend <- get_legend(screeplots$Cape_richness_QDS)

# Remove all panels' legends for panelling
screeplots %<>% map(remove_legend)

# Remove right-panel y-axis for panelling
screeplots$SWA_richness_QDS %<>% remove_ylab()

screeplots_blank <- map(screeplots, function(x) {
  x + scale_colour_manual(values = c("white", "white", "white", "white"))
})
screeplots_blank

# Plot piecharts of variable contributions -------------------------------------
# (variable _class_)

piecharts_var_class <- foreach(model_name_ = model_names) %do% {
  contribs_data_summary %>%
    filter(
      model_type == "replicates",
      model_name == model_name_,
      !is.na(mean_rel.inf)
    ) %>%
    # FIXME: order pie-slices by size
    #mutate(var = reorder(var, desc(mean_rel.inf))) %>%
    mutate(var_class = factor(var_class, levels = c(
      "Elevation",
      "Climate",
      "NDVI",
      "Soil"
    ))) %>%
    ggplot(aes("", mean_rel.inf, col = var_class, fill = var_class)) +
      geom_col() +
      coord_polar("y", start = 0) +
      scale_colour_manual(values = var_colours, drop = FALSE) +
      scale_fill_manual(values = var_colours, drop = FALSE) +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = transparent,
        plot.background =  transparent,
        legend.position = "none"
      )
}
names(piecharts_var_class) <- str_replace_all(model_names, " ", "_")

# Plot piecharts of roughness vs absolute contributions ------------------------
# (variable _type_)

piecharts_var_type <- foreach(model_name_ = model_names) %do% {
  contribs_data_summary %>%
    filter(
      model_type == "replicates",
      model_name == model_name_,
      !is.na(mean_rel.inf)
    ) %>%
    # FIXME: order pie-slices by size
    #mutate(var = reorder(var, desc(mean_rel.inf))) %>%
    ggplot(aes("", mean_rel.inf, col = var_type, fill = var_type)) +
      geom_col() +
      coord_polar("y", start = 0) +
      scale_colour_manual(values = c("grey75", "black")) +
      scale_fill_manual(values = c("grey75", "black")) +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = transparent,
        plot.background =  transparent,
        legend.title = element_blank()
      )
}
names(piecharts_var_type) <- str_replace_all(model_names, " ", "_")

# Get the legend (for use when combined all 9 plots)
var_type_legend <- get_legend(piecharts_var_type$Cape_richness_QDS)
# Now remove legends for panelling below
piecharts_var_type %<>% map(remove_legend)

# Inset the piecharts in the screeplots ----------------------------------------

panel_width <- length(unique(contribs_data_summary$var))

screepieplots <- foreach(screeplot_ = screeplots,
                         piechart_var_class_ = piecharts_var_class,
                         piechart_var_type_ = piecharts_var_type) %do% {
  screeplot_ +
    annotation_custom(
      ggplotGrob(piechart_var_class_),
      xmin = 0.15 * panel_width,  xmax = 0.65 * panel_width,
      ymin = 0.50 * panel_height, ymax = panel_height
    ) +
    annotation_custom(
      ggplotGrob(piechart_var_type_),
      xmin = 0.55 * panel_width,  xmax = 1.05 * panel_width,
      ymin = 0.50 * panel_height, ymax = panel_height
    )
}

# Combine panels ---------------------------------------------------------------

var_legend <- plot_grid(
  var_type_legend, var_class_legend, white_rect,
  nrow = 3,
  rel_heights = c(1, 1, 0.25)
)

screeplots_blank <- plot_grid(plotlist = screeplots_blank, rel_widths = c(1, 0.9))
screeplots_blank <- plot_grid(
  screeplots_blank, var_legend,
  nrow = 1,
  rel_widths = c(1, 0.2)
)

screeplots <- plot_grid(plotlist = screeplots, rel_widths = c(1, 0.9))
screeplots <- plot_grid(
  screeplots, var_legend,
  nrow = 1,
  rel_widths = c(1, 0.2)
)

screepieplots <- plot_grid(plotlist = screepieplots, rel_widths = c(1, 0.9))
screepieplots <- plot_grid(
  screepieplots, var_legend,
  nrow = 1,
  rel_widths = c(1, 0.2)
)

# Save to disc -----------------------------------------------------------------

ggsave(
  here(
    "SAAB-AMA-SASSB-2019-talk/figures/",
    "fig-3-species-environment-relationships_blank.png"
  ),
  screeplots_blank,
  width = 6, height = 3,
  dpi = 300
)

ggsave(
  here(
    "SAAB-AMA-SASSB-2019-talk/figures/",
    "fig-3-species-environment-relationships_nopie.png"
  ),
  screeplots,
  width = 6, height = 3,
  dpi = 300
)

ggsave(
  here(
    "SAAB-AMA-SASSB-2019-talk/figures/",
    "fig-3-species-environment-relationships.png"
  ),
  screepieplots,
  width = 6, height = 3,
  dpi = 300
)
