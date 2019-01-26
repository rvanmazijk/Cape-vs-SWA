library(here)
source(here(
  "R/03_figure-scripts/main-figures/",
  "fig-3-species-environment-relationships.R"
))
library(ggalluvial)

alluvial_plots <- foreach(model_name_ = model_names) %do% {
  contribs_data_summary %>%
    filter(
      model_type == "replicates",
      model_name == model_name_,
      !is.na(mean_rel.inf)
    ) %>%
    ggplot(aes(y = mean_rel.inf, axis1 = var_class, axis2 = var_type)) +
      geom_alluvium(aes(col = var_class, fill = var_class, alpha = var_type)) +
      geom_stratum(alpha = 0) +
      geom_label(stat = "stratum", label.size = 0, label.strata = TRUE) +
      ggtitle(model_name_) +
      scale_colour_manual(values = var_colours, drop = FALSE) +
      scale_fill_manual(values = var_colours, drop = FALSE) +
      scale_alpha_manual(values = c(0.4, 0.9)) +
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
names(alluvial_plots) <- str_replace_all(model_names, " ", "_")

ggsave(
  here("figures/fig-6-alluvial-plots.png"),
  plot_grid(plotlist = alluvial_plots),
  width = 12, height = 6,
  dpi = 300
)
