# Import PCA-objects -----------------------------------------------------------

scales <- c("0.10", "QDS", "HDS", "DS")
heterogeneity_PCAs <- map(scales, ~read_rds(here(
  "results",
  glue("heterogeneity_{.x}_PCA")
)))
names(heterogeneity_PCAs) <- c("point1", "QDS", "HDS", "DS")

# Import heterogeneity dataframes to disc --------------------------------------

heterogeneity_dfs <- map(scales, ~read_csv(glue(
  "{data_dir}/",
  "heterogeneity-data-{.x}.csv"
)))
names(heterogeneity_dfs) <- c("point1", "QDS", "HDS", "DS")

# Plot PCA-biplots -------------------------------------------------------------

my_palette2 <- change_col_alpha(my_palette, alpha = 0.05)

PC_biplots <- pmap(list(heterogeneity_PCAs,
                        heterogeneity_dfs,
                        c("point1", "QDS", "HDS", "DS")),
  ~ autoplot(..1, data = ..2, colour = "region",
      loadings = TRUE,
      loadings.colour = "black"
    ) +
    lims(
      x = case_when(
        ..3 == "point1" ~ c(-0.070, 0.070),
        ..3 == "QDS"    ~ c(-0.160, 0.160),
        ..3 == "HDS"    ~ c(-0.250, 0.250),
        ..3 == "DS"     ~ c(-0.475, 0.475)
      ),
      y = case_when(
        ..3 == "point1" ~ c(-0.070, 0.070),
        ..3 == "QDS"    ~ c(-0.160, 0.160),
        ..3 == "HDS"    ~ c(-0.250, 0.250),
        ..3 == "DS"     ~ c(-0.475, 0.475)
      )
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_colour_manual(name = "Region", values = my_palette2) +
    theme(
      axis.ticks   = element_blank(),
      axis.text    = element_blank(),
      axis.title   = element_blank(),
      panel.border = element_blank()
    )
)
my_legend <- get_legend(PC_biplots$point1)
PC_biplots %<>% map(~ . + theme(legend.position = "none"))
PC_biplots <- plot_grid(
  plotlist = PC_biplots, nrow = 2,
  labels         = c("0.10°×0.10°", "QDS", "HDS", "DS"),
  label_fontface = "plain",
  label_x        = 0.075,
  label_y        = 0.975,
  hjust          = 0
)
PC_biplots <- plot_grid(
  PC_biplots, my_legend,
  nrow = 1, rel_widths = c(1, 0.2)
)

# Save for SI ------------------------------------------------------------------

ggsave(
  here("figures/plot-PCA-biplots_for-IBS-ECBC-2021.pdf"),
  PC_biplots,
  width = 8, height = 6
)
ggsave(
  here("figures/plot-PCA-biplots_for-IBS-ECBC-2021.png"),
  PC_biplots,
  width = 8, height = 6
)
