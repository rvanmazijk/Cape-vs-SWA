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

# Rename variables as numbers for neater plotting
heterogeneity_PCAs %<>% map(~{
  names(.x$center) <-
    names(.x$scale) <-
    rownames(.x$rotation) <-
    1:9
  .x
})
colnames(heterogeneity_dfs$point1)[4:13] <- 1:10
heterogeneity_dfs[c("QDS", "HDS", "DS")] %<>% map(~{
  colnames(.x)[3:12] <- 1:10
  .x
})

PC_biplots <- pmap(list(heterogeneity_PCAs,
                        heterogeneity_dfs,
                        c("point1", "QDS", "HDS", "DS")),
  ~ autoplot(..1, data = ..2, colour = "region",
      loadings = TRUE,
      loadings.colour = "black",
      loadings.label = TRUE,
      loadings.label.colour = "black",
      loadings.label.hjust = -0.25,
      loadings.label.size = 3
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    lims(
      x = case_when(
        ..3 == "point1" ~ c(-0.070, 0.070),
        ..3 == "QDS"    ~ c(-0.160, 0.125),
        ..3 == "HDS"    ~ c(-0.250, 0.225),
        ..3 == "DS"     ~ c(-0.400, 0.475)
      ),
      y = case_when(
        ..3 == "point1" ~ c(-0.070, 0.070),
        ..3 == "QDS"    ~ c(-0.125, 0.160),
        ..3 == "HDS"    ~ c(-0.250, 0.225),
        ..3 == "DS"     ~ c(-0.400, 0.475)
      )
    ) +
    scale_colour_manual(name = "Region", values = c("grey30", "grey80")) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
)
my_legend <- get_legend(PC_biplots$point1)
PC_biplots %<>% map(~ . + theme(legend.position = "none"))
PC_biplots <- plot_grid(
  plotlist = PC_biplots, nrow = 2,
  labels         = c("(a) 0.10°×0.10°", "(b) QDS", "(c) HDS", "(d) DS"),
  label_fontface = "plain",
  label_x        = 0.150,
  label_y        = 0.975,
  hjust          = 0
)
PC_biplots <- plot_grid(
  PC_biplots, my_legend,
  nrow = 1, rel_widths = c(1, 0.2)
)

# Save for SI ------------------------------------------------------------------

ggsave(
  here("figures/plot-PCA-biplots.pdf"),
  PC_biplots,
  width = 8, height = 6
)
ggsave(
  here("figures/plot-PC-biplots.png"),
  PC_biplots,
  width = 8, height = 6
)
