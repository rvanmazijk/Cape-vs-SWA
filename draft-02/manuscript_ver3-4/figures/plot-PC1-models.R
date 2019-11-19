# Plot PC1 univariate models

#map(heterogeneity_PCAs, summary)[2:4]
#>          Proportion of Variance (PC1)
#> $QDS                           0.4244
#> $HDS                           0.3902
#> $DS                            0.4126

m_QDS <- lm(QDS_richness ~ PC1 * region, data$QDS)
#visreg::visreg(m_QDS, xvar = "PC1", by = "region", overlay = TRUE, gg = TRUE)
glance(m_QDS)$r.squared

m_HDS <- lm(HDS_richness ~ PC1, data$HDS)
glance(m_HDS)$r.squared

m_DS <- lm(DS_richness ~ PC1, data$DS)
glance(m_DS)$r.squared

master_ylim <- c(0, max(data$DS$DS_richness))

QDS_plot <- ggplot(data$QDS, aes(PC1, QDS_richness, fill = region)) +
  geom_point(shape = 21, size = 1.25) +
  geom_smooth(aes(colour = region), method = lm, se = FALSE) +
  labs(
    x     = "PC1 (42.44%)",
    y     = bquote(italic("S")),
    title = bquote("(a)  QDS ("*italic("R")^{"2"}*" = 0.14)")
  ) +
  scale_fill_manual(name = "Region", values = c("black", "white")) +
  scale_colour_manual(name = "Region", values = c("black", "grey50")) +
  coord_cartesian(ylim = master_ylim) +
  theme(
    legend.position = "none",
    axis.text.y     = element_text(angle = 90, hjust = 0.5)
  )

HDS_plot <- ggplot(data$HDS, aes(PC1, HDS_richness)) +
  geom_point(aes(fill = region), shape = 21, size = 1.5) +
  geom_smooth(method = lm, colour = "black", se = FALSE) +
  labs(
    x     = "PC1 (39.02%)",
    title = bquote("(b)  HDS ("*italic("R")^{"2"}*" = 0.19)")
  ) +
  scale_fill_manual(name = "Region", values = c("black", "white")) +
  coord_cartesian(ylim = master_ylim) +
  theme(
    legend.position = "none",
    axis.ticks.y    = element_blank(),
    axis.text.y     = element_blank(),
    axis.title.y    = element_blank()
  )

DS_plot <- ggplot(data$DS, aes(PC1, DS_richness)) +
  geom_point(aes(fill = region), shape = 21, size = 2.0) +
  geom_smooth(method = lm, colour = "black", se = FALSE) +
  labs(
    x      = "PC1 (41.26%)",
    title  = bquote("(c)  DS ("*italic("R")^{"2"}*" = 0.28)")
  ) +
  scale_fill_manual(name = "Region", values = c("black", "white")) +
  coord_cartesian(ylim = master_ylim) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank()
  )

my_legend <- get_legend(DS_plot)
DS_plot <- DS_plot + theme(legend.position = "none")

all_scales_plots <- plot_grid(
  QDS_plot, HDS_plot, DS_plot, my_legend,
  nrow = 1, rel_widths = c(1, 0.9, 0.9, 0.3)
)

# Save to disc
ggsave(
  here(
    "draft-02/manuscript_ver3/figures",
    "plot-PC1-models.pdf"
  ),
  all_scales_plots,
  width = 9, height = 3
)
ggsave(
  here(
    "draft-02/manuscript_ver3/figures",
    "plot-PC1-models.pNG"
  ),
  all_scales_plots, dpi = 600,
  width = 9, height = 3
)
