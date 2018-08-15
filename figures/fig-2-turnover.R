# Make Fig. 2 (Species richness and turnover)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
#source(here::here("data/01_import-region-polygons.R"))
#source(here::here("data/02_import-floral-data.R"))

# Compile data -----------------------------------------------------------------

GCFR_spp <- readOGR(here::here(
  "outputs/species-turnover-and-richness/GCFR_spp_2018-08-14"
))
SWAFR_spp <- readOGR(here::here(
  "outputs/species-turnover-and-richness/SWAFR_spp_2018-08-14"
))

vars_to_keep <- c(
  "HDS_richness",
  "n_QDS",
  "mean_QDS_richness",
  "mean_QDS_jaccard"
)
names(GCFR_spp)[6:9] <- vars_to_keep
names(SWAFR_spp)[6:9] <- vars_to_keep

# TODO: de-duplicate this code below (copied from analyses/)

GCFR_spp_data <- GCFR_spp@data %>%
  select(vars_to_keep) %>%
  distinct()
SWAFR_spp_data <- SWAFR_spp@data %>%
  select(vars_to_keep) %>%
  distinct()

richness_turnover_data <-
  rbind(
    cbind(region = "Cape", GCFR_spp_data),
    cbind(region = "SWA", SWAFR_spp_data)
  ) %>%
  as_tibble() %>%
  filter(n_QDS > 1) %>%  # turnover is non-sensicle for 1 QDS)
  mutate(
    add_residual_turnover = HDS_richness - mean_QDS_richness,
    add_residual_turnover_prop = add_residual_turnover / HDS_richness,
    mul_residual_turnover = HDS_richness / mean_QDS_richness
  )

# Run tests --------------------------------------------------------------------

# TODO: de-duplicate this code below too (copied from analyses/)

mean_QDS_jaccard_test <- richness_turnover_data %$%
  wilcox.test(
    mean_QDS_jaccard[region == "Cape"],
    mean_QDS_jaccard[region == "SWA"],
    alternative = "two.sided"
  ) %>%
  tidy()

add_residual_turnover_prop_test <- richness_turnover_data %$%
  wilcox.test(
    add_residual_turnover_prop[region == "Cape"],
    add_residual_turnover_prop[region == "SWA"],
    alternative = "two.sided"
  ) %>%
  tidy()

# And again, but only with HDS where no. QDS = 4
mean_QDS_jaccard_test2 <- richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  wilcox.test(
    mean_QDS_jaccard[region == "Cape"],
    mean_QDS_jaccard[region == "SWA"],
    alternative = "two.sided"
  ) %>%
  tidy()

add_residual_turnover_prop_test2 <- richness_turnover_data %>%
  filter(n_QDS == 4) %$%
  wilcox.test(
    add_residual_turnover_prop[region == "Cape"],
    add_residual_turnover_prop[region == "SWA"],
    alternative = "two.sided"
  ) %>%
  tidy()

# Make histograms --------------------------------------------------------------

y_lim <- 35

mean_QDS_jaccard_panel <- richness_turnover_data %>%
  ggplot(aes(mean_QDS_jaccard, fill = region)) +
    geom_histogram(position = "dodge", bins = 20) +
    scale_fill_manual(name = "Region", values = my_palette) +
    labs(
      x = expression(paste(
        italic(bar("J")["QDS"])
      )),
      y = "No. HDS"
    ) +
    ylim(0, y_lim) +
    annotate(
      "text", x = 0.65, y = 0.8 * y_lim,
      parse = TRUE,
      label = paste0(
        ifelse(mean_QDS_jaccard_test$p.value < 0.0001,
          "italic(P) < '0.0001'",  # extra quote to avoid '1e04' rendering
          glue("italic(P) == {mean_QDS_jaccard_test$p.value}")
        )
      )
    ) +
    theme(legend.position = "none")
add_residual_turnover_prop_panel <- richness_turnover_data %>%
  ggplot(aes(add_residual_turnover_prop, fill = region)) +
    geom_histogram(position = "dodge", bins = 20) +
    scale_fill_manual(name = "Region", values = my_palette) +
    labs(
      x = expression(paste(
        italic("T"["HDS"]), " (proportion of ", italic("S"["QDS"]), ")"
      )),
      y = "No. HDS"
    ) +
    ylim(0, y_lim) +
    annotate(
      "text", x = 0.4, y = 0.8 * y_lim,
      parse = TRUE,
      label = paste0(
        ifelse(add_residual_turnover_prop_test$p.value < 0.0001,
          "italic(P) < '0.0001'",
          glue("italic(P) == {add_residual_turnover_prop_test$p.value}")
        )
      )
    ) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )
both_turnover_panels <- plot_grid(
  mean_QDS_jaccard_panel, add_residual_turnover_prop_panel,
  rel_widths = c(1, 1.2),
  labels = c("(a)", "(b)"), hjust = c(-2.25, -0.75), vjust = c(2, 2)
)
ggsave(
  here::here("figures/fig-2-turnover.png"),
  both_turnover_panels,
  width = 7, height = 3,
  dpi = 300
)

# Again, but with only HDS where no. QDS = 4
mean_QDS_jaccard_panel2 <- richness_turnover_data %>%
  filter(n_QDS == 4) %>%
  ggplot(aes(mean_QDS_jaccard, fill = region)) +
    geom_histogram(position = "dodge", bins = 20) +
    scale_fill_manual(name = "Region", values = my_palette) +
    labs(
      x = expression(paste(
        italic(bar("J")["QDS"])
      )),
      y = "No. HDS"
    ) +
    ylim(0, y_lim) +
    annotate(
      "text", x = 0.65, y = 0.8 * y_lim,
      parse = TRUE,
      label = paste0(
        ifelse(mean_QDS_jaccard_test2$p.value < 0.0001,
          "italic(P) < '0.0001'",
          glue("italic(P) == {mean_QDS_jaccard_test2$p.value}")
        )
      )
    ) +
    theme(legend.position = "none")
add_residual_turnover_prop_panel2 <- richness_turnover_data %>%
  filter(n_QDS == 4) %>%
  ggplot(aes(add_residual_turnover_prop, fill = region)) +
    geom_histogram(position = "dodge", bins = 20) +
    scale_fill_manual(name = "Region", values = my_palette) +
    labs(
      x = expression(paste(
        italic("T"["HDS"]), " (proportion of ", italic("S"["QDS"]), ")"
      )),
      y = "No. HDS"
    ) +
    ylim(0, y_lim) +
    annotate(
      "text", x = 0.5, y = 0.8 * y_lim,
      parse = TRUE,
      label = paste0(
        ifelse(add_residual_turnover_prop_test2$p.value < 0.0001,
          "italic(P) < '0.0001'",
          glue("italic(P) == {add_residual_turnover_prop_test2$p.value}")
        )
      )
    ) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )
both_turnover_panels2 <- plot_grid(
  mean_QDS_jaccard_panel2, add_residual_turnover_prop_panel2,
  rel_widths = c(1, 1.25),
  labels = c("(a)", "(b)"), hjust = c(-2.25, -0.75), vjust = c(2, 2)
)
ggsave(
  here::here("figures/fig-2-turnover_n-QDS-4.png"),
  both_turnover_panels2,
  width = 7, height = 3,
  dpi = 300
)
