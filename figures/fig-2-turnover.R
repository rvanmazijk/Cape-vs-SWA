# Make Fig. 2 (Species richness and turnover)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))

output_path <- here::here("outputs/turnover")

turnover_results <- read_csv(glue(
  "{output_path}/turnover_results.csv"
))
richness_turnover_data <- read_csv(glue(
  "{output_path}/richness_turnover_data.csv"
))

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
      label = turnover_results %$% paste(
        ifelse(round(U_p_value[test == "mean_QDS_jaccard"],
                     digits = 4) <= 0.0001,
          "italic(P[U]) < '0.0001'",  # extra quote to avoid '1e04' rendering
          glue("italic(P[U]) == {\\
            round(U_p_value[test == 'mean_QDS_jaccard'], digits = 4)
          }")
        )
      )
    ) +
    annotate(
      "text", x = 0.65, y = 0.7 * y_lim,
      parse = TRUE,
      label = turnover_results %$% paste(glue(
        "italic(CLES) == {\\
        CLES_value[test == 'mean_QDS_jaccard'] %>%
          round(digits = 2) %>%
          format(nsmall = 2)
        }"
      ))
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
      label = turnover_results %$% paste(
        ifelse(round(U_p_value[test == "add_residual_turnover_prop"],
                     digits = 4) <= 0.0001,
          "italic(P[U]) < '0.0001'",  # extra quote to avoid '1e04' rendering
          glue("italic(P[U]) == {\\
            round(U_p_value[test == 'add_residual_turnover_prop'], digits = 4)
          }")
        )
      )
    ) +
    annotate(
      "text", x = 0.4, y = 0.7 * y_lim,
      parse = TRUE,
      label = turnover_results %$% paste(glue(
        "italic(CLES) == {\\
        CLES_value[test == 'add_residual_turnover_prop'] %>%
          round(digits = 2) %>%
          format(nsmall = 2)
        }"
      ))
    ) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
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

# Again, but with only HDS where no. QDS = 4?
# TODO ?
