# Make Fig. 2 (Species richness and turnover)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

output_path <- here("outputs/turnover")

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
      "text", x = 0.8, y = 0.9 * y_lim,
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
    )

# Save to disc -----------------------------------------------------------------

ggsave(
  here::here("SAAB-AMA-SASSB-2019-talk/figures/fig-2-turnover.png"),
  mean_QDS_jaccard_panel,
  width = 4, height = 3,
  dpi = 300
)

# TODO: Again, but with only HDS where no. QDS = 4?
