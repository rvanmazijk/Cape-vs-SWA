library(here)
source(here("draft-02/manuscript_ver3/R/01_setup.R"))
library(MVN)

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

data %>%
  map(gather, environmental_variable, heterogeneity_value, Elevation:pH) %>%
  map(mutate, environmental_variable = environmental_variable %>%
    str_replace_all("_", " ") %>%
    factor(levels = var_names)
  ) %>%
  imap(
    ~ ggplot(.x) +
      aes_string(
        "heterogeneity_value", glue("{.y}_richness"),
        fill = "region"
      ) +
      geom_point(shape = 21) +
      scale_fill_manual(name = "Region", values = c("black", "white")) +
      labs(x = "Heterogeneity value", y = bquote(italic("S")[.(.y)])) +
      facet_wrap(~environmental_variable, nrow = 2) +
      theme(
        legend.position = c(0.9, 0.25),
        axis.text.y     = element_text(angle = 90, hjust = 0.5)
      )
  )

mvn(data$QDS[, c(str_replace_all(var_names, " ", "_"), "QDS_richness")])
mvn(data$HDS[, c(str_replace_all(var_names, " ", "_"), "HDS_richness")])
mvn(data$DS[,  c(str_replace_all(var_names, " ", "_"), "DS_richness")])
