# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 2: Check for collinearity between environmental variables
# Cape vs SWA publication
# Ruan van Mazijk

output_path <- here("outputs/species-environment-relationships")

# GCFR -------------------------------------------------------------------------

png(
  filename = glue("{output_path}/Cape_collinearity-check_0.8-cutoff_HDS.png"),
  width = 17, height = 15, units = "cm", res = 100
)
GCFR_predictor_names_HDS <- removeCollinearity(
  raster.stack = GCFR_variables_HDS_stack[[-c(1, 2)]],  # exclude responses
  select.variables = FALSE,  # To do manually
  multicollinearity.cutoff = 0.8,  # My thumb-suck. TODO: try 0.7?
  plot = TRUE
)
dev.off()

png(
  filename = glue("{output_path}/Cape_collinearity-check_0.8-cutoff_QDS.png"),
  width = 17, height = 15, units = "cm", res = 100
)
GCFR_predictor_names_QDS <- removeCollinearity(
  raster.stack = GCFR_data_QDS_stack[[-1]],  # exclude responses
  select.variables = FALSE,  # To do manually
  multicollinearity.cutoff = 0.8,  # My thumb-suck. TODO: try 0.7?
  plot = TRUE
)
dev.off()

# SWAFR ------------------------------------------------------------------------

png(
  filename = glue("{output_path}/SWA_collinearity-check_0.8-cutoff_HDS.png"),
  width = 17, height = 15, units = "cm", res = 100
)
SWAFR_predictor_names_HDS <- removeCollinearity(
  raster.stack = SWAFR_variables_HDS_stack[[-c(1, 2)]],
  select.variables = FALSE,
  multicollinearity.cutoff = 0.8,
  plot = TRUE
)
dev.off()

png(
  filename = glue("{output_path}/SWA_collinearity-check_0.8-cutoff_QDS.png"),
  width = 17, height = 15, units = "cm", res = 100
)
SWAFR_predictor_names_QDS <- removeCollinearity(
  raster.stack = SWAFR_data_QDS_stack[[-1]],
  select.variables = FALSE,
  multicollinearity.cutoff = 0.8,
  plot = TRUE
)
dev.off()

# BOTH -------------------------------------------------------------------------

# Own version of removeCollinearity that can handle a dataframe as input
# (Modified from virtualspecies::removeCollinearity)
removeCollinearity2 <- function(env.df,
                                multicollinearity.cutoff = 0.8,
                                select.variables = FALSE,
                                plot = TRUE) {
    cor.matrix <- matrix(
      data = 0,
      nrow = ncol(env.df), ncol = ncol(env.df),
      dimnames = list(names(env.df), names(env.df))
    )
    cor.matrix <- 1 - abs(stats::cor(env.df, method = "pearson"))
    dist.matrix <- stats::as.dist(cor.matrix)
    ahc <- stats::hclust(dist.matrix, method = "complete")
    groups <- stats::cutree(ahc, h = 1 - multicollinearity.cutoff)
    if (length(groups) == max(groups)) {
      message(paste(
        "  - No multicollinearity detected in your data at threshold ",
        multicollinearity.cutoff, "\n", sep = ""
      ))
      mc <- FALSE
    } else {
      mc <- TRUE
    }
    if (plot) {
      op <- par(no.readonly = TRUE)
      graphics::par(mar = c(5.1, 5.1, 4.1, 3.1))
      plot(
        ahc, hang = -1,
        xlab = "", ylab = "Distance (1 - Pearson's r)",  main = "",
        las = 1, sub = "", axes = FALSE
      )
      graphics::axis(2, at = seq(0, 1, length = 6), las = 1)
      if (mc) {
        graphics::title(paste(
          "Groups of intercorrelated variables at cutoff",
          multicollinearity.cutoff
        ))
        par(xpd = TRUE)
        rect.hclust(ahc, h = 1 - multicollinearity.cutoff)
      } else {
        graphics::title(paste(
          "No intercorrelation among variables at cutoff",
          multicollinearity.cutoff
        ))
      }
      par(op)
    }
    if (select.variables) {
      sel.vars <- NULL
      for (i in 1:max(groups)) {
        sel.vars <- c(sel.vars, sample(names(groups[groups == i]), 1))
      }
    } else {
      if (mc) {
        sel.vars <- list()
        for (i in groups) {
          sel.vars[[i]] <- names(groups)[groups == i]
        }
      } else {
        sel.vars <- names(env.df)
      }
    }
    sel.vars
}

png(
  filename = glue("{output_path}/BOTH_collinearity-check_0.8-cutoff_QDS.png"),
  width = 17, height = 15, units = "cm", res = 100
)
BOTH_predictor_names_QDS <- removeCollinearity2(
  env.df = BOTH_data_QDS[, -c(1, 2, 21)],
  select.variables = FALSE,
  multicollinearity.cutoff = 0.8,
  plot = TRUE
)
dev.off()

png(
  filename = glue("{output_path}/BOTH_collinearity-check_0.8-cutoff_HDS.png"),
  width = 17, height = 15, units = "cm", res = 100
)
BOTH_predictor_names_HDS <- removeCollinearity2(
  env.df = BOTH_data_HDS[, -c(1, 2, 3)],
  select.variables = FALSE,
  multicollinearity.cutoff = 0.8,
  plot = TRUE
)
dev.off()

# Select final predictor variables ---------------------------------------------

# Take first variable in each collinear cluster
GCFR_predictor_names_QDS  %<>% map_chr(1)
GCFR_predictor_names_HDS  %<>% map_chr(1)
SWAFR_predictor_names_QDS %<>% map_chr(1)
SWAFR_predictor_names_HDS %<>% map_chr(1)
BOTH_predictor_names_QDS  %<>% map_chr(1)
BOTH_predictor_names_HDS  %<>% map_chr(1)

# For bare-minimum BRT work on UCT HPC:
output_path <- here(
  "R/02_analyses",
  "analyse-species-environment-relationships/run-on-UCT-HPC"
)
write.csv(
  GCFR_predictor_names_QDS,
  glue("{output_path}/GCFR_predictor_names_QDS.csv")
)
write.csv(
  GCFR_predictor_names_HDS,
  glue("{output_path}/GCFR_predictor_names_HDS.csv")
)
write.csv(
  SWAFR_predictor_names_QDS,
  glue("{output_path}/SWAFR_predictor_names_QDS.csv")
)
write.csv(
  SWAFR_predictor_names_QDS,
  glue("{output_path}/SWAFR_predictor_names_HDS.csv")
)
write.csv(
  BOTH_predictor_names_QDS,
  glue("{output_path}/BOTH_predictor_names_QDS.csv")
)
write.csv(
  BOTH_predictor_names_QDS,
  glue("{output_path}/BOTH_predictor_names_HDS.csv")
)

