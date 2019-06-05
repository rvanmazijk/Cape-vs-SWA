# Analyse environmental heterogeneity ("roughness") across spatial scales
# Cape vs SWA
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
library(source("analyses-May-2019/03_collate-and-transform-data.R"))

# PCAs -------------------------------------------------------------------------

roughness_PCAs <- map(roughness_matrices, ~prcomp(.x[, -1], scale. = TRUE))
map(roughness_PCAs, summary)
# Force PC1 scores to be positive if all vars rotations are negative
roughness_PCAs %<>% map(function(PCA) {
  if (all(PCA$rotation[, 1] <= 0)) {
    PCA$rotation[, 1] %<>% multiply_by(-1)
    PCA$x[, 1]        %<>% multiply_by(-1)
  }
  PCA
})
# Plot
map2(roughness_PCAs, roughness_matrices,
  ~ autoplot(
    .x,
    data = .y,
    colour = "region"
  )
)

# Store PC1 & 2 in matrices for later
roughness_matrices <- map2(roughness_PCAs, roughness_matrices,
  function(PCA, layer) {
    layer$PC1 <- PCA$x[, 1]
    layer$PC2 <- PCA$x[, 2]
    layer
  }
)

# TODO: Interrogate rotations associated with different variables
#PCA_rotations <- roughness_PCAs %$%
#  rotation %>%
#  {tibble(
#    var = rownames(.),
#    PC1 = .[, 1],
#    PC2 = .[, 2]
#  )} %>%
#  gather(PC, rotation, -var)
#ggplot(PCA_rotations, aes(var, rotation)) +
#  geom_col() +
#  facet_wrap(~PC, scales = "free") +
#  theme(axis.text.x = element_text(angle = 45))

# LDAs -------------------------------------------------------------------------
# (Linear Discriminant Analysis)

# TODO

#roughness_matrix_scaled <- roughness_matrix
#roughness_matrix_scaled[, -1] %<>% scale()
#roughness_LDA <- lda(region ~ ., roughness_matrix_scaled, )
#roughness_LDA
#roughness_LDA_values <- predict(roughness_LDA)
#ldahist(roughness_LDA_values$x[, 1], g = roughness_matrix$region)  # DF1

# FIXME: No DF2?
#plot(
#  roughness_LDA_values$x[, 1],
#  roughness_LDA_values$x[, 2]
#)

# FIXME:
#roughness_EDS_LDA <- lda(region ~ ., roughness_EDS_matrix)
#roughness_EDS_LDA
#roughness_EDS_LDA_values <- predict(roughness_EDS_LDA)
#ldahist(roughness_EDS_LDA_values$x[, 1], g = roughness_EDS_matrix$region)
#roughness_QDS_LDA <- lda(region ~ ., roughness_QDS_matrix)
#roughness_QDS_LDA
#roughness_QDS_LDA_values <- predict(roughness_QDS_LDA)
#ldahist(roughness_QDS_LDA_values$x[, 1], g = roughness_QDS_matrix$region)
#roughness_HDS_LDA <- lda(region ~ ., roughness_HDS_matrix)
#roughness_HDS_LDA
#roughness_HDS_LDA_values <- predict(roughness_HDS_LDA)
#ldahist(roughness_HDS_LDA_values$x[, 1], g = roughness_HDS_matrix$region)
#roughness_3QDS_LDA <- lda(region ~ ., roughness_3QDS_matrix)
#roughness_3QDS_LDA
#roughness_3QDS_LDA_values <- predict(roughness_3QDS_LDA)
#ldahist(roughness_3QDS_LDA_values$x[, 1], g = roughness_3QDS_matrix$region)

# CLESs ------------------------------------------------------------------------

# Pseudo-code:
#   for each resolution,
#     for each variable,
#       CLES(Cape, SWA)

output_path <- here("outputs/roughness")

# Collate roughness data
roughness_data <- list(
  "0.05º" = roughness_matrix,
  "EDS"   = roughness_EDS_matrix,
  "QDS"   = roughness_QDS_matrix,
  "HDS"   = roughness_HDS_matrix,
  "3QDS"  = roughness_3QDS_matrix
)
GCFR_roughness_data <- roughness_data %>%
  map(filter, region == "GCFR") %>%
  map(dplyr::select, -region, -PC2)
SWAFR_roughness_data <- roughness_data %>%
  map(filter, region == "SWAFR") %>%
  map(dplyr::select, -region, -PC2)

# (WARNING: this takes a while...)
if (prompt_continue()) {  # only run if haven't already
  set.seed(1234)
  CLES_results <- map2_df(GCFR_roughness_data, SWAFR_roughness_data,
    .id = "resolution",  # for every spatial resolution,
    ~ map2_df(.x, .y,
      .id = "variable",  # for every variable in each region,
      function(.x, .y) {
        CLES_value <- CLES(.y, .x)
        message("Done")
        tibble(CLES_value)
      }
    )
  )
  # Save results to disc
  write_csv(
    CLES_results,
    glue("{output_path}/CLES_results.csv")
  )
} else {  # or import from disc
  CLES_results <- read_csv(glue("{output_path}/CLES_results.csv"))
}

CLES_results %<>%
  filter(variable != "region") %>%
  mutate(resolution = case_when(
    resolution == "0.05º" ~ 0.05,
    resolution == "EDS"   ~ 0.125,
    resolution == "QDS"   ~ 0.25,
    resolution == "HDS"   ~ 0.50,
    resolution == "3QDS"  ~ 0.75
  ))

# Plot & model CLES_value ~ resolution "in general"
m_all <- lm(CLES_value ~ resolution, CLES_results)
summary(m_all)
visreg::visreg(m_all)

# Plot & model all fits for all vars separately
m_seps <- CLES_results %>%
  split(.$variable) %>%
  map(~lm(CLES_value ~ resolution, .x))
ggplot(CLES_results, aes(resolution, CLES_value)) +
  geom_point() +
  geom_smooth(method = lm) +
  geom_hline(yintercept = 0.5, lty = "dashed") +
  facet_wrap(~variable)
# NOTE: PC2 crosses the CLES=0.5 line!!!
# Coefficients' plot:
m_seps %>%
  map_df(.id = "variable", tidy, conf.int = TRUE) %>%
  filter(term == "resolution") %>%
  mutate(sig = case_when(
    p.value <= 0.001 ~ "***",
    p.value <= 0.01  ~ "**",
    p.value <= 0.05  ~ "*",
    p.value <= 0.10  ~ "*",
    TRUE             ~ ""
  )) %>%
  ggplot(aes(variable, estimate, colour = p.value, label = sig)) +
    geom_hline(yintercept = 0, lty = "dashed") +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
    geom_text(colour = "black", size = 8, nudge_y = 0.05) +
    scale_colour_distiller(palette = "Spectral")
