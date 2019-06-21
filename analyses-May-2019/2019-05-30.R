# Setup
setwd("~/Google Drive/work/projects/Cape-vs-SWA-extras/")
library(tidyverse)
library(magrittr)

# Import data
HDS_data <- read_csv("sent-to-coauthors/BOTH_variables_HDS.csv")
QDS_data <- read_csv("sent-to-coauthors/BOTH_variables_QDS.csv")

# Perform PCAs of environmental heterogeneity data
PCA_results <- HDS_data %>%
  select_at(str_which(names(HDS_data), "rough")) %>%
  as.data.frame() %>%
  log() %>%
  prcomp(scale. = TRUE)
PCA_results_QDS <- QDS_data %>%
  select_at(str_which(names(QDS_data), "rough")) %>%
  as.data.frame() %>%
  log() %>%
  prcomp(scale. = TRUE)

# Force scores to be positive if all vars rotations are negative
if (all(PCA_results$rotation[, 1] <= 0)) {
  PCA_results$rotation[, 1] %<>% multiply_by(-1)
  PCA_results$x[, 1]        %<>% multiply_by(-1)
}
if (all(PCA_results_QDS$rotation[, 1] <= 0)) {
  PCA_results_QDS$rotation[, 1] %<>% multiply_by(-1)
  PCA_results_QDS$x[, 1]        %<>% multiply_by(-1)
}

# Have a look at the variance explained by PC1--2
summary(PCA_results)
summary(PCA_results_QDS)

# Add PC1--2 to the dataset
HDS_data %<>%
  cbind(
    PC1 = PCA_results$x[, 1],
    PC2 = PCA_results$x[, 2]
  ) %>%
  as_tibble()
QDS_data %<>%
  cbind(
    PC1 = PCA_results_QDS$x[, 1],
    PC2 = PCA_results_QDS$x[, 2]
  ) %>%
  as_tibble()

# PC1--2 biplot
ggplot(HDS_data, aes(PC1, PC2, colour = region)) +
  geom_point()
ggplot(QDS_data, aes(PC1, PC2, colour = region)) +
  geom_point()

# Plot histograms of PC1--2 scores
HDS_data %>%
  gather(PC, score, PC1:PC2) %>%
  ggplot(aes(score, fill = region)) +
    geom_histogram(bins = 20, position = "dodge") +
    facet_grid(~PC, scales = "free")
QDS_data %>%
  gather(PC, score, PC1:PC2) %>%
  ggplot(aes(score, fill = region)) +
    geom_histogram(bins = 20, position = "dodge") +
    facet_grid(~PC, scales = "free")

# Interrogate rotations associated with different variables
PCA_rotations <- PCA_results %$%
  rotation %>%
  {tibble(
    var = rownames(.),
    PC1 = .[, 1],
    PC2 = .[, 2]
  )} %>%
  gather(PC, rotation, -var)
PCA_rotations_QDS <- PCA_results %$%
  rotation %>%
  {tibble(
    var = rownames(.),
    PC1 = .[, 1],
    PC2 = .[, 2]
  )} %>%
  gather(PC, rotation, -var)
ggplot(PCA_rotations, aes(var, rotation)) +
  geom_col() +
  facet_wrap(~PC, scales = "free") +
  theme(axis.text.x = element_text(angle = 45))
ggplot(PCA_rotations_QDS, aes(var, rotation)) +
  geom_col() +
  facet_wrap(~PC, scales = "free") +
  theme(axis.text.x = element_text(angle = 45))

# Make Whittaker's additive turnover
HDS_data %<>% mutate(
  add_turnover      = HDS_richness - mean_QDS_richness,
  add_turnover_prop = add_turnover / HDS_richness,
  div_turnover      = HDS_richness / mean_QDS_richness
)

# ...
HDS_data %>%
  gather(response, value, -region, -PC1, -PC2) %>%
  gather(PC, score, PC1, PC2) %>%
  filter(response %in% c(
    "HDS_richness"#,
    #"mean_QDS_richness",
    #"mean_QDS_turnover",
    #"add_turnover_prop",
    #"div_turnover"
  )) %>%
  ggplot(aes(score, value, colour = region)) +
    geom_smooth(method = lm) +
    geom_point() +
    facet_grid(response ~ PC, scales = "free")

write_csv(HDS_data, "sent-to-coauthors/Cape-vs-SWA_variables_HDS_v4.csv")
write_csv(QDS_data, "sent-to-coauthors/Cape-vs-SWA_variables_QDS_v4.csv")

# Maps
# HDS
ggplot(HDS_data, aes(lon, lat, colour = HDS_richness)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free")
ggplot(HDS_data, aes(lon, lat, colour = mean_QDS_richness)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free")
ggplot(HDS_data, aes(lon, lat, colour = mean_QDS_turnover)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free")
ggplot(HDS_data, aes(lon, lat, colour = add_turnover_prop)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free")
ggplot(HDS_data, aes(lon, lat, colour = div_turnover)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free")
ggplot(HDS_data, aes(lon, lat, colour = rough_Elevation)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free")
ggplot(QDS_data, aes(lon, lat, colour = rough_Clay)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free")
ggplot(HDS_data, aes(lon, lat, colour = PC1)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free")
ggplot(HDS_data, aes(lon, lat, colour = PC2)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free")
# QDS
ggplot(QDS_data, aes(lon, lat, fill = QDS_richness)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_grid(~region, scales = "free")
ggplot(QDS_data, aes(lon, lat, fill = rough_Elevation)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_grid(~region, scales = "free")
ggplot(QDS_data, aes(lon, lat, fill = rough_Clay)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_grid(~region, scales = "free")
ggplot(QDS_data, aes(lon, lat, fill = PC1)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_grid(~region, scales = "free")
ggplot(QDS_data, aes(lon, lat, fill = PC2)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_grid(~region, scales = "free")

# ...
ggplot(QDS_data, aes(PC2, log(rough_Elevation), colour = region)) +
  geom_point()
ggplot(QDS_data, aes(PC2, log(rough_CEC), colour = region)) +
  geom_point()
