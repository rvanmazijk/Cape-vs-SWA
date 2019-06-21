# Setup
setwd("~/Google Drive/work/projects/Cape-vs-SWA-extras/")
library(tidyverse)
library(magrittr)

# Import data
#QDS_data <- read_csv("sent-to-coauthors/Cape-vs-SWA_variables_QDS.csv")
QDS_data <- read_csv("sent-to-coauthors/BOTH_variables_QDS.csv")
#HDS_data <- read_csv("sent-to-coauthors/Cape-vs-SWA_variables_HDS_v2.csv")
HDS_data <- read_csv("sent-to-coauthors/BOTH_variables_HDS.csv")

# Perform PCAs of environmental data,
# separately for absolute and roughness variables
PCA_results <-
  list(
    #QDS_absol = QDS_data[,  3:11],
    QDS_rough = log(QDS_data[, 12:20]),
    #HDS_absol = HDS_data[,  5:13],
    HDS_rough = log(HDS_data[, 14:22])
  ) %>%
  map(prcomp, scale. = TRUE)

# Force scores to be positive if all vars rotations are negative
for (i in seq_along(PCA_results)) {
  PC1_rotations <- PCA_results[[i]]$rotation[, 1]
  PC1_scores    <- PCA_results[[i]]$x[, 1]
  if (all(PC1_rotations <= 0)) {
    PCA_results[[i]]$rotation[, 1] <- -PC1_rotations
    PCA_results[[i]]$x[, 1]        <- -PC1_scores
  }
}

# Have a look at the variance explained by PC1--3
map(PCA_results, function(x) {
  importances <- summary(x)$importance[2, 1:3]
  c(importances, cumulative = sum(importances))
})

# Restructure data for plotting
PCA_results_long <- map2(
  .x = PCA_results,
  .y = list(
    QDS_data$region, #QDS_data$region,
    HDS_data$region#, HDS_data$region
  ),
  ~ tibble(
    region = .y,
    PC1    = .x$x[, 1],
    PC2    = .x$x[, 2],
    PC3    = .x$x[, 3]
  )
)
# Or
#PCA_results_long <- map2(
#  .x = PCA_results,
#  .y = c(
#    QDS_data %$% list(region, region),
#    HDS_data %$% list(region, region)
#  ),
#  ~ .x %$%
#    tibble(
#      region = .y,
#      PC1    = x[, 1],
#      PC2    = x[, 2],
#      PC3    = x[, 3]
#    )
#)
# Or
#PCA_results_long <- map2(
#  .x = PCA_results,
#  .y = c(
#    QDS_data %$% list(region, region),
#    HDS_data %$% list(region, region)
#  ),
#  ~ as_tibble(cbind(
#    region = .y,
#    .x$x[, 1:3]
#  ))
#)

# Plot biplots
imap(PCA_results_long,
  ~ ggplot(.x) +
    aes(PC1, PC2, colour = region) +
    geom_point() +
    ggtitle(.y)
)
imap(PCA_results_long,
  ~ ggplot(.x) +
    aes(PC1, PC3, colour = region) +
    geom_point() +
    ggtitle(.y)
)

# Plot histograms of PC1--3 scores
#map(c(quote(PC1), quote(PC2), quote(PC3)), function(PC) {
#  imap(PCA_results_long,
#    ~ ggplot(.x) +
#      aes(!!PC, fill = region) +
#      geom_histogram(bins = 20, position = "dodge") +
#      ggtitle(.y)
#  )
#})
# Or
PCA_results_long %>%
  bind_rows(.id = "dataset") %>%
  separate(dataset, c("scale", "type")) %>%
  gather(PC, score, PC1:PC3) %>%
  ggplot(aes(score, fill = region)) +
    geom_histogram(bins = 20, position = "dodge") +
    facet_grid(scale + type ~ PC, scales = "free")

# Interrogate rotations associated with different variables
PCA_rotations <- map(PCA_results,
  ~ .x %$%
    rotation %>%
    {tibble(
      var = rownames(.),
      PC1 = .[, 1],
      PC3 = .[, 2],
      PC2 = .[, 3]
    )} %>%
    gather(PC, rotation, -var)
)
#imap(PCA_rotations,
#  ~ ggplot(.x, aes(var, rotation)) +
#    geom_col() +
#    facet_grid(~PC) +
#    ggtitle(.y) +
#    theme(axis.text.x = element_text(angle = 90))
#)
# Or
PCA_rotations %>%
  bind_rows(.id = "dataset") %>%
  separate(dataset, c("scale", "type")) %>%
  filter(scale == "HDS") %>%
  ggplot(aes(var, rotation)) +
    geom_col() +
    facet_wrap(~PC, scales = "free") +
    theme(axis.text.x = element_text(angle = 45))

# Combine data for plotting against richness and turnover
PCA_results_comb <- list(
  QDS = tibble(
    region        = PCA_results_long$QDS_rough$region,
    richness      = QDS_data$QDS_richness,
    #PC1_absol     = PCA_results_long$QDS_absol$PC1,
    PC1_rough     = PCA_results_long$QDS_rough$PC1,
    PC2_rough     = PCA_results_long$QDS_rough$PC2
  ),
  HDS = tibble(
    region        = PCA_results_long$HDS_rough$region,
    richness      = HDS_data$HDS_richness,
    mean_richness = HDS_data$mean_QDS_richness,
    turnover      = HDS_data$mean_QDS_turnover,
    #PC1_absol     = PCA_results_long$HDS_absol$PC1,
    PC1_rough     = PCA_results_long$HDS_rough$PC1,
    PC2_rough     = PCA_results_long$HDS_rough$PC2
  )
)

# Make Whittaker's additive turnover
PCA_results_comb$HDS %<>% mutate(
  add_turnover      = richness - mean_richness,
  add_turnover_prop = add_turnover / richness
)

# Plot QDS stuff
#map(c(quote(PC1_absol), quote(PC1_rough)),
#  ~ ggplot(PCA_results_comb$QDS) +
#    aes(!!.x, richness, colour = region) +
#    geom_point() +
#    geom_smooth(method = lm)
#)
# Or
PCA_results_comb %$%
  QDS %>%
  gather(PC, value, -region, -richness) %>%
  ggplot(aes(value, log(richness), colour = region)) +
    geom_point() +
    geom_smooth(method = lm) +
    facet_grid(~PC, scales = "free_x")

# Plot HDS stuff
# Against absolute environment PC
#PCA_results_comb %$%
#  HDS %>%
#  mutate(log_richness = log(richness)) %>%
#  gather(response, value, -region, -PC1_absol, -PC1_rough) %>%
#  ggplot(aes(PC1_absol, value, colour = region)) +
#    geom_point() +
#    geom_smooth(method = lm) +
#    facet_wrap(~response, scales = "free_y")
# Against roughness PCs
PCA_results_comb %$%
  HDS %>%
  mutate(log_richness = log(richness)) %>%
  gather(response, value, -region, -PC1_rough, -PC2_rough) %>%
  filter(response %in% c(
    "add_turnover_prop",
    "mean_richness",
    "richness",
    "turnover"
  )) %>%
  ggplot(aes(PC1_rough, value, colour = region)) +
    geom_smooth(method = lm) +
    geom_point() +
    facet_wrap(~response, scales = "free_y")
PCA_results_comb %$%
  HDS %>%
  mutate(log_richness = log(richness)) %>%
  gather(response, value, -region, -PC1_rough, -PC2_rough) %>%
  filter(response %in% c(
    "add_turnover_prop",
    "mean_richness",
    "richness",
    "turnover"
  )) %>%
  ggplot(aes(PC2_rough, value, colour = region)) +
    geom_smooth(method = lm) +
    geom_point() +
    facet_wrap(~response, scales = "free_y")

# ...
PCA_results_comb %$%
  HDS %>%
  mutate(mean_richness_prop = mean_richness / richness) %>%
  select(add_turnover_prop, mean_richness_prop) %>%
  as.data.frame() %>%
  prcomp() %$%
  x[, 1] %>%
  {tibble(
    region = PCA_results_comb$HDS$region,
    PC1 = .
  )} %>%
  ggplot(aes(PC1, fill = region)) +
    geom_histogram(bins = 20, position = "dodge")
