# Make Fig. 5 (BRT variable rank concordance)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

output_path <- here(
  "outputs/species-environment-relationships/",
  "from-local-machines"
)

# Import BRT contribution data -------------------------------------------------
# (Copy-pasted code from fig-3-*.R)
# TODO: generate data below elsewhere as is used by both this figure script
#   and figure 3's

contribution_data <- map_df(
  c(
    "QDS-richness-models-contributions.csv",
    "HDS-richness-models-contributions.csv",
    "HDS-turnover-models-contributions.csv"
  ),
  ~ cbind(path = .x, read_csv(glue("{output_path}/{.x}")))
)

contribution_data %<>%
  as_tibble() %>%
  mutate(
    region = case_when(
      region == "GCFR" ~ "Cape",
      region == "SWAFR" ~ "SWA"
    ),
    response = str_extract(path, "(richness|turnover)"),
    scale = str_extract(path, "(QDS|HDS)")
  ) %>%
  mutate(model_name = paste(region, response, scale)) %>%
  select(-path, -rep)

zero_contrib_vars <- map_dfr(
  list(
    list(region = "Cape", response = "richness", scale = "QDS"),
    list(region = "Cape", response = "richness", scale = "HDS"),
    list(region = "Cape", response = "turnover", scale = "HDS"),
    list(region = "SWA",  response = "richness", scale = "QDS"),
    list(region = "SWA",  response = "richness", scale = "HDS"),
    list(region = "SWA",  response = "turnover", scale = "HDS")
  ),
  ~ tibble(
    region = .x$region,
    response = .x$response,
    scale = .x$scale,
    model_name = paste(.x$region, .x$response, .x$scale),
    var = get_zero_contrib_vars(.x$region, .x$response, .x$scale),
    rel.inf = 0,
    model_type = "replicates"
  )
)
contribution_data %<>% full_join(zero_contrib_vars)
any(contribution_data$rel.inf == 0)
contribution_data %>% filter(rel.inf == 0)

contribution_data %<>%
  mutate(
    var_class = case_when(
      var %in% c(var_names[[1]], rough_var_names[[1]]) ~ "Elevation",
      var %in% c(var_names[2:4], rough_var_names[2:4]) ~ "Climate",
      var %in% c(var_names[[5]], rough_var_names[[5]]) ~ "NDVI",
      var %in% c(var_names[6:9], rough_var_names[6:9]) ~ "Soil"
    ),
    var_type = str_extract(var, "rough")
  ) %>%
  mutate(var_type = ifelse(is.na(var_type), "absolute", var_type))
contribution_data$var_class %<>% factor(levels = c(
  "Elevation",
  "Climate",
  "NDVI",
  "Soil"
))

contribs_data_ranks <- contribution_data %>%
  filter(model_type == "replicates") %>%
  group_by(
    region, model_name,
    var, var_class, var_type
  ) %>%
  summarise(mean_rel.inf = mean(rel.inf, na.rm = TRUE)) %>%
  mutate(mean_rel.inf = ifelse(mean_rel.inf == 0, NA, mean_rel.inf)) %>%
  ungroup() %>%
  group_by(model_name) %>%
  mutate(rank = rank(-mean_rel.inf, na.last = "keep")) %>%
  ungroup() %>%
  mutate(model_name = model_name %>%
    str_remove("(Cape|SWA) ") %>%
    factor(levels = c(
      "turnover HDS", "richness HDS", "richness QDS"
    ))
  )

contribs_rank_diffs <- contribs_data_ranks %>%
  select(-mean_rel.inf) %>%
  mutate(model_name = str_replace_all(model_name, " ", "_")) %>%
  group_by(region) %>%
  spread(model_name, rank) %>%
  mutate(
    diff_richness_QDS_HDS = abs(richness_QDS - richness_HDS),
    diff_richness_turnover_HDS = abs(richness_HDS - turnover_HDS)
  )

# Have a look:
contribs_rank_diffs %>%
  select(region, var, diff_richness_QDS_HDS, diff_richness_turnover_HDS) %>%
  as.data.frame()

contribs_rank_diffs2 <- contribs_rank_diffs %>%
  gather(
    comparison, rank_diff,
    diff_richness_QDS_HDS, diff_richness_turnover_HDS
  ) %>%
  group_by(region, comparison) %>%
  summarise(mean_rank_diff = rank_diff %>%
    mean(na.rm = TRUE) %>%
    round(2)
  ) %>%
  mutate(var_class = NA, var_type = NA) %>%
  split(.$comparison)
contribs_rank_diffs2

# Compute *randomised null* average change in rank between two sets
random_mean_rank_diffs <- function(data, label_column,
                                   rank_column1, rank_column2) {
  set.seed(1234)
  replicate(999, simplify = "vector", {
    data <- data[, c(
      label_column,
      rank_column1, rank_column2
    )]
    data[[rank_column1]] %<>% sample()
    data[[rank_column2]] %<>% sample()
    rank_diffs <- abs(data[[rank_column1]] - data[[rank_column2]])
    mean(rank_diffs, na.rm = TRUE)
  })
}
richness_QDS_HDS_nulls <- foreach(region_ = c("Cape", "SWA")) %do% {
  random_mean_rank_diffs(
    ungroup(contribs_rank_diffs)[contribs_rank_diffs$region == region_, ],
    label_column = "var",
    rank_column1 = "richness_QDS", rank_column2 = "richness_HDS"
  )
}
names(richness_QDS_HDS_nulls) <- c("Cape", "SWA")
richness_turnover_HDS_nulls <- foreach(region_ = c("Cape", "SWA")) %do% {
  random_mean_rank_diffs(
    ungroup(contribs_rank_diffs)[contribs_rank_diffs$region == region_, ],
    label_column = "var",
    rank_column1 = "richness_HDS", rank_column2 = "turnover_HDS"
  )
}
names(richness_turnover_HDS_nulls) <- c("Cape", "SWA")

obs_null_test <- function(obs, null) {
  hist(null)
  abline(v = obs, lty = "dashed")
  obs_rank <- rank(c(obs, null))[1]
  p_value <- obs_rank / (length(null) + 1)
  data.frame(obs_rank, length_null = length(null), p_value)
}
contribs_rank_p_values <- list(
  richness_QDS_HDS = rbind(
    cbind(
      region = "Cape", var_class = NA, var_type = NA,
      obs_null_test(
        obs = contribs_rank_diffs2$diff_richness_QDS_HDS$mean_rank_diff[1],
        null = richness_QDS_HDS_nulls$Cape
      )
    ),
    cbind(
      region = "SWA", var_class = NA, var_type = NA,
      obs_null_test(
        obs = contribs_rank_diffs2$diff_richness_QDS_HDS$mean_rank_diff[2],
        null = richness_QDS_HDS_nulls$SWA
      )
    )
  ),
  richness_turnover_HDS = rbind(
    cbind(
      region = "Cape", var_class = NA, var_type = NA,
      obs_null_test(
        obs = contribs_rank_diffs2$diff_richness_turnover_HDS$mean_rank_diff[1],
        null = richness_turnover_HDS_nulls$Cape
      )
    ),
    cbind(
      region = "SWA", var_class = NA, var_type = NA,
      obs_null_test(
        obs = contribs_rank_diffs2$diff_richness_turnover_HDS$mean_rank_diff[2],
        null = richness_turnover_HDS_nulls$SWA
      )
    )
  )
)

contribs_rank_p_values <-
  map2(contribs_rank_diffs2, contribs_rank_p_values, full_join)

# Plot lines connecting variables ordered by rank ------------------------------
# See ranked-list-concordance.R

contribs_data_ranks %<>% mutate(
  model_name = case_when(
    model_name == "richness QDS" ~ "1 - richness QDS",
    model_name == "richness HDS" ~ "2 - richness HDS",
    model_name == "turnover HDS" ~ "3 - turnover HDS"
  ) %>%
  factor(levels = c(
    "3 - turnover HDS",
    "2 - richness HDS",
    "1 - richness QDS"
  ))
)

contribs_rank_plot <-
  ggplot(contribs_data_ranks,
    aes(model_name, rank, col = var_class, fill = var_type)
  ) +
  geom_line(aes(group = var)) +
  geom_point(size = 2, shape = 21, stroke = 1.05) +  #aes(size = mean_rel.inf)
  geom_text(
    data = as.data.frame(contribs_rank_p_values$diff_richness_QDS_HDS),
    col = "grey25", size = 3,
    x = 3.3, y = 7.5,
    aes(label = glue(  #bar(abs(italic(Delta)))
      "Delta[1-2] == {mean_rank_diff} ~ ~\\
      (italic(P) == {p_value})"
    )),
    parse = TRUE
  ) +
  geom_text(
    data = as.data.frame(contribs_rank_p_values$diff_richness_turnover_HDS),
    col = "grey25", size = 3,
    x = 0.7, y = 7.5,
    aes(label = glue(  #bar(abs(italic(Delta)))
      "Delta[2-3] == {mean_rank_diff} ~ ~\\
      (italic(P) == {p_value})"
    )),
    parse = TRUE
  ) +
  labs(x = "Model", y = "Rank of influence (decreasing)") +
  coord_flip() +
  scale_colour_manual(values = var_colours, na.translate = FALSE) +
  scale_fill_manual(values = c("white", "black")) +
  facet_grid(~ region) +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Save to disc -----------------------------------------------------------------

ggsave(
  here("figures/fig-5-BRT-variable-rank-concordance.png"),
  contribs_rank_plot,
  width = 6, height = 2.25,
  dpi = 300
)
