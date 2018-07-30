# Analyse environmental roughness varying across spatial scales
# (Now with jackknife-sampling!)
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
map(pre_analysis_import_paths, source)

set.seed(1234)

# Test
if (FALSE) {
  result <- list(SWAFR_variables[[1]], GCFR_variables[[1]]) %>%
    map(~ prep_layer2(.x, resolution = 0.25))
      x = ..1, y = ..2,
      resolution = 0.25,
      n = 1, size = 500
    )
  )
  result %>%
    map(bind_rows) %>%
    map(summarise_if,
        is.numeric,
        .funs = list(mean = mean, sd = sd)) %>%
    bind_rows(.id = "variable")
  # Works!
}

# Run on QDS, HDS, 3QDS
resolutions <- c(0.05, 0.25, 0.50, 0.75)
bootstrap_results <- vector("list", length = length(resolutions))
for (i in 2:4) {  # Doing 0.05º separately, using disc, below
  result <- pmap(
    .l = list(
      ..1 = SWAFR_variables,
      ..2 = GCFR_variables,
      ..3 = var_names
    ),
    .f = ~ compare_roughness_bootstrapped(
      x = ..1, x_region_name = "SWAFR",
      y = ..2, y_region_name = "GCFR",
      variable = ..3, resolution = resolutions[[i]],
      n_samples = 1000,
      use_disc = TRUE  # These can all fit in memory, but using disc just in case
    )
  )
  bootstrap_results[[i]] <- result %>%
    map(bind_rows) %>%
    map(summarise_if,
        is.numeric,
        .funs = list(mean = mean, sd = sd)) %>%
    bind_rows(.id = "variable")
}
#names(bootstrap_results) <- c("0.05º", "QDS", "HDS", "3QDS")
names(bootstrap_results) <- c("QDS", "HDS", "3QDS")
bootstrap_results %<>% bind_rows(.id = "resolution")

pos <- position_dodge(0.15)
ggplot(bootstrap_results,
       aes(resolution, CLES_mean,
           col = variable)) +
  geom_point(position = pos) +
  geom_linerange(aes(ymin = CLES_mean - CLES_sd,
                     ymax = CLES_mean + CLES_sd),
                 position = pos) +
  geom_line(aes(group = variable), position = pos)

# Do for 0.05 "manually", as is an onerous computation
# TODO: Re-run this, as my first run was before I fixed the mistake
#       where bootstrap_sample() returned the matrix sideways (stupid default)
result_0.05 <- pmap(
  .l = list(
    ..1 = SWAFR_variables,
    ..2 = GCFR_variables,
    ..3 = var_names
  ),
  .f = ~ compare_roughness_bootstrapped(
    x = ..1, x_region_name = "SWAFR",
    y = ..2, y_region_name = "GCFR",
    variable = ..3, resolution = resolutions[[1]],
    n_samples = 1000,
    use_disc = TRUE
  )
)
bootstrap_results[[1]] <- result_0.05 %>%
  map(bind_rows) %>%
  map(summarise_if,
      is.numeric,
      .funs = list(mean = mean, sd = sd)) %>%
  bind_rows(.id = "variable")
bootstrap_results_0.05 <- result_0.05 %>%
  map(bind_rows) %>%
  map(summarise_if,
      is.numeric,
      .funs = list(mean = mean, sd = sd)) %>%
  bind_rows(.id = "variable")

#! Old script continues here ---------------------------------------------------

# Save to disc
write_csv(
  test_results_summary,
  here::here("outputs/04_roughness-across-scales/test_results_summary.csv")
)

# CLES for those tests ---------------------------------------------------------

# <https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test#Effect_sizes>
test_results_CLES <- foreach(resolution = list(0.05, 0.25, 0.50, 0.75)) %do% {
  out <- foreach(GCFR = GCFR_variables, SWAFR = SWAFR_variables) %do% {
    canprot::CLES(
      na.omit(prep_layer(GCFR)[]),
      na.omit(prep_layer(SWAFR)[])
    )
  }
  names(out) <- var_names
  out
}
names(test_results_CLES) <- c("0.05º", "QDS", "HDS", "3QDS")

test_results_CLES_for_plot <- test_results_CLES %>%
  map(as_tibble) %$%
  rbind(.$`0.05º`, .$QDS, .$HDS, .$`3QDS`) %>%
  cbind(resolution = names(test_results_CLES), .) %>%
  gather(variable, CLES, -resolution) %>%
  as_tibble()

# Save to disc
write_csv(
  test_results_CLES_for_plot,
  here::here("outputs/04_roughness-across-scales/test_results_CLES_for_plot.csv")
)

# Prep data for violin plot ----------------------------------------------------

data_for_violin_plot <- foreach(resolution = list(0.05, 0.25, 0.50, 0.75)) %do% {
  rbind(
    cbind(region = "GCFR", map2_df(GCFR_variables, resolution, prep_layer)),
    cbind(region = "SWAFR", map2_df(SWAFR_variables, resolution, prep_layer))
  )
}
data_for_violin_plot_tidy <- data_for_violin_plot %$%
  rbind(cbind(resolution = "0.05º", .[[1]]),
        cbind(resolution = "QDS",   .[[2]]),
        cbind(resolution = "HDS",   .[[3]]),
        cbind(resolution = "3QDS",  .[[4]])) %>%
  as_tibble() %>%
  gather(variable, roughness, -resolution, -region) %>%
  na.omit() %>%
  group_by(resolution, variable) %>%
  mutate(z_roughness = scale(roughness)) %>%  # Z-scale!
  ungroup() %>%
  mutate(variable = factor(variable, levels = var_names),
         region = ifelse(region == "GCFR", "Cape", "SWA"))

# Save to disc
write_csv(
  data_for_violin_plot_tidy,
  here::here("outputs/04_roughness-across-scales/data_for_violin_plot.csv")
)

# Analyse IQ95R (and IQ99R) ~ scale --------------------------------------------

IQ95R_data <- data_for_violin_plot_tidy %>%
  group_by(resolution, region, variable) %>%
  summarise(IQ99R = IQ99R(z_roughness),
            IQ95R = IQ95R(z_roughness)) %>%
  gather(quantile, IXR,
         -resolution, -region, -variable) %>%
  mutate(quantile = ifelse(quantile == "IQ99R",
                           0.99,
                           ifelse(quantile == "IQ95R",
                                  0.95,
                                  NA))) %>%
  ungroup()

# Save to disc
write_csv(
  IQ95R_data,
  here::here("outputs/04_roughness-across-scales/IQ95R_data.csv")
)
