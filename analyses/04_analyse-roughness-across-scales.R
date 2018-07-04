# Analyse environmental roughness varying across spatial scales
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
map(pre_analysis_import_paths, source)

set.seed(1234)

# Test 0.05deg, QDS, HDS, 3QDS comparisons -------------------------------------
# Using Mann-Whitney U tests to compare roughness values for GCFR vs SWAFR

test_results <- foreach(resolution = list(0.05, 0.25, 0.50, 0.75)) %do% {
  test_results_at_a_res <-
    map2_df(GCFR_variables,
            SWAFR_variables,
            compare_roughness,
            resolution = resolution,
            force_mann_whitney_u = TRUE) %>%
    cbind(variable = var_names, .) %>%
    as_tibble()
}
names(test_results) <- c("0.05º", "QDS", "HDS", "3QDS")

test_results_summary <- test_results %>%
  map(mutate, sig = p.value < 0.05) %>%
  map(dplyr::select, variable, sig) %$%
  tibble(variable = var_names,
         `0.05º` = .$`0.05deg`$sig,
         QDS = .$QDS$sig,
         HDS = .$HDS$sig,
         `3QDS` = .$`3QDS`$sig)

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
data_for_violin_plot <- data_for_violin_plot %$%
  rbind(cbind(resolution = "0.05º", .[[1]]),
        cbind(resolution = "QDS",   .[[2]]),
        cbind(resolution = "HDS",   .[[3]]),
        cbind(resolution = "3QDS",  .[[4]])) %>%
  as_tibble() %>%
  gather(variable, roughness, -resolution, -region) %>%
  na.omit() %>%
  group_by(resolution, variable) %>%
  mutate(z_roughness = scale(roughness))  # Z-scale that shit!
data_for_violin_plot$variable %<>% factor(levels = var_names)
data_for_violin_plot %<>% mutate(region = ifelse(region == "GCFR", "Cape", "SWA"))

# Save to disc
write_csv(
  data_for_violin_plot,
  here::here("outputs/04_roughness-across-scales/data_for_violin_plot.csv")
)

# Analyse IQ95R (and IQ99R) ~ scale --------------------------------------------

IQ95R_data <- data_for_violin_plot %>%
  group_by(resolution, variable) %>%
  mutate(z_roughness = scale(roughness)) %>%  # Z-scale that shit!
  group_by(resolution, region, variable) %>%
  summarise(IQ99R = IQ99R(z_roughness),
            IQ95R = IQ95R(z_roughness)) %>%
  gather(quantile, IXR,
         -resolution, -region, -variable) %>%
  mutate(quantile = ifelse(quantile == "IQ99R",
                           0.99,
                           ifelse(quantile == "IQ95R",
                                  0.95,
                                  NA)))
IQ95R_data %<>% mutate(region = ifelse(region == "GCFR", "Cape", "SWA"))

# Save to disc
write_csv(
  IQ95R_data,
  here::here("outputs/04_roughness-across-scales/IQ95R_data.csv")
)

# Junk -------------------------------------------------------------------------

# Plot lm fits of 95%-interquantile range ~ scale * region
#ggplot(IXR_data %>%
#           filter(quantile == 0.95),
#       aes(as.numeric(resolution), IXR, col = region)) +
#    geom_point() +
#    geom_smooth(method = "lm")

# Plot difference in GCFR and SWAFR 95%-interquantile ranges ~ scale
#IXR_data_diff <- IXR_data  %>%
#    group_by(region, resolution, variable, quantile) %>%
#    summarise(IXR) %>%
#    spread(region, IXR) %>%
#    mutate(diff = GCFR - SWAFR)
#ggplot(IXR_data_diff %>%
#           filter(quantile == 0.95),
#       aes(resolution, diff,
#           col = variable,
#           #alpha = quantile,
#           group = variable)) +
#    geom_point() +
#    geom_path() +
#    facet_wrap(~ variable) +
#    geom_hline(yintercept = 0, lty = "dashed") +
#    ylab("GCFR - SWAFR")

# Plot lm fit of diff in 95%-interquantile range ~ scale
#ggplot(IXR_data_diff %>%
#           filter(quantile == 0.95),
#       aes(as.numeric(resolution), diff)) +
#    geom_point() +
#    geom_smooth(method = "lm")
