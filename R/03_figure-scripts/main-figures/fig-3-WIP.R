# Explore data visually --------------------------------------------------------

GCFR_biplots <- foreach(variable_ = GCFR_predictor_names_QDS) %do% {
  ggplot(GCFR_data_QDS, aes_string(variable_, "QDS_richness")) +
    geom_point() +
    scale_y_continuous(trans = "log")
}
names(GCFR_biplots) <- GCFR_predictor_names_QDS
GCFR_biplots$rough_Elevation

SWAFR_biplots <- foreach(variable_ = SWAFR_predictor_names_QDS) %do% {
  ggplot(SWAFR_data_QDS, aes_string(variable_, "QDS_richness")) +
    geom_point() +
    scale_y_continuous(trans = "log")
}
names(SWAFR_biplots) <- SWAFR_predictor_names_QDS
SWAFR_biplots$rough_Elevation

# Visualise BRT-model fits

GCFR_model %$%
  tibble(fit_log_richness = fitted, obs_log_richness = data$y) %>%
    ggplot(aes(obs_log_richness, fit_log_richness)) +
    geom_smooth(method = lm) +
    geom_abline(intercept = 0, slope = 1, lty = "dashed") +
    geom_point()
SWAFR_model %$%
  tibble(fit_log_richness = fitted, obs_log_richness = data$y) %>%
    ggplot(aes(obs_log_richness, fit_log_richness)) +
    geom_smooth(method = lm) +
    geom_abline(intercept = 0, slope = 1, lty = "dashed") +
    geom_point()
