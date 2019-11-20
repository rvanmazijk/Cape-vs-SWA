data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

predictor_names <- c(str_replace_all(var_names, " ", "_"), "PC1")

full_formula <- predictor_names[predictor_names != "PC1"] %>%
  {c(., paste(., "* region"))} %>%
  paste(collapse = " + ")

m_QDS_richness <- vector("list", length = 1000)
m_HDS_richness <- vector("list", length = 1000)
m_DDS_richness <- vector("list", length = 1000)
for (i in 1:1000) {
  m_QDS_richness[[i]] <- lm(
    glue("QDS_richness ~ {full_formula}"),
    data$QDS %>%
      group_by(region) %>%
      sample_frac(0.5)
  )
  m_HDS_richness[[i]] <- lm(
    glue("HDS_richness ~ {full_formula}"),
    data$HDS %>%
      group_by(region) %>%
      sample_frac(0.5)
  )
  m_DS_richness[[i]] <- lm(
    glue("DS_richness ~ {full_formula}"),
    data$DS %>%
      group_by(region) %>%
      sample_frac(0.5)
  )
}
m_QDS_richness_tidy <- map_dfr(m_QDS_richness, tidy, .id = "rep")
m_HDS_richness_tidy <- map_dfr(m_HDS_richness, tidy, .id = "rep")
m_DS_richness_tidy  <- map_dfr(m_DS_richness,  tidy, .id = "rep")
m_richness_tidy <- bind_rows(.id = "scale", list(
  QDS = m_QDS_richness_tidy,
  HDS = m_HDS_richness_tidy,
  DS  = m_DS_richness_tidy
))
m_richness_tidy %<>%
  group_by(scale, term) %>%
  summarise_at(vars("estimate", "p.value"), list(mean = mean, sd = sd)) %>%
  filter(term != "(Intercept)") %>%
  mutate(term_type = str_remove(term, "\\:?regionSWAFR")) %>%
  filter(term_type != "") %>%
  mutate(
    estimate_upp = estimate_mean + estimate_sd,
    estimate_low = estimate_mean - estimate_sd,
     p.value_upp =  p.value_mean + p.value_sd,
     p.value_low =  p.value_mean - p.value_sd
  )

ggplot(m_richness_tidy, aes(p.value_mean, estimate_mean, colour = term_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey75") +
  geom_label(aes(label = term_type), vjust = 0, hjust = 0) +
  geom_point() +
  geom_errorbar( aes(ymin = estimate_low, ymax = estimate_upp), width  = 0) +
  geom_errorbarh(aes(xmin =  p.value_low,  xmax = p.value_upp), height = 0) +
  facet_grid(scale ~ ., scales = "free_y")

ggplot(m_richness_tidy, aes(term, estimate_mean, colour = term_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey75") +
  geom_point() +
  geom_errorbar( aes(ymin = estimate_low, ymax = estimate_upp), width  = 0) +
  facet_grid(~scale, scales = "free_x") +
  coord_flip()

