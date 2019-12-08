model_ANOVAs <- read_csv(here(
  "draft-02/manuscript_ver3-4/results",
  "model-ANOVA-for-Tony.csv"
))
model_ANOVAs %>%
  #filter(term != "Residuals") %>%
  arrange(desc(var_explained)) %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  ggplot(aes(term, var_explained)) +
    geom_col() +
    facet_grid(~response, scales = "free_x") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
