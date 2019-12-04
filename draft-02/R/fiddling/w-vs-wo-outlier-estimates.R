with_outliers <- read_csv(here("draft-02/manuscript_ver3-4/results/model-summary-for-Tony.csv"))
sans_outliers <- read_csv(here("draft-02/manuscript_ver3-4/results/refit-model-summary-for-Tony.csv"))
models <-
  full_join(with_outliers, sans_outliers, by = c("response", "term"), suffix = c("_wo", "_so")) %>%
  filter(term != "(Intercept)") %>%
  mutate(term_type = term %>%
    str_remove("region(GC|SWA)FR") %>%
    str_remove("\\:")
  ) %>%
  filter(term_type != "(Intercept)")

ggplot(models, aes(estimate_wo, estimate_so, colour = term_type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey75") +
  geom_vline(xintercept = 0, size = 0.1) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_point() +
  facet_wrap(~response, scales = "free")
