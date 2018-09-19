library(here)
source(here("R/setup.R"))
saved_BRT_paths <- list.files(full.names = TRUE, here(
  "outputs/species-environment-relationships/saved-BRT-RDSs/all-tc-lr-BRTs"
))
# FIXME: only read in latest run of models
saved_BRTS_summaries <- map_df(
  .x = saved_BRT_paths,
  .f = function(.x) {
    saved_BRT <- read_rds(.x)
    if (class(saved_BRT) != "gbm") {
      class(saved_BRT) <- "gbm.object"
    }
    #print(saved_BRT)
    model_code_pattern <-
      "worker-\\d{1,}_tc-\\d_lr-[^_]{1,}_\\d{4}-\\d{2}-\\d{2}_BRTs\\.RDS$"
    model_code <- .x %>%
      str_extract(model_code_pattern) %>%
      as_vector()
    model_summaries <- map_df(
      .x = saved_BRT,
      .id = "richness_or_turnover",
      .f = ~ map_df(
        .x = .x,
        .id = "region",
        .f = my_BRT_summary
      )
    )
    #print(model_summaries)
    model_summaries <-
      cbind(model_code, model_summaries) %>%
      as_tibble() %>%
      mutate(
        tc = model_code %>%
          str_extract("tc-\\d{1}") %>%
          str_remove("tc-") %>%
          as.numeric(),
        lr = model_code %>%
          str_extract("lr-[^_]{1,}") %>%
          str_remove("lr-") %>%
          factor(levels = c("1e-04", "5e-04", "0.001", "0.005", "0.01"))
      )
    print(nrow(model_summaries))
    model_summaries
  }
)
saved_BRT_summary_plots <- foreach(region_ = c("Cape", "SWA")) %do% {
  foreach(richness_or_turnover_ = c("HDS_richness_BRT", "mean_QDS_turnover_BRT")) %do% {
    foreach(diagnostic_ = c("nt", "pseudo_r2", "pred_obs_r2")) %do% {
      saved_BRTS_summaries %>%
        filter(
          region == region_,
          richness_or_turnover == richness_or_turnover_
        ) %>%
        ggplot(aes_string("tc", "lr", fill = diagnostic_)) +
          geom_tile() +
          scale_fill_viridis_c(limits = case_when(
            diagnostic_ == "nt" ~ c(0, 10000),
            diagnostic_ == "pseudo_r2" ~ c(0, 1),
            diagnostic_ == "pred_obs_r2" ~ c(0, 1)
          )) +
          ggtitle(glue("{region_}, {richness_or_turnover_}"))
    }
  }
}
saved_BRT_summary_plots %>%
  flatten() %>%
  flatten() %>%
  plot_grid(plotlist = ., nrow = 4, ncol = 3)
# OR
saved_BRTS_summaries %>%
  filter(richness_or_turnover == "HDS_richness_BRT") %>%
  mutate(nt = nt / 10000) %>%
  gather(
    diagnostic, value,
    nt, pseudo_r2, pred_obs_r2
  ) %>%
  ggplot(aes(tc, lr, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c() +
    facet_grid(diagnostic ~ region)
# OR (BW-version)
saved_BRTS_summaries %>%
  select(-contribs) %>%
  filter(richness_or_turnover == "HDS_richness_BRT") %>%
  mutate(nt = nt / 10000) %>%
  gather(
    diagnostic, value,
    nt, pseudo_r2, pred_obs_r2
  ) %>%
  ggplot(aes(tc, value, col = lr)) +
    geom_point() +
    scale_color_viridis_d() +
    facet_grid(diagnostic ~ region + lr)
