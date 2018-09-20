library(here)
source(here("R/setup.R"))
saved_BRT_paths <- list.files(full.names = TRUE, here(
  "outputs/species-environment-relationships/saved-BRT-RDSs/all-tc-lr-BRTs"
))
read_BRT_set <- function(path, max.level = 2, verbose = TRUE) {
  stopifnot(is.character(path))
  saved_BRT_set <- read_rds(path)
  if (class(saved_BRT_set) != "gbm") {
    class(saved_BRT_set) <- "gbm.object"
  }
  if (verbose) {
    str(saved_BRT_set, max.level = max.level)
  }
  saved_BRT_set
}
get_model_code <- function(path, model_code_pattern = NULL) {
  if (is.null(model_code_pattern)) {
    model_code_pattern <-
      "worker-\\d{1,}_tc-\\d_lr-[^_]{1,}_\\d{4}-\\d{2}-\\d{2}_BRTs\\.RDS$"
  }
  path %>%
    str_extract(model_code_pattern) %>%
    as_vector()
}
my_BRT_set_summary <- function(paths) {
  all_model_summaries <- tibble()
  for (path in paths) {
    saved_BRT_set <- read_BRT_set(path)
    model_code <- get_model_code(path)
    a_tc_lr_settings_summaries <- tibble()
    for (i in seq_along(saved_BRT_set)) {
      model_type <- names(saved_BRT_set)[[i]]
      a_types_summaries <- tibble()
      for (j in seq_along(saved_BRT_set[[i]])) {
        region <- names(saved_BRT_set[[i]])[[j]]
        a_regions_summary <- cbind(
          model_code,
          model_type,
          region,
          my_BRT_summary(saved_BRT_set[[i]][[j]])
        )
        a_types_summaries %<>% rbind(a_regions_summary)
      }
      a_tc_lr_settings_summaries %<>% rbind(a_types_summaries)
    }
    all_model_summaries %<>% rbind(a_tc_lr_settings_summaries)
  }
  as_tibble(all_model_summaries)
}
all_model_summaries <- saved_BRT_paths %>%
  my_BRT_set_summary() %>%
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
if (nrow(all_model_summaries) == length(saved_BRT_paths) * 4) {
  print(glue(
    "All {length(saved_BRT_paths)} * 4 = {length(saved_BRT_paths) * 4} \\
    models summarised"
  ))
}
all_model_summaries %>%
  #filter(richness_or_turnover == "HDS_richness_BRT") %>%
  mutate(nt = nt / 10000) %>%
  gather(
    diagnostic, value,
    nt, pseudo_r2, pred_obs_r2, pred_obs_r2_exp
  ) %>%
  ggplot(aes(tc, lr, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_grid(diagnostic ~ region)
