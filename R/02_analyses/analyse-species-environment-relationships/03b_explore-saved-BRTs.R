library(here)
source(here("R/setup.R"))
saved_BRTs <- list.files(full.names = TRUE, here(
  "outputs/species-environment-relationships/saved-BRT-RDSs/all-tc-lr-BRTs"
))
saved_BRTS_summaries <- map_df(
  .x = saved_BRTs,
  .f = function(.x) {
    model_code <- .x %>%
      str_extract_all(
        "worker-\\d{1,}_tc-\\d_lr-.{1,}_\\d{4}-\\d{2}-\\d{2}_BRTs\\.RDS$"
      ) %>%
      str_split("_") %>%
      print()
    #
    #  tibble(tc = .[[2]], lr = .[[3]]) %>%
    #  map_df(~ str_remove(., "[a-z]{2}-")) %>%
    #  map_df(as.numeric)
    model_summaries <- map(read_rds(.x), map, my_BRT_summary)
    model_summaries <- cbind(model_code, model_summaries = model_summaries)
    model_summaries
  }
)
