remove_dir <- function(x, levels_to_keep = 0) {
  stopifnot({
    is.character(x)
    is.numeric(levels_to_keep)
  })
  x %<>% str_remove("^/")
  n_levels <- str_count(x, "/")
  if (levels_to_keep == n_levels) {
    message("Why are you even using this function?")
  } else if (levels_to_keep > n_levels) {
    stop("Cannot remove more levels than path has")
  } else {
    for (i in 1:(n_levels - levels_to_keep)) {
      x %<>% str_remove("[^/]+/")
    }
    if (levels_to_keep == 0) {
      x %<>% str_remove("/")
    }
  }
  x
}
remove_duplicates <- function(data, index_name) {
  # Checks if any rows are duplicates according to some index,
  # then remove all but the first row w/ that index
  stopifnot({
    is.data.frame(data)
    is.character(index_name)
  })
  data$is_duplicate <- NA
  data$is_duplicate[[1]] <- FALSE  # necessarily so
  for (i in 2:nrow(data)) {
    data$is_duplicate[[i]] <-
      data[[index_name]][[i]] %in% data[[index_name]][1:(i - 1)]
  }
  data %>%
    filter(!is_duplicate) %>%
    select(-is_duplicate)
}
external_archive <- "/media/ruan/RUAN_UCT/projects/Cape-vs-SWA_archive/"
archive_filenames <- list.files(external_archive,
  full.names   = TRUE,
  recursive    = TRUE,
  include.dirs = FALSE
)
representative_model_filenames <- c(
  "GCFR_HDS-richness-gbm-step_BRT_2018-12-06_312.RDS",
  "GCFR_HDS-turnover-gbm-step_BRT_2018-12-11_746.RDS",
  "GCFR_QDS-richness-gbm-step_BRT_2018-12-04_305.RDS",
  "SWAFR_HDS-richness-gbm-step_BRT_2018-12-06_325.RDS",
  "SWAFR_HDS-turnover-gbm-step_BRT_2018-12-10_406.RDS",
  "SWAFR_QDS-richness-gbm-step_BRT_2018-12-03_59.RDS"
)
representative_models <-tibble(filename = archive_filenames) %>%
  mutate(sans_path = remove_dir(filename)) %>%
  filter(sans_path %in% representative_model_filenames) %>%
  mutate(rep_no = sans_path %>%
    # b/c some models' copies in > 1 archive folder
    str_extract("\\d+\\.RDS$") %>%
    str_remove("\\.RDS$")
  ) %>%
  remove_duplicates("rep_no") %>%
  pull(filename)
