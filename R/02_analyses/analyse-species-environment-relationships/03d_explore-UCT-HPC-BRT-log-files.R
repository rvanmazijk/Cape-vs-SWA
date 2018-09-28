# Exploring UCT HPC job output files, for debugging all-tc-lr runs
#   to find ideal BRT tc and lr presets
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

# Job 2117157 ------------------------------------------------------------------

# .... Read in all the job's *_log.txt files
job_2117157_logs_paths <- list.files(full.names = TRUE, here(
  "outputs",
  "species-environment-relationships",
  "parallel-core-worker-logs",
  "all-tc-lr-BRTs",
  "from-UCT-HPC",
  "job-2117157-worker-logs"
))
job_2117157_logs <- job_2117157_logs_paths %>%
  map(readLines) %>%
  map(paste, collapse = "\n") %>%
  as_vector()
model_codes <- str_extract(
  job_2117157_logs_paths,
  "worker-\\d{1,}_tc-\\d_lr-[^_]{1,}\\d{4}-\\d{2}-\\d{2}_log.txt$"
)
# (Note the lack of underscores in places...
#   My bad. In early version of code I forgot to include those in
#   writeRDS() file argument)

# .... Define some functions to help -------------------------------------------

get_tc <- function(model_code) {
  model_code %>%
    str_extract("tc-\\d{1}") %>%
    str_remove("tc-") %>%
    as.numeric()
}
get_lr <- function(model_code) {
  model_code %>%
    str_extract("lr-[^_]{1,}") %>%
    str_remove("lr-") %>%
    str_remove("\\d{4}-\\d{2}-\\d{2}$") %>%  # Trim dates (see note above)
    as.numeric()
}
get_print_statements <- function(raw_log) {
  raw_log %>%
    # Get all lines that announce that a new BRT-model (one!) is being fit
    str_extract_all("\\[\\d\\]\ \\\"Fitting.+\n") %>%
    # Trim the "[1] \" stuff at the front of the print statement,
    map(str_remove_all, "\\[\\d\\]\ \\\"") %>%
    # and the \n at the back.
    map(str_remove_all, "\\\"\n")
}
is_logged <- function(print_statements) {
  str_detect(
    print_statements,
    "\\(logged\\)"
  )
}
get_model_type <- function(print_statements) {
  str_extract(
    print_statements,
    "(HDS_richness|mean_QDS_turnover)"
  )
}

# .... Check which model-sets failed part way ----------------------------------

job_checking_table <-
  tibble(
    model_code = model_codes,
    raw_log = job_2117157_logs
  ) %>%
  mutate(
    tc = get_tc(model_code),
    lr = get_lr(model_code),
    # Ascertain whether a specific model ran by searching the raw logs for clues
    print_statements = get_print_statements(raw_log)
  ) %>%
  unnest() %>%
  mutate(
    logged = is_logged(print_statements),
    model_type = ifelse(logged,
      paste0("log_", get_model_type(print_statements)),
      get_model_type(print_statements)
    )
  ) %>%
  group_by(tc, lr, model_type) %>%
  summarise(n = n()) %>%
  mutate(both_regions_ran = ifelse(n == 4, TRUE, NA)) %>%
  as.data.frame()

job_checking_table %>%
  group_by(tc, lr) %>%
  summarise(n = n()) %$%
  ifelse(all(n == 2),
    "All model presets were at least started/attempted, for both regions",
    NULL
  )

filter(job_checking_table, is.na(both_regions_ran))
