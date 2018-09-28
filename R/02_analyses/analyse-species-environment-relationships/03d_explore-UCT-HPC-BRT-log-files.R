# Exploring UCT HPC job output files, for debugging all-tc-lr runs
#   to find ideal BRT tc and lr presets
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

# Job 2117157 ------------------------------------------------------------------

# Read in all the job's *_log.txt files
job_2117157_logs <-
  list.files(full.names = TRUE, here(
    "outputs",
    "species-environment-relationships",
    "parallel-core-worker-logs",
    "all-tc-lr-BRTs",
    "from-UCT-HPC",
    "job-2117157-worker-logs"
  )) %>%
  map(readLines) %>%
  map(paste, collapse = "\n") %>%
  as_vector()

# Check if each model ran to both
all_did_run <- function(jobs, logged_unlogged) {
  stopifnot(is.character(logged_unlogged))
  did_run <- str_detect(jobs, pattern = logged_unlogged)
  print(glue(
    "{length(jobs[did_run])}/{length(jobs)} BRT-sets \\
    ({logged_unlogged}) did actually run"
  ))
}
all_did_run(job_2117157_logs, "logged")
all_did_run(job_2117157_logs, "unlogged")
