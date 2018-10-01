# Exploring UCT HPC job output files, for debugging all-tc-lr runs
#   to find ideal BRT tc and lr presets
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

# Define some functions to help
get_tc <- function(model_code) {
  model_code %>%
    str_extract("tc-\\d{1}") %>%
    str_remove("tc-") %>%
    as.numeric()
}
get_lr <- function(model_code, trim = c("date", "ext")) {
  lr <- model_code %>%
    str_extract("lr-[^_]{1,}") %>%
    str_remove("lr-")
  if (trim == "date") {
    lr %<>% str_remove("\\d{4}-\\d{2}-\\d{2}$")  # Trim dates (see note above)
  } else if (trim == "ext") {
    lr %<>% sans_ext()  # Trim .e and .o
  }
  as.numeric(lr)
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

# Job 2117157 ------------------------------------------------------------------

# .... Read in all the job's *_log.txt files -----------------------------------

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

# .... Check which model-sets failed part way ----------------------------------

job_checking_table <-
  tibble(
    model_code = model_codes,
    raw_log = job_2117157_logs
  ) %>%
  mutate(
    tc = get_tc(model_code),
    lr = get_lr(model_code, trim = "date"),
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
# Only the mean QDS turnover BRT-models fail...

# Jobs 2117578 to 2117602 ------------------------------------------------------

# .... Read in all the jobs' .e and .o files -----------------------------------
# (Didn't write capture.output() *_log.txt files for these)

jobs_e_o_files <-
  tibble(path = list.files(full.names = TRUE, here(
    "outputs",
    "species-environment-relationships",
    "parallel-core-worker-logs",
    "all-tc-lr-BRTs",
    "from-UCT-HPC",
    "jobs-2117578-to-2117602-e-o-files"
  ))) %>%
  mutate(
    path = str_extract(path, "outputs/.+$"),
    model_code = str_extract(path, "tc-\\d_lr-[^_]{1,}\\.(e|o)\\d{7}$"),
    tc = get_tc(model_code),
    lr = get_lr(model_code, trim = "ext"),
    e_o = path %>%
      str_extract("\\.(e|o)\\d{7}$") %>%
      str_remove("^\\.") %>%
      str_remove("\\d{7}$"),
    contents = map(here(path), read_file),
    print_statements = get_print_statements(contents)
  )

# The .e files have the gbm.step() etc. messages
jobs_e_o_files %>%
  filter(e_o == "e") %>%
  select(print_statements)  # All length = 0
jobs_e_o_files %>%
  filter(e_o == "e") %>%
  select(contents) %>%
  unnest()

# Print statements all went to .o files
jobs_e_o_files %>%
  filter(e_o == "o") %>%
  select(print_statements)
jobs_e_o_files %>%
  filter(e_o == "o") %>%
  select(contents) %>%
  unnest()

# Let's tidy up the dataframe accordingly
jobs_e_o_files %>%
  select(-contents, -print_statements) %>%
  mutate(
    output_ext = file_ext(path),
    path = sans_ext(path),
    model_code = sans_ext(model_code)
  ) %>%
  spread(e_o, path) %>%
  mutate(
    e_path = with_ext(e, output_ext),
    o_path = with_ext(o, output_ext)
  ) %>%
  select(-output_ext, -e, -o) %>%
  mutate(e_path = lag(e_path)) %>%
  na.omit()
