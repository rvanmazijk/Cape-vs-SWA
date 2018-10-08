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
rm_print_statements <- function(raw_log) {
  str_remove_all(raw_log, "Fitting.+\n")
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
  "from-UCT-HPC",
  "all-tc-lr-BRTs",
  "worker-logs",
  "job-2117157-worker-logs"
))
job_2117157_logs <- job_2117157_logs_paths %>%
  map(readLines) %>%
  map(paste, collapse = "\n") %>%
  as_vector()2128806
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
    logged = is_logged(print_statements),  # To double check
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
    "from-UCT-HPC",
    "all-tc-lr-BRTs",
    "worker-logs",
    "jobs-2117578-to-2117602-e-o-files"
  ))) %>%
  mutate(
    path = str_extract(path, "outputs/.+$"),
    output_ext = file_ext(path),
    model_code = path %>%
      str_extract("tc-\\d_lr-[^_]{1,}\\.(e|o)\\d{7}$"),
    tc = get_tc(model_code),
    lr = get_lr(model_code, trim = "ext"),
    e_o = path %>%
      str_extract("\\.(e|o)\\d{7}$") %>%
      str_remove("^\\.") %>%
      str_remove("\\d{7}$"),
    contents = map(here(path), read_file),
    my_print_statements = get_print_statements(contents)
  )

# .... Check which model-sets failed part way ----------------------------------

# The .e files have the gbm.step() etc. messages
jobs_e_o_files %>%
  filter(e_o == "e") %>%
  select(my_print_statements)  # All length = 0
jobs_e_o_files %>%
  filter(e_o == "e") %>%
  select(contents) %>%
  unnest()

# Print statements all went to .o files
jobs_e_o_files %>%
  filter(e_o == "o") %>%
  select(my_print_statements)
jobs_e_o_files %>%
  filter(e_o == "o") %>%
  select(contents) %>%
  unnest()

# Let's tidy up the dataframe accordingly
jobs_e_o_files %<>%
  select(-path, -output_ext, -model_code, -e_o) %>%
  rename(gbm_messages = contents) %>%
  mutate(gbm_messages = gbm_messages %>%
    lag() %>%
    map(rm_print_statements) %>%
    str_split("\n")
  ) %>%
  filter(map(my_print_statements, length) > 0)

# Let's have a look now
jobs_e_o_files %>%
  select(-my_print_statements) %>%
  unnest() %>%
  filter(str_detect(gbm_messages, "Error"))
jobs_e_o_files %>%
  select(-gbm_messages) %>%
  unnest() %>%
  mutate()

# Jobs 2128782 to 2128806 ------------------------------------------------------

# .... Read in all the jobs' .e and .o files -----------------------------------
# (Didn't write capture.output() *_log.txt files for these)

jobs_e_o_files <-
  tibble(path = list.files(full.names = TRUE, here(
    "outputs",
    "species-environment-relationships",
    "from-UCT-HPC",
    "all-tc-lr-BRTs",
    "worker-logs",
    "jobs-2128782-to-2128806-e-o-files"
  ))) %>%
  mutate(
    path = str_extract(path, "outputs/.+$"),
    output_ext = file_ext(path),
    model_code = path %>%
      str_extract("tc-\\d_lr-[^_]{1,}\\.(e|o)\\d{7}$"),
    tc = get_tc(model_code),
    lr = get_lr(model_code, trim = "ext"),
    e_o = path %>%
      str_extract("\\.(e|o)\\d{7}$") %>%
      str_remove("^\\.") %>%
      str_remove("\\d{7}$"),
    contents = map(here(path), read_file)
  ) %>%
  filter(str_detect(contents, "")) %>%  # Removes empty .o files
  select(-path, -output_ext, -model_code, -e_o) %>%
  mutate(contents = str_split(contents, "\n")) %>%
  unnest()

# .... Check which model-sets failed part way ----------------------------------

View(jobs_e_o_files)
# I found some runs with no simpler predictor sets found by gbm.simplify()...
# They bombed out
# Back to the drawing board!

# Jobs 2130463 to 2130487 ------------------------------------------------------

# .... Read in all the jobs' .e and .o files -----------------------------------
# (Didn't write capture.output() *_log.txt files for these)

jobs_e_o_files <-
  tibble(path = list.files(full.names = TRUE, here(
    "outputs",
    "species-environment-relationships",
    "from-UCT-HPC",
    "all-tc-lr-BRTs",
    "worker-logs",
    "jobs-2130463-to-2130487-e-o-files"
  ))) %>%
  mutate(
    path = str_extract(path, "outputs/.+$"),
    output_ext = file_ext(path),
    model_code = path %>%
      str_extract("tc-\\d_lr-[^_]{1,}\\.(e|o)\\d{7}$"),
    tc = get_tc(model_code),
    lr = get_lr(model_code, trim = "ext"),
    e_o = path %>%
      str_extract("\\.(e|o)\\d{7}$") %>%
      str_remove("^\\.") %>%
      str_remove("\\d{7}$"),
    contents = map(here(path), read_file)
  ) %>%
  filter(str_detect(contents, "")) %>%  # Removes empty .o files
  select(-path, -output_ext, -model_code, -e_o) %>%
  mutate(contents = str_split(contents, "\n")) %>%
  unnest()

# .... Check which model-sets failed part way ----------------------------------

View(jobs_e_o_files)

# .... Plots -------------------------------------------------------------------

brt_paths <- list.files(full.names = TRUE, here(
  "outputs",
  "species-environment-relationships",
  "from-UCT-HPC",
  "all-tc-lr-BRTs",
  "saved-BRT-RDS",
  "jobs-2130463-to-2130487-RDS"
))
saved_brt_summaries <- imap_dfr(brt_paths, function(.x, .y) {
  saved_brt <- read_rds(.x)
  print(map(saved_brt, summary))
  message(glue("{.y}/25 BRT-sets read into memory"))
  model_summaries <-
    map_df(saved_brt, .id = "response", function(.x) {
      map_df(.x, .id = "region", function(.x) {
        if (is.null(.x)) {
          return(tibble(
            nt = NA,
            pseudo_r2 = NA,
            pred_obs_r2 = NA,
            pred_obs_r2_exp = NA,
            contribs = NA
          ))
        } else {
          class(.x) <- "gbm"
          return(my_BRT_summary(.x))
        }
      })
    }) %>%
    cbind(path = .x, .) %>%
    as_tibble() %>%
    mutate(
      tc = get_tc(path),
      lr = path %>%
        str_extract("tc-\\d_.+$") %>%
        str_remove("_BRTs\\.RDS$") %>%
        get_lr(trim = "date")
    )
  message(glue("{.y}/25 BRT-sets summarised"))
  message(glue("================================="))
  model_summaries
})
saved_brt_summaries %>%
  group_by(tc, lr, region, response) %>%
  summarise(n = n()) %$%
  all(n == 1)
foreach(response_ = c("HDS_richness_BRT", "mean_QDS_turnover_BRT")) %do% {
  saved_brt_summaries %>%
    filter(response == response_) %>%
    mutate(nt = nt / 10000, lr = as.factor(lr)) %>%
    gather(
      diagnostic, value,
      nt, pseudo_r2, pred_obs_r2
    ) %>%
    ggplot(aes(tc, lr, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c() +
    facet_grid(diagnostic ~ region)
}

