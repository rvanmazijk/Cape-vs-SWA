# Fitting BRT-models -----------------------------------------------------------

fit_gbm_step <- function(variables, predictor_names,
                         response_name, log_response = TRUE,
                         tc, lr, nt) {
  # Convenience function to fit and/or refit a BRT model
  stopifnot(exprs = {
    class(variables) == "RasterStack"
    is.character(predictor_names)
  })
  variables %<>%
    as.data.frame() %>%
    na.exclude()
  if (log_response) {
    variables[[response_name]] %<>% log()
  }
  print(glue(
    "Fitting {response_name} ({ifelse(log_response, 'logged', 'unlogged')}) \
    with tc = {tc}, lr = {lr}, max.trees = {nt}"
  ))
  gbm.step(
    data = variables,
    gbm.x = predictor_names,
    gbm.y = response_name,
    tree.complexity = tc,
    learning.rate = lr,
    max.trees = nt,
    family = "gaussian"
  )
}

simplify_predictors <- function(x) {
  # Convenience function for gbm.simplify()
  stopifnot(class(x) == "gbm")
  gbm_simp <- gbm.simplify(x)
  # Drop as many variables as can if multiple nos. of drops are optimal
  # (hence (max(which(mean == min(mean)))))
  optimal_no_drops <- gbm_simp %$%
    deviance.summary %$%
    which(mean == min(mean)) %>%
    max()
  gbm_simp$pred.list[[optimal_no_drops]]
}

# Tidy-functions for extracting model details from text ------------------------

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

# BRT-model summary statistics -------------------------------------------------

pseudo_r2 <- function(x) {
  # Get the pseudo-R^2 of a gbm model
  stopifnot(class(x) == "gbm")
  x %$% {
    1 - (cv.statistics$deviance.mean / self.statistics$mean.null)
  }
}

get_pred_obs <- function(x) {
  stopifnot(class(x) == "gbm")
  x %$%
    tibble(
      pred = fitted,
      obs = data$y
    ) %>%
    mutate(
      exp_pred = exp(pred),
      exp_obs = exp(obs)
    )
}

model_pred_obs <- function(x) {
  stopifnot(is.data.frame(x))
  pred_obs_m <- lm(pred ~ obs, data = x)
  pred_obs_m_exp <- lm(exp_pred ~ exp_obs, data = x)
  list(
    pred_obs_m = pred_obs_m,
    pred_obs_m_exp = pred_obs_m_exp
  )
}

r2 <- function(m) {
  stopifnot(class(m) == "lm")
  m %>%
    glance() %>%
    select(r.squared) %>%
    as_vector()
}

pred_obs_r2 <- function(x) {
  stopifnot(class(x) == "gbm")
  x %>%
    get_pred_obs() %>%
    model_pred_obs() %>%
    map(r2) # For the log and exp models
}

quality_label <- function(x, model_name_, kind) {
  stopifnot(kind %in% c("pseudo_r2", "pred_obs_r2", "nt"))
  x[x$model_name == model_name_, kind] %>%
    as_vector() %>%
    round(digits = 2) %>%
    format(nsmall = if (kind == "nt") 0 else 2) %>%
    as.character()
}

my_BRT_summary <- function(x) {
  # Gives the nt, pseudo-R^2 and variables' contributions for a BRT
  stopifnot(class(x) == "gbm")
  nt <- x$n.trees
  pseudo_r2 <- pseudo_r2(x)
  pred_obs_r2s <- pred_obs_r2(x)
  pred_obs_r2 <- pred_obs_r2s$pred_obs_m
  pred_obs_r2_exp <- pred_obs_r2s$pred_obs_m_exp
  contribs <- list(x$contributions)
  tibble(
    nt,
    pseudo_r2,
    pred_obs_r2,
    pred_obs_r2_exp,
    contribs
  )
}

get_zero_contrib_vars <- function(region_, response_) {
  all_vars <- unique(model_contributions$var)
  some_vars <- model_contributions %>%
    filter(region == region_, response == response_) %>%
    select(var) %>%
    as_vector()
  all_vars[!(all_vars %in% some_vars)]
}

# Permuting response columns for randomised BRT-models -------------------------

permute_vector <- function(x) {
  # Shuffles positions of values in vector
  x[sample(length(x))]
}

permute_wo_nas <- function(x) {
  # Shuffles positions of values in vector,
  # but keeps NAs in their starting positions
  x[!is.na(x)] %<>% permute_vector()
  x
}
