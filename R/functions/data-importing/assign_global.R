assign_global <- function(x) {
  # Assign a variable to the global environment,
  # not simply the parent environment (as with <<-),
  # with the same name
  assign(
    x = deparse(substitute(x)),
    value = x,
    envir = .GlobalEnv
  )
}
