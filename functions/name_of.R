name_of <- function(x, quietly = FALSE) {
  current_env <- parent.frame(1)
  x <- deparse(substitute(x, current_env))
  if (!quietly) {
    print(glue(
      "Current environment of {x} is:"
    ))
    print(current_env)
    if (identical(current_env, .GlobalEnv)) {
      warning(glue(
        "name_of() is designed for use within other functions. \\
        Running in global simply returns 'x'"
      ))
    }
  }
  x
}
