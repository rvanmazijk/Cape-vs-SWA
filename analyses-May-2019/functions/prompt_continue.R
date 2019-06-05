prompt_continue <- function() {
  continue <- readline(message(glue(
    "Only run the code below if you haven't already & saved the results to disc.
    Are you sure you want to continue? [y]
    (Press any other key to cancel.)"
  )))
  continue == "y"
}
