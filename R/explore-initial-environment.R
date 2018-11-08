# Exploring my initiral R environment :)
# Cape vs SWA publication
# Ruan van Mazijk

library(here)
source(here("R/setup.R"))

get_class <- function(obj_name) {
  stopifnot(is.character(obj_name))
  obj <- eval(parse(text = obj_name))
  obj_class <- class(obj)[[1]]
  data.frame(obj_name, obj_class)
}

ls() %>%
  map_df(get_class) %>%
  group_by(obj_class) %>%
  summarise(n = n())
