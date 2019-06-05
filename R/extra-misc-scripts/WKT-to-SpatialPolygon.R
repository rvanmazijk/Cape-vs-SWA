library(magrittr)
library(here)
library(stringr)
library(rgeos)
readLines(here("data/derived-data/borders/GCFR_POLYGON_WKT.txt")) %>%
  str_replace_all("\\%2C", ",") %>%
  str_replace_all("\\+\\-", " ") %>%
  {paste0("POLYGON ((", ., "))")} %>%
  readWKT() %>%
  plot()
