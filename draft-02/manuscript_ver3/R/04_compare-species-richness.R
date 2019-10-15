# Import data ------------------------------------------------------------------

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS.csv"))
)

# Test for differences in richness and turnover --------------------------------

test_diff <- function(response) {
  dataset <- data %$% {
    if      (response ==     "QDS_richness")                       QDS
    else if (response %in% c("HDS_richness", "QDS_turnover_prop")) HDS
    else if (response %in% c("DS_richness",  "HDS_turnover_prop")) DS
  }
  U_test <- wilcox.test(
    dataset[[response]][dataset$region == "GCFR"],
    dataset[[response]][dataset$region == "SWAFR"],
    conf.int = TRUE
  )
  #print(tidy(U_test))
  tibble(
    metric     = response,
    GCFR_mean  = mean(dataset[[response]][dataset$region == "GCFR"]),
    SWAR_mean  = mean(dataset[[response]][dataset$region == "SWAFR"]),
    P_U        = tidy(U_test)$p.value,
    CLES_value = CLES(
      dataset[[response]][dataset$region == "SWAFR"],
      dataset[[response]][dataset$region == "GCFR"]
    )
  )
}

responses <- c(
  "QDS_richness", "HDS_richness", "DS_richness",
  "QDS_turnover_prop", "HDS_turnover_prop"
)
richness_test_results <- map_dfr(responses, test_diff)

# Print table
richness_test_results
## # A tibble: 5 x 5
##   metric            GCFR_mean SWAR_mean         P_U CLES_value
##   <chr>                 <dbl>     <dbl>       <dbl>      <dbl>
## 1 QDS_richness        397.      334.    0.406            0.516
## 2 HDS_richness        945.      791.    0.275            0.542
## 3 DS_richness        2080.     1527.    0.0384           0.658
## 4 QDS_turnover_prop     0.635     0.600 0.000000532      0.696
## 5 HDS_turnover_prop     0.552     0.484 0.00125          0.741

data
