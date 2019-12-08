# 1) Checking for name-acceptedness --------------------------------------------

# Fiddling to test and get-to-grips-with the two `taxize::` functions
if (FALSE) {

  # Note, `tnrs()` doesn't check for homonyms/synonyms if there isn't an
  # authority in the query!
  # There are two solutions to this (for me):
  #     1) `tnrs(GBIF_SWAFR_tidy$scientificname)`
  #     2) `gnr_resolve()`

  # Cf these:
  tnrs_test <- test_tnrs(how_many = 5, flora = GBIF_SWAFR)
  # (Note, do on _tidy when doing actual one!)
  gnr_test <- test_gnr(how_many = 5, flora = GBIF_SWAFR, data_source_ids = 12)

  # TNRS has 1x output row per submitted name
  tnrs_test$df
  # GNR has ≥1x output row per submitted name
  gnr_test$df

  # So, in the case where there is only 1 output row for a given name in
  # BOTH TNRS and GNR outputs, the test of name-consistency is easy:

  # > is_consistent(tnrs_test$df, gnr_test$df, index = 1)
  # > is_consistent(tnrs_test$df, gnr_test$df, 2)
  # > is_consistent(tnrs_test$df, gnr_test$df, 4)

  # But when the GNR output has ≥1 row per submitted name (as is actually
  # very helpful), it gets a bit more tricky, e.g.:

  #> is_consistent(tnrs_test$df, gnr_test$df, 3)
  #> is_consistent(tnrs_test$df, gnr_test$df, 5)
}

# Now, let's do it for real on real GBIF data
summary(GCFR_spp_sets)
summary(SWAFR_spp_sets)

# .... Flag inconsistent names -------------------------------------------------

# Now let's do this in stages/chunks just for safety/sanity:
# FIXME: all the dirs in here are old,
#        from when project was on MacBook aot repo

# first_100_unique_names <- function() { ----

# Some exploratory checks as we go to be safe

# GCFR <- function() { ----

GCFR_names_1_100 <- flag_inconsistent_names(GBIF_GCFR_tidy, 1:100)
saveRDS(
  GCFR_names_1_100,
  here::here(
    "Results",
    "GCFR_names_1_100_2017-07-26.rds"
  )
)
GCFR_names_1_100 <- readRDS(here::here(
  "Results",
  "GCFR_names_1_100_2017-07-26.rds"
))

GCFR_names_1_100
GCFR_names_1_100$consistency_check_conds %>%
  filter(consistent) %>%
  nrow()  # 90% are fine! Not bad!

# Let's look at Acanthopsis, for e.g.:
GCFR_names_1_100$tnrs_actual %>%
  filter(str_detect(submittedname, "Acanthopsis"))
GCFR_names_1_100$gnr_actual %>%
  filter(str_detect(user_supplied_name, "Acanthopsis"))
GCFR_names_1_100$consistency_check_gnr_df %>%
  filter(str_detect(user_supplied_name, "Acanthopsis"))
GCFR_names_1_100$consistency_check_conds %>%
  filter(str_detect(submitted_name, "Acanthopsis"))
GCFR_names_1_100$consistency_check_conds %>%
  filter(str_detect(submitted_name, "Acanthopsis")) %>%
  print_gnr_matches()
# And Acacia:
GCFR_names_1_100$consistency_check_conds %>%
  filter(str_detect(submitted_name, "Acacia")) %>%
  print_gnr_matches()

# NB:
GCFR_names_1_100$flagged

# SWAFR <- function() { ----

SWAFR_names_1_100 <- flag_inconsistent_names(GBIF_SWAFR_tidy, 1:100)
saveRDS(
  SWAFR_names_1_100,
  here::here(
    "Results",
    "SWAFR_names_1_100_2017-07-26.rds"
  )
)
SWAFR_names_1_100 <- readRDS(here::here(
  "Results",
  "SWAFR_names_1_100_2017-07-26.rds"
))
SWAFR_names_1_100
SWAFR_names_1_100$consistency_check_conds %>%
  filter(consistent) %>%
  nrow() # Not bad!
# Let's peek at Acacia:
SWAFR_names_1_100$consistency_check_conds %>%
  filter(str_detect(submitted_name, "Acacia"))
SWAFR_names_1_100$consistency_check_conds %>%
  filter(str_detect(submitted_name, "Acacia")) %>%
  print_gnr_matches()

# NB:
SWAFR_names_1_100$flagged

# the_rest_of_the_unique_names <- function() { ----

# For 101st to 500th unique names
GCFR_names_101_500 <- flag_inconsistent_names(GBIF_GCFR_tidy, 101:500)
saveRDS(GCFR_names_101_500, here::here(
  "Results",
  "GCFR_names_101_500_2017-07-26.rds"
))
GCFR_names_101_500 <- readRDS(here::here(
  "Results",
  "GCFR_names_101_500_2017-07-26.rds"
))
SWAFR_names_101_500 <- flag_inconsistent_names(GBIF_SWAFR_tidy, 101:500)
saveRDS(SWAFR_names_101_500, here::here(
  "Results",
  "SWAFR_names_101_500_2017-07-26.rds"
))
SWAFR_names_101_500 <- readRDS(here::here(
  "Results",
  "SWAFR_names_101_500_2017-07-26.rds"
))

# TODO: cont re-indenting to K&R-esque from here

# For 501st to 1500th unique names
GCFR_names_501_1500 <- flag_inconsistent_names(GBIF_GCFR_tidy, 501:1500)
saveRDS(GCFR_names_501_1500,
    here::here("Results", "GCFR_names_501_1500_2017-07-26.rds"))
GCFR_names_501_1500 <-
  readRDS(here::here("Results", "GCFR_names_501_1500_2017-07-26.rds"))
SWAFR_names_501_1500 <- flag_inconsistent_names(GBIF_SWAFR_tidy, 501:1500)
saveRDS(SWAFR_names_501_1500,
    here::here("Results", "SWAFR_names_501_1500_2017-07-26.rds"))
SWAFR_names_501_1500 <-
  readRDS(here::here("Results", "SWAFR_names_501_1500_2017-07-26.rds"))

# For 1501st to 3000th unique names
GCFR_names_1501_3000 <- flag_inconsistent_names(GBIF_GCFR_tidy, 1501:3000)
saveRDS(GCFR_names_1501_3000,
    here::here("Results", "GCFR_names_1501_3000_2017-07-26.rds"))
GCFR_names_1501_3000 <-
  readRDS(here::here("Results", "GCFR_names_1501_3000_2017-07-26.rds"))
SWAFR_names_1501_3000 <- flag_inconsistent_names(GBIF_SWAFR_tidy, 1501:3000)
saveRDS(SWAFR_names_1501_3000,
    here::here("Results", "SWAFR_names_1501_3000_2017-07-26.rds"))
SWAFR_names_1501_3000 <-
  readRDS(here::here("Results", "SWAFR_names_1501_3000_2017-07-26.rds"))

# For 3001st to 5000th unique names
GCFR_names_3001_5000 <- flag_inconsistent_names(GBIF_GCFR_tidy, 3001:5000)
saveRDS(GCFR_names_3001_5000,
    here::here("Results", "GCFR_names_3001_5000_2017-07-26.rds"))
GCFR_names_3001_5000 <-
  readRDS(here::here("Results", "GCFR_names_3001_5000_2017-07-26.rds"))
SWAFR_names_3001_5000 <- flag_inconsistent_names(GBIF_SWAFR_tidy, 3001:5000)
saveRDS(SWAFR_names_3001_5000,
    here::here("Results", "SWAFR_names_3001_5000_2017-07-26.rds"))
SWAFR_names_3001_5000 <-
  readRDS(here::here("Results", "SWAFR_names_3001_5000_2017-07-26.rds"))

# For GCFR 5001st to 8000th unique names
GCFR_names_5001_8000 <- flag_inconsistent_names(GBIF_GCFR_tidy, 5001:8000)
saveRDS(GCFR_names_5001_8000,
    here::here("Results", "GCFR_names_5001_8000_2017-07-26.rds"))
GCFR_names_5001_8000 <-
  readRDS(here::here("Results", "GCFR_names_5001_8000_2017-07-26.rds"))

# For SWAFR 5001st to end
SWAFR_names_5001_end <-
  flag_inconsistent_names(GBIF_SWAFR_tidy,
              5001:length(unique(GBIF_SWAFR_tidy$species)))
saveRDS(SWAFR_names_5001_end,
    here::here("Results", "SWAFR_names_5001_end_2017-07-26.rds"))
SWAFR_names_5001_end <-
  readRDS(here::here("Results", "SWAFR_names_5001_end_2017-07-26.rds"))

# For GCFR 8001st to end (TODO)
GCFR_names_8001_end <-
  flag_inconsistent_names(GBIF_GCFR_tidy,
              8001:length(unique(GBIF_GCFR_tidy$species)))
saveRDS(GCFR_names_8001_end,
    here::here("Results", "GCFR_names_8001_end_2017-07-26.rds"))
GCFR_names_8001_end <-
  readRDS(here::here("Results", "GCFR_names_8001_end_2017-07-26.rds"))

# write_compile_flagged_spp <- function() { ----

# FIXME: all the dirs in here are old,
#        from when project was on MacBook aot repo

# GCFR <- function() { ----

GCFR_names_flagged <- rbind_flagged_spp(c(
  GCFR_names_1_100,
  GCFR_names_101_500,
  GCFR_names_501_1500,
  GCFR_names_1501_3000,
  GCFR_names_3001_5000,
  GCFR_names_5001_8000,
  GCFR_names_8001_end
))
saveRDS(GCFR_names_flagged, here::here(
  "Results",
  "GCFR_names_flagged_2017-07-26.rds"
))

# SWAFR <- function() { ----

SWAFR_names_flagged <- rbind_flagged_spp(c(
  SWAFR_names_1_100,
  SWAFR_names_101_500,
  SWAFR_names_501_1500,
  SWAFR_names_1501_3000,
  SWAFR_names_3001_5000,
  SWAFR_names_5001_end
))
saveRDS(SWAFR_names_flagged, here::here(
  "Results",
  "SWAFR_names_flagged_2017-07-26.rds"
))

# write_edited_flagged_spp <- function() { ----

GCFR_names_flagged <- readRDS(here::here(
  "Data",
  "derived-data",
  "flora",
  "GCFR_names_flagged_2017-07-26.rds"
))

SWAFR_names_flagged <- readRDS(here::here(
  "Data",
  "derived-data",
  "flora",
  "SWAFR_names_flagged_2017-07-26.rds"
))

# Make extra columns so that the multiple gnr natches names
# can fit in the csv

# spread_gnr_matches <- function() { ----

GCFR_names_flagged %<>%
  count_gnr_matched_names() %>%
  add_gnr_matches_as_cols(ignore = 1:7) %>%
  distribute_gnr_matches_to_cols(ignore = 1:7) %>%
  dplyr::select(-gnr_matched_names)

# But before I do SWAFR, beware:
# there is a species with *183* gnr matches...
# What the fuck??
SWAFR_names_flagged %>%
  filter(gnr_matched_names %>% map(length) == 183) %$%
  gnr_matched_names
# They seem to mostly be the same...
# Guess what:
# I think I need to add a function to only add *unique* gnr matches
#
# *tick tock*
#
# Done! Now Let's try:
SWAFR_names_flagged %<>%
  count_gnr_matched_names() %>%
  add_gnr_matches_as_cols(ignore = 1:7) %>%
  distribute_gnr_matches_to_cols(ignore = 1:7) %>%
  dplyr::select(-gnr_matched_names)
# Fab!

# remake_tnrs_cond_with_binom_only <- function() { ----

GCFR_names_flagged %<>%
  mutate(tnrs_cond = {submitted_name == tnrs_matched_name})
SWAFR_names_flagged %<>%
  mutate(tnrs_cond = {submitted_name == tnrs_matched_name})

# Save ----

write.csv(GCFR_names_flagged, here::here(
  "Data",
  "derived-data",
  "flora",
  "GCFR_names_flagged.csv"
))
write.csv(SWAFR_names_flagged, here::here(
  "Data",
  "derived-data",
  "flora",
  "SWAFR_names_flagged.csv"
))

# !! Already done this too !!

write_CRAP_spp <- function() {

  GCFR_names_CRAP <- GCFR_names_flagged %>%
    filter(not(tnrs_cond) & not(gnr_cond))
  SWAFR_names_CRAP <- SWAFR_names_flagged %>%
    filter(not(tnrs_cond) & not(gnr_cond))

  write.csv(GCFR_names_CRAP, here::here(
    "Data",
    "derived-data",
    "flora",
    "GCFR_names_CRAP.csv"
  ))
  write.csv(SWAFR_names_CRAP, here::here(
    "Data",
    "derived-data",
    "flora",
    "SWAFR_names_CRAP.csv"
  ))

}

# !! Already done this too !!

write_non_CRAP_spp <- function() {

  GCFR_names_flagged_not_CRAP <- GCFR_names_flagged %>%
    filter(submitted_name %notin% GCFR_names_CRAP$submitted_name)
  SWAFR_names_flagged_not_CRAP <- SWAFR_names_flagged %>%
    filter(submitted_name %notin% SWAFR_names_CRAP$submitted_name)

  write.csv(GCFR_names_flagged_not_CRAP, here::here(
    "Data",
    "derived-data",
    "flora",
    "GCFR_names_flagged_not_CRAP.csv"
  ))
  write.csv(SWAFR_names_flagged_not_CRAP, here::here(
    "Data",
    "derived-data",
    "flora",
    "SWAFR_names_flagged_not_CRAP.csv"
  ))

}

# !! Already done this too !!

write_non_CRAP_bad_tnrs_spp <- function() {

  # I.e. those that are fine, because GNR has matched them,
  # But out of interest I wanna see why TNRS failed them
  # (usually disagreements about spelling)

  GCFR_names_flagged_not_CRAP_bad_tnrs <- GCFR_names_flagged_not_CRAP %>%
    filter(not(tnrs_cond))
  SWAFR_names_flagged_not_CRAP_bad_tnrs <- SWAFR_names_flagged_not_CRAP %>%
    filter(not(tnrs_cond))

  write.csv(GCFR_names_flagged_not_CRAP_bad_tnrs, here::here(
    "Data",
    "derived-data",
    "flora",
    "GCFR_names_flagged_not_CRAP_bad_tnrs.csv"
  ))
  write.csv(SWAFR_names_flagged_not_CRAP_bad_tnrs, here::here(
    "Data",
    "derived-data",
    "flora",
    "SWAFR_names_flagged_not_CRAP_bad_tnrs.csv"
  ))

}

# !! Already done this too !!

write_non_CRAP_bad_gnr_spp <- function() {

  # I.e. those that are fine, because GNR has matched them,
  # But out of interest I wanna see why TNRS failed them
  # (usually disagreements about spelling)

  GCFR_names_flagged_not_CRAP_bad_gnr <- GCFR_names_flagged_not_CRAP %>%
    filter(not(gnr_cond))
  SWAFR_names_flagged_not_CRAP_bad_gnr <- SWAFR_names_flagged_not_CRAP %>%
    filter(not(gnr_cond))

  write.csv(GCFR_names_flagged_not_CRAP_bad_gnr, here::here(
    "Data",
    "derived-data",
    "flora",
    "GCFR_names_flagged_not_CRAP_bad_gnr.csv"
  ))
  write.csv(SWAFR_names_flagged_not_CRAP_bad_gnr, here::here(
    "Data",
    "derived-data",
    "flora",
    "SWAFR_names_flagged_not_CRAP_bad_gnr.csv"
  ))

}

manual_explore_flagged <- function() {

  import_GCFR <- function() {
    GCFR_names_flagged <- read.csv(here::here(
      "Data",
      "derived-data",
      "flora",
      "GCFR_names_flagged.csv"
    ))
    GCFR_names_CRAP <- read.csv(here::here(
      "Data",
      "derived-data",
      "flora",
      "GCFR_names_CRAP.csv"
    ))
    GCFR_names_flagged_not_CRAP <- read.csv(here::here(
      "Data",
      "derived-data",
      "flora",
      "GCFR_names_flagged_not_CRAP.csv"
    ))
    GCFR_names_flagged_not_CRAP_bad_tnrs <- read.csv(here::here(
      "Data",
      "derived-data",
      "flora",
      "GCFR_names_flagged_not_CRAP_bad_tnrs.csv"
    ))
  }

  import_SWAFR <- function() {
    SWAFR_names_flagged <- read.csv(here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_names_flagged.csv"
    ))
    SWAFR_names_CRAP <- read.csv(here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_names_CRAP.csv"
    ))
    SWAFR_names_flagged_not_CRAP <- read.csv(here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_names_flagged_not_CRAP.csv"
    ))
    SWAFR_names_flagged_not_CRAP_bad_tnrs <- read.csv(here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_names_flagged_not_CRAP_bad_tnrs.csv"
    ))
  }

  # Explore and manual cleaning of those STILL flagged
  # I.e. flagged_not_CRAP
  # Seems to be that those still flagged_not_CRAP are actually fine!
  # At least one of GNR and TNRS is catching them, so it's fine! :^)

  GCFR_names_flagged_not_CRAP
  SWAFR_names_flagged_not_CRAP

  GCFR_names_flagged
  SWAFR_names_flagged

  GCFR_names_CRAP
  SWAFR_names_CRAP

  GCFR_spp_sets
  SWAFR_spp_sets

}


drop_flagged_spp <- function() {

  # What proportion of GBIF names are CRAP?
  nrow(GCFR_names_CRAP) / length(GCFR_spp_sets$species)
  nrow(SWAFR_names_CRAP) / length(SWAFR_spp_sets$species)
  # Not bad for GCFR, but quite a lot for SWAFR...

  # Drop the CRAP names from the species lists
  GCFR_spp_sets$species %<>% without(GCFR_names_CRAP$submitted_name)
  SWAFR_spp_sets$species %<>% without(SWAFR_names_CRAP$submitted_name)

  # Check spatial distribution of CRAP species
  plot(
    decimallatitude ~ decimallongitude,
    xlim = range(GBIF_GCFR_tidy$decimallongitude),
    ylim = range(GBIF_GCFR_tidy$decimallatitude),
    data = GBIF_GCFR_tidy[
      GBIF_GCFR_tidy$species %in% GCFR_names_CRAP$submitted_name,
    ]
  )
  plot(
    decimallatitude ~ decimallongitude,
    xlim = range(GBIF_SWAFR_tidy$decimallongitude),
    ylim = range(GBIF_SWAFR_tidy$decimallatitude),
    data = GBIF_SWAFR_tidy[
      GBIF_SWAFR_tidy$species %in% SWAFR_names_CRAP$submitted_name,
    ]
  )
  # That's fine!!
  # Both regions show no spatial patterns in CRAP names' species' locations
  # I.e. CRAP names occur randomly wrt geography

}
