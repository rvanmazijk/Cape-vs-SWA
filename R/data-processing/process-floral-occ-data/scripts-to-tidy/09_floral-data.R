# 2) Synonmy checks ------------------------------------------------------------

# Getting a list of synonyms for e/ spp (w/i e/ flora) that aren't CRAP,
# and checking if any of those synonyms are in my original flora,
# and then changing them such that there are no more synonyms.
# I.e. I just want one name (doesn't matter which) for each species in e/ region

GCFR <- function() {

  GCFR_spp_sets$species
  length(GCFR_spp_sets$species)

  test <- function() {

    GCFR_synonyms_test <- vector(
      "list",
      length = length(GCFR_spp_sets$species)
    )
    for (i in 1:50) {
      GCFR_synonyms_test[[i]] <- GCFR_spp_sets$species[i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(GCFR_synonyms_test[[i]])) {
        GCFR_synonyms_test[[i]] <- "no syns found"
      }
    }

    # Fiddles/checks
    GCFR_synonyms_test[1:50]
    GCFR_ammended_test <- ammend_synonyms(
      GCFR_spp_sets$species,
      GCFR_synonyms_test
    )
    GCFR_spp_sets$species[GCFR_ammended_test != GCFR_spp_sets$species]
    GCFR_synonyms_test[[2]] %in% GCFR_spp_sets$species
    length(GCFR_ammended_test) == length(GCFR_spp_sets$species)
    any(GCFR_ammended_test != GCFR_spp_sets$species)

  }

  first_1000_spp <- function() {

    GCFR_synonyms_1_1000 <- vector("list", length = 1000)

    for (i in 1:216) {
      print(i)
      GCFR_synonyms_1_1000[[i]] <- GCFR_spp_sets$species[i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(GCFR_synonyms_test[[i]])) {
        GCFR_synonyms_test[[i]] <- "no syns found"
      }
    }

    for (i in 217:426) {
      print(i)
      GCFR_synonyms_1_1000[[i]] <- GCFR_spp_sets$species[i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(GCFR_synonyms_test[[i]])) {
        GCFR_synonyms_test[[i]] <- "no syns found"
      }
    }

    # No. 427 (*Albuca unifolia*) ...
    for (i in 427:427) {
      print(i)
      GCFR_synonyms_1_1000[[i]] <- GCFR_spp_sets$species[i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(GCFR_synonyms_test[[i]])) {
        GCFR_synonyms_test[[i]] <- "no syns found"
      }
    }

    for (i in 428:1000) {
      print(i)
      GCFR_synonyms_1_1000[[i]] <- GCFR_spp_sets$species[i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(GCFR_synonyms_test[[i]])) {
        GCFR_synonyms_test[[i]] <- "no syns found"
      }
    }

    saveRDS(GCFR_synonyms_1_1000, here::here(
      "Data",
      "derived-data",
      "flora",
      "GCFR_synonyms_1_1000_2017-09-06_with-early-check.rds"
    ))

  }

  in_between_checks <- function() {

    GCFR_spp_sets$species[1000]
    GCFR_synonyms_1_1000[1000]

    GCFR_ammended_test <- ammend_synonyms(
        GCFR_spp_sets$species[1:1000],
        GCFR_synonyms_1_1000[1:1000]
    )
    any(GCFR_ammended_test != GCFR_spp_sets$species[1:1000])
    sort(
      GCFR_spp_sets$species[1:1000][
        GCFR_ammended_test != GCFR_spp_sets$species[1:1000]
      ]
    )
    sort(
      GCFR_ammended_test[
        GCFR_ammended_test != GCFR_spp_sets$species[1:1000]
      ]
    )

    GCFR_ammended_test[GCFR_ammended_test == "Albuca unifolia"]
    GCFR_ammended_test[GCFR_ammended_test == "Albuca unifoliata"]

  }

  second_1000_spp <- function() {
    GCFR_synonyms_1001_2000 <- vector("list", length = 1000)
    for (i in 1:1000) {
      print(i)
      GCFR_synonyms_1001_2000[[i]] <- GCFR_spp_sets$species[1000 + i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(GCFR_synonyms_1001_2000[[i]])) {
        GCFR_synonyms_1001_2000[[i]] <- "no syns found"
      }
    }
    saveRDS(GCFR_synonyms_1001_2000, here::here(
      "Data",
      "derived-data",
      "flora",
      "GCFR_synonyms_1001_2000_with-early-checks.rds"
    ))
    beep(sound = 5)
  }

  spp_2001_to_5000 <- function() {
    GCFR_synonyms_2001_5000 <- vector("list", length = 3000)
    for (i in 1:1874) {
      print(i)
      GCFR_synonyms_2001_5000[[i]] <- GCFR_spp_sets$species[2000 + i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(GCFR_synonyms_2001_5000[[i]])) {
        GCFR_synonyms_2001_5000[[i]] <- "no syns found"
      }
    }
    # Partial save along the way
    saveRDS(GCFR_synonyms_2001_5000, here::here(
      "Data",
      "derived-data",
      "flora",
      "GCFR_synonyms_2001_3874_with-early-checks.rds"
    ))

    # TODO: change to `here::here()`
    GCFR_synonyms_2001_5000 <- readRDS(paste(sep = "/",
      getwd(),
      "Data",
      "derived-data",
      "flora",
      "GCFR_synonyms_2001_3874_with-early-checks.rds"
    ))
    for (i in 1874:3000) {
      print(i)
      GCFR_synonyms_2001_5000[[i]] <- GCFR_spp_sets$species[2000 + i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(GCFR_synonyms_2001_5000[[i]])) {
        GCFR_synonyms_2001_5000[[i]] <- "no syns found"
      }
    }
    # TODO: change to `here::here()`
    saveRDS(GCFR_synonyms_2001_5000, paste(sep = "/",
      getwd(),
      "Data",
      "derived-data",
      "flora",
      "GCFR_synonyms_2001_5000_with-early-checks.rds"
    ))
    beep(sound = 5)
  }

  spp_5001_to_8000 <- function() {
    GCFR_synonyms_5001_8000 <- vector("list", length = 3000)
    for (i in 1:3000) {
      print(i)
      GCFR_synonyms_5001_8000[[i]] <- GCFR_spp_sets$species[5000 + i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(GCFR_synonyms_5001_8000[[i]])) {
        GCFR_synonyms_5001_8000[[i]] <- "no syns found"
      }
    }
    # TODO: change to `here::here()`
    saveRDS(GCFR_synonyms_5001_8000, paste(sep = "/",
      getwd(),
      "Data",
      "derived-data",
      "flora",
      "GCFR_synonyms_5001_8000_with-early-checks.rds"
    ))
    beep(sound = 5)
  }

  spp_8001_to_10000 <- function() {
    GCFR_synonyms_8001_10000 <- vector("list", length = 2000)
    for (i in 1:2000) {
      print(i)
      GCFR_synonyms_8001_10000[[i]] <- GCFR_spp_sets$species[8000 + i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(GCFR_synonyms_8001_10000[[i]])) {
        GCFR_synonyms_8001_10000[[i]] <- "no syns found"
      }
    }
    # TODO: change to `here::here()`
    saveRDS(GCFR_synonyms_8001_10000, paste(sep = "/",
      getwd(),
      "Data",
      "derived-data",
      "flora",
      "GCFR_synonyms_8001_10000_with-early-checks.rds"
    ))
    beep(sound = 5)
  }

  in_between_peak <- function() {
    GCFR_synonyms_8001_10000 %>%
      unlist() %>%
      without("no syns") %>%
      length()
  }

  spp_10001_to_12000 <- function() {
    GCFR_synonyms_10001_12000 <- vector("list", length = 2000)
    for (i in 1:2000) {
      print(i)
      GCFR_synonyms_10001_12000[[i]] <- GCFR_spp_sets$species[10000 + i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(GCFR_synonyms_10001_12000[[i]])) {
        GCFR_synonyms_10001_12000[[i]] <- "no syns found"
      }
    }
    saveRDS(GCFR_synonyms_10001_12000, here::here(
      "Data",
      "derived-data",
      "flora",
      "GCFR_synonyms_10001_12000_with-early-checks.rds"
    ))
    beep(sound = 5)
  }

  spp_12001_to_end <- function() {
    GCFR_synonyms_12001_end <- vector(
      "list",
      length = length(GCFR_spp_sets$species) - 12000
    )
    for (i in seq_along(GCFR_synonyms_12001_end)) {
      print(i)
      GCFR_synonyms_12001_end[[i]] <- GCFR_spp_sets$species[12000 + i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(GCFR_synonyms_12001_end[[i]])) {
        GCFR_synonyms_12001_end[[i]] <- "no syns found"
      }
    }
    saveRDS(GCFR_synonyms_12001_end, here::here(
      "Data",
      "derived-data",
      "flora",
      "GCFR_synonyms_12001_end_with-early-checks.rds"
    ))
    beep(sound = 5)
  }

}

SWAFR <- function() {

  first_2000_spp <- function() {
    SWAFR_synonyms_1_2000 <- vector("list", length = 2000)
    for (i in 1:768) {
      print(i)
      SWAFR_synonyms_1_2000[[i]] <- SWAFR_spp_sets$species[i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(SWAFR_synonyms_1_2000[[i]])) {
        SWAFR_synonyms_1_2000[[i]] <- "no syns found"
      }
    }
    for (i in 768:1062) {
      print(i)
      SWAFR_synonyms_1_2000[[i]] <- SWAFR_spp_sets$species[i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(SWAFR_synonyms_1_2000[[i]])) {
        SWAFR_synonyms_1_2000[[i]] <- "no syns found"
      }
    }
    saveRDS(SWAFR_synonyms_1_2000, here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_synonyms_1_1062_with-early-check.rds"
    ))
    SWAFR_synonyms_1_2000 <- readRDS(here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_synonyms_1_1062_with-early-check.rds"
    ))
    for (i in 1062:2000) {
      print(i)
      SWAFR_synonyms_1_2000[[i]] <- SWAFR_spp_sets$species[i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(SWAFR_synonyms_1_2000[[i]])) {
        SWAFR_synonyms_1_2000[[i]] <- "no syns found"
      }
    }
    saveRDS(SWAFR_synonyms_1_2000, here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_synonyms_1_2000_with-early-checks.rds"
    ))
  }

  spp_2001_to_4000 <- function() {
    SWAFR_synonyms_2001_4000 <- vector("list", length = 2000)
    for (i in 1:2000) {
      print(i)
      SWAFR_synonyms_2001_4000[[i]] <- SWAFR_spp_sets$species[2000 + i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(SWAFR_synonyms_2001_4000[[i]])) {
        SWAFR_synonyms_2001_4000[[i]] <- "no syns found"
      }
    }
    saveRDS(SWAFR_synonyms_2001_4000, here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_synonyms_2001_4000_with-early-checks.rds"
    ))
  }

  spp_4001_to_6000 <- function() {
    SWAFR_synonyms_4001_6000 <- vector("list", length = 2000)
    for (i in 1:639) {
      print(i)
      SWAFR_synonyms_4001_6000[[i]] <- SWAFR_spp_sets$species[4000 + i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(SWAFR_synonyms_4001_6000[[i]])) {
        SWAFR_synonyms_4001_6000[[i]] <- "no syns found"
      }
    }
    for (i in 639:2000) {
      print(i)
      SWAFR_synonyms_4001_6000[[i]] <- SWAFR_spp_sets$species[4000 + i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(SWAFR_synonyms_4001_6000[[i]])) {
        SWAFR_synonyms_4001_6000[[i]] <- "no syns found"
      }
    }
    saveRDS(SWAFR_synonyms_4001_6000, here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_synonyms_4001_6000_with-early-checks.rds"
    ))
  }

  spp_6001_to_end <- function() {
    SWAFR_synonyms_6001_end <- vector(
      "list",
      length = length(SWAFR_spp_sets$species) - 6000
    )
    for (i in seq_along(SWAFR_synonyms_6001_end)) {
      print(i)
      SWAFR_synonyms_6001_end[[i]] <- SWAFR_spp_sets$species[6000 + i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(SWAFR_synonyms_6001_end[[i]])) {
        SWAFR_synonyms_6001_end[[i]] <- "no syns found"
      }
    }
    saveRDS(SWAFR_synonyms_6001_end, here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_synonyms_6001_6810_with-early-checks.rds"
    ))
    SWAFR_synonyms_6001_end <- readRDS(here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_synonyms_6001_6810_with-early-checks.rds"
    ))
    for (i in 809:length(SWAFR_synonyms_6001_end)) {
      print(i)
      SWAFR_synonyms_6001_end[[i]] <- SWAFR_spp_sets$species[6000 + i] %>%
        get_synonyms() %>%
        without(NA)
      if (is_empty(SWAFR_synonyms_6001_end[[i]])) {
        SWAFR_synonyms_6001_end[[i]] <- "no syns found"
      }
    }
    saveRDS(SWAFR_synonyms_6001_end, here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_synonyms_6001_end_with-early-checks.rds"
    ))
  }

}


# Read in and merge all synonym lists again ------------------------------------

need_these_too <- function() {

  GCFR_spp_sets <- readRDS(here::here(
    "Data",
    "derived-data",
    "flora",
    "GCFR_spp_sets_local.rds"
  ))
  SWAFR_spp_sets <- readRDS(here::here(
    "Data",
    "derived-data",
    "flora",
    "SWAFR_spp_sets_local.rds"
  ))
  GBIF_GCFR <- read_tsv(paste(
    giswd, "GBIF", "GBIF_GCFRquery_2017-07-24.csv",
    sep = "/"
  ))
  GBIF_SWAFR <- read_tsv(paste(
    giswd, "GBIF", "GBIF_SWAFRquery_2017-07-24.csv",
    sep = "/"
  ))
  GBIF_GCFR_tidy <- tidy_gbif(GBIF_GCFR)
  GBIF_SWAFR_tidy <- tidy_gbif(GBIF_SWAFR)

  # More tidying (a few NAs creeped in) --------------------------------------

  GBIF_GCFR_tidy$species[is.na(GBIF_GCFR_tidy$species)] <-
    binom_only(GBIF_GCFR_tidy$scientificname[is.na(GBIF_GCFR_tidy$species)])

  GBIF_SWAFR_tidy$species[is.na(GBIF_SWAFR_tidy$species)] <-
    binom_only(GBIF_GCFR_tidy$scientificname[is.na(GBIF_GCFR_tidy$species)])

}

and_then_the_synonyms <- function() {

  GCFR_synonyms_files <- c(
    "GCFR_synonyms_1_1000_2017-09-06_with-early-check.rds",
    "GCFR_synonyms_1001_2000_with-early-checks.rds",
    "GCFR_synonyms_2001_5000_with-early-checks.rds",
    "GCFR_synonyms_5001_8000_with-early-checks.rds",
    "GCFR_synonyms_8001_10000_with-early-checks.rds",
    "GCFR_synonyms_10001_12000_with-early-checks.rds",
    "GCFR_synonyms_12001_end_with-early-checks.rds"
  )
  GCFR_synonyms <- c()
  for (i in seq_along(GCFR_synonyms_files)) {
    GCFR_synonyms[[i]] <- readRDS(here::here(
      "Data",
      "derived-data",
      "flora",
      GCFR_synonyms_files[i]
    ))
  }
  GCFR_synonyms %<>% unlist(recursive = FALSE)

  SWAFR_synonyms_files <- c(
    "SWAFR_synonyms_1_2000_with-early-checks.rds",
    "SWAFR_synonyms_2001_4000_with-early-checks.rds",
    "SWAFR_synonyms_4001_6000_with-early-checks.rds",
    "SWAFR_synonyms_6001_end_with-early-checks.rds"
  )
  SWAFR_synonyms <- c()
  for (i in seq_along(SWAFR_synonyms_files)) {
    SWAFR_synonyms[[i]] <- readRDS(here::here(
      "Data",
      "derived-data",
      "flora",
      SWAFR_synonyms_files[i]
    ))
  }
  SWAFR_synonyms %<>% unlist(recursive = FALSE)

}

# Removed CRAP names, and ammend synonyms --------------------------------------

GCFR_names_CRAP <- read.csv(here::here(
  "Data",
  "derived-data",
  "flora",
  "GCFR_names_CRAP.csv"
))
SWAFR_names_CRAP <- read.csv(here::here(
  "Data",
  "derived-data",
  "flora",
  "SWAFR_names_CRAP.csv"
))

GCFR_spp_sets$species %<>% without(GCFR_names_CRAP$submitted_name)
SWAFR_spp_sets$species %<>% without(SWAFR_names_CRAP$submitted_name)

# Ammend synonyms in lists

# But beware:
"no syns found" %in% unlist(GCFR_synonyms)   # TRUE
"no syns"       %in% unlist(GCFR_synonyms)   # TRUE
"no syns found" %in% unlist(SWAFR_synonyms)  # TRUE
"no syns"       %in% unlist(SWAFR_synonyms)  # TRUE
# Converge "no syns found" and "no syns" both to NA
GCFR_synonyms %<>% no_syns_to_NA()
SWAFR_synonyms %<>% no_syns_to_NA()
# Now it's fine :)
"no syns found" %in% unlist(GCFR_synonyms)   # FALSE
"no syns"       %in% unlist(GCFR_synonyms)   # FALSE
"no syns found" %in% unlist(SWAFR_synonyms)  # FALSE
"no syns"       %in% unlist(SWAFR_synonyms)  # FALSE


# Now for the grand events:

GBIF_GCFR_tidy_species_ammended <- GBIF_GCFR_tidy$species %>%
  ammend_synonyms(
    spp_set = GCFR_spp_sets$species,
    syns = GCFR_synonyms
  )
saveRDS(GBIF_GCFR_tidy_species_ammended, here::here(
  "Data",
  "derived-data",
  "flora",
  "GBIF_GCFR_tidy_species_ammended_column_2017-09-14.rds"
))
any(is.na(GBIF_GCFR_tidy_species_ammended))  # FALSE; yay!

GBIF_SWAFR_tidy_species_ammended <- GBIF_SWAFR_tidy$species %>%
  ammend_synonyms(
    spp_set = SWAFR_spp_sets$species,
    syns = SWAFR_synonyms
  )
saveRDS(GBIF_SWAFR_tidy_species_ammended, here::here(
  "Data",
  "derived-data",
  "flora",
  "GBIF_SWAFR_tidy_species_ammended_column_2017-09-14.rds"
))
any(is.na(GBIF_SWAFR_tidy_species_ammended))  # FALSE; yay!


# And also save the dictionaries of species & synonyms for reference:
GCFR_synonym_dictionary <- make_dict(
  spp_set = GCFR_spp_sets$species,
  syns = GCFR_synonyms
)
saveRDS(GCFR_synonym_dictionary, here::here(
  "Data",
  "derived-data",
  "flora",
  "GCFR_synonym_dictionary.rds"
))
SWAFR_synonym_dictionary <- make_dict(
  spp_set = SWAFR_spp_sets$species,
  syns = SWAFR_synonyms
)
saveRDS(GCFR_synonym_dictionary, here::here(
  "Data",
  "derived-data",
  "flora",
  "SWAFR_synonym_dictionary.rds"
))

# Document changed species lists:
GCFR_spp_change_log <- tibble(
  raw_name = GBIF_GCFR_tidy$species,
  ammended_name = GBIF_GCFR_tidy_species_ammended,
  has_changed = {GBIF_GCFR_tidy$species != GBIF_GCFR_tidy_species_ammended}
)
write.csv(GCFR_spp_change_log, here::here(
  "Data",
  "derived-data",
  "flora",
  "GCFR_spp_change_log.csv"
))
GCFR_spp_change_log %>% filter(has_changed) %>% unique()  # Cool!
SWAFR_spp_change_log <- tibble(
  raw_name = GBIF_SWAFR_tidy$species,
  ammended_name = GBIF_SWAFR_tidy_species_ammended,
  has_changed = {GBIF_SWAFR_tidy$species != GBIF_SWAFR_tidy_species_ammended}
)
write.csv(SWAFR_spp_change_log, here::here(
  "Data",
  "derived-data",
  "flora",
  "SWAFR_spp_change_log.csv"
))
SWAFR_spp_change_log %>% filter(has_changed) %>% unique()  # Cool!

# Aside: Yay!!!
length(GCFR_spp_sets$species) == length(unique(GCFR_spp_sets$species))    # TRUE
length(SWAFR_spp_sets$species) == length(unique(SWAFR_spp_sets$species))  # TRUE


# Ammend synonyms in GBIF_..._tidy dfs AND remove ..._CRAP names

length(GBIF_GCFR_tidy_species_ammended) == length(GBIF_GCFR_tidy$species)    # Yay!
length(GBIF_SWAFR_tidy_species_ammended) == length(GBIF_SWAFR_tidy$species)  # Yay!

length(GBIF_GCFR_tidy_species_ammended)
length(GBIF_GCFR_tidy_species_ammended %>% without(GCFR_names_CRAP$submitted_name))

length(GBIF_SWAFR_tidy_species_ammended)
length(GBIF_SWAFR_tidy_species_ammended %>% without(SWAFR_names_CRAP$submitted_name))

# Replace the GBIF species columns with these ammended versions
GBIF_GCFR_tidy$species <- GBIF_GCFR_tidy_species_ammended
GBIF_SWAFR_tidy$species <- GBIF_SWAFR_tidy_species_ammended
