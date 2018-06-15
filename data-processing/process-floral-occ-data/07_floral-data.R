# Process floral occurrence data
# GCFR-SWAFR publication
# Ruan van Mazijk

# Importing and cleaning plant occurence record data (GBIF) --------------------

# Preamble ---------------------------------------------------------------------

# Note, a lot of this script has been functionalised.
# This is artificial.
# It is simply to get nicer sub-section indentationfor the script's structure.
# And to prevent the time-consuming code blocks from running if I
# `source()` or `cmd-A, cmd-enter` this script.

# Create polygons to query occs in GBIF ----------------------------------------
# (Uses "Well Known Text" format)

# !! Already done this !!

# TODO: update dirs for reproducibility

if (FALSE) {
    SWAFR_border_buffered %>%
        make_wkt() %>%
        write(file = here::here(
            "Data",
            "SWAFR_POLYGON_WKT.txt"
        ))  # old dirs
    GCFR_border_buffered %>%
        make_wkt() %>%
        write(file = here::here(
            "Data",
            "GCFR_POLYGON_WKT.txt"
        ))  # old dirs
}

# e.g. <URL>
#   http://www.gbif.org/occurrence/search?
#       TAXON_KEY=7707728&
#       TAXON_KEY=6&
#       HAS_COORDINATE=true&
#       GEOMETRY=
#           118.90000445+-34.50000126%2C
#           118.90000445+-35.50000127%2C
#           ...
#           118.90000445+-34.50000126
# </URL>
# as derived from `make_wkt()`'s .txt outputs above



# Import GBIF query results (raw) from disc ------------------------------------

# These two `.tsv`s are large, so bear with
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

# Summarise the possible species sets based on the different columns
GCFR_spp_sets <- summarise_spp_sets(GBIF_GCFR_tidy)
summary(GCFR_spp_sets)
SWAFR_spp_sets <- summarise_spp_sets(GBIF_SWAFR_tidy)
summary(SWAFR_spp_sets)

# Save copies of the species lists to local repo
saveRDS(GCFR_spp_sets, here::here(
    "Data",
    "derived-data",
    "flora",
    "GCFR_spp_sets_local.rds"
))
GCFR_spp_sets <- readRDS(here::here(
    "Data",
    "derived-data",
    "flora",
    "GCFR_spp_sets_local.rds"
))
saveRDS(SWAFR_spp_sets, here::here(
    "Data",
    "derived-data",
    "flora",
    "SWAFR_spp_sets_local.rds"
))
SWAFR_spp_sets <- readRDS(here::here(
    "Data",
    "derived-data",
    "flora",
    "SWAFR_spp_sets_local.rds"
))


# 1) Checking for name-acceptedness --------------------------------------------

# Fiddling to test and get-to-grips-with the two `taxize::` functions

test_tnrs_gnr <- function() {

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

# !! Already done this !!

run_flag_inconsistent_names <- function() {

    # Now let's do this in stages/chunks just for safety/sanity:

    # FIXME: all the dirs in here are old,
    #        from when project was on MacBook aot repo

    first_100_unique_names <- function() {

        # Some exploratory checks as we go to be safe

        GCFR <- function() {

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

        }

        SWAFR <- function() {

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

        }

    }

    the_rest_of_the_unique_names <- function() {

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

    }

}

# !! Already done this too !!

write_compile_flagged_spp <- function() {

    # FIXME: all the dirs in here are old,
    #        from when project was on MacBook aot repo

    GCFR <- function() {
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
    }

    SWAFR <- function() {
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
    }

}

# !! Already done this too !!

write_edited_flagged_spp <- function() {

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

    spread_gnr_matches <- function() {
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

    }

    remake_tnrs_cond_with_binom_only <- function() {

        GCFR_names_flagged %<>%
            mutate(tnrs_cond = {submitted_name == tnrs_matched_name})

        SWAFR_names_flagged %<>%
            mutate(tnrs_cond = {submitted_name == tnrs_matched_name})

    }

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

}

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

    # TODO: ?

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

    # TODO: ?

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


# 3) Check invasive status, etc. -----------------------------------------------

if (FALSE) {

    # Read in Mike's *huge* `.csv`s, to find his alien list for the GCFR

    Mike_GCFR_cleaned <- as.tibble(read.csv(paste0(
        giswd,
        "GBIF/",
        "Mikes-help-w-species-data/",
        "Genera_data_essentials_cleaned.csv"
    )))
    Mike_GCFR_final <- as.tibble(read.csv(paste0(
        giswd,
        "GBIF/",
        "Mikes-help-w-species-data/",
        "Genera_data_essentials_final.csv"
    )))

    Mike_GCFR_cleaned[Mike_GCFR_cleaned$Alien.Indig != "I", ]
    Mike_GCFR_final$species[Mike_GCFR_final$Alien.Indig != "I"]

    summary(Mike_GCFR_cleaned$Alien.Indig)
    summary(Mike_GCFR_final$Alien.Indig)

}

# Now, rather use the GISD data

GSID_ZA <- read.csv(sep = ";", here::here(
    "Data",
    "raw-data",
    "flora",
    "invasive-specie_export-gisd_ZA_2017-09-14.csv"
))
GSID_AU <- read.csv(sep = ";", here::here(
    "Data",
    "raw-data",
    "flora",
    "invasive-specie_export-gisd_AU_2017-09-14.csv"
))
GSID_ZA$Species

# Remove THESE species from the master lists:
GBIF_GCFR_tidy %<>%
    filter(species %notin% GSID_ZA$Species)
GBIF_SWAFR_tidy %<>%
    filter(species %notin% GSID_AU$Species)


# 4) Compiling the clean floras --------------------------------------------------

GCFR_clean_flora <- GBIF_GCFR_tidy
SWAFR_clean_flora <- GBIF_SWAFR_tidy
write.csv(GCFR_clean_flora, here::here(
    "Data",
    "derived-data",
    "flora",
    "GCFR_clean_flora_2017-09-14.csv"
))
write.csv(SWAFR_clean_flora, here::here(
    "Data",
    "derived-data",
    "flora",
    "SWAFR_clean_flora_2017-09-14.csv"
))

# Done!


# Make rasters of floral richness from floral occs -----------------------------

import_clean_flora <- function() {
    # Use `readr::read_csv()` to prevent type changes in columns,
    # and to make tibble :^)
    GCFR_clean_flora <- read_csv(here::here(
        "Data",
        "derived-data",
        "flora",
        "GCFR_clean_flora_2017-09-14.csv"
    ))
    SWAFR_clean_flora <- read_csv(here::here(
        "Data",
        "derived-data",
        "flora",
        "SWAFR_clean_flora_2017-09-14.csv"
    ))
}

import_QDS_rasters <- function() {
    GCFR_QDS_raster <- raster(here::here(
        "Data",
        "derived-data",
        "Borders",
        "GCFR_QDS_raster.tif"
    ))
    GCFR_HDS_raster <- raster(here::here(
        "Data",
        "derived-data",
        "Borders",
        "GCFR_HDS_raster.tif"
    ))
    GCFR_3QDS_raster <- raster(here::here(
        "Data",
        "derived-data",
        "Borders",
        "GCFR_3QHDS_raster.tif"
    ))
    SWAFR_QDS_raster <- raster(here::here(
        "Data",
        "derived-data",
        "Borders",
        "SWAFR_QDS_raster.tif"
    ))
    SWAFR_HDS_raster <- raster(here::here(
        "Data",
        "derived-data",
        "Borders",
        "SWAFR_HDS_raster.tif"
    ))
    SWAFR_3QDS_raster <- raster(here::here(
        "Data",
        "derived-data",
        "Borders",
        "SWAFR_3QHDS_raster.tif"
    ))
}

GCFR <- function() {
    GCFR_richness_QDS <- make_richness_raster(
        flora_occs    = GCFR_clean_flora,
        region_raster = GCFR_QDS_raster,
        crs           = std_CRS
    )
    plot(GCFR_richness_QDS$raster)
    GCFR_richness_HDS <- make_richness_raster(
        flora_occs    = GCFR_clean_flora,
        region_raster = GCFR_HDS_raster,
        crs           = std_CRS
    )
    plot(GCFR_richness_HDS$raster)
    GCFR_richness_3QDS <- make_richness_raster(
        flora_occs    = GCFR_clean_flora,
        region_raster = GCFR_3QDS_raster,
        crs           = std_CRS
    )
    plot(GCFR_richness_3QDS$raster)
    save <- function() {
        writeRaster(GCFR_richness_QDS$raster, here::here(
            "Data",
            "derived-data",
            "flora",
            "GCFR_richness_QDS_2017-09-16.tif"
        ))
        writeRaster(GCFR_richness_HDS$raster, here::here(
            "Data",
            "derived-data",
            "flora",
            "GCFR_richness_HDS_2017-09-16.tif"
        ))
        writeRaster(GCFR_richness_3QDS$raster, here::here(
            "Data",
            "derived-data",
            "flora",
            "GCFR_richness_3QDS_2017-09-16.tif"
        ))
    }
}
SWAFR <- function() {
    SWAFR_richness_QDS <- make_richness_raster(
        flora_occs    = SWAFR_clean_flora,
        region_raster = SWAFR_QDS_raster,
        crs           = std_CRS
    )
    plot(SWAFR_richness_QDS$raster)
    SWAFR_richness_HDS <- make_richness_raster(
        flora_occs    = SWAFR_clean_flora,
        region_raster = SWAFR_HDS_raster,
        crs           = std_CRS
    )
    plot(SWAFR_richness_HDS$raster)
    SWAFR_richness_3QDS <- make_richness_raster(
        flora_occs    = SWAFR_clean_flora,
        region_raster = SWAFR_3QDS_raster,
        crs           = std_CRS
    )
    plot(SWAFR_richness_3QDS$raster)
    save <- function() {
        writeRaster(SWAFR_richness_QDS$raster, here::here(
            "Data",
            "derived-data",
            "flora",
            "SWAFR_richness_QDS_2017-09-16.tif"
        ))
        writeRaster(SWAFR_richness_HDS$raster, here::here(
            "Data",
            "derived-data",
            "flora",
            "SWAFR_richness_HDS_2017-09-16.tif"
        ))
        writeRaster(SWAFR_richness_3QDS$raster, here::here(
            "Data",
            "derived-data",
            "flora",
            "SWAFR_richness_3QDS_2017-09-16.tif"
        ))
    }
}

# Trim floral occurrences outside of regions -----------------------------------

GCFR_clean_flora <- read_csv(here::here("data/derived-data/flora/GCFR_clean_flora_2017-09-14.csv"))
SWAFR_clean_flora <- read_csv(here::here("data/derived-data/flora/SWAFR_clean_flora_2017-09-14.csv"))

# .... GCFR --------------------------------------------------------------------

# Make SpatialPointsDataFrames for species, genus, and family occurences
GCFR_clean_flora_spdf_species <- make_SpatialPointsDataFrame(
    GCFR_clean_flora,
    feature_columns = "species"
)
GCFR_clean_flora_spdf_genus <- make_SpatialPointsDataFrame(
    GCFR_clean_flora,
    feature_columns = "genus"
)
GCFR_clean_flora_spdf_family <- make_SpatialPointsDataFrame(
    GCFR_clean_flora,
    feature_columns = "family"
)

# Query their presences in the GCFR border
GCFR_point_query_species <-
    GCFR_clean_flora_spdf_species %over% GCFR_border
GCFR_point_query_genus <-
    GCFR_clean_flora_spdf_genus %over% GCFR_border
GCFR_point_query_family <-
    GCFR_clean_flora_spdf_family %over% GCFR_border
stopifnot(
    length(GCFR_clean_flora_spdf_species) == length(!is.na(GCFR_point_query_species))
)
stopifnot(
    length(GCFR_clean_flora_spdf_genus) == length(!is.na(GCFR_point_query_genus))
)
stopifnot(
    length(GCFR_clean_flora_spdf_family) == length(!is.na(GCFR_point_query_family))
)

# Trim!
trimmed_GCFR_clean_flora_spdf_species <-
    GCFR_clean_flora_spdf_species[!is.na(GCFR_point_query_species)[, 1], ]
trimmed_GCFR_clean_flora_spdf_genus <-
    GCFR_clean_flora_spdf_genus[!is.na(GCFR_point_query_genus)[, 1], ]
trimmed_GCFR_clean_flora_spdf_family <-
    GCFR_clean_flora_spdf_family[!is.na(GCFR_point_query_family)[, 1], ]

# .... SWAFR -------------------------------------------------------------------

# Make SpatialPointsDataFrames for species, genus, and family occurences
SWAFR_clean_flora_spdf_species <- make_SpatialPointsDataFrame(
    SWAFR_clean_flora,
    feature_columns = "species"
)
SWAFR_clean_flora_spdf_genus <- make_SpatialPointsDataFrame(
    SWAFR_clean_flora,
    feature_columns = "genus"
)
SWAFR_clean_flora_spdf_family <- make_SpatialPointsDataFrame(
    SWAFR_clean_flora,
    feature_columns = "family"
)

# Query their presences in the SWAFR border
SWAFR_point_query_species <-
    SWAFR_clean_flora_spdf_species %over% SWAFR_border
SWAFR_point_query_genus <-
    SWAFR_clean_flora_spdf_genus %over% SWAFR_border
SWAFR_point_query_family <-
    SWAFR_clean_flora_spdf_family %over% SWAFR_border
stopifnot(
    length(SWAFR_clean_flora_spdf_species) == length(!is.na(SWAFR_point_query_species))
)
stopifnot(
    length(SWAFR_clean_flora_spdf_genus) == length(!is.na(SWAFR_point_query_genus))
)
stopifnot(
    length(SWAFR_clean_flora_spdf_family) == length(!is.na(SWAFR_point_query_family))
)
trimmed_SWAFR_clean_flora_spdf_species <-
    SWAFR_clean_flora_spdf_species[!is.na(SWAFR_point_query_species)[, 1], ]
trimmed_SWAFR_clean_flora_spdf_genus <-
    SWAFR_clean_flora_spdf_genus[!is.na(SWAFR_point_query_genus)[, 1], ]
trimmed_SWAFR_clean_flora_spdf_family <-
    SWAFR_clean_flora_spdf_family[!is.na(SWAFR_point_query_family)[, 1], ]

# Get pixel IDs for points -----------------------------------------------------

trimmed_GCFR_clean_flora_spdf_species$cell_nos <- cellFromXY(
    GCFR_richness_QDS,
    trimmed_GCFR_clean_flora_spdf_species
)
trimmed_GCFR_clean_flora_spdf_genus$cell_nos <- cellFromXY(
    GCFR_richness_QDS,
    trimmed_GCFR_clean_flora_spdf_genus
)
trimmed_GCFR_clean_flora_spdf_family$cell_nos <- cellFromXY(
    GCFR_richness_QDS,
    trimmed_GCFR_clean_flora_spdf_family
)

trimmed_SWAFR_clean_flora_spdf_species$cell_nos <- cellFromXY(
    SWAFR_richness_QDS,
    trimmed_SWAFR_clean_flora_spdf_species
)
trimmed_SWAFR_clean_flora_spdf_genus$cell_nos <- cellFromXY(
    SWAFR_richness_QDS,
    trimmed_SWAFR_clean_flora_spdf_genus
)
trimmed_SWAFR_clean_flora_spdf_family$cell_nos <- cellFromXY(
    SWAFR_richness_QDS,
    trimmed_SWAFR_clean_flora_spdf_family
)

# Save finals ------------------------------------------------------------------

write_rds(
    trimmed_GCFR_clean_flora_spdf_species,
    here::here("data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_species")
)
write_rds(
    trimmed_GCFR_clean_flora_spdf_genus,
    here::here("data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_genus")
)
write_rds(
    trimmed_GCFR_clean_flora_spdf_family,
    here::here("data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_family")
)
write_rds(
    trimmed_SWAFR_clean_flora_spdf_species,
    here::here("data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_species")
)
write_rds(
    trimmed_SWAFR_clean_flora_spdf_genus,
    here::here("data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_genus")
)
write_rds(
    trimmed_SWAFR_clean_flora_spdf_family,
    here::here("data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_family")
)

# Compile communities by cell --------------------------------------------------

communities_by_cell_GCFR_QDS <- compile_communities_by_cell(
    trimmed_GCFR_clean_flora_spdf_species,
    "species"
)
# TODO: genus
# fam
communities_by_cell_SWAFR_QDS <- compile_communities_by_cell(
    trimmed_SWAFR_clean_flora_spdf_species,
    "species"
)
#TODO:
# GENUS
# FAM

write_rds(
    communities_by_cell_GCFR_QDS,
    here::here("data/derived-data/flora/communities_by_cell_GCFR_QDS")
)
write_rds(
    communities_by_cell_SWAFR_QDS,
    here::here("data/derived-data/flora/communities_by_cell_SWAFR_QDS")
)
