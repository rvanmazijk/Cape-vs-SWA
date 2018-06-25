# Import species lists
# FIXME
# GCFR_spp_sets <- readRDS(here::here("data/derived-data/flora/GCFR_spp_sets_local.rds"))
# SWAFR_spp_sets <- readRDS(here::here("data/derived-data/flora/SWAFR_spp_sets_local.rds"))
# GCFR_names_CRAP <- read.csv(here::here("data/derived-data/flora/GCFR_names_CRAP.csv"))
# SWAFR_names_CRAP <- read.csv(here::here("data/derived-data/flora/SWAFR_names_CRAP.csv"))
GCFR_clean_flora <- read_csv(here::here("data/derived-data/flora/GCFR_clean_flora_2017-09-14.csv"))
SWAFR_clean_flora <- read_csv(here::here("data/derived-data/flora/SWAFR_clean_flora_2017-09-14.csv"))

# Summarise for use in-text
richness <- list(
    # FIXME
    n_before_checks = list(
        GCFR = "TODO", #GCFR_spp_sets$species,
        SWAFR = "TODO" #SWAFR_spp_sets$species
    ),
    n_bad_names = list(
        GCFR = "TODO", #GCFR_names_CRAP$submitted_name,
        SWAFR = "TODO" #SWAFR_names_CRAP$submitted_name
    ),
    n_final = list(
        GCFR = GCFR_clean_flora$species,
        SWAFR = SWAFR_clean_flora$species
    )
)
richness %<>%
    map(map, unique) %>%
    map(map, length)

# Save
write_rds(richness, here::here("outputs/richness.RDS"))

# Tidy up
rm(
    # FIXME
    #GCFR_spp_sets,
    #SWAFR_spp_sets,
    #GCFR_names_CRAP,
    #SWAFR_names_CRAP,
    GCFR_clean_flora,
    SWAFR_clean_flora
)
