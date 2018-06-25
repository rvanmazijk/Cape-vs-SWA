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
GCFR_clean_flora <- read.csv(here::here(
    "Data",
    "derived-data",
    "flora",
    "GCFR_clean_flora_2017-09-14.csv"
))
SWAFR_clean_flora <- read.csv(here::here(
    "Data",
    "derived-data",
    "flora",
    "SWAFR_clean_flora_2017-09-14.csv"
))
# Summarise for use in-text
GBIF_in_text <- list(
    pre_accepted_N = list(
        GCFR = GCFR_spp_sets$species,
        SWAFR = SWAFR_spp_sets$species
    ),
    CRAP_names = list(
        GCFR = GCFR_names_CRAP$submitted_name,
        SWAFR = SWAFR_names_CRAP$submitted_name
    ),
    final_N = list(
        GCFR = GCFR_clean_flora$species,
        SWAFR = SWAFR_clean_flora$species
    )
)
GBIF_in_text %<>%
    map(map, unique) %>%
    map(map, length)
