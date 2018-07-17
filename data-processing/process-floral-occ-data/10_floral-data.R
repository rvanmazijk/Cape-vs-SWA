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
