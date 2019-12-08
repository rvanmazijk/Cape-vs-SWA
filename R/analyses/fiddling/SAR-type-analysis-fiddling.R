# Setup
library(here)
source(here("draft-02/R/setup.R"))

# Import data
GCFR <- read_rds(here(
  "data/derived-data/flora",
  "trimmed_GCFR_clean_flora_spdf_species"
))
SWAFR <- read_rds(here(
  "data/derived-data/flora",
  "trimmed_SWAFR_clean_flora_spdf_species"
))

# Put QDS-codes in species spatial points data frame
import_region_polygons()
GCFR  %<>% get_geocodes(GCFR_QDS[,  "qdgc"])
SWAFR %<>% get_geocodes(SWAFR_QDS[, "qdgc"])

sample_SAR_data <- function(region) {
  QDS_names <- unique(region$qdgc)
  n_QDS <- length(QDS_names) / 2
  d <- NULL
  for (j in 1:10) {
    r <- vector(mode = "numeric", length = n_QDS)
    for (i in 1:n_QDS) {
      sampled_QDS <- sample(QDS_names, i)
      r[[i]] <- length(unique(region$species[region$qdgc == sampled_QDS]))
    }
    d <- rbind(d, data.frame(n_QDS_sampled = 1:n_QDS, richness = r))
    print(j)
  }
  d
}
GCFR_SAR_data  <- sample_SAR_data(GCFR)
SWAFR_SAR_data <- sample_SAR_data(SWAFR)
SAR_data <- rbind(
  cbind(region = "GCFR",  GCFR_SAR_data),
  cbind(region = "SWAFR", SWAFR_SAR_data)
)
ggplot(SAR_data, aes(n_QDS_sampled, richness, colour = region)) +
  geom_point()

m <- lm(log(richness) ~ log(n_QDS_sampled)*region, SAR_data)
summary(m)
visreg::visreg(m, xvar = "n_QDS_sampled", by = "region", overlay = TRUE)

plot(log(richness) ~ log(n_QDS_sampled), SAR_data, col = SAR_data$region)
m_GCFR <- lm(
  log(richness) ~ log(n_QDS_sampled),
  SAR_data[SAR_data$region == "GCFR", ]
)
m_SWAFR <- lm(
  log(richness) ~ log(n_QDS_sampled),
  SAR_data[SAR_data$region == "SWAFR", ]
)
abline(m_GCFR)
abline(m_SWAFR)
