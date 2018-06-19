# Relate floral species turnover, and richness, with environmental roughness
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("analyses/01_import-region-polygons.R"))
source(here::here("analyses/02_import-floral-data.R"))
source(here::here("analyses/03_import-environmental-data.R"))
import_all_objects_auto(here::here("analyses/06_outputs"))

# Elevation (abs) test ---------------------------------------------------------

GCFR_elevation_cell_nos <- unlist(cellFromPolygon(
    GCFR_variables_HDS$Elevation,
    GCFR_border
))
elevation_data <- tibble(
    HDS_cell_no = paste0("cell_", GCFR_elevation_cell_nos),
    elevation = GCFR_variables_HDS$Elevation[GCFR_elevation_cell_nos]
)
richness_data <- gamma_beta_alpha_HDS[
    gamma_beta_alpha_HDS$region == "GCFR" &
    gamma_beta_alpha_HDS$rank == "species" &
    gamma_beta_alpha_HDS$HDS_cell_no %in% paste0("cell_", GCFR_elevation_cell_nos),
    c("richness", "HDS_cell_no")
]
data <- full_join(elevation_data, richness_data)
plot(richness ~ elevation, data)

# All variables test -----------------------------------------------------------

all_prepped_variable_data <- data.frame(HDS_cell_no = "blank")
i <- 1
for (variable in GCFR_variables_HDS) {
    variable_cell_nos <- unlist(cellFromPolygon(variable, GCFR_border))
    prepped_variable_data <- tibble(
        HDS_cell_no = paste0("cell_", variable_cell_nos),
        value = variable[variable_cell_nos]
    )
    names(prepped_variable_data)[[2]] <- var_names[[i]]
    all_prepped_variable_data %<>% full_join(prepped_variable_data)
    i %<>% add(1)
}
# In progress:
all_prepped_roughness_data <- data.frame(HDS_cell_no = "blank")
for (variable in GCFR_variables_QDS) {  # NB: QDS now, as will avg to w/i HDS
    roughness <- focal_sd(variable)
}
#/
all_prepped_variable_data <- all_prepped_variable_data[-1, ]
richness_data <- gamma_beta_alpha_HDS[
    gamma_beta_alpha_HDS$region == "GCFR" &
    gamma_beta_alpha_HDS$rank == "species" &
    gamma_beta_alpha_HDS$HDS_cell_no %in% all_prepped_variable_data$HDS_cell_no,
    c("richness", "HDS_cell_no")
]
data <- full_join(richness_data, all_prepped_variable_data)
data %<>% filter_all(any_vars(is.infinite(.)))
plot(richness ~ Elevation, data)
plot(richness ~ MAP, data)
plot(richness ~ PDQ, data)
plot(richness ~ `Surface T`, data)
plot(richness ~ NDVI, data)
plot(richness ~ Clay, data)
plot(richness ~ `Soil C`, data)
plot(richness ~ pH, data)
