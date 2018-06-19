# Relate floral species turnover, and richness, with environmental roughness
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("analyses/01_import-region-polygons.R"))
source(here::here("analyses/02_import-floral-data.R"))
source(here::here("analyses/03_import-environmental-data.R"))
import_all_objects_auto(here::here("analyses/06_outputs"))

# All variables test -----------------------------------------------------------

all_prepped_variable_data <- data.frame(HDS_cell_no = "blank")
i <- 1
for (variable in GCFR_variables_HDS) {
    variable_cell_nos <- unlist(cellFromPolygon(variable, GCFR_border))
    prepped_variable_data <- tibble(
        HDS_cell_no = paste0("cell_", variable_cell_nos),
        value = variable[variable_cell_nos]
    )
    names(prepped_variable_data)[[2]] <- str_replace(var_names[[i]], " ", "_")
    all_prepped_variable_data %<>% full_join(prepped_variable_data)
    i %<>% add(1)
}
all_prepped_variable_data <- all_prepped_variable_data[-1, ]

all_prepped_roughness_data <- data.frame(HDS_cell_no = "blank")
i <- 1
for (variable in GCFR_variables_QDS) {  # NB: QDS now, as will avg to w/i HDS
    roughness <- aggregate(variable, fun = sd)
    roughness_cell_nos <- unlist(cellFromPolygon(roughness, GCFR_border))
    prepped_roughness_data <- tibble(
        HDS_cell_no = paste0("cell_", roughness_cell_nos),
        value = roughness[roughness_cell_nos]
    )
    names(prepped_roughness_data)[[2]] <- paste0(
        "roughness_",
        str_replace(var_names[[i]], " ", "_")
    )
    all_prepped_roughness_data %<>% full_join(prepped_roughness_data)
    i %<>% add(1)
}
all_prepped_roughness_data <- all_prepped_roughness_data[-1, ]

richness_data <- gamma_beta_alpha_HDS[
    gamma_beta_alpha_HDS$region == "GCFR" &
    gamma_beta_alpha_HDS$rank == "species" &
    gamma_beta_alpha_HDS$HDS_cell_no %in% all_prepped_variable_data$HDS_cell_no,
    c("richness", "HDS_cell_no")
]

data <- full_join(
    richness_data,
    full_join(
        all_prepped_variable_data,
        all_prepped_roughness_data
    )
)
data[, -2] %<>% map(log)
names(data)[-2] %<>% paste0("log_", .)

