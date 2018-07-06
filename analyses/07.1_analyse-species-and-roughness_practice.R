# Relate floral species turnover, and richness, with environmental roughness
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
map(pre_analysis_import_paths, source)

tidy_var_names <- function(x) {
  str_replace_all(tolower(x), " ", "_")
}
names(GCFR_variables_QDS) %<>% tidy_var_names()

GCFR <-
  c(richness = mask(GCFR_richness_QDS, GCFR_border_buffered),
    GCFR_variables_QDS) %>%
  map(~ .[]) %>%
  as_tibble()

compile_species_enviro_roughness <- function(variables_at_focal_scale,
                                             focal_scale = c("HDS", "threeQDS"),
                                             variables_at_QDS,
                                             var_names,
                                             gamma_beta_alpha_at_focal_scale,
                                             rank = c("species", "genus", "family"),
                                             border,
                                             region = c("GCFR", "SWAFR")) {

  stopifnot(
    is.list(variables_at_focal_scale) &
    is.list(variables_at_QDS)
  )

  # Collate absolute environmental variable values, traking cell no.
  all_prepped_variable_data <- data.frame(focal_scale_cell_no = "blank")
  i <- 1
  for (variable in variables_at_focal_scale) {
    variable_cell_nos <- unlist(cellFromPolygon(variable, border))
    prepped_variable_data <- tibble(
      focal_scale_cell_no = paste0("cell_", variable_cell_nos),
      value = variable[variable_cell_nos]
    )
    names(prepped_variable_data)[[2]] <- str_replace(var_names[[i]], " ", "_")
    all_prepped_variable_data %<>% full_join(prepped_variable_data)
    i %<>% add(1)
  }
  all_prepped_variable_data <- all_prepped_variable_data[-1, ]
  names(all_prepped_variable_data)[1] <- paste0(focal_scale, "_cell_no")

  # Calculate roughness as the sd of QDS cells in an HDS or 3QDS,
  # then collate roughness values, traking cell no.
  all_prepped_roughness_data <- data.frame(focal_scale_cell_no = "blank")
  i <- 1
  for (variable in variables_at_focal_scale) {
    roughness <- raster::aggregate(
      variable,
      fact = switch(focal_scale,
        "HDS" = 2,
        "threeQDS" = 3
      ),
      fun = sd,
      na.rm = TRUE
    )
    roughness_cell_nos <- unlist(cellFromPolygon(roughness, border))
    prepped_roughness_data <- tibble(
      focal_scale_cell_no = paste0("cell_", roughness_cell_nos),
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
  names(all_prepped_roughness_data)[1] <- paste0(focal_scale, "_cell_no")

  # Grab all the richnes and turnover data for the focal scale, QDS scale,
  # and the taxonomic rank asked for
  richness_data <- gamma_beta_alpha_at_focal_scale[
    (gamma_beta_alpha_at_focal_scale$region == region) &
    (gamma_beta_alpha_at_focal_scale$rank == rank) &
    (gamma_beta_alpha_at_focal_scale[[paste0(focal_scale, "_cell_no")]] %in%
      all_prepped_variable_data[[paste0(focal_scale, "_cell_no")]]),
    c(
      "richness", "avg_QDS_richness", "avg_QDS_turnover",
      paste0(focal_scale, "_cell_no")
    )
  ]

  data <- full_join(
    richness_data,
    full_join(
      all_prepped_variable_data,
      all_prepped_roughness_data
    )
  )
  return(data)

}

# Run data compilation ---------------------------------------------------------

# .... GCFR --------------------------------------------------------------------

# ......... HDS focal scale ----------------------------------------------------

species_enviro_roughness_GCFR_HDS <- compile_species_enviro_roughness(
  variables_at_focal_scale = GCFR_variables_HDS,
  focal_scale = "HDS",
  variables_at_QDS = GCFR_variables_QDS,
  var_names = var_names,
  gamma_beta_alpha_at_focal_scale = gamma_beta_alpha_HDS,
  rank = "species",
  border = GCFR_border,
  region = "GCFR"
)
genus_enviro_roughness_GCFR_HDS <- compile_species_enviro_roughness(
  variables_at_focal_scale = GCFR_variables_HDS,
  focal_scale = "HDS",
  variables_at_QDS = GCFR_variables_QDS,
  var_names = var_names,
  gamma_beta_alpha_at_focal_scale = gamma_beta_alpha_HDS,
  rank = "genus",
  border = GCFR_border,
  region = "GCFR"
)
family_enviro_roughness_GCFR_HDS <- compile_species_enviro_roughness(
  variables_at_focal_scale = GCFR_variables_HDS,
  focal_scale = "HDS",
  variables_at_QDS = GCFR_variables_QDS,
  var_names = var_names,
  gamma_beta_alpha_at_focal_scale = gamma_beta_alpha_HDS,
  rank = "family",
  border = GCFR_border,
  region = "GCFR"
)

# ........ 3QDS focal scale ----------------------------------------------------

species_enviro_roughness_GCFR_3QDS <- compile_species_enviro_roughness(
  variables_at_focal_scale = GCFR_variables_3QDS,
  focal_scale = "threeQDS",
  variables_at_QDS = GCFR_variables_3QDS,
  var_names = var_names,
  gamma_beta_alpha_at_focal_scale = gamma_beta_alpha_3QDS,
  rank = "species",
  border = GCFR_border,
  region = "GCFR"
)
genus_enviro_roughness_GCFR_3QDS <- compile_species_enviro_roughness(
  variables_at_focal_scale = GCFR_variables_3QDS,
  focal_scale = "threeQDS",
  variables_at_QDS = GCFR_variables_3QDS,
  var_names = var_names,
  gamma_beta_alpha_at_focal_scale = gamma_beta_alpha_3QDS,
  rank = "genus",
  border = GCFR_border,
  region = "GCFR"
)
family_enviro_roughness_GCFR_3QDS <- compile_species_enviro_roughness(
  variables_at_focal_scale = GCFR_variables_3QDS,
  focal_scale = "threeQDS",
  variables_at_QDS = GCFR_variables_3QDS,
  var_names = var_names,
  gamma_beta_alpha_at_focal_scale = gamma_beta_alpha_3QDS,
  rank = "family",
  border = GCFR_border,
  region = "GCFR"
)

# .... SWAFR -------------------------------------------------------------------

# ........ HDS focal scale -----------------------------------------------------

species_enviro_roughness_SWAFR_HDS <- compile_species_enviro_roughness(
  variables_at_focal_scale = SWAFR_variables_HDS,
  focal_scale = "HDS",
  variables_at_QDS = SWAFR_variables_QDS,
  var_names = var_names,
  gamma_beta_alpha_at_focal_scale = gamma_beta_alpha_HDS,
  rank = "species",
  border = SWAFR_border,
  region = "SWAFR"
)
genus_enviro_roughness_SWAFR_HDS <- compile_species_enviro_roughness(
  variables_at_focal_scale = SWAFR_variables_HDS,
  focal_scale = "HDS",
  variables_at_QDS = SWAFR_variables_QDS,
  var_names = var_names,
  gamma_beta_alpha_at_focal_scale = gamma_beta_alpha_HDS,
  rank = "genus",
  border = SWAFR_border,
  region = "SWAFR"
)
family_enviro_roughness_SWAFR_HDS <- compile_species_enviro_roughness(
  variables_at_focal_scale = SWAFR_variables_HDS,
  focal_scale = "HDS",
  variables_at_QDS = SWAFR_variables_QDS,
  var_names = var_names,
  gamma_beta_alpha_at_focal_scale = gamma_beta_alpha_HDS,
  rank = "family",
  border = SWAFR_border,
  region = "SWAFR"
)

# ........ 3QDS focal scale ----------------------------------------------------

species_enviro_roughness_SWAFR_3QDS <- compile_species_enviro_roughness(
  variables_at_focal_scale = SWAFR_variables_3QDS,
  focal_scale = "threeQDS",
  variables_at_QDS = SWAFR_variables_3QDS,
  var_names = var_names,
  gamma_beta_alpha_at_focal_scale = gamma_beta_alpha_3QDS,
  rank = "species",
  border = SWAFR_border,
  region = "SWAFR"
)
genus_enviro_roughness_SWAFR_3QDS <- compile_species_enviro_roughness(
  variables_at_focal_scale = SWAFR_variables_3QDS,
  focal_scale = "threeQDS",
  variables_at_QDS = SWAFR_variables_3QDS,
  var_names = var_names,
  gamma_beta_alpha_at_focal_scale = gamma_beta_alpha_3QDS,
  rank = "genus",
  border = SWAFR_border,
  region = "SWAFR"
)
family_enviro_roughness_SWAFR_3QDS <- compile_species_enviro_roughness(
  variables_at_focal_scale = SWAFR_variables_3QDS,
  focal_scale = "threeQDS",
  variables_at_QDS = SWAFR_variables_3QDS,
  var_names = var_names,
  gamma_beta_alpha_at_focal_scale = gamma_beta_alpha_3QDS,
  rank = "family",
  border = SWAFR_border,
  region = "SWAFR"
)

# Group all data together and save ---------------------------------------------

taxa_enviro_roughness_HDS <- rbind(
  cbind(
    region = "GCFR",
    cbind(rank = "species", species_enviro_roughness_GCFR_HDS),
    cbind(rank = "genus", genus_enviro_roughness_GCFR_HDS),
    cbind(rank = "family", family_enviro_roughness_GCFR_HDS)
  ),
  cbind(
    region = "SWAFR",
    cbind(rank = "species", species_enviro_roughness_SWAFR_HDS),
    cbind(rank = "genus", genus_enviro_roughness_SWAFR_HDS),
    cbind(rank = "family", family_enviro_roughness_SWAFR_HDS)
  )
)
write_csv(
  taxa_enviro_roughness_HDS,
  here::here("analyses/07_outputs/taxa_enviro_roughness_HDS.csv")
)

taxa_enviro_roughness_3QDS <- rbind(
  cbind(
    region = "GCFR",
    cbind(rank = "species", species_enviro_roughness_GCFR_3QDS),
    cbind(rank = "genus", genus_enviro_roughness_GCFR_3QDS),
    cbind(rank = "family", family_enviro_roughness_GCFR_3QDS)
  ),
  cbind(
    region = "SWAFR",
    cbind(rank = "species", species_enviro_roughness_SWAFR_3QDS),
    cbind(rank = "genus", genus_enviro_roughness_SWAFR_3QDS),
    cbind(rank = "family", family_enviro_roughness_SWAFR_3QDS)
  )
)
write_csv(
  taxa_enviro_roughness_3QDS,
  here::here("analyses/07_outputs/taxa_enviro_roughness_3QDS.csv")
)
