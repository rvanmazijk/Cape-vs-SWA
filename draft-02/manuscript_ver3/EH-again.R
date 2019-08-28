# ...
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

# Load packages and functions
library(here)
source(here("draft-02/R/setup.R"))

# Import processed environmental data ------------------------------------------

data_dir <- here("data/derived-data/May-2019")
GCFR_variables_masked2 <- map(var_names,
  ~raster(glue("{data_dir}/GCFR_{.x}_masked2.tif"))
)
names(GCFR_variables_masked2) <- str_replace_all(var_names, " ", "_")
SWAFR_variables_masked2 <- map(var_names,
  ~raster(glue("{data_dir}/SWAFR_{.x}_masked2.tif"))
)
names(SWAFR_variables_masked2) <- str_replace_all(var_names, " ", "_")

import_region_polygons()

# ... --------------------------------------------------------------------------

# [A = absolute; R = roughness]

roughness_cells2 <- function(x, ...) {
  if (length(x) == 4 & !(any(is.nan(x)))) {
    out <- vector(length = length(x))
    for (i in seq_along(x)) {
      out[[i]] <- mean(abs(x[i] - x[-i]), ...)
    }
    return(mean(out, ...))
  } else {
    return(NaN)
  }
}

SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS)
  plot()

SWAFR_new_EH_elev <- list(

  SWAFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.10-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 3, fun = roughness_cells2) %>%  # [R]  0.15-DS
  #  plot()

  SWAFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 2, fun = mean) %>%               # [A] (0.10-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.20-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 4, fun = roughness_cells2) %>%  # [R]  0.20-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 5, fun = roughness_cells2) %>%  # [R]  0.25-DS
  #  plot()

  SWAFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 3, fun = mean) %>%               # [A] (0.15-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.30-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 6, fun = roughness_cells2) %>%  # [R]  0.30-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 7, fun = roughness_cells2) %>%  # [R]  0.35-DS
  #  plot()

  SWAFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 4, fun = mean) %>%               # [A] (0.20-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.40-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 8, fun = roughness_cells2) %>%  # [R]  0.40-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 9, fun = roughness_cells2) %>%  # [R]  0.45-DS
  #  plot()

  SWAFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 5, fun = mean) %>%               # [A] (0.25-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.50-DS  [HDS]
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 10, fun = roughness_cells2) %>% # [R]  0.50-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 11, fun = roughness_cells2) %>% # [R]  0.55-DS
  #  plot()

  SWAFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 6, fun = mean) %>%               # [A] (0.30-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.60-DS
    plot(),

  SWAFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 7, fun = mean) %>%               # [A] (0.35-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.70-DS
    plot(),

  SWAFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 8, fun = mean) %>%               # [A] (0.40-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.80-DS
    plot(),

  SWAFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 9, fun = mean) %>%               # [A] (0.45-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.90-DS
    plot(),

  SWAFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 10, fun = mean) %>%              # [A] (0.50-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  1.00-DS  [DS]
    plot()

)

GCFR_new_EH_elev <- list(

  GCFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.10-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 3, fun = roughness_cells2) %>%  # [R]  0.15-DS
  #  plot()

  GCFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 2, fun = mean) %>%               # [A] (0.10-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.20-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 4, fun = roughness_cells2) %>%  # [R]  0.20-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 5, fun = roughness_cells2) %>%  # [R]  0.25-DS
  #  plot()

  GCFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 3, fun = mean) %>%               # [A] (0.15-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.30-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 6, fun = roughness_cells2) %>%  # [R]  0.30-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 7, fun = roughness_cells2) %>%  # [R]  0.35-DS
  #  plot()

  GCFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 4, fun = mean) %>%               # [A] (0.20-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.40-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 8, fun = roughness_cells2) %>%  # [R]  0.40-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 9, fun = roughness_cells2) %>%  # [R]  0.45-DS
  #  plot()

  GCFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 5, fun = mean) %>%               # [A] (0.25-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.50-DS  [HDS]
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 10, fun = roughness_cells2) %>% # [R]  0.50-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 11, fun = roughness_cells2) %>% # [R]  0.55-DS
  #  plot()

  GCFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 6, fun = mean) %>%               # [A] (0.30-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.60-DS
    plot(),

  GCFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 7, fun = mean) %>%               # [A] (0.35-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.70-DS
    plot(),

  GCFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 8, fun = mean) %>%               # [A] (0.40-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.80-DS
    plot(),

  GCFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 9, fun = mean) %>%               # [A] (0.45-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.90-DS
    plot(),

  GCFR_variables_masked2$Elevation %>%               # [A] (0.05-DS) ->
    aggregate(fact = 10, fun = mean) %>%              # [A] (0.50-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  1.00-DS  [DS]
    plot()

)

SWAFR_new_EH_elev %>%
  imap_dfr(~as.data.frame(cbind(
    resolution = .y,
    EH         = getValues(.x)
  ))) %>%
  filter(!is.nan(EH)) %>%
  mutate(resolution = (resolution + 1)*0.05*2) %>%
  ggplot(aes(resolution, EH)) +
    geom_jitter()

GCFR_new_EH_elev %>%
  imap_dfr(~as.data.frame(cbind(
    resolution = .y,
    EH         = getValues(.x)
  ))) %>%
  filter(!is.nan(EH)) %>%
  mutate(resolution = (resolution + 1)*0.05*2) %>%
  ggplot(aes(resolution, EH)) +
  geom_jitter()

new_EH_elev_CLES <- map2(SWAFR_new_EH_elev, GCFR_new_EH_elev,
  ~CLES(.x[!is.nan(.x)], .y[!is.nan(.y)])
)
plot(as_vector(new_EH_elev_CLES))

SWAFR_new_EH_MAP <- list(

  SWAFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.10-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 3, fun = roughness_cells2) %>%  # [R]  0.15-DS
  #  plot()

  SWAFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 2, fun = mean) %>%               # [A] (0.10-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.20-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 4, fun = roughness_cells2) %>%  # [R]  0.20-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 5, fun = roughness_cells2) %>%  # [R]  0.25-DS
  #  plot()

  SWAFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 3, fun = mean) %>%               # [A] (0.15-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.30-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 6, fun = roughness_cells2) %>%  # [R]  0.30-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 7, fun = roughness_cells2) %>%  # [R]  0.35-DS
  #  plot()

  SWAFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 4, fun = mean) %>%               # [A] (0.20-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.40-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 8, fun = roughness_cells2) %>%  # [R]  0.40-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 9, fun = roughness_cells2) %>%  # [R]  0.45-DS
  #  plot()

  SWAFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 5, fun = mean) %>%               # [A] (0.25-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.50-DS  [HDS]
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 10, fun = roughness_cells2) %>% # [R]  0.50-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 11, fun = roughness_cells2) %>% # [R]  0.55-DS
  #  plot()

  SWAFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 6, fun = mean) %>%               # [A] (0.30-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.60-DS
    plot(),

  SWAFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 7, fun = mean) %>%               # [A] (0.35-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.70-DS
    plot(),

  SWAFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 8, fun = mean) %>%               # [A] (0.40-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.80-DS
    plot(),

  SWAFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 9, fun = mean) %>%               # [A] (0.45-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.90-DS
    plot(),

  SWAFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 10, fun = mean) %>%              # [A] (0.50-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  1.00-DS  [DS]
    plot()

)

GCFR_new_EH_MAP <- list(

  GCFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.10-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 3, fun = roughness_cells2) %>%  # [R]  0.15-DS
  #  plot()

  GCFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 2, fun = mean) %>%               # [A] (0.10-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.20-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 4, fun = roughness_cells2) %>%  # [R]  0.20-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 5, fun = roughness_cells2) %>%  # [R]  0.25-DS
  #  plot()

  GCFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 3, fun = mean) %>%               # [A] (0.15-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.30-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 6, fun = roughness_cells2) %>%  # [R]  0.30-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 7, fun = roughness_cells2) %>%  # [R]  0.35-DS
  #  plot()

  GCFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 4, fun = mean) %>%               # [A] (0.20-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.40-DS
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 8, fun = roughness_cells2) %>%  # [R]  0.40-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 9, fun = roughness_cells2) %>%  # [R]  0.45-DS
  #  plot()

  GCFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 5, fun = mean) %>%               # [A] (0.25-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.50-DS  [HDS]
    plot(),

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 10, fun = roughness_cells2) %>% # [R]  0.50-DS
  #  plot()

  #SWAFR_variables_masked2$Elevation %>%              # [A] (0.05-DS) ->
  #  aggregate(fact = 11, fun = roughness_cells2) %>% # [R]  0.55-DS
  #  plot()

  GCFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 6, fun = mean) %>%               # [A] (0.30-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.60-DS
    plot(),

  GCFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 7, fun = mean) %>%               # [A] (0.35-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.70-DS
    plot(),

  GCFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 8, fun = mean) %>%               # [A] (0.40-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.80-DS
    plot(),

  GCFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 9, fun = mean) %>%               # [A] (0.45-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  0.90-DS
    plot(),

  GCFR_variables_masked2$MAP %>%               # [A] (0.05-DS) ->
    aggregate(fact = 10, fun = mean) %>%              # [A] (0.50-DS) ->
    aggregate(fact = 2, fun = roughness_cells2) %T>%  # [R]  1.00-DS  [DS]
    plot()

)

SWAFR_new_EH_MAP %>%
  imap_dfr(~as.data.frame(cbind(
    resolution = .y,
    EH         = getValues(.x)
  ))) %>%
  filter(!is.nan(EH)) %>%
  mutate(resolution = (resolution + 1)*0.05*2) %>%
  ggplot(aes(resolution, EH)) +
    geom_jitter()

GCFR_new_EH_MAP %>%
  imap_dfr(~as.data.frame(cbind(
    resolution = .y,
    EH         = getValues(.x)
  ))) %>%
  filter(!is.nan(EH)) %>%
  mutate(resolution = (resolution + 1)*0.05*2) %>%
  ggplot(aes(resolution, EH)) +
    geom_jitter()

new_EH_MAP_CLES <- map2(SWAFR_new_EH_MAP, GCFR_new_EH_MAP,
  ~CLES(.x[!is.nan(.x)], .y[!is.nan(.y)])
)
plot(as_vector(new_EH_MAP_CLES))

foo <- data.frame(rbind(
  cbind(
    method = "old",
    rbind(
      cbind(
        region = "GCFR",
        EH     = HDS$Elevation_roughness[HDS$region == "GCFR"]
      ),
      cbind(
        region = "SWAFR",
        EH     = HDS$Elevation_roughness[HDS$region == "SWAFR"]
      )
    )
  ),
  cbind(
    method = "new",
    region = c(
      rep(
        "GCFR",
        length(GCFR_new_EH_elev[[5]][!is.nan(GCFR_new_EH_elev[[5]])])
      ),
      rep(
        "SWAFR",
        length(SWAFR_new_EH_elev[[5]][!is.nan(SWAFR_new_EH_elev[[5]])])
      )
    ),
    EH = scale(log1p(c(
      GCFR_new_EH_elev[[5]][!is.nan(GCFR_new_EH_elev[[5]])],
      SWAFR_new_EH_elev[[5]][!is.nan(SWAFR_new_EH_elev[[5]])]
    )))
  )
))
foo$EH %<>%
  as.character() %>%
  as.numeric()
ggplot(foo, aes(region, EH, colour = method)) +
  geom_boxplot()
# Yay!(?)

# Compare to this:
roughness_matrices$HDS %>%
  mutate(Elevation = scale(log1p(Elevation))) %>%
  ggplot(aes(region, Elevation)) +
  geom_boxplot()

hist(HDS$MAP_roughness[HDS$region == "GCFR"])
hist(scale(GCFR_new_EH_MAP[[5]][!is.nan(GCFR_new_EH_MAP[[5]])]))
hist(HDS$MAP_roughness[HDS$region == "SWAFR"])
hist(scale(SWAFR_new_EH_MAP[[5]][!is.nan(SWAFR_new_EH_MAP[[5]])]))
