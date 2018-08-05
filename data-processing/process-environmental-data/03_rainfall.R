# Processing environmental data: Rainfall
# CHIRPs processing from NASA for my two regions
# Cape vs SWA publication
# Ruan van Mazijk

# Import regions' boxes
GCFR_box <- readOGR(here::here("data/derived-data/borders/GCFR_box/"))
SWAFR_box <- readOGR(here::here("data/derived-data/borders/SWAFR_box/"))

# MAP --------------------------------------------------------------------------

# .... Make a stack of each years rainfall ---------------------------------

# Implementing in parallel for each region

# Pseudo-code idea: implementing this in parallel:
#       > years <- 1981:2016  # no 2017 annual yet, duh
#       > stacked_chirps <- stack()
#       > for (i in seq_along(years)) {
#       >     x <-
#       >         paste0(
#       >             giswd,
#       >             "CHIRPS_v2.0/",
#       >             "global_annual/",
#       >             "chirps-v2.0.",
#       >             years[i], ".tif"
#       >         ) %>%
#       >         raster() %>%
#       >         mask(GCFR_box) %>%
#       >         crop(GCFR_box)
#       >     stacked_chirps %<>% stack(x)
#       >     glue("chirps-v2.0.{years[i]} is done")
#       > }
#       > beep(2)
#       > writeRaster(
#       >     stacked_chirps,
#       >     filename = here::here(
#       >         "Data",
#       >         "derived-data",
#       >         "rainfall",
#       >         "CHIRPS_annual_GCFR_box.grd"
#       >     ),
#       >     bandorder = "BIL",
#       >     overwrite = TRUE
#       > )

cluster <- makeCluster(detectCores() - 1, outfile = "")
clusterMap(cluster,
  box = c(GCFR_box, SWAFR_box),
  region = c("GCFR", "SWAFR"),
  giswd = giswd,
  function(box, region, giswd) {
    years <- 1981:2016 # no 2017 annual yet, duh
    stacked_chirps <- raster::stack()

    for (i in seq_along(years)) {
      x <- raster::crop(raster::mask(raster::raster(paste0(
        giswd,
        "CHIRPS_v2.0/",
        "global_annual/",
        "chirps-v2.0.",
        years[i], ".tif"
      )), box), box)
      stacked_chirps <- raster::stack(stacked_chirps, x)
      message(glue::glue("
        chirps-v2.0.{years[i]} is done
      "))
    }

    raster::writeRaster(
      stacked_chirps,
      filename = here::here(
        "data/derived-data/rainfall",
        glue::glue("CHIRPS_annual_{region}_box.grd")
      ),
      bandorder = "BIL",
      overwrite = TRUE
    )
    message(glue::glue("
      CHIRPS_annual_{region}_box.grd written to disk
    "))
  }
)
stopCluster(cluster)

# .... Mean those stacks to get MAP ----------------------------------------

# GCFR
chirps_annual_GCFR <- stack()
for (i in 1:36) {
  x <- raster(
    band = i,
    here::here("data/derived-data/rainfall/CHIRPS_annual_GCFR_box.grd")
  )
  chirps_annual_GCFR %<>% stack(x)
}
chirps_annual_GCFR
MAP_GCFR <- mean(chirps_annual_GCFR)
# Trim the ocean
MAP_GCFR[MAP_GCFR < -100] <- NA
# Save
writeRaster(
  MAP_GCFR,
  overwrite = TRUE,
  here::here("data/derived-data/rainfall/MAP_GCFR_box.tif")
)

# SWAFR
chirps_annual_SWAFR <- stack()
for (i in 1:36) {
  x <- raster(
    band = i,
    here::here("data/derived-data/rainfall/CHIRPS_annual_SWAFR_box.grd")
  )
  chirps_annual_SWAFR %<>% stack(x)
}
chirps_annual_SWAFR
MAP_SWAFR <- mean(chirps_annual_SWAFR)
# Trim the ocean
MAP_SWAFR[MAP_SWAFR < -100] <- NA
# Save
writeRaster(
  MAP_SWAFR,
  overwrite = TRUE,
  here::here("data/derived-data/rainfall/MAP_SWAFR_box.tif")
)

# Bioclimatic variables --------------------------------------------------------

make_monthly_layers_in_box <- function() {
  # For rainfall
  years <- 1981:2016
  # 2017 Jan, Feb will be added manually below
  months <- c(
    "01", "02", "03", "04",
    "05", "06", "07", "08",
    "09", "10", "11", "12"
  )
  stacked_chirps <- stack()
  for (i in seq_along(years)) {
    for (j in seq_along(months)) {
      x <- raster::crop(raster::mask(raster::raster(paste0(
        giswd,
        "CHIRPS_v2.0/",
        "global_monthly/",
        "chirps-v2.0.",
        years[i], ".", months[j], ".tif"
      )), box), box)
      stacked_chirps <- raster::stack(stacked_chirps, x)
      message(glue::glue("
        chirps-v2.0.{years[i]}.{months[j]} is finished.
      "))
    }
  }
  # Add the two months of 2017 manually
  # (accidentally didnt include them in the 1st loop)
  for (j in 1:2) {
    x <- raster::crop(raster::mask(raster::raster(paste0(
      giswd,
      "CHIRPS_v2.0/",
      "global_monthly/",
      "chirps-v2.0.2017.", months[j], ".tif"
    )), box), box)
    stacked_chirps <- raster::stack(stacked_chirps, x)
  }
  raster::writeRaster(
    stacked_chirps,
    filename = here::here(
      "data/derived-data/rainfall",
      glue::glue("CHIRPS_monthly_{region}_box.grd")
    ),
    bandorder = "BIL",
    overwrite = TRUE
  )
}
make_monthly_layers_in_box(
  box = GCFR_box,
  region = "GCFR",
  giswd = giswd
)
make_monthly_layers_in_box(
  box = SWAFR_box,
  region = "SWAFR",
  giswd = giswd
)

# TODO: update to reflect reality
# Read stack back from those files written to disc above
# And loop to add each band (= layer) to the stack
chirps_monthly_GCFR <- stack()
for (i in 1:434) {
  x <- raster(
    band = i,
    paste0(
      # go up to parent dir of repo,
      "../",
      "Hons-thesis_more-things/",
      # then down again to where CHIRPS_box/ is
      "CHIRPS_box/",
      "CHIRPS_monthly_GCFR_box.grd"
    )
  )
  chirps_monthly_GCFR %<>% stack(x)
}
chirps_monthly_SWAFR <- stack()
for (i in 1:434) {
  x <- raster(
    band = i,
    paste0(
      # go up to parent dir of repo,
      "../",
      "Hons-thesis_more-things/",
      # then down again to where CHIRPS_box/ is
      "CHIRPS_box/",
      "CHIRPS_monthly_SWAFR_box.grd"
    )
  )
  chirps_monthly_SWAFR %<>% stack(x)
}
chirps_names <- names(chirps_monthly_SWAFR)

# .... GCFR --------------------------------------------------------------------

# ........ Organise by year & month --------------------------------------------

years <- 1981:2017
chirps_by_year <- vector("list", length = length(years))
for (i in seq_along(years)) {
  chirps_by_year[[i]] <-
    chirps_names[str_detect(chirps_names, pattern = glue("
      \\.{years[i]}\\.
    "))]
  names(chirps_by_year)[i] <- years[i]
}
for (i in seq_along(chirps_by_year)) {
  names(chirps_by_year[[i]]) <-
    if (names(chirps_by_year[i]) == "2017") {
      c("Jan", "Feb")
    } else {
      c(
        "Jan", "Feb", "Mar", "Apr",
        "May", "Jun", "Jul", "Aug",
        "Sep", "Oct", "Nov", "Dec"
      )
    }
}
chirps_by_year_by_month <- chirps_by_year

# ........ Average by month ----------------------------------------------------

# Jan, Feb
jan_stack <- stack()
for (i in seq_along(years)) {
  x <- chirps_monthly_GCFR[[
    chirps_by_year_by_month[[as.character(years[i])]]["Jan"]
  ]]
  jan_stack %<>% stack(x)
}
jan_stack
writeRaster(jan_stack, here::here(
  "Data",
  "derived-data",
  "rainfall",
  "CHIRPS_01_GCFR_box.tif"
))
writeRaster(mean(jan_stack), here::here(
  "Data",
  "derived-data",
  "rainfall",
  "CHIRPS_mean01_GCFR_box.tif"
))
feb_stack <- stack()
for (i in seq_along(years)) {
  x <- chirps_monthly_GCFR[[
    chirps_by_year_by_month[[as.character(years[i])]]["Feb"]
  ]]
  feb_stack %<>% stack(x)
}
feb_stack
writeRaster(feb_stack, here::here(
  "Data",
  "derived-data",
  "rainfall",
  "CHIRPS_02_GCFR_box.tif"
))
writeRaster(mean(feb_stack), here::here(
  "Data",
  "derived-data",
  "rainfall",
  "CHIRPS_mean02_GCFR_box.tif"
))

# Mar-Dec
months <- c(
  "Mar", "Apr",
  "May", "Jun", "Jul", "Aug",
  "Sep", "Oct", "Nov", "Dec"
)
for (j in seq_along(months)) {
  month_stack <- stack()
  for (i in seq_along(years[years != 2017])) {
    x <- chirps_monthly_GCFR[[
      chirps_by_year_by_month[[as.character(years[i])]][months[j]]
    ]]
    month_stack %<>% stack(x)
  }
  writeRaster(month_stack, here::here(
    "Data",
    "derived-data",
    "rainfall",
    # glue("{j+2}") because already done Jan & Feb
    glue("CHIRPS_0{j+2}_GCFR_box.tif")
  ))
  writeRaster(mean(month_stack), here::here(
    "Data",
    "derived-data",
    "rainfall",
    glue("CHIRPS_mean0{j+2}_GCFR_box.tif")
  ))
}

# ........ Make monthly avg stack ----------------------------------------------

GCFR_monthly_CHIRPS_means <- stack()
for (i in 1:12) {
  x <- raster(here::here(
    "Data",
    "derived-data",
    "rainfall",
    glue("CHIRPS_mean0{i}_GCFR_box.tif")
  ))
  GCFR_monthly_CHIRPS_means %<>% stack(x)
}
# Trim ocean
for (i in 1:12) {
  GCFR_monthly_CHIRPS_means[[i]][
    GCFR_monthly_CHIRPS_means[[i]] < -100
  ] <- NA
}
GCFR_monthly_CHIRPS_means
plot(GCFR_monthly_CHIRPS_means)
writeRaster(
  GCFR_monthly_CHIRPS_means,
  here::here(
    "Data",
    "derived-data",
    "rainfall",
    "GCFR_monthly_CHIRPS_means_no_ocean.tif"
  )
)

# ........ Make bioclim vars ---------------------------------------------------

# Practice
if (FALSE) {

  # Doesn't work...
  #   > install.packages(
  #   +     "climates",
  #   +     ,
  #   +     repos = "http://rforge.net/",
  #   +     dependencies = TRUE,
  #   +     type = "source"
  #   + )
  #   > library(climates)
  #   > climates::bioclim(tmin)

  # Whereas this is still supported
  p_load(dismo)
  tmin <- c(10, 12, 14, 16, 18, 20, 22, 21, 19, 17, 15, 12)
  tmax <- tmin + 5
  prec <- c(0, 2, 10, 30, 80, 160, 80, 20, 40, 60, 20, 0)
  dismo::biovars(prec, tmin, tmax)

  # But it "requires" all 3 input vars,
  # even if you only want a subset of the BIOCLIM 19 vars

  # So I will dissect the function to find the bits I need:

  # Copied & modified this from within the source of `biovars`
  # (<https://github.com/cran/dismo/blob/master/R/biovars.R>)
  # !! Now *all* in helper-funs/ !!

  if (FALSE) {
    window <- function(x) {
      lng <- length(x)
      x <- c(x, x[1:3])
      m <- matrix(ncol = 3, nrow = lng)
      for (i in 1:3) {
        m[, i] <- x[i:(lng + i - 1)]
      }
      apply(m, MARGIN = 1, FUN = sum)
    }

    # precip by quarter (3 months)
    prec_box <- as.matrix(GCFR_monthly_CHIRPS_means)
    # or
    prec_GCFR <- as.matrix(crop(GCFR_monthly_CHIRPS_means, GCFR_border))

    # P15. Precipitation Seasonality(Coefficient of Variation)
    # the "1 +" is to avoid strange CVs for areas
    # where mean rainfaill is < 1)
    # Note: This does not vary *too much*
    # depending on the extent of the input raster
    pcv <- apply(prec_box + 1, 1, cv)
    pcv <- raster(t(matrix(pcv, nrow = 211, ncol = 133)))
    plot(pcv)
    # cf.
    pcv <- apply(prec_GCFR + 1, 1, cv)
    pcv <- raster(t(matrix(pcv, nrow = 211, ncol = 133)))
    plot(pcv)

    # P16. Precipitation of Wettest Quarter
    # Note: This *does* change notably
    # depending on the extent of the input raster
    wet <- t(apply(prec_box, 1, window))
    pwq <- apply(wet, 1, max)
    pwq <- raster(t(matrix(pwq, nrow = 211, ncol = 133)))
    plot(pwq)
    # cf.
    wet <- t(apply(prec_GCFR, 1, window))
    pwq <- apply(wet, 1, max)
    pwq <- raster(t(matrix(pwq, nrow = 211, ncol = 133)))
    plot(pwq)
    # 2017-09-16 14:48 --- Note: actually it DOESN'T change,
    # it's just the scale!
    # The cropped input yields exactly the same values
    # in the GCFR as the full one :)

    # P17. Precipitation of Driest Quarter
    pdq <- apply(wet, 1, min)
    pdq <- raster(t(matrix(pdq, nrow = 300, ncol = 200)))
    plot(pdq)

    tmp <- t(apply(as.matrix(GCFR), 1, window)) / 3
    # P10. Mean Temperature of Warmest Quarter
    twq <- apply(tmp, 1, max)
  }
}

# Now for real
GCFR_PWQ <- biovars_PWQ(GCFR_monthly_CHIRPS_means)
GCFR_PDQ <- biovars_PDQ(GCFR_monthly_CHIRPS_means)
GCFR_PCV <- biovars_PCV(GCFR_monthly_CHIRPS_means)
writeRaster(GCFR_PWQ, here::here(
  "Data",
  "derived-data",
  "rainfall",
  "GCFR_PWQ_box.tiff"
))
writeRaster(GCFR_PDQ, here::here(
  "Data",
  "derived-data",
  "rainfall",
  "GCFR_PDQ_box.tiff"
))
writeRaster(GCFR_PCV, here::here(
  "Data",
  "derived-data",
  "rainfall",
  "GCFR_PCV_box.tiff"
))

# .... SWAFR -------------------------------------------------------------------

# ........ Organise by year & month --------------------------------------------

years <- 1981:2017
chirps_by_year <- vector("list", length = length(years))
for (i in seq_along(years)) {
  chirps_by_year[[i]] <-
    chirps_names[str_detect(chirps_names, pattern = glue("
      \\.{years[i]}\\.
    "))]
  names(chirps_by_year)[i] <- years[i]
}
for (i in seq_along(chirps_by_year)) {
  names(chirps_by_year[[i]]) <-
    if (names(chirps_by_year[i]) == "2017") {
      c("Jan", "Feb")
    } else {
      c(
        "Jan", "Feb", "Mar", "Apr",
        "May", "Jun", "Jul", "Aug",
        "Sep", "Oct", "Nov", "Dec"
      )
    }
}
chirps_by_year_by_month <- chirps_by_year

# ........ Average by month ----------------------------------------------------

# Jan, Feb
jan_stack <- stack()
for (i in seq_along(years)) {
  x <- chirps_monthly_SWAFR[[
    chirps_by_year_by_month[[as.character(years[i])]]["Jan"]
  ]]
  jan_stack %<>% stack(x)
}
jan_stack
writeRaster(jan_stack, here::here(
  "Data",
  "derived-data",
  "rainfall",
  "CHIRPS_01_SWAFR_box.tif"
))
writeRaster(mean(jan_stack), here::here(
  "Data",
  "derived-data",
  "rainfall",
  "CHIRPS_mean01_SWAFR_box.tif"
))
feb_stack <- stack()
for (i in seq_along(years)) {
  x <- chirps_monthly_SWAFR[[
    chirps_by_year_by_month[[as.character(years[i])]]["Feb"]
  ]]
  feb_stack %<>% stack(x)
}
feb_stack
writeRaster(feb_stack, here::here(
  "Data",
  "derived-data",
  "rainfall",
  "CHIRPS_02_SWAFR_box.tif"
))
writeRaster(mean(feb_stack), here::here(
  "Data",
  "derived-data",
  "rainfall",
  "CHIRPS_mean02_SWAFR_box.tif"
))

# Mar-Dec
months <- c(
  "Mar", "Apr",
  "May", "Jun", "Jul", "Aug",
  "Sep", "Oct", "Nov", "Dec"
)
for (j in seq_along(months)) {
  month_stack <- stack()
  for (i in seq_along(years[years != 2017])) {
    x <- chirps_monthly_SWAFR[[
    chirps_by_year_by_month[[as.character(years[i])]][months[j]]
    ]]
    month_stack %<>% stack(x)
  }
  writeRaster(month_stack, here::here(
    "Data",
    "derived-data",
    "rainfall",
    # glue("{j+2}") because already done Jan & Feb
    glue("CHIRPS_0{j+2}_SWAFR_box.tif")
  ))
  writeRaster(mean(month_stack), here::here(
    "Data",
    "derived-data",
    "rainfall",
    glue("CHIRPS_mean0{j+2}_SWAFR_box.tif")
  ))
}

# ........ Make monthly avg stack ----------------------------------------------

SWAFR_monthly_CHIRPS_means <- stack()
for (i in 1:12) {
  x <- raster(here::here(
    "Data",
    "derived-data",
    "rainfall",
    glue("CHIRPS_mean0{i}_SWAFR_box.tif")
  ))
  SWAFR_monthly_CHIRPS_means %<>% stack(x)
}
# Trim ocean
for (i in 1:12) {
  SWAFR_monthly_CHIRPS_means[[i]][
    SWAFR_monthly_CHIRPS_means[[i]] < -100
  ] <- NA
}
SWAFR_monthly_CHIRPS_means
plot(SWAFR_monthly_CHIRPS_means)
writeRaster(
  SWAFR_monthly_CHIRPS_means,
  here::here(
    "Data",
    "derived-data",
    "rainfall",
    "SWAFR_monthly_CHIRPS_means_no_ocean.tif"
  )
)

# ........ Make bioclim vars ---------------------------------------------------

SWAFR_PWQ <- biovars_PWQ(SWAFR_monthly_CHIRPS_means)
SWAFR_PDQ <- biovars_PDQ(SWAFR_monthly_CHIRPS_means)
SWAFR_PCV <- biovars_PCV(SWAFR_monthly_CHIRPS_means)
writeRaster(SWAFR_PWQ, here::here(
  "Data",
  "derived-data",
  "rainfall",
  "SWAFR_PWQ_box.tiff"
))
writeRaster(SWAFR_PDQ, here::here(
  "Data",
  "derived-data",
  "rainfall",
  "SWAFR_PDQ_box.tiff"
))
writeRaster(SWAFR_PCV, here::here(
  "Data",
  "derived-data",
  "rainfall",
  "SWAFR_PCV_box.tiff"
))

# Done!
