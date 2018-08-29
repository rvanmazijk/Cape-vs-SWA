# Trapezoidal integration (DEPRECATED)

# sensu Hengl et al. 2017. PLoS ONE
integrate_trapezoid <- function(depths = NULL, value_at_depths) {
  if (is.null(depths)) {
    depths <- list(
      sl1 = 0.00,
      sl2 = 0.05,
      sl3 = 0.15,
      sl4 = 0.30,
      sl5 = 0.60,
      sl6 = 1.00,
      sl7 = 2.00
    )
  }
  integral <- 0
  for (i in 2:length(depths)) {
    integral %<>% add(
      (depths[[i]] - depths[[i - 1]]) *
      ((value_at_depths[[i - 1]] + value_at_depths[[i]]) / 2)
    )
  }
  integral %<>% divide_by(
    depths[[length(depths)]]
  )
  return(integral)
}

# Own example
integrate_trapezoid(
  depths = list(
    sl1 = 0.00,
    sl2 = 0.05,
    sl3 = 0.15,
    sl4 = 0.30,
    sl5 = 0.60,
    sl6 = 1.00,
    sl7 = 2.00
  ),
  value_at_depths = 1:7
)

# Reproducing example from Hengl et al. (2017) text (p. 4)
integrate_trapezoid(
  depths = c(0, 5, 15, 30),
  value_at_depths = c(4.5, 5.0, 5.3, 5.0)
)

# Is this the same as / can it be down with a weighted mean?
# Seeing as my depths are always the same, maybe so?

# <new function def is in helper-funs/>

integrate_trapezoid(value_at_depths = 1:7)
# Yup! Same as deprecated trapezoid-rule integration *proper* above :)

# Use `raster::stackApply()` to implem this on SoilGrids data
# Test:

# Read in all depths of a single block
CECSOL_M_250m_01_stack <- stack()
for (i in 1:7) {
  x <- raster(glue("
    {giswd}SoilGrids250m/GCFR/CECSOL_M_sl{i}_250m_01.tiff
  "))
  CECSOL_M_250m_01_stack %<>% stack(x)
}
CECSOL_M_250m_01 <- CECSOL_M_250m_01_stack %>%
  calc(integrate_trapezoid)
# It works!!!
# (the bizarrely high max of the final single layer not artificial,
# ther are a few pixels in the various depth bands with
# values ca. 100 too (I checked))


# How nice that I can still follow the recommendations of Hengl et al. (2017)!
