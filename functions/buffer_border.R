simple_SpatialPolygonsDataFrame <- function(x, value = "val",
                                            row_names = names(x)) {
  stopifnot(class(x) == "SpatialPolygons")
  x_data <- data.frame(x = value, row.names = row_names)
  SpatialPolygonsDataFrame(x, x_data)
}

buffer_border <- function(border, region = c("GCFR", "SWAFR"),
                          width = 0.1, dissolve = TRUE) {
  buffered_border <- raster::buffer(border, width = width, dissolve = dissolve)
  simple_SpatialPolygonsDataFrame(buffered_border, region)
}

tidy_border <- function(border) {
  remove_holes <- function(x) {
    holes <- unlist(map(x, ~ .@hole))
    x[!holes]
  }
  border %<>% raster::buffer(width = 0, dissolve = TRUE)
  border@polygons[[1]]@Polygons %<>% remove_holes()
  simple_SpatialPolygonsDataFrame(border)
}
