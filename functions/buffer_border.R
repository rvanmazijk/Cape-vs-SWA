buffer_border <- function(border, region = c("GCFR", "SWAFR"),
                          width = 0.1, dissolve = TRUE) {
  buffered_border <- raster::buffer(border, width = width, dissolve = dissolve)
  border_data <- data.frame(region = region, row.names = names(buffered_border))
  buffered_border %<>% SpatialPolygonsDataFrame(border_data)
  buffered_border
}
