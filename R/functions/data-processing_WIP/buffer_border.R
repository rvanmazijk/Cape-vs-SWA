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

# FIDDLES
#region <- GCFR_border
#
## <https://gis.stackexchange.com/questions/163445/getting-topologyexception-input#-geom-1-is-invalid-which-is-due-to-self-intersec>
## ...
## this is a well known R / GEOS hack (usually combined with the above) to
## deal with "bad" polygons
#region <- gBuffer(region, byid=TRUE, width=0)
#
## <https://philmikejones.wordpress.com/2015/09/03/dissolve-polygons-in-r/>
## ...
## Now the dissolve
#region <- gUnaryUnion(region)
#plot(region)
#
## <https://stackoverflow.com/questions/12663263/dissolve-holes-in-polygon-in-r>
## ...
## Remove the "holes" (holes have ringDir = -1)
#region@polygons[[1]]@Polygons <-
#  region@polygons[[1]]@Polygons[
#    unlist(map(
#      region@polygons[[1]]@Polygons,
#      ~ .@ringDir == 1
#    ))
#  ]
#plot(region)
#
## Remove overlapping polygons too
#region <- maptools::unionSpatialPolygons(region)
#
#region@polygons[[1]]@Polygons <-
#  region@polygons[[1]]@Polygons[
#    max(
#      unlist(map(
#        region@polygons[[1]]@Polygons,
#        ~ .@area
#      ))
#    )
#  ]
#plot(region)
