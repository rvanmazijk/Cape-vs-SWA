#' Convert a GBIF data-frame to a SpatialPointsDataFrame
#'
#' @param df Data-frame, from GBID
#' @param lat_column Character, name of the latitude column in \code{df}
#' @param lon_column Character, name of the longiitude column in \code{df}
#' @param feature_columns Character, name of the species/genus/family column in \code{df}
#' @param CRS Character, the coordinate reference system
#'
#' @return A SpatialPointsDataFrame
make_SpatialPointsDataFrame <- function(df,
                                        lat_column = "decimallatitude",
                                        lon_column = "decimallongitude",
                                        feature_columns = c("family", "genus", "species"),
                                        CRS = std_CRS) {
  return(SpatialPointsDataFrame(
    coords = df[, c(lon_column, lat_column)],
    data = df[, c(feature_columns)],
    proj4string = CRS(CRS)
  ))
}

#' Collate a SpatialPointsDataFrame into a list of taxa in grid-cells
#'
#' @param trimmed_points SpatialPointsDataFrame of occurences
#' @param feature_column Character, name of the taxa column in \code{trimmed_points}
#' @param cell_nos Character, defaults to the cell numbers in \code{trimmed_points}
#' @param debug_length Numeric, how many of the cells to limit the run to.
#' @param quiet Logical, whether console updates with information on current operations
#'
#' @return A list of lists
compile_communities_by_cell <- function(trimmed_points,
                                        feature_column = c("species", "genus", "family"),
                                        cell_nos = levels(as.factor(trimmed_points$cell_nos)),
                                        debug_length = NULL,
                                        quiet = FALSE) {

  stopifnot(class(trimmed_points) == "SpatialPointsDataFrame")

  if (!is.null(debug_length)) {
    cell_nos <- cell_nos[1:debug_length]
  }

  communities_by_cell <- vector("list", length = length(cell_nos))
  names(communities_by_cell) <- paste0("cell_", cell_nos)
  for (i in seq_along(cell_nos)) {
    communities_by_cell[[i]] <- trimmed_points[[feature_column]][
      trimmed_points$cell_nos == cell_nos[[i]]
    ]
    if (!quiet) {
      flush.console()
      cat(sep = "",
        "Community described for cell no. ",
        cell_nos[[i]], " (", i, "/", length(cell_nos), ")\r"
      )
    }
  }
  if (!quiet) {
    cat(sep = "",
      "Communities described for all ",
      length(cell_nos), " cells\n"
    )
  }

  communities_by_cell

}

# Tests
if (FALSE) {
  compile_communities_by_cell(
    trimmed_GCFR_clean_flora_spdf_family,
    "species",
    debug_length = 10
  )
}
