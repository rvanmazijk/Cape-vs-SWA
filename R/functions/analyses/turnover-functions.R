qdgc2hdgc <- function(x) {
  # QDS -> HDS by dropping the last letter
  substr(x, 1, nchar(x) - 1)
}
get_geocodes <- function(flora_points, QDS_polygon) {
  flora_points@data$qdgc <- over(flora_points, QDS_polygon)[[1]]
  flora_points@data$hdgc <- map_chr(flora_points@data$qdgc, ~
    .x %>%
      as.character() %>%
      qdgc2hdgc()
  )
  flora_points
}

calc_richness_turnover <- function(flora_points, QDS_polygon, output_path,
                                   region_name = NULL, date = NULL) {
  # Master function to calculate richness and turnover metrics in grid cells
  # using a SpatialPointsDataFrame of species occurrences
  stopifnot(exprs = {
    class(flora_points) == "SpatialPointsDataFrame"
    class(QDS_polygon) == "SpatialPolygonsDataFrame"
  })

  # Get the QDS and HDS geocodes -----------------------------------------------

  flora_points %<>% get_geocodes(QDS_polygon[, "qdgc"])

  # Calculate average Jaccard distance betw QDS in each HDS --------------------

  # Init empty columns for data to come
  flora_points@data$HDS_richness <- NA
  flora_points@data$n_QDS <- NA
  flora_points@data$mean_QDS_richness <- NA
  flora_points@data$mean_QDS_turnover <- NA

  pb <- txtProgressBar(0, length(levels(factor(flora_points$hdgc))))
  for (i in seq_along(levels(factor(flora_points$hdgc)))) {

    # Current HDS geocode name
    current_HDS <- levels(factor(flora_points$hdgc))[[i]]

    # Filter to only those QDS w/i the current HDS
    spp_in_hdgc_by_qdgc <- flora_points@data %>%
      filter(hdgc == current_HDS) %>%
      select(species, qdgc)

    # Construct a community matrix containing the 4 QDS
    # Make an empty matrix:
    community_matrix <- matrix(
      nrow = length(unique(spp_in_hdgc_by_qdgc$qdgc)),
      ncol = length(unique(spp_in_hdgc_by_qdgc$species)),
      dimnames = list(
        unique(spp_in_hdgc_by_qdgc$qdgc),
        unique(spp_in_hdgc_by_qdgc$species)
      )
    )
    # Fill it:
    for (j in seq(nrow(community_matrix))) {
      for (k in seq(ncol(community_matrix))) {
        community_matrix[j, k] <-
          spp_in_hdgc_by_qdgc %>%
          filter(
            qdgc == rownames(community_matrix)[[j]] &
            species == colnames(community_matrix)[[k]]
          ) %>%
          magrittr::extract2("species") %>%
          unique() %>%
          is_empty() %>%
          not()
      }
    }

    # Output mean Jaccard + richness measures ----------------------------------

    # Output the summary data to the right rows in the spatial points data frame
    flora_points$HDS_richness[flora_points$hdgc == current_HDS] <-
      community_matrix %>%
      colnames() %>%
      length()
    flora_points$mean_QDS_turnover[flora_points$hdgc == current_HDS] <-
      community_matrix %>%
      vegdist(method = "jaccard") %>%
      mean(na.rm = TRUE)
    flora_points$n_QDS[flora_points$hdgc == current_HDS] <-
      community_matrix %>%
      nrow()
    flora_points$mean_QDS_richness[flora_points$hdgc == current_HDS] <-
      community_matrix %>%
      t() %>%
      as.data.frame() %>%
      summarise_if(is.logical, function(x) length(x[x])) %>%
      t() %>%
      mean(na.rm = TRUE)

    setTxtProgressBar(pb, i)

  }
  close(pb)

  # Save to disc ---------------------------------------------------------------

  if (is.null(date)) {
    date <- Sys.Date()
  }
  if (is.null(region_name)) {
    region_name <- "region"
  }
  for (layer in names(flora_points)) {
    writeOGR(
      flora_points,
      dsn = glue(
        "{output_path}/{region_name}_species_{date}"
      ),
      layer = layer,
      driver = "ESRI Shapefile"
    )
  }

  flora_points
}
