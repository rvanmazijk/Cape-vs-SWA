calc_all_pw_jaccard <- function(trimmed_points, richness_QDS,
                                feature_column = c("species", "genus", "family"),
                                quiet = FALSE,
                                debug_mode = FALSE, debug_length = NULL) {

    stopifnot(
        class(trimmed_points) == "SpatialPointsDataFrame" &
        class(richness_QDS) == "RasterLayer"
    )

    stopifnot(
        (debug_mode & !is.null(debug_length)) |
        (!debug_mode & is.null(debug_length))
    )

    cell_nos <- levels(as.factor(trimmed_points$cell_nos))
    if (!quiet) {
        message(glue("
            \nAll {length(cell_nos)} cell nos. extracted
        "))
    }

    # Compile list of species in each grid-cell --------------------------------

    if (!debug_mode) {

        communities_by_cell <- vector("list", length = length(cell_nos))
        names(communities_by_cell) <- paste0("cell_", cell_nos)
        for (i in seq_along(cell_nos)) {
            communities_by_cell[[i]] <- trimmed_points[[feature_column]][
                trimmed_points$cell_nos == cell_nos[[i]]
            ]
            if (!quiet) {
                flush.console()
                cat(
                    "Community described for cell no. ",
                    cell_nos[[i]], " (", i, "/", length(cell_nos), ") \r"
                )
            }
        }
        if (!quiet) {
            message(glue("
                \nCommunities described for all {length(cell_nos)} cells
            "))
        }

    } else {

        if (!quiet) {
            message(glue("\nDEBUG MODE"))
        }
        communities_by_cell <- vector("list", length = debug_length)
        names(communities_by_cell) <- paste0("cell_", cell_nos[1:debug_length])
        for (i in 1:debug_length) {
            communities_by_cell[[i]] <- trimmed_points[[feature_column]][
                trimmed_points$cell_nos == cell_nos[[i]]
            ]
            if (!quiet) {
                flush.console()
                cat(
                    "Community described for cell no. ",
                    cell_nos[[i]], " (", i, "/", debug_length, ") \r"
                )
            }
        }
        if (!quiet) {
            message(glue("
                \nCommunities described for all {debug_length} cells
            "))
        }

    }

    # Calculate species turnover between cells ---------------------------------

    if (!debug_mode) {

        turnovers_betw_cells <-
            matrix(nrow = length(cell_nos), ncol = length(cell_nos))
        rownames(turnovers_betw_cells) <- paste0("cell_", cell_nos)
        colnames(turnovers_betw_cells) <- paste0("cell_", cell_nos)
        for (i in seq_along(cell_nos)) {
            for (j in seq_along(cell_nos)) {
                turnovers_betw_cells[i, j] <-
                    if (names(communities_by_cell)[[i]] ==
                        names(communities_by_cell)[[j]]) {
                        0
                    } else {
                        jaccard_distance(
                            communities_by_cell[[i]],
                            communities_by_cell[[j]]
                        )
                    }
            }
            if (!quiet) {
                flush.console()
                cat(
                    "Calculated Jaccard distances relative to cell no. ",
                    cell_nos[[i]], " (", i, "/", length(cell_nos), ") \r"
                )
            }
        }
        if (!quiet) {
            message(glue("
                \nCalculated all {length(cell_nos)} pairwise Jaccard distances
            "))
        }

    } else {

        turnovers_betw_cells <-
            matrix(nrow = debug_length, ncol = debug_length)
        rownames(turnovers_betw_cells) <- paste0("cell_", cell_nos[1:debug_length])
        colnames(turnovers_betw_cells) <- paste0("cell_", cell_nos[1:debug_length])
        for (i in 1:debug_length) {
            for (j in 1:debug_length) {
                if (!quiet) {
                    flush.console()
                    cat(
                        "Jaccard(i = ", i, ", j = ", j, ") \r"
                    )
                }
                turnovers_betw_cells[i, j] <-
                    if (names(communities_by_cell)[[i]] ==
                        names(communities_by_cell)[[j]]) {
                        0
                    } else {
                        jaccard_distance(
                            communities_by_cell[[i]],
                            communities_by_cell[[j]]
                        )
                    }
            }
            if (!quiet) {
                flush.console()
                cat(
                    "Calculated Jaccard distances relative to cell no. ",
                    cell_nos[[i]], " (", i, "/", debug_length, ") \r"
                )
            }
        }
        if (!quiet) {
            message(glue("
                \nCalculated all {debug_length} pairwise Jaccard distances
            "))
        }

    }

    # Calculate geographical distances between all cell pairs ------------------

    if (!debug_mode) {

        geodists_betw_cells <- pointDistance(
            p1 = xyFromCell(richness_QDS, cell = as.numeric(cell_nos)),
            p2 = xyFromCell(richness_QDS, cell = as.numeric(cell_nos)),
            lonlat = TRUE,
            allpairs = TRUE
        )

    } else {

        geodists_betw_cells <- pointDistance(
            p1 = xyFromCell(richness_QDS, cell = as.numeric(cell_nos[1:debug_length])),
            p2 = xyFromCell(richness_QDS, cell = as.numeric(cell_nos[1:debug_length])),
            lonlat = TRUE,
            allpairs = TRUE
        )
    }

    if (!quiet) {
        message("\nCalculated all pairwise geographic distances")
    }
    turnovers_betw_cells_df <- as_tibble(turnovers_betw_cells)
    turnovers_betw_cells_df$cell_no_a <- colnames(turnovers_betw_cells)
    turnovers_betw_cells_df %<>% gather(
        cell_no_b, turnover,
        -cell_no_a
    )

    geodists_betw_cells_df <- as_tibble(geodists_betw_cells)
    colnames(geodists_betw_cells_df) <- colnames(turnovers_betw_cells)
    geodists_betw_cells_df$cell_no_a <- colnames(turnovers_betw_cells)
    geodists_betw_cells_df %<>% gather(
        cell_no_b, geodist,
        -cell_no_a
    )

    turnover_and_geodist_betw_cells_df <- full_join(
        turnovers_betw_cells_df,
        geodists_betw_cells_df
    )
    if (!quiet) {
        message("\nDone")
    }

    return(turnover_and_geodist_betw_cells_df)

}
