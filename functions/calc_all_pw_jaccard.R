calc_all_pw_jaccard <- function(trimmed_points = NULL,
                                communities_by_cell = NULL,
                                richness_QDS,
                                feature_column = c("species", "genus", "family"),
                                cell_nos = NULL,
                                debug_length = NULL,
                                quiet = FALSE) {

    stopifnot(class(richness_QDS) == "RasterLayer")

    if (!is.null(debug_length)) {
        cell_nos <- debug_length
    } else if (!is.null(trimmed_points)) {
        if (!quiet) {
            message(glue("DEBUG MODE"))
        }
        cell_nos <- levels(as.factor(trimmed_points$cell_nos))
        if (!quiet) {
            message(glue("
                All {length(cell_nos)} cell nos. extracted
            "))
        }
    } else if (is.null(cell_nos)) {
        stop("Please manually provide cell_nos")
    }

    # Compile list of species in each grid-cell --------------------------------

    if (is.null(communities_by_cell) & !is.null(trimmed_points)) {
        communities_by_cell <- compile_communities_by_cell(
            trimmed_points,
            feature_column,
            cell_nos,
            quiet = quiet
        )
    } else if (!is.null(communities_by_cell) & is.null(trimmed_points)) {
        if (!quiet) {
            message(glue("
                Communities pre-described for all {length(cell_nos)} cells\n
                Accepting input communities_by_cell
            "))
        }
    }

    # Calculate species turnover between cells ---------------------------------

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
            Calculated all {length(cell_nos)} pairwise Jaccard distances
        "))
    }

    # Calculate geographical distances between all cell pairs ------------------

    geodists_betw_cells <- pointDistance(
        p1 = xyFromCell(richness_QDS, cell = as.numeric(cell_nos)),
        p2 = xyFromCell(richness_QDS, cell = as.numeric(cell_nos)),
        lonlat = TRUE,
        allpairs = TRUE
    )

    if (!quiet) {
        message("Calculated all pairwise geographic distances")
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
        message("Done")
    }

    return(turnover_and_geodist_betw_cells_df)

}
