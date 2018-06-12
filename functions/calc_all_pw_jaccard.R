calc_all_pw_jaccard <- function(trimmed_points, richness_QDS, quiet = FALSE) {

    stopifnot(
        class(trimmed_points) == "SpatialPointsDataFrame" &
        class(richness_QDS) == "RasterLayer"
    )

    cell_nos <- levels(as.factor(trimmed_points$cell_nos))
    if (!quiet) {
        print("Cell nos. extracted")
    }

    communities_by_cell <- vector("list", length = length(cell_nos))
    names(communities_by_cell) <- paste0("cell_", cell_nos)
    for (i in seq_along(cell_nos)) {
        communities_by_cell[[i]] <-
            trimmed_points$species[trimmed_points$cell_nos == cell_nos[[i]]]
        if (!quiet) {
            print(glue("
                Community described for cell no. {cell_nos[[i]]} \\
                ({i}/{length(cell_nos)})
            "))
        }
    }
    if (!quiet) {
        print("Communities described for all cells")
    }

    turnovers_betw_cells <-
        matrix(nrow = length(cell_nos), ncol = length(cell_nos))
    rownames(turnovers_betw_cells) <- paste0("cell_", cell_nos)
    colnames(turnovers_betw_cells) <- paste0("cell_", cell_nos)
    for (i in seq_along(cell_nos)) {
        for (j in seq_along(cell_nos)) {
            turnovers_betw_cells[i, j] <- jaccard_distance(
                communities_by_cell[[i]],
                communities_by_cell[[j]]
            )
        }
        if (!quiet) {
            print(glue("
                Calculated Jaccard distances relative to cell no. {cell_nos[[i]]} \\
                ({i}/{length(cell_nos)})
            "))
        }
    }
    if (!quiet) {
        print("Calculated all pairwise Jaccard distances")
    }

    geodists_betw_cells <- pointDistance(
        p1 = xyFromCell(richness_QDS, cell = as.numeric(cell_nos)),
        p2 = xyFromCell(richness_QDS, cell = as.numeric(cell_nos)),
        lonlat = TRUE,
        allpairs = TRUE
    )
    print("Calculated all pairwise geographic distances")

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
        print("Done")
    }

    return(turnover_and_geodist_betw_cells_df)

}
