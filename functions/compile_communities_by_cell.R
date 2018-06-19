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

    return(communities_by_cell)

}

# Tests
if (FALSE) {
    compile_communities_by_cell(
        trimmed_GCFR_clean_flora_spdf_family,
        "species",
        debug_length = 10
    )
}
