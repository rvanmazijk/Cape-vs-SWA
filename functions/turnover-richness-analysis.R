jaccard_distance <- function(a, b) {
  (length(dplyr::union(a, b)) - length(dplyr::intersect(a, b))) /
  length(dplyr::union(a, b))
}

calc_all_pw_jaccard <- function(trimmed_points = NULL,
                                communities_by_cell = NULL,
                                richness_QDS,
                                feature_column = c("species", "genus", "family"),
                                cell_nos = NULL,
                                debug_length = NULL,
                                quiet = FALSE) {

  stopifnot(class(richness_QDS) == "RasterLayer")

  if (!is.null(cell_nos)) {
    cell_nos <- levels(as.factor(trimmed_points$cell_nos))
    if (!quiet) {
      cat(sep = "",
        "All ", length(cell_nos), " cell nos. extracted\n"
      )
    }
  } else if (!is.null(debug_length) & !is.null(trimmed_points)) {
    if (!quiet) {
      cat(sep = "",
        "DEBUG MODE (running for only ",
        debug_length,
        " cells)\n"
      )
    }
    cell_nos <- cell_nos[1:debug_length]
    if (!quiet) {
      cat(sep = "",
        "All ", length(cell_nos), " cell nos. extracted\n"
      )
    }
  } else {
    stop("Please manually provide cell_nos\r")
  }

  # Compile list of species in each grid-cell --------------------------------

  if (is.null(communities_by_cell) & !is.null(trimmed_points)) {
    communities_by_cell <- compile_communities_by_cell(
      trimmed_points,
      feature_column,
      cell_nos = cell_nos,
      debug_length = debug_length,
      quiet = quiet
    )
  } else if (!is.null(communities_by_cell) & is.null(trimmed_points)) {
    if (!quiet) {
      cat(sep = "",
        "Communities pre-described for all ",
        length(cell_nos), "cells\n",
        "Accepting input communities_by_cell\n"
      )
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
      cat(sep = "",
        "Calculated Jaccard distances relative to cell no. ",
        cell_nos[[i]], " (", i, "/", length(cell_nos), ")\r"
      )
    }
  }
  if (!quiet) {
    cat(sep = "",
      "Calculated all ",
      length(cell_nos),
      "pairwise Jaccard distances\n"
    )
  }

  # Calculate geographical distances between all cell pairs ------------------

  geodists_betw_cells <- pointDistance(
    p1 = xyFromCell(richness_QDS, cell = as.numeric(cell_nos)),
    p2 = xyFromCell(richness_QDS, cell = as.numeric(cell_nos)),
    lonlat = TRUE,
    allpairs = TRUE
  )

  if (!quiet) {
    cat(sep = "",
      "Calculated all ",
      length(geodists_betw_cells),
      "pairwise geographic distances\n"
    )
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
    cat("Done")
  }
  turnover_and_geodist_betw_cells_df

}

# Tests
if (FALSE) {
  calc_all_pw_jaccard(
    trimmed_GCFR_clean_flora_spdf_family,
    richness_QDS = GCFR_richness_QDS,
    feature_column = "species",
    debug_length = 10
  )
}

get_constituent_QDS_cells <- function(cells_df,
                                      query_focal_scale, query_cell_no) {
    cells_df[cells_df[[query_focal_scale]] == as.numeric(query_cell_no), ]
}

compile_gamma_beta_alpha <- function(cells, communities_by_cell,
                                     region = c("GCFR", "SWAFR"),
                                     focal_scale = c("HDS", "threeQDS")) {
  cells <- cells[cells$region == region, ]
  focal_scale <- paste0(focal_scale, "_cell_no")
  focal_scale_cell_nos <- levels(as.factor(cells[[focal_scale]]))
  gamma_beta_alpha <- foreach(focal_scale_cell_no = focal_scale_cell_nos) %do% {
    QDS_cell_nos <- get_constituent_QDS_cells(
      cells,
      focal_scale,
      focal_scale_cell_no
    )
    QDS_cells <- communities_by_cell[
      names(communities_by_cell) %in%
      paste0("cell_", QDS_cell_nos$QDS_cell_no)
    ]
    turnover_betw_QDS_cells <-
      matrix(nrow = length(QDS_cells), ncol = length(QDS_cells))
    for (i in seq_along(QDS_cells)) {
      for (j in seq_along(QDS_cells)) {
        turnover_betw_QDS_cells[i, j] <- jaccard_distance(
          QDS_cells[[i]],
          QDS_cells[[j]]
        )
      }
    }
    list(
      richness = length(unique(unlist(QDS_cells))),
      avg_QDS_richness = mean(unlist(map(QDS_cells, length)), na.rm = TRUE),
      avg_QDS_turnover = mean(as.dist(turnover_betw_QDS_cells), na.rm = TRUE)
    )
  }
  names(gamma_beta_alpha) <-
    paste0("cell_", levels(as.factor(cells[[focal_scale]])))
  gamma_beta_alpha2 <- as.data.frame(gamma_beta_alpha[[1]])
  foreach(cell = gamma_beta_alpha[-1]) %do% {
    gamma_beta_alpha2 %<>% rbind(as.data.frame(cell))
  }
  gamma_beta_alpha2
}
