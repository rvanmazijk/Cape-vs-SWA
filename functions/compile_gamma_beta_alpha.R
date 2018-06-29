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
