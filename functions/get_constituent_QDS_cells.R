get_constituent_QDS_cells <- function(cells_df,
                                      query_focal_scale, query_cell_no) {
    cells_df[cells_df[[query_focal_scale]] == as.numeric(query_cell_no), ]
}
