make_SpatialPointsDataFrame <- function(df,
                                        lat_column = "decimallatitude",
                                        lon_column = "decimallongitude",
                                        feature_columns = c("family",
                                                            "genus",
                                                            "species"),
                                        CRS = std_CRS) {
    return(SpatialPointsDataFrame(
        coords = df[, c(lon_column, lat_column)],
        data = df[, c(feature_columns)],
        proj4string = CRS(CRS)
    ))
}
