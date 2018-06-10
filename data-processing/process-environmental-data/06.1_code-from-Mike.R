# Take the average value of the soils data
# Set up soil depth weighting factors to do
# depth averageing of layers.

# This from the depth to mid-points
# between various sample layers used
# in the data set.

depth_weightings <- c(
    0.041666667,
    0.065,
    0.116666667,
    0.193333333,
    0.35,
    0.233333333
)

# List files.
# Note pattern command ^ indicates start, $ indcates end
# List files for each character
file_pattern <- c(1:9)
list_shape <- matrix(data = NA, ncol = 9, nrow = 6)
Soil_layer_list <- c(
    "BLDFIE",
    "CECSOL",
    "CLYPPT",
    "CRFVOL",
    "OCSTHA",
    "ORCDRC",
    "PHIHOX",
    "SLTPPT",
    "SNDPPT",
    "AWCh1"
)
for (i in 1:length(Soil_layer_list)) {
    file_pattern[i] <- paste0(
        "^",
        Soil_layer_list[i],
        "_sd[[:digit:]]_M_02_apr_2014.tif$"
    )
    list_shape[, i] <- sort(list.files(
        path = "???",
        pattern = file_pattern[i],
        full.names = TRUE,
        recursive = FALSE,
        include.dirs = FALSE
    ))
}

# Setup function to crop raster to limits set by Schulz
# and also bio resolution of Worldclim set.

resample_raster <- function(tif_file) {
    tif_file %>%
        raster() %>%
        crop("???") %>%
        reclassify(c(-Inf, -1000, -9999)) %>%
        projectRaster(crs = std_CRS, method = "bilinear") %>%
        resample(Sample2)
}

# Get crop and mean rasters to the extent of SA

BDRLOG_d_mean <-
    resample(
        projectRaster(
            reclassify(
                crop(
                    raster("???"),
                    SA_extent
                ),
                c(-Inf, -1000, -9999)
            ),
            crs = "+proj=longlat +datum=WGS84",
            method = 'bilinear'
        ),
        Sample2
    )
file_name <- paste(sep = "",
    "???",
    "BDRLOG",
    "_d_mean"
)
writeRaster(
    BDRLOG_d_mean,
    file_name,
    format = "ascii",
    overwrite = TRUE
)
plot(BDRLOG_d_mean)

BDRICM_d_mean <-
    resample(
        projectRaster(
            reclassify(crop(
                raster(
                    "?"
                ),
                SA_extent
            ), c(-Inf, -1000, -9999)),
            crs = "+proj=longlat +datum=WGS84",
            method = 'bilinear'
        ),
        Sample2
    )
file_name <- paste(sep = "",
    "???",
    "BDRICM",
    "_d_mean"
)
writeRaster(
    BDRICM_d_mean,
    file_name,
    format = "ascii",
    overwrite = TRUE
)
plot(BDRICM_d_mean)


BLD_d_mean <-
    weighted.mean(
        brick(lapply(
            list_shape[, 1],
            FUN = resample_raster
        )),
        w = depth_weightings,
        na.rm = TRUE
    )
file_name <- paste(sep = "",
    "???",
    Soil_layer_list[1],
    "_d_mean"
)
writeRaster(
    BLD_d_mean,
    file_name,
    format = "ascii",
    overwrite = TRUE
)
plot(BLD_d_mean)
