# Processing environmental data: NDVI
# NDVI MODIS processing from NASA for my two regions
# Cape vs SWA publication
# Ruan van Mazijk

# Import order-information-file to get URLs for MOD13C2 (NDVI) rasters ---------

order <- as_tibble(read.csv(here::here("data/raw-data/NASA-ladsweb-MOD13C2-order-501172795.csv")))

# Download HDF4 files ----------------------------------------------------------

# e.g.
#   `https://ladsweb.modaps.eosdis.nasa.gov/
#       archive/orders/501172795/
#       MOD13C2.A2000032.006.2015147122546.hdf`
# becomes ->
#   `ftp://ladsweb.modaps.eosdis.nasa.gov/
#       orders/501172795/
#       MOD13C2.A2000032.006.2015147122546.hdf`

FTP <- "ftp://ladsweb.modaps.eosdis.nasa.gov/orders/501172795/"

# Test
if (FALSE) {
    download.file(
        url = paste0(FTP, order$name[1]),
        destfile = paste0(
            giswd,
            "MOD13C2/",
            order$name[1]
        ),
        method = "wget",
        quiet = TRUE,
        cacheOK = TRUE,
        extra = getOption("download.file.extra")
    )
    # Works!
}

# Actual download run
for (file in order$name) {
    download.file(
        url = paste0(FTP, file),
        destfile = paste0(
            giswd,
            "MOD13C2/",
            file
        ),
        method = "wget",
        quiet = TRUE,
        cacheOK = TRUE,
        extra = getOption("download.file.extra")
    )
}

# Convert HDF4s to GEOTIFFs ----------------------------------------------------

files <- list.files(
    path = paste0(
        giswd,
        "MOD13C2/"
    ),
    pattern = "\\.hdf$"
)

bash_gdalinfo(
    x = files[1],
    x_dir = paste0(
        giswd,
        "MOD13C2/"
    )
)
bash_gdalinfo_one(
    x = files[1],
    band = "':MOD_Grid_monthly_CMG_VI:CMG 0.05 Deg Monthly NDVI'",
    # (extra single quote bc of space in band-name)
    x_dir = paste0(
        giswd,
        "MOD13C2/"
    )
)

# Use parallel::
cluster <- makeCluster(detectCores() - 1, outfile = "")
parLapply(cluster,
    files,
    bash_gdaltranslate_one,
    band = "':MOD_Grid_monthly_CMG_VI:CMG 0.05 Deg Monthly NDVI'",
    x_dir = paste0(
        giswd,
        "MOD13C2/"
    ),
    out_dir = paste0(
        giswd,
        "MOD13C2_GeoTiffs/"
    )
)
stopCluster(cluster)

# Crop & mask to GCFR/SWAFR ----------------------------------------------------

files <- list.files(
    path = paste0(
        giswd,
        "MOD13C2_GeoTiffs/"
    ),
    pattern = "\\.tif$"
)

# .... GCFR --------------------------------------------------------------------

# Get the box
GCFR_box <- readRDS(here::here("data/derived-data/borders/GCFR_box.rds"))

# Reproject it to the CRS of the NASA tiffs
proj4string(GCFR_box) == std_CRS
NASA_CRS <- crs(raster(paste0(giswd, "MOD13C2_GeoTiffs/", files[1])))
GCFR_box_NASA <-  spTransform(GCFR_box, CRSobj = NASA_CRS)

# Use this, in parallel, to crop those tiffs
cluster <- makeCluster(detectCores() - 1, outfile = "")
parLapply(cluster,
    files,
    crop_MOD13C2,
    box = GCFR_box_NASA,
    filedir = paste0(
        giswd,
        "MOD13C2_GeoTiffs/"
    ),
    outdir = paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "GCFR/"
    ),
    suffix = "_GCFR"
)
stopCluster(cluster)

# .... SWAFR ------------------------------------------------------------------

# Get the box
SWAFR_box <- readRDS(here::here("data/derived-data/borders/SWAFR_box.rds"))

# Reproject it to the CRS of the NASA tiffs
proj4string(SWAFR_box) == std_CRS
NASA_CRS <- crs(raster(paste0(giswd, "MOD13C2_GeoTiffs/", files[1])))
SWAFR_box_NASA <-  spTransform(SWAFR_box, CRSobj = NASA_CRS)

# Use this, in parallel, to crop those tiffs
cluster <- makeCluster(detectCores() - 1, outfile = "")
parLapply(cluster,
    files,
    crop_MOD13C2,
    box = SWAFR_box_NASA,
    filedir = paste0(
        giswd,
        "MOD13C2_GeoTiffs/"
    ),
    outdir = paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "SWAFR/"
    ),
    suffix = "_SWAFR"
)
stopCluster(cluster)

# Check CRS
crs(raster(paste0(giswd, "MOD13C2_GeoTiffs/", "GCFR")))

# Reproject to std_CRS ---------------------------------------------------------

# .... GCFR --------------------------------------------------------------------

files <- list.files(
    path = paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "GCFR/"
    ),
    pattern = "\\.tif$"
)
crs(raster(paste0(giswd, "MOD13C2_GeoTiffs/", "GCFR/", files[1])))

cluster <- makeCluster(detectCores() - 1, outfile = "")
parLapply(cluster,
    files,
    project_MOD13C2,
    crs = std_CRS,
    filedir = paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "GCFR/"
    ),
    outdir = paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "GCFR/"
    ),
    suffix = "_std_CRS"
)
stopCluster(cluster)

# .... SWAFR -------------------------------------------------------------------

files <- list.files(
    path = paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "SWAFR/"
    ),
    pattern = "\\.tif$"
)
crs(raster(paste0(giswd, "MOD13C2_GeoTiffs/", "SWAFR/", files[1])))

cluster <- makeCluster(detectCores() - 1, outfile = "")
parLapply(cluster,
    files,
    project_MOD13C2,
    crs = std_CRS,
    filedir = paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "SWAFR/"
    ),
    outdir = paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "SWAFR/"
    ),
    suffix = "_std_CRS"
)
stopCluster(cluster)

# Compute mean annual NDVI -----------------------------------------------------

# .... GCFR --------------------------------------------------------------------

files <- list.files(
    path = paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "GCFR/"
    ),
    pattern = "std_CRS\\.tif$"
)

# ........ Organise by year & month --------------------------------------------

years <- 2000:2017
files_by_year <- vector("list", length = length(years))
for (i in seq_along(years)) {
    year_index <- str_detect(
        files,
        pattern = glue("A{years[i]}")
    )
    files_by_year[[i]] <- files[year_index]
    names(files_by_year)[i] <- years[i]
}

for (i in seq_along(files_by_year)) {
    if (names(files_by_year[i]) == "2000") {
        files_by_year[[i]] <- c(
            NA,
            files_by_year[[i]]
        )
    }
}

for (i in seq_along(files_by_year)) {
    names(files_by_year[[i]]) <-
        if (names(files_by_year[i]) == "2017") {
            c(
                "Jan", "Feb", "Mar", "Apr"
            )
        } else {
            c(
                "Jan", "Feb", "Mar", "Apr",
                "May", "Jun", "Jul", "Aug",
                "Sep", "Oct", "Nov", "Dec"
             )
        }
}

# ........ Make monthly avgs ---------------------------------------------------

# ............ January ---------------------------------------------------------

GCFR_month_NDVI_stack <- stack()
for (i in 2:length(2000:2017)) {
    layer <- raster(paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "GCFR/",
        files_by_year[[i]]["Jan"]
    ))
    GCFR_month_NDVI_stack %<>% stack(layer)
}
writeRaster(
    GCFR_month_NDVI_stack,
    overwrite = TRUE,
    glue("
        {giswd}\\
        MOD13C2_GeoTiffs/\\
        GCFR/\\
        MOD13C2_GCFR_std_CRS_Jan.tif
    ")
)
writeRaster(
    mean(GCFR_month_NDVI_stack),
    overwrite = TRUE,
    glue("
        {giswd}\\
        MOD13C2_GeoTiffs/\\
        GCFR/\\
        MOD13C2_GCFR_std_CRS_Jan_mean.tif
    ")
)

# ............ February, March, April ------------------------------------------

for (month in c("Feb", "Mar", "Apr")) {
    GCFR_month_NDVI_stack <- stack()
    for (i in 1:length(2000:2017)) {
        layer <- raster(paste0(
            giswd,
            "MOD13C2_GeoTiffs/",
            "GCFR/",
            files_by_year[[i]][month]
        ))
        GCFR_month_NDVI_stack %<>% stack(layer)
    }
    writeRaster(
        GCFR_month_NDVI_stack,
        overwrite = TRUE,
        glue("
            {giswd}\\
            MOD13C2_GeoTiffs/\\
            GCFR/\\
            MOD13C2_GCFR_std_CRS_{month}.tif
        ")
    )
    writeRaster(
        mean(GCFR_month_NDVI_stack),
        overwrite = TRUE,
        glue("
            {giswd}\\
            MOD13C2_GeoTiffs/\\
            GCFR/\\
            MOD13C2_GCFR_std_CRS_{month}_mean.tif
        ")
    )
}

# ............ May to December -------------------------------------------------

for (month in c("May", "Jun", "Jul", "Aug",
                "Sep", "Oct", "Nov", "Dec")) {
    GCFR_month_NDVI_stack <- stack()
    for (i in 1:length(2000:2016)) {
        layer <- raster(paste0(
            giswd,
            "MOD13C2_GeoTiffs/",
            "GCFR/",
            files_by_year[[i]][month]
        ))
        GCFR_month_NDVI_stack %<>% stack(layer)
    }
    writeRaster(
        GCFR_month_NDVI_stack,
        overwrite = TRUE,
        glue("
            {giswd}\\
            MOD13C2_GeoTiffs/\\
            GCFR/\\
            MOD13C2_GCFR_std_CRS_{month}.tif
        ")
    )
    writeRaster(
        mean(GCFR_month_NDVI_stack),
        overwrite = TRUE,
        glue("
            {giswd}\\
            MOD13C2_GeoTiffs/\\
            GCFR/\\
            MOD13C2_GCFR_std_CRS_{month}_mean.tif
        ")
    )
}

# ........ Finally, make annual avg NDVI ---------------------------------------

files <- list.files(
    path = paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "GCFR/"
    ),
    pattern = "std_CRS_.+_mean\\.tif$"
)
GCFR_all_months <- stack()
for (i in 1:12) {
    layer <- raster(paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "GCFR/",
        files[i]
    ))
    GCFR_all_months %<>% stack(layer)
}
writeRaster(
    GCFR_all_months,
    overwrite = TRUE,
    glue("
        {giswd}\\
        MOD13C2_GeoTiffs/\\
        GCFR/\\
        MOD13C2_GCFR_std_CRS_all_months.tif
    ")
)
writeRaster(
    mean(GCFR_all_months),
    overwrite = TRUE,
    glue("
        {giswd}\\
        MOD13C2_GeoTiffs/\\
        GCFR/\\
        MOD13C2_GCFR_std_CRS_annual_avg.tif
    ")
)

# .... SWAFR -------------------------------------------------------------------

files <- list.files(
    path = paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "SWAFR/"
    ),
    pattern = "std_CRS\\.tif$"
)

# ........ Organise by year & month --------------------------------------------

years <- 2000:2017
files_by_year <- vector("list", length = length(years))
for (i in seq_along(years)) {
    year_index <- str_detect(
        files,
        pattern = glue("A{years[i]}")
    )
    files_by_year[[i]] <- files[year_index]
    names(files_by_year)[i] <- years[i]
}

for (i in seq_along(files_by_year)) {
    if (names(files_by_year[i]) == "2000") {
        files_by_year[[i]] <- c(
            NA,
            files_by_year[[i]]
        )
    }
}

for (i in seq_along(files_by_year)) {
    names(files_by_year[[i]]) <-
        if (names(files_by_year[i]) == "2017") {
            c(
                "Jan", "Feb", "Mar", "Apr"
            )
        } else {
            c(
                "Jan", "Feb", "Mar", "Apr",
                "May", "Jun", "Jul", "Aug",
                "Sep", "Oct", "Nov", "Dec"
             )
        }
}

# ........ Make monthly avgs ---------------------------------------------------

# ............ January ---------------------------------------------------------

GCFR_month_NDVI_stack <- stack()
for (i in 2:length(2000:2017)) {
    layer <- raster(paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "SWAFR/",
        files_by_year[[i]]["Jan"]
    ))
    GCFR_month_NDVI_stack %<>% stack(layer)
}
writeRaster(
    GCFR_month_NDVI_stack,
    overwrite = TRUE,
    glue("
        {giswd}\\
        MOD13C2_GeoTiffs/\\
        SWAFR/\\
        MOD13C2_SWAFR_std_CRS_Jan.tif
    ")
)
writeRaster(
    mean(GCFR_month_NDVI_stack),
    overwrite = TRUE,
    glue("
        {giswd}\\
        MOD13C2_GeoTiffs/\\
        SWAFR/\\
        MOD13C2_SWAFR_std_CRS_Jan_mean.tif
    ")
)

# ............ February, March, April ------------------------------------------

for (month in c("Feb", "Mar", "Apr")) {
    GCFR_month_NDVI_stack <- stack()
    for (i in 1:length(2000:2017)) {
        layer <- raster(paste0(
            giswd,
            "MOD13C2_GeoTiffs/",
            "SWAFR/",
            files_by_year[[i]][month]
        ))
        GCFR_month_NDVI_stack %<>% stack(layer)
    }
    writeRaster(
        GCFR_month_NDVI_stack,
        overwrite = TRUE,
        glue("
            {giswd}\\
            MOD13C2_GeoTiffs/\\
            SWAFR/\\
            MOD13C2_SWAFR_std_CRS_{month}.tif
        ")
    )
    writeRaster(
        mean(GCFR_month_NDVI_stack),
        overwrite = TRUE,
        glue("
            {giswd}\\
            MOD13C2_GeoTiffs/\\
            SWAFR/\\
            MOD13C2_SWAFR_std_CRS_{month}_mean.tif
        ")
    )
}

# ............ May to December -------------------------------------------------

for (month in c("May", "Jun", "Jul", "Aug",
                "Sep", "Oct", "Nov", "Dec")) {
    GCFR_month_NDVI_stack <- stack()
    for (i in 1:length(2000:2016)) {
        layer <- raster(paste0(
            giswd,
            "MOD13C2_GeoTiffs/",
            "SWAFR/",
            files_by_year[[i]][month]
        ))
        GCFR_month_NDVI_stack %<>% stack(layer)
    }
    writeRaster(
        GCFR_month_NDVI_stack,
        overwrite = TRUE,
        glue("
            {giswd}\\
            MOD13C2_GeoTiffs/\\
            SWAFR/\\
            MOD13C2_SWAFR_std_CRS_{month}.tif
        ")
    )
    writeRaster(
        mean(GCFR_month_NDVI_stack),
        overwrite = TRUE,
        glue("
            {giswd}\\
            MOD13C2_GeoTiffs/\\
            SWAFR/\\
            MOD13C2_SWAFR_std_CRS_{month}_mean.tif
        ")
    )
}

# ........ Finally, make annual avg NDVI ---------------------------------------

files <- list.files(
    path = paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "SWAFR/"
    ),
    pattern = "std_CRS_.+_mean\\.tif$"
)
GCFR_all_months <- stack()
for (i in 1:12) {
    layer <- raster(paste0(
        giswd,
        "MOD13C2_GeoTiffs/",
        "SWAFR/",
        files[i]
    ))
    GCFR_all_months %<>% stack(layer)
}
writeRaster(
    GCFR_all_months,
    overwrite = TRUE,
    glue("
        {giswd}\\
        MOD13C2_GeoTiffs/\\
        SWAFR/\\
        MOD13C2_SWAFR_std_CRS_all_months.tif
    ")
)
writeRaster(
    mean(GCFR_all_months),
    overwrite = TRUE,
    glue("
        {giswd}\\
        MOD13C2_GeoTiffs/\\
        SWAFR/\\
        MOD13C2_SWAFR_std_CRS_annual_avg.tif
    ")
)

# Crop-mask to "boxes" ---------------------------------------------------------

# TODO: Why did I do this again?
# Ag, doesn't matter,,,

GCFR_box <- readRDS(here::here("data/derived-data/borders/GCFR_box.rds"))
GCFR_NDVI <- raster(glue("
    {giswd}\\
    MOD13C2_GeoTiffs/\\
    GCFR/\\
    MOD13C2_GCFR_std_CRS_annual_avg.tif
"))
GCFR_NDVI %<>%
    crop(GCFR_box) %>%
    mask(GCFR_box)
writeRaster(
    GCFR_NDVI,
    here::here("data/derived-data/NDVI/GCFR_NDVI.tif")
)

SWAFR_box <- readRDS(here::here("data/derived-data/borders/SWAFR_box.rds"))
SWAFR_NDVI <- raster(glue("
    {giswd}\\
    MOD13C2_GeoTiffs/\\
    SWAFR/\\
    MOD13C2_SWAFR_std_CRS_annual_avg.tif
"))
SWAFR_NDVI %<>%
    crop(SWAFR_box) %>%
    mask(SWAFR_box)
writeRaster(
    SWAFR_NDVI,
    here::here("data/derived-data/NDVI/SWAFR_NDVI.tif")
)
