# Processing environmental data: Land surface temperature
# MODIS processing from NASA for my two regions
# Cape vs SWA publication
# Ruan van Mazijk

# Mean annual land surface temp ------------------------------------------------

# .... Download from NASA ------------------------------------------------------
# After requesting an order for the regions required

order <- read.csv(paste0(datwd, "NASA_ladsweb_MOD11C3_order501145540.csv"))
as_tibble(order)
order$name %<>% as.character()
order$last_modified %<>% lubridate::as_datetime()
FTP <- "ftp://ladsweb.modaps.eosdis.nasa.gov/orders/501145540/"

pb <- txtProgressBar(min = 0, max = length(order$name), style = 3)
for (i in 1:194) {
  download.file(
    url = paste0(FTP, order$name[i]),
    destfile = paste0(giswd, "MOD11C3/order_501145540/", order$name[i]),
    method = "wget",
    quiet = 2,
    cacheOK = T,
    extra = getOption("download.file.extra")
  )
  setTxtProgressBar(pb, i)
}
close(pb)

# Second download order needed for 195+,
# because of a problem w/ the precvious order

order2 <- read.csv(paste0(datwd, "NASA_ladsweb_MOD11C3_order501148261.csv"))
as_tibble(order2)
order2$name %<>% as.character()
order2$last_modified %<>% lubridate::as_datetime()
FTP <- "ftp://ladsweb.modaps.eosdis.nasa.gov/orders/501148261/"

order[, ] == order2[, ]
# Same :)

pb <- txtProgressBar(min = 0, max = length(order2$name), style = 3)
for (i in 195:length(order2$name)) {
  download.file(
    url = paste0(FTP, order2$name[i]),
    destfile = paste0(giswd, "MOD11C3/order_501145540/", order2$name[i]),
    method = "wget",
    quiet = 2,
    cacheOK = T,
    extra = getOption("download.file.extra")
  )
  setTxtProgressBar(pb, i)
}
close(pb)

# Let's check to see if we got them all:
f <- list.files(
  path = paste0(giswd, "MOD11C3/order_501145540/"),
  pattern = ".hdf"
)
length(f)
summary(order2)
# All 207 are there :)

# .... Converting MOD11C3 from HDF4 to GeoTiff ---------------------------------

bash_gdaltranslate_loop(
  x = order2$name,
  band = ":MODIS_MONTHLY_0.05DEG_CMG_LST:LST_Day_CMG", # Daytime LST (Kelvin)
  out_format = ".tif",
  x_dir = paste0(giswd, "MOD11C3/order_501145540/"),
  out_dir = paste0(giswd, "MOD11C3/order_501145540_GeoTiffs/")
)

# .... GCFR --------------------------------------------------------------------

# ........ Make MOD11C3 monthly stacks -----------------------------------------

# Inspect jan_file_query, just to see:
jan_file_query <- modfile_query(month = "Jan")
levels(jan_file_query$month)
# Write Jan, Feb, Mar and May's mean_monthly_LSTs to disc AND to object,
# for years 2000:2017 (default):
modis_jan <-
  proj_crop_mask_mean_write(df_of_files = modfile_query(month = "Jan"))
modis_feb <-
  proj_crop_mask_mean_write(df_of_files = modfile_query(month = "Feb"))
modis_mar <-
  proj_crop_mask_mean_write(df_of_files = modfile_query(month = "Mar"))
modis_apr <-
  proj_crop_mask_mean_write(df_of_files = modfile_query(month = "Apr"))

# Write Ma to  Jun, ... Dec's mean_monthly_LST to disc AND to object,
# for years 2000:2016 (b.c. 2017 May+ DNE):
modis_dec <- proj_crop_mask_mean_write(df_of_files = modfile_query(
  month = "Dec",
  years = 2000:2016
))
modis_may <- proj_crop_mask_mean_write(df_of_files = modfile_query(
  month = "May",
  years = 2000:2016
))
modis_jun <- proj_crop_mask_mean_write(df_of_files = modfile_query(
  month = "Jun",
  years = 2000:2016
))
modis_jul <- proj_crop_mask_mean_write(df_of_files = modfile_query(
  month = "Jul",
  years = 2000:2016
))
modis_aug <- proj_crop_mask_mean_write(df_of_files = modfile_query(
  month = "Aug",
  years = 2000:2016
))
modis_sep <- proj_crop_mask_mean_write(df_of_files = modfile_query(
  month = "Sep",
  years = 2000:2016
))
modis_oct <- proj_crop_mask_mean_write(df_of_files = modfile_query(
  month = "Oct",
  years = 2000:2016
))
modis_nov <- proj_crop_mask_mean_write(df_of_files = modfile_query(
  month = "Nov",
  years = 2000:2016
))

# ........ Save months into stack ----------------------------------------------

# Now let's load all those objects back into a raster-stack,
# for use with making bioclim vars:
monthly_LST_GCFR_0.05_buffered <- stack()
for (i in 1:12) {
  x <- raster(paste0(
    reswd,
    "MODIS_",
    i,
    "_GCFR_0.05_buffered.grd"
  ))
  monthly_LST_GCFR_0.05_buffered %<>% stack(x)
}
names(monthly_LST_GCFR_0.05_buffered) <- c(
  "Jan", "Feb", "Mar",
  "Apr", "May", "Jun",
  "Jul", "Aug", "Sep",
  "Oct", "Nov", "Dec"
)
writeRaster(
  monthly_LST_GCFR_0.05_buffered,
  filename = paste0(reswd, "MODIS_monthly_means_GCFR_0.05_buffered.grd"),
  bandorder = "BIL",
  overwrite = T
)

# ........ Make mean annual LST ------------------------------------------------

MA_LST_GCFR_0.05_buffered <- mean(monthly_LST_GCFR_0.05_buffered)
writeRaster(
  MA_LST_GCFR_0.05_buffered,
  filename = paste0(reswd, "MODIS_annual_mean_GCFR_0.05_buffered.grd"),
  bandorder = "BIL",
  overwrite = T
)

# .... SWAFR -------------------------------------------------------------------

# ........ Make MOD11C3 monthly stacks -----------------------------------------

modis_jan <- proj_crop_mask_mean_write(
  df_of_files = modfile_query(month = "Jan"),
  region_name = "SWAFR",
  slave_border = SWAFR_border_buffered
)
modis_feb <- proj_crop_mask_mean_write(
  df_of_files = modfile_query(month = "Feb"),
  region_name = "SWAFR",
  slave_border = SWAFR_border_buffered
)
modis_mar <- proj_crop_mask_mean_write(
  df_of_files = modfile_query(month = "Mar"),
  region_name = "SWAFR",
  slave_border = SWAFR_border_buffered
)
modis_apr <- proj_crop_mask_mean_write(
  df_of_files = modfile_query(month = "Apr"),
  region_name = "SWAFR",
  slave_border = SWAFR_border_buffered
)

modis_dec <- proj_crop_mask_mean_write(
  df_of_files = modfile_query(
    month = "Dec",
    years = 2000:2016
  ),
  region_name = "SWAFR",
  slave_border = SWAFR_border_buffered
)

modis_may <- proj_crop_mask_mean_write(
  df_of_files = modfile_query(
    month = "May",
    years = 2000:2016
  ),
  region_name = "SWAFR",
  slave_border = SWAFR_border_buffered
)
modis_jun <- proj_crop_mask_mean_write(
  df_of_files = modfile_query(
    month = "Jun",
    years = 2000:2016
  ),
  region_name = "SWAFR",
  slave_border = SWAFR_border_buffered
)
modis_jul <- proj_crop_mask_mean_write(
  df_of_files = modfile_query(
    month = "Jul",
    years = 2000:2016
  ),
  region_name = "SWAFR",
  slave_border = SWAFR_border_buffered
)
modis_aug <- proj_crop_mask_mean_write(
  df_of_files = modfile_query(
    month = "Aug",
    years = 2000:2016
  ),
  region_name = "SWAFR",
  slave_border = SWAFR_border_buffered
)
modis_sep <- proj_crop_mask_mean_write(
  df_of_files = modfile_query(
    month = "Sep",
    years = 2000:2016
  ),
  region_name = "SWAFR",
  slave_border = SWAFR_border_buffered
)
modis_oct <- proj_crop_mask_mean_write(
  df_of_files = modfile_query(
    month = "Oct",
    years = 2000:2016
  ),
  region_name = "SWAFR",
  slave_border = SWAFR_border_buffered
)
modis_nov <- proj_crop_mask_mean_write(
  df_of_files = modfile_query(
    month = "Nov",
    years = 2000:2016
  ),
  region_name = "SWAFR",
  slave_border = SWAFR_border_buffered
)

# ........ Save months into stack ----------------------------------------------

monthly_LST_SWAFR_0.05_buffered <- stack()
for (i in 1:12) {
  x <- raster(paste0(
    reswd,
    "MODIS_",
    i,
    "_SWAFR_0.05_buffered.grd"
  ))
  monthly_LST_SWAFR_0.05_buffered %<>% stack(x)
}
names(monthly_LST_SWAFR_0.05_buffered) <- c(
  "Jan", "Feb", "Mar",
  "Apr", "May", "Jun",
  "Jul", "Aug", "Sep",
  "Oct", "Nov", "Dec"
)
writeRaster(
  monthly_LST_SWAFR_0.05_buffered,
  filename = paste0(reswd, "MODIS_monthly_means_SWAFR_0.05_buffered.grd"),
  bandorder = "BIL",
  overwrite = T
)

# ........ Make mean annual LST ------------------------------------------------

MA_LST_SWAFR_0.05_buffered <- mean(monthly_LST_SWAFR_0.05_buffered)
writeRaster(
  MA_LST_SWAFR_0.05_buffered,
  filename = paste0(reswd, "MODIS_annual_mean_SWAFR_0.05_buffered.grd"),
  bandorder = "BIL",
  overwrite = T
)

# Quarterly-derived bioclimatic variables --------------------------------------

# .... Import monthly LST stacks -----------------------------------------------

GCFR_monthly_LST <- import_raster_stack(
  "temperature",
  "MODIS_monthly_means_GCFR_0.05_buffered.grd",
  n_bands = 12
)
proj4string(GCFR_monthly_LST) == std_CRS # TRUE
res(GCFR_monthly_LST)
SWAFR_monthly_LST <- import_raster_stack(
  "temperature",
  "MODIS_monthly_means_SWAFR_0.05_buffered.grd",
  n_bands = 12
)
proj4string(SWAFR_monthly_LST) == std_CRS # TRUE
res(SWAFR_monthly_LST)

# .... Make LST in the warmest quarter and coolest quarter ---------------------

GCFR_TWQ <- biovars_TWQ(GCFR_monthly_LST)
GCFR_TCQ <- biovars_TCQ(GCFR_monthly_LST)

SWAFR_TWQ <- biovars_TWQ(SWAFR_monthly_LST)
SWAFR_TCQ <- biovars_TCQ(SWAFR_monthly_LST)

# Save
writeRaster(
  GCFR_TWQ,
  here::here("data/derived-data/temperature/GCFR_TWQ_buffered.tif")
)
writeRaster(
  GCFR_TCQ,
  here::here("data/derived-data/temperature/GCFR_TCQ_buffered.tif")
)
writeRaster(
  SWAFR_TWQ,
  here::here("data/derived-data/temperature/SWAFR_TWQ_buffered.tif")
)
writeRaster(
  SWAFR_TCQ,
  here::here("data/derived-data/temperature/SWAFR_TCQ_buffered.tif")
)
