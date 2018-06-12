# Utility functions for converting, reprojecting
# and otherwise handling MOD11C3 files
# Cape vs SWA publication
# Ruan van Mazijk

# A series of hierarchical functions that call on e/o w/ the following logic,
# in pseudo-code:
#
#   > modfile_query(
#   >     for each month {
#   >         modfile_query_slave(
#   >             for each year {
#   >                 ...
#   >             }
#   >             return list of files for each month
#   >         )
#   >     }
#   >     return data-frame of the monthly lists
#   > )
#
#   > proj_crop_mask_mean(
#   >     for each month {
#   >         if not already processed {
#   >             proj_crop_mask_mean_slave(
#   >                 for each year {
#   >                   ...
#   >                 }
#   >                 return raster of a month's mean LST
#   >             )
#   >         } else {
#   >             import list of processed rasters for each month
#   >         }
#   >     }
#   >     return list of rasters for each month
#   > )
#
#   > proj_crop_mask_mean_write(
#   >     proj_crop_mask_mean(
#   >         for each month {
#   >             if not already processed {
#   >                 proj_crop_mask_mean_slave(
#   >                     for each year {
#   >                       ...
#   >                     }
#   >                     return raster of a month's mean LST
#   >                 )
#   >             } else {
#   >                 import list of rasters for each month
#   >             }
#   >         }
#   >     )
#   >     then write.raster(
#   >         for each months {
#   >             ...
#   >         }
#   >     )
#   > )

# Note, because of the AWFUL situation (2017-06-20 22:40) where 2017 is missing the months
# that haven't happened yet, I am forced (down the line by the raster functions below)
# to only query one month @ a time.
# That's safer too, seeing as I keep losing perfectly good executions on Feb
# again again and again because of problems with May...
# SO! That's the "recommended usage": only one month!!!


#' modfile_query
#'
#' A query fn that calls on `modfile_query_slave()` in a loop through the months given
#' (but only provide one, please! (bad design, I know)), in order to query a directory
#' for files of a certain pattern (used by default for MODIS `.tif`s here)
#'
#' @param month A string of the ONE month desired,
#'              following the naming system in the default set provided
#'              in the fn def below
#' @param years A numeric vector of the years desired
#' @param slave_dir A string, passed to `modfile_query_slave()`,
#'                  specifying the directory of the (MODIS) `.tif`
#'                  files to be searched for
#' @param slave_prefix A string, passed to `modfile_query_slave()`,
#'                     to descrbe the first bit of the file-name to search for
#'                     (in this case the default is "MOD11C3.A")
#' @param slave_suffix A string, passed to `modfile_query_slave()`,
#'                     describing the file extension to search for
#'                     (in this case the default is the regex ".*.tif")
#'
#' @return A data-frame containing the directory addresses of the
#'         (MODIS) files as queried for, arranged by month (even though
#'         there is only one)
modfile_query <- function(month = c("Jan", "Feb", "Mar",
                                    "Apr", "May", "Jun",
                                    "Jul", "Aug", "Sep",
                                    "Oct", "Nov", "Dec"),
                          years        = 2000:2017,
                          slave_dir    = modwd,
                          slave_prefix = "MOD11C3.A",
                          slave_suffix = ".*.tif") {

    list_of_files <- vector("list", length = length(month))

    for (i in 1:length(month)) {
        list_of_files[[i]] <- modfile_query_slave( # passes all these to slave function below
            month  = month[i],
            years  = years,
            dir    = slave_dir,
            prefix = slave_prefix,
            suffix = slave_suffix
        )
    }

    df_of_files        <- reshape2::melt(list_of_files)
    names(df_of_files) <- c("file_name", "year", "month")
    df_of_files$year   <- as.factor(df_of_files$year)
    for (i in 1:length(df_of_files$month)) {
        df_of_files$month[i] <- switch( # Only one month in fn input!
            month,                      # So that the month label column makes sense!
            Jan =  1, Feb =  2, Mar = 3,
            Apr =  4, May =  5, Jun = 6,
            Jul =  7, Aug =  8, Sep = 9,
            Oct = 10, Nov = 11, Dec = 12
        )
    }
    df_of_files$month  <- as.factor(df_of_files$month)
    # So that the raster functions don't shit themselves:
    df_of_files        <- na.exclude(df_of_files)

    df_of_files

}


#' modfile_query_slave
#'
#' A query fn that calls on `base::list.files()` in a loop through the years provided
#'
#' @param month A string, as in 'modfile_query()`
#' @param years A number, as in 'modfile_query()`
#' @param dir A string, as passed to this fn by 'modfile_query()`
#' @param prefix A string, as passed to this fn by 'modfile_query()`
#' @param suffix A string, as passed to this fn by 'modfile_query()`
#'
#' @return A lsit containing the directory addresses of the
#'         (MODIS) files as queried for
modfile_query_slave <- function(month, years,
                                dir    = modwd,
                                prefix = "MOD11C3.A",
                                suffix = ".*.tif") {

    #if (month == "Jan") stop("Jan has already been done!")
    # Just in case!!!!!!!! (mostly deprecated though) (for GCFR)

    slave_list <- vector("list", length = length(years))
    for (i in 1:length(years)) {

        if (years[i] == 2000) { # To solve the Y2KJan problem....

            month_file_index <- switch(
                month,
                Jan =  0, Feb =  1, Mar = 2,
                Apr =  3, May =  4, Jun = 5,
                Jul =  6, Aug =  7, Sep = 9,
                Oct =  9, Nov = 10, Dec = 11
            )
            list_files <- list.files(
                path   = dir,
                pattern = paste0(prefix, years[i], suffix)
            )
            slave_list[[i]] <- list_files[month_file_index]

        } else {

            month_file_index <- switch(
                month,
                Jan =  1, Feb =  2, Mar = 3,
                Apr =  4, May =  5, Jun = 6,
                Jul =  7, Aug =  8, Sep = 9,
                Oct = 10, Nov = 11, Dec = 12
            )
            list_files <- list.files(
                path    = dir,
                pattern = paste0(prefix, years[i], suffix)
            )
            slave_list[[i]] <- list_files[month_file_index]

        }

    }

    names(slave_list) <- paste0("Year_", years)
    slave_list

}


#' proj_crop_mask_mean_write
#'
#' A command function that calls on `proj_crop_mask_mean()` (which calls in turn on
#' `proj_crop_mask_mean_slave()`) in a loop through months provided in the output of
#' `modfile_query()` (only one, please!), to reproject rasters to a different
#' map projection, and crop and mask them to a border's confines,
#' AND simultaneously writes those resulting rasters to disc
#'
#' @param df_of_files A dataframe, produced by \code{modfile_query()},
#'                    containing the file-names of the rasters you want to
#'                    reproject, crop, & mask
#' @param region_name A string, either "GCFR" or "SWAFR"
#' @param slave_crs A CRS object's string (\code{std_CRS} is the default),
#'                  to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_border A spatial polygons dataframe, describing the border
#'                     within which the rasters must be cropped & masked
#' @param slave_dir A string, the directory that the file-names in \code{df_of_files}
#'                  are in, to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_prefix A string, passed to `modfile_query_slave()`,
#'                     to descrbe the first bit of the file-name to search for
#'                     (in this case the default is "MOD11C3.A"),
#'                     to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_suffix A string, passed to `modfile_query_slave()`,
#'                     describing the file extension to search for
#'                     (in this case the default is the regex ".*.tif"),
#'                     to be passed to \code{proj_crop_mask_mean_slave()}
#'
#' @return A list object, containing the reprojected, cropped, masked rasters
#' @return Also write the rasters in that list to disc
proj_crop_mask_mean_write <- function(df_of_files,
                                      region_name  = c("GCFR", "SWAFR"),
                                      slave_crs    = std_CRS,
                                      slave_border = GCFR_border_buffered,
                                      slave_dir    = modwd,
                                      slave_prefix = "MOD11C3.A",
                                      slave_suffix = ".*.tif") {

    list_of_rasters <- proj_crop_mask_mean( # passes all these to slave functions below
        df_of_files  = df_of_files,
        slave_crs    = slave_crs,
        slave_border = slave_border,
        slave_dir    = slave_dir,
        slave_prefix = slave_prefix,
        slave_suffix = slave_suffix
    )

    for (i in 1:length(levels(df_of_files$month))) {
        list_of_rasters[[i]] %>%
            raster::writeRaster(
                filename = paste0(
                    reswd, "MODIS_",
                    levels(df_of_files$month)[i], "_", region_name, "_0.05_buffered.grd"
                ),
                overwrite = T
            )
    }

    list_of_rasters

}


#' proj_crop_mask_mean
#'
#' A command function that calls on `proj_crop_mask_slave()` in a loop through months
#' provided in the output of `modfile_query()` (only one, please) to reproject rasters
#' to a different map projection, and crop and mask them to a border's confines.
#'
#' @param df_of_files A dataframe, produced by \code{modfile_query()},
#'                    containing the file-names of the rasters you want to
#'                    reproject, crop, & mask
#' @param slave_crs A CRS object's string (\code{std_CRS} is the default),
#'                  to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_border A spatial polygons dataframe, describing the border
#'                     within which the rasters must be cropped & masked,
#'                     to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_dir A string, the directory that the file-names in \code{df_of_files}
#'                  are in, to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_prefix A string, passed to `modfile_query_slave()`,
#'                     to descrbe the first bit of the file-name to search for
#'                     (in this case the default is "MOD11C3.A"),
#'                     to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_suffix A string, passed to `modfile_query_slave()`,
#'                     describing the file extension to search for
#'                     (in this case the default is the regex ".*.tif"),
#'                     to be passed to \code{proj_crop_mask_mean_slave()}
#'
#' @return A list object, containing the reprojected, cropped, masked rasters
proj_crop_mask_mean <- function(df_of_files,
                                slave_crs, slave_border,
                                slave_dir, slave_prefix, slave_suffix) {

    pb_month <- txtProgressBar(min = 0, max = length(levels(df_of_files$month)), style = 3)
    list_of_rasters <- vector("list", length = length(levels(df_of_files$month)))

    for (i in 1:length(levels(df_of_files$month))) {

        list_of_rasters[[i]] <- proj_crop_mask_mean_slave( # passes to slave fns below
            df_of_files = df_of_files,
            slave_month = levels(df_of_files$month)[i],
            crs         = slave_crs,
            border      = slave_border,
            dir         = slave_dir,
            prefix      = slave_prefix,
            suffix      = slave_suffix
        )

        setTxtProgressBar(pb_month, i)

    }

    names(list_of_rasters) <- levels(df_of_files$month)
    list_of_rasters

}


#' proj_crop_mask_mean_slave
#'
#' A command function that calls on a sequence of `raster()`, `projectRaster()`, `mask()`,
#' & `crop()` in a loop through the years in the output of `modfile_query()`,
#' for a given month
#'
#' @param df_of_files A dataframe, produced by \code{modfile_query()},
#'                    containing the file-names of the rasters you want to
#'                    reproject, crop, & mask
#' @param slave_month A string, the month to filter \code{df_of_files} to
#' @param crs A CRS object's string (\code{std_CRS} is the default)
#' @param border A spatial polygons dataframe, describing the border
#'               within which the rasters must be cropped & masked,
#'               to be passed to \code{raster::mask()} & \code{raster::crop()}
#' @param dir A string, the directory that the file-names in \code{df_of_files} are in
#' @param prefix A string, passed to `modfile_query_slave()`,
#'               to descrbe the first bit of the file-name to search for
#'               (in this case the default is "MOD11C3.A")
#' @param suffix A string, passed to `modfile_query_slave()`,
#'               describing the file extension to search for
#'               (in this case the default is the regex ".*.tif")
#'
#' @return A raster, reprojected, masked, & cropped
proj_crop_mask_mean_slave <- function(df_of_files, slave_month,
                                      crs, border,
                                      dir, prefix, suffix) {

    pb_year <- txtProgressBar(min = 0, max = length(levels(df_of_files$year)), style = 3)
    # Initialise an empty stack to fill w/ each yr's raster in the loop below:
    raster_stack <- stack()

    for (i in 1:length(levels(df_of_files$year))) {

        x <- df_of_files %>%
            filter(
                (year  == levels(df_of_files$year)[i]) && # only use the current yr
                    (month == slave_month)                    # & the month given by the slave-owner fns
            ) %>%
            dplyr::select(file_name) %>%
            as_vector() %>%
            as.character.factor()

        x <- paste0(dir, x)
        x <- raster(x) # read-in as raster
        setTxtProgressBar(pb_year, i - 0.8)
        cat("\n\r", levels(df_of_files$year)[i], ": Raster made")

        x %<>% raster::projectRaster(crs = crs) # reproject to std_CRS
        setTxtProgressBar(pb_year, i - 0.6)
        flush.console()
        cat("\n\r", levels(df_of_files$year)[i], ": Reprojected to", substitute(crs))

        x %<>% raster::mask(mask = border) # mask...
        setTxtProgressBar(pb_year, i - 0.6)
        flush.console()
        cat("\n\r", levels(df_of_files$year)[i], ": Masked to", substitute(border))

        x %<>% raster::crop(y = border) # ... & crop
        setTxtProgressBar(pb_year, i - 0.4)
        flush.console()
        cat("\n\r", levels(df_of_files$year)[i], ": Cropped to extent of", substitute(border))

        raster_stack %<>% stack(x) # add it to the stack
        setTxtProgressBar(pb_year, i - 0.2)
        flush.console()
        cat("\n\r", levels(df_of_files$year)[i], ": Added to raster stack")

    }

    # Smoosh that stack down into one mean raster
    raster_out <- mean(raster_stack)
    setTxtProgressBar(pb_year, i)
    cat("\n\r", "Mean of stack calculated")

    raster_out

}
