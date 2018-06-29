# Using bash-QGIS-GDAL commands in R
# Cape vs SWA publication
# Ruan van Mazijk

# This was necessary on my system (Macintosh), as explained in functions/README.md

#' bash_gdalinfo
#' Applies the `gdal_info` algorithm to a `.HDF` file, using `bash`
#' @param x A string, specifying the the `.HDF` file name
#' @param x_dir A string, the directory \code{x} is in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                 (QGIS GDAL on my MacBook is the default)
#' @return The `gdal_info` output describing \code{x}
bash_gdalinfo <- function(x,
                          x_dir = getwd(),
                          gdal_dir = "/Library/Frameworks/GDAL.framework/Versions/1.11/Programs/") {
  system(paste0(
    gdal_dir, "gdalinfo ",
    "\"", x_dir, x, "\""
  ))
}

#' bash_gdalinfo_one
#' Applies the `gdal_info` algorithm to a band in a `.HDF` file, using `bash`
#' @param x A string, specifying the the `.HDF` file name
#' @param prefix A string, e.g. "HDF4_EOS:EOS_GRID:", needed for the
#'               `gdal-info` algorithm to parse properly
#' @param band A string, e.g. ":MODIS_Grid_Month_6km_LST:Emis_20", specifying the band
#' @param x_dir A string, the directory \code{x} is in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                 (QGIS GDAL on my MacBook is the default)
#' @return The `gdal_info` output describing the specified band \code{x}
bash_gdalinfo_one <- function(x, prefix = "HDF4_EOS:EOS_GRID:", band,
                              x_dir = getwd(),
                              gdal_dir = "/Library/Frameworks/GDAL.framework/Versions/1.11/Programs/") {
  system(paste0(
    gdal_dir, "gdalinfo ",
    prefix, "\"", x_dir, x, "\"", band
  ))
}

#' bash_gdaltranslate_all
#' Applies `gdal_translate` to all bands of some `.HDF` file in `bash`,
#' using the gdal library executables that QGIS uses, in order to change the file format
#' @param x A string, specifying the the `.HDF` file name
#' @param out A string, specifying the desired output file---typically a `.tif`
#' @param x_dir A string, the directory \code{x} is in
#' @param out_dir A string, the directory \code{out} is to be put in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                 (QGIS GDAL on my MacBook is the default)
#' @return A converted `.HDF` to, e.g., a `.tif`, in \code{out_dir}
bash_gdaltranslate_all <- function(x, out,
                                   x_dir = getwd(), out_dir = getwd(),
                                   gdal_dir = "/Library/Frameworks/GDAL.framework/Versions/1.11/Programs/") {
  system(paste0(
    gdal_dir, "gdal_translate -sds ",
    "\"", x_dir,   x,   "\"", " ",
    "\"", out_dir, out, "\""
  ))
}

#' bash_gdaltranslate_one
#' Applies `gdal_translate` to one band of some `.HDF` file in `bash`,
#' using the gdal library executables that QGIS uses, in order to change the file format
#' @param x A string, specifying the the `.HDF` file name
#' @param prefix A string, e.g. "HDF4_EOS:EOS_GRID:", needed for the
#'               `gdal-info` algorithm to parse properly
#' @param band A string, e.g. ":MODIS_Grid_Month_6km_LST:Emis_20", specifying the band
#' @param out A string, specifying the desired output file---typically a `.tif`
#' @param x_dir A string, the directory \code{x} is in
#' @param out_dir A string, the directory \code{out} is to be put in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                 (QGIS GDAL on my MacBook is the default)
#' @return A converted `.HDF` band to, e.g., a `.tif`, in \code{out_dir}
bash_gdaltranslate_one <- function(x, prefix = "HDF4_EOS:EOS_GRID:", band, out,
                                   x_dir = getwd(), out_dir = getwd(),
                                   gdal_dir = "/Library/Frameworks/GDAL.framework/Versions/1.11/Programs/") {
  system(paste0(
    gdal_dir, "gdal_translate ",
    prefix, "\"", x_dir,   x,   "\"", band, " ",
    "\"", out_dir, out, "\""
  ))
}

#' bash_gdaltranslate_loop
#' Applies `gdal_translate` to one band of some each `.HDF` file in a list, in `bash`,
#' by calling on `bash_gdaltranslate_one()` in a loop through that list,
#' using the gdal library executables that QGIS uses, in order to change the file format
#' @param x A character vector, specifying the the `.HDF` file names
#' @param prefix A string, e.g. "HDF4_EOS:EOS_GRID:", needed for the
#'               `gdal-info` algorithm to parse properly
#' @param band A string, e.g. ":MODIS_Grid_Month_6km_LST:Emis_20", specifying the band
#' @param out_format A string, specifying the desired outputs'
#'                   file formats---default `.tif`
#' @param x_dir A string, the directory \code{x} is in
#' @param out_dir A string, the directory \code{out} is to be put in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                 (QGIS GDAL on my MacBook is the default)
#' @return A converted batch of `.HDF` bands to, e.g., a `.tif`, in \code{out_dir}
bash_gdaltranslate_loop <- function(x, prefix = "HDF4_EOS:EOS_GRID:", band,
                                    out_format = ".tif",
                                    x_dir = getwd(), out_dir = getwd(),
                                    gdal_dir = "/Library/Frameworks/GDAL.framework/Versions/1.11/Programs/") {
  if (!is.atomic(x) | !is.character(x)) {
    warning("x must be an atomic (i.e. 1-D) character vector!")
    warning("e.g. `order$name` (NOT the data-frame `order`)")
  }
  for (i in 1:length(x)) {
    bash_gdaltranslate_one(
      x = x[i], band = band,
      out = paste0(
        x[i] %>% substr(0, nchar(.) - 4),
        out_format
      ),
      x_dir = x_dir, out_dir = out_dir
    )
  }
}
