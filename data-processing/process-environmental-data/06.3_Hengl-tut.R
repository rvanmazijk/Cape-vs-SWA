# Hengl's tutorial --------------------------------------------------------

p_load(
  RCurl,
  rgdal,
  GSIF,
  raster,
  plotKML,
  XML,
  lattice,
  aqp,
  soiltexture
)

wg_url <- url("http://gsif.isric.org/lib/exe/fetch.php?media=admin.af.rda")
load(wg_url)
proj4string(admin.af) == std_CRS
# Let's try forcing anyway?
proj4string(admin.af) <- "+proj=longlat +datum=WGS84"
country_af <- as(admin.af, "SpatialLines")
ZA <- admin.af[admin.af$FORMAL_EN == "Republic of South Africa", ]

# Original URL from tut (doesn't work; site itself is missing??):
wcs <- "http://webservices.isric.org/geoserver/wcs?"

# Trying this URL for WCS
# (part of the URL that the SoilGrids250m GUI sends me to)
wcs <- "http://85.214.241.121:8080/geoserver/wcs?"

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent = l1)
l1.l <- newXMLNode("CoverageName", "CECSOL_M_sl1_250m", parent = l1)
xml_out <- "ORCDRC_M_sl1.xml"
saveXML(l1, file = xml_out)

system(glue("
  /Library/Frameworks/GDAL.framework/Versions/1.11/Programs/\\
  gdalinfo {xml_out}
"))

bb <- matrix(nrow = 2, c(-180, -62.00081, 179.9999, 87.37))
te <- as.vector(ZA@bbox)
o.x <- 172800 + round(172800 * (te[1] - bb[1, 2]) / (bb[1, 2] - bb[1, 1]))
o.y <- round(67200 * (bb[2, 2] - te[4]) / (bb[2, 2] - bb[2, 1]))
d.y <- round(67200 * (te[4] - te[2]) / (bb[2, 2] - bb[2, 1]))
d.x <- round(172800 * (te[3] - te[1]) / (bb[1, 2] - bb[1, 1]))
o.x; o.y; d.x; d.y

system(glue("
  /Library/Frameworks/GDAL.framework/Versions/1.11/Programs/\\
  gdal_translate {xml_out} CECSOL_M_sl1_ZA.tif \\
  -co \"COMPRESS=DEFLATE\" -srcwin {o.x} {o.y} {d.x} {d.y}
"))
