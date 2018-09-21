**README COPIED FROM /RUAN_UCT/GIS/**

(Some) NASA MODIS data layers
Downloaded by Ruan van Mazijk

Preamble
================================================================================

These data are some of what I used during my Honours project, wherein I compared the environmental correlates of vascular plant species richness in the Cape (Greater Cape Floristic Region, GCFR) and Southwest Australia (Southwest Australia Floristic Region, SWAFR).

All data were downloaded for the whole planet (except for burnt area; see below) using NASA's LADSWEB portal (<https://ladsweb.modaps.eosdis.nasa.gov/>), and then subset to the GCFR and SWAFR.

The HDF-files are the raw files downloaded from NASA. I then converted these to TIFF-files using GDAL, a geo-computational library usable on the command line, but also in ArcGIS/QGIS and R. This was done because HDF v4.0 is a legacy format many programs cannot interpret directly, but NASA keeps using it for some reason.

Land Surface Temperature
================================================================================

Data-product details:

- Product code: MOD11C3
- Product full name: MODIS/Terra Land-Surface Temperature/Emissivity Monthly Global 0.05Deg CMG
- Satellites used: MODIS-Terra
- Description: Monthly average land surface temperature values

Specifications for data downloaded:

- NASA LADSWEB order no.: 501145540
- Spatial extent: Global
- Temporal extent: Feb 2000 to Apr 2017
- Downloaded on: 2017-05-23

NDVI
================================================================================

Data-product details:

- Product code: MOD13C2
- Product full name: MODIS/Terra Vegetation Indices Monthly L3 Global 0.05Deg CMG
- Satellites used: MODIS-Terra
- Description: Monthly average NDVI values

Specifications for data downloaded:

- NASA LADSWEB order no.: 501172795
- Spatial extent: Global
- Temporal extent: Feb 2000 to Apr 2017
- Downloaded on: 2017-09-12

Burnt Area
================================================================================

Note: I ultimately never used this in my Honours work.

Data-product details:

- Product code: MCD64A1
- Product full name: Combined Level 3 Direct Broadcast Burned Area Monthly Global 500m SIN Grid
- Satellites used: MODIS-Terra, MODIS-Aqua

Specifications for data downloaded:

- NASA LADSWEB order nos.: 501172791, 501172893 (one order per region separately)
- Spatial extent:
  - GCFR: H19V11, H20V11, H19V12, H20V12 tiles
  - SWAFR: H27V11, H28V11, H27V12, H28V12 tiles
- Downloaded on: 2017-09-12
