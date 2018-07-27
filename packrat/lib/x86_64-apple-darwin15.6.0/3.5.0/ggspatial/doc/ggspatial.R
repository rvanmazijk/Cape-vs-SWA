## ----setup, include = FALSE----------------------------------------------
library(ggspatial)
rosm::set_default_cachedir(system.file("rosm.cache", package = "ggspatial"))
knitr::opts_chunk$set(fig.width = 4, fig.height = 3, dpi = 150)

## ------------------------------------------------------------------------
library(ggspatial)
load_longlake_data()

## ------------------------------------------------------------------------
ggplot() +
  annotation_spatial(longlake_waterdf) +
  layer_spatial(longlake_depthdf, aes(col = DEPTH.M))

## ------------------------------------------------------------------------
ggplot() +
  annotation_spatial(longlake_waterdf) +
  layer_spatial(longlake_depthdf, aes(col = DEPTH.M)) +
  annotation_scale(location = "tl") +
  annotation_north_arrow(location = "br", which_north = "true")

## ---- message=FALSE------------------------------------------------------
ggplot() +
  annotation_map_tile(type = "osm") +
  layer_spatial(longlake_depthdf, aes(col = DEPTH.M))

## ---- message=FALSE, warning=FALSE---------------------------------------
cities <- data.frame(
  x = c(-63.58595, 116.41214), 
  y = c(44.64862, 40.19063), 
  city = c("Halifax", "Beijing")
)

library(ggrepel) # needed for geom_text_repel()

ggplot(cities, aes(x, y)) +
  annotation_map_tile(type = "stamenwatercolor") +
  geom_spatial_point() +
  stat_spatial_identity(aes(label = city), geom = "text_repel", box.padding = 1) +
  coord_sf(crs = 3995)

