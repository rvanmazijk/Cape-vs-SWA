# Visualise model results ------------------------------------------------------

#### WIP
plot(Elevation_se ~ GCFR_all_QDS_pts@data$Elevation, data = GCFR_models$elev$SDF@data)
spplot(
  GCFR_models$elev$SDF["Elevation"], scales = list(draw = TRUE),
  sp.layout = list(GCFR_border_buffered)
)
spplot(
  SWAFR_models$elev$SDF["Elevation"], scales = list(draw = TRUE),
  sp.layout = list(SWAFR_border_buffered)
)
####
