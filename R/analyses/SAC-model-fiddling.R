library(nlme)

heterogeneity_QDS <-
  glue("{data_dir}/raster-layers/heterogeneity-QDS_{var_names_tidy}.tif") %>%
  stack() %>%
  set_names(var_names_tidy)
GCFR_heterogeneity <- crop(heterogeneity_QDS, GCFR_border_buffered)

richness_QDS <- raster(glue("{data_dir}/raster-layers/QDS-richness_QDS.tif"))
GCFR_richness <- crop(richness_QDS, GCFR_border_buffered)

data <-
  #stack(heterogeneity_QDS, richness_QDS) %>%
  stack(GCFR_heterogeneity, GCFR_richness) %>%
  {cbind(
    xyFromCell(., 1:ncell(.)),
    as.data.frame(.)
  )} %>%
  na.exclude() %>%
  mutate(region = ifelse(x > 60, "SWAFR", "GCFR"))
colnames(data)[12] <- "richness"

data$richness %<>% log10()

formula <- var_names_tidy %>%
  paste(collapse = " + ") %>%
  {paste(., "+ region")} %>%
  {paste("richness ~", .)} %>%
  as.formula()
m <- gls(log10(richness) ~ Elevation, data, corGaus(form = ~ x + y))
m <- gls(formula, data, corGaus(form = ~ x + y))

summary(m)

residuals_QDS <- heterogeneity_QDS$Elevation
residuals_QDS[!is.na(residuals_QDS[])] <- residuals(m)

GCFR_residuals <- GCFR_heterogeneity$Elevation
GCFR_residuals[!is.na(GCFR_residuals[])] <- residuals(m)

plot(crop(residuals_QDS, GCFR_border_buffered))
plot(crop(residuals_QDS, SWAFR_border_buffered))

plot(GCFR_residuals)
plot(residuals(m) ~ m$fitted)

plot(richness ~ Elevation, data)
fit <- predict(m, newdata = data.frame(Elevation = seq(from = -3, to = 3, by = 0.1)), interval = "confidence")
mini_m <- lm(log10(richness) ~ Elevation, data)
mini_fit <- predict(mini_m, newdata = data.frame(Elevation = seq(from = -3, to = 3, by = 0.1)), interval = "confidence")
lines(10^fit ~ seq(from = -3, to = 3, by = 0.1), col = "red")
lines(10^mini_fit[, "fit"] ~ seq(from = -3, to = 3, by = 0.1))
lines(10^mini_fit[, "lwr"] ~ seq(from = -3, to = 3, by = 0.1), col = "grey")
lines(10^mini_fit[, "upr"] ~ seq(from = -3, to = 3, by = 0.1), col = "grey")


summary(lm(data$richness ~ m$fitted))

m %>%
  summary() %>%
  extract2("tTable") %>%
  as_tibble(rownames = "term") %>%
  set_colnames(c("term", "estimate", "se", "t", "P")) %>%
  dplyr::select(term, estimate, P) %>%
  filter(term != "(Intercept)") %>%
  ggplot() +
    aes(term, estimate, colour = P < 0.05) +
    geom_point()
