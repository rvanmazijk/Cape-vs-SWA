library(nlme)

heterogeneity_QDS <-
  glue("{data_dir}/raster-layers/heterogeneity-QDS_{var_names_tidy}.tif") %>%
  stack() %>%
  set_names(var_names_tidy)
heterogeneity_HDS <-
  glue("{data_dir}/raster-layers/heterogeneity-HDS_{var_names_tidy}.tif") %>%
  stack() %>%
  set_names(var_names_tidy)
heterogeneity_DS <-
  glue("{data_dir}/raster-layers/heterogeneity-DS_{var_names_tidy}.tif") %>%
  stack() %>%
  set_names(var_names_tidy)
richness_QDS <- raster(glue("{data_dir}/raster-layers/QDS-richness_QDS.tif"))
richness_HDS <- raster(glue("{data_dir}/raster-layers/HDS-richness_HDS.tif"))
richness_DS  <- raster(glue("{data_dir}/raster-layers/DS-richness_DS.tif"))

GCFR_heterogeneity_QDS  <- crop(heterogeneity_QDS, GCFR_border_buffered)
SWAFR_heterogeneity_QDS <- crop(heterogeneity_QDS, SWAFR_border_buffered)
GCFR_heterogeneity_HDS  <- crop(heterogeneity_HDS, GCFR_border_buffered)
SWAFR_heterogeneity_HDS <- crop(heterogeneity_HDS, SWAFR_border_buffered)
GCFR_heterogeneity_DS   <- crop(heterogeneity_DS, GCFR_border_buffered)
SWAFR_heterogeneity_DS  <- crop(heterogeneity_DS, SWAFR_border_buffered)

GCFR_richness_QDS  <- crop(richness_QDS, GCFR_border_buffered)
SWAFR_richness_QDS <- crop(richness_QDS, SWAFR_border_buffered)
GCFR_richness_HDS  <- crop(richness_HDS, GCFR_border_buffered)
SWAFR_richness_HDS <- crop(richness_HDS, SWAFR_border_buffered)
GCFR_richness_DS   <- crop(richness_DS, GCFR_border_buffered)
SWAFR_richness_DS  <- crop(richness_DS, SWAFR_border_buffered)

GCFR_data_QDS <-
  stack(GCFR_heterogeneity_QDS, GCFR_richness_QDS) %>%
  {cbind(
    xyFromCell(., 1:ncell(.)),
    as.data.frame(.)
  )} %>%
  na.exclude()
SWAFR_data_QDS <-
  stack(SWAFR_heterogeneity_QDS, SWAFR_richness_QDS) %>%
  {cbind(
    xyFromCell(., 1:ncell(.)),
    as.data.frame(.)
  )} %>%
  na.exclude()
GCFR_data_HDS <-
  stack(GCFR_heterogeneity_HDS, GCFR_richness_HDS) %>%
  {cbind(
    xyFromCell(., 1:ncell(.)),
    as.data.frame(.)
  )} %>%
  na.exclude()
SWAFR_data_HDS <-
  stack(SWAFR_heterogeneity_HDS, SWAFR_richness_HDS) %>%
  {cbind(
    xyFromCell(., 1:ncell(.)),
    as.data.frame(.)
  )} %>%
  na.exclude()
GCFR_data_DS <-
  stack(GCFR_heterogeneity_DS, GCFR_richness_DS) %>%
  {cbind(
    xyFromCell(., 1:ncell(.)),
    as.data.frame(.)
  )} %>%
  na.exclude()
SWAFR_data_DS <-
  stack(SWAFR_heterogeneity_DS, SWAFR_richness_DS) %>%
  {cbind(
    xyFromCell(., 1:ncell(.)),
    as.data.frame(.)
  )} %>%
  na.exclude()

colnames(GCFR_data_QDS)[12]  <- "richness"
colnames(SWAFR_data_QDS)[12] <- "richness"
colnames(GCFR_data_HDS)[12]  <- "richness"
colnames(SWAFR_data_HDS)[12] <- "richness"
colnames(GCFR_data_DS)[12]   <- "richness"
colnames(SWAFR_data_DS)[12]  <- "richness"

formula <- var_names_tidy %>%
  paste(collapse = " + ") %>%
  {paste("richness ~", .)} %>%
  as.formula()

GCFR_lm_QDS  <- lm(formula, GCFR_data_QDS)
GCFR_m_QDS  <- gls(formula, GCFR_data_QDS,  corGaus(form = ~ x + y))
SWAFR_m_QDS <- gls(formula, SWAFR_data_QDS, corGaus(form = ~ x + y))
GCFR_m_HDS  <- gls(formula, GCFR_data_HDS,  corGaus(form = ~ x + y))
SWAFR_m_HDS <- gls(formula, SWAFR_data_HDS, corGaus(form = ~ x + y))
# FIXME:
#GCFR_m_DS   <- gls(formula, GCFR_data_DS,  corGaus(form = ~ x + y))
SWAFR_m_DS  <- gls(formula, SWAFR_data_DS, corGaus(form = ~ x + y))

list(
  GCFR  = list(QDS = GCFR_m_QDS,  HDS = GCFR_m_HDS),
  SWAFR = list(QDS = SWAFR_m_QDS, HDS = SWAFR_m_HDS, DS = SWAFR_m_DS)
) %>%
  map(map, summary) %>%
  map(map, extract2, "tTable") %>%
  map(map, as_tibble, rownames = "term") %>%
  map(bind_rows, .id = "scale") %>%
  bind_rows(.id = "region") %>%
  set_colnames(c("region", "scale", "term", "estimate", "se", "t", "P")) %>%
  dplyr::select(scale, region, term, estimate, se, P) %>%
  filter(term != "(Intercept)") %>%
  ggplot() +
    aes(term, estimate, fill = region, group = region, alpha = case_when(
      P < 0.05  ~ "< 0.05",
      P < 0.10  ~ "< 0.10",
      P >= 0.10 ~ "NS"
    )) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey75") +
    geom_errorbar(
      aes(
        ymin = estimate - se,
        ymax = estimate + se
      ),
      width = 0,
      position = position_dodge(width = 0.25)
    ) +
    geom_point(
      shape = 21,
      position = position_dodge(width = 0.25)
    ) +
    scale_fill_manual(name = "Region", values = c("black", "white")) +
    scale_alpha_manual(name = bquote(italic("P")), values = c(1.0, 0.33, 0.05)) +
    coord_flip() +
    facet_grid(~scale, scales = "free_x")

GCFR_m_QDS_Elevation  <- gls(richness ~ Elevation, GCFR_data_QDS,  corGaus(form = ~ x + y))
SWAFR_m_QDS_Elevation <- gls(richness ~ Elevation, SWAFR_data_QDS, corGaus(form = ~ x + y))
GCFR_m_HDS_Elevation  <- gls(richness ~ Elevation, GCFR_data_HDS,  corGaus(form = ~ x + y))
SWAFR_m_HDS_Elevation <- gls(richness ~ Elevation, SWAFR_data_HDS, corGaus(form = ~ x + y))
GCFR_m_DS_Elevation   <- gls(richness ~ Elevation, GCFR_data_DS,   corGaus(form = ~ x + y))
SWAFR_m_DS_Elevation  <- gls(richness ~ Elevation, SWAFR_data_DS,  corGaus(form = ~ x + y))

summary(GCFR_m_QDS_Elevation )$tTable[2, 4]
summary(SWAFR_m_QDS_Elevation)$tTable[2, 4]
summary(GCFR_m_HDS_Elevation )$tTable[2, 4]
summary(SWAFR_m_HDS_Elevation)$tTable[2, 4]
summary(GCFR_m_DS_Elevation  )$tTable[2, 4]
summary(SWAFR_m_DS_Elevation )$tTable[2, 4]

PC1_QDS <- raster(glue("{data_dir}/raster-layers/PC1_QDS.tif"))
GCFR_PC1_QDS  <- crop(PC1_QDS, GCFR_border_buffered)
SWAFR_PC1_QDS <- crop(PC1_QDS, SWAFR_border_buffered)

data_foo <- stack(PC1_QDS, richness_QDS) %>%
  {cbind(
    xyFromCell(., 1:ncell(.)),
    as.data.frame(.)
  )} %>%
  na.exclude() %>%
  set_colnames(c("x", "y", "PC1", "richness"))
sm <- gls(richness ~ PC1, data_foo, corGaus(form = ~ x + y))
m  <-  lm(richness ~ PC1, data_foo)

plot(richness ~ PC1, data_foo)
abline(m)
abline(sm, col = "red")


GCFR_data_foo <- stack(GCFR_PC1_QDS, GCFR_richness_QDS) %>%
  {cbind(
    xyFromCell(., 1:ncell(.)),
    as.data.frame(.)
  )} %>%
  na.exclude() %>%
  set_colnames(c("x", "y", "PC1", "richness"))
GCFR_sm <- gls(richness ~ PC1, GCFR_data_foo, corGaus(form = ~ x + y))
GCFR_lm <-  lm(richness ~ PC1, GCFR_data_foo)

summary(GCFR_lm)$coefficients
summary(GCFR_sm)$tTable

plot(richness ~ PC1, GCFR_data_foo)
abline(GCFR_lm)
abline(GCFR_sm, col = "red")

hist(residuals(GCFR_lm))
hist(residuals(GCFR_sm))

plot(residuals(GCFR_sm) ~ residuals(GCFR_lm))

visreg::visreg(GCFR_sm)
visreg::visreg(GCFR_lm)
