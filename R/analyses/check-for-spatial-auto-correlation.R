my_Moran <- function(r, n_sim = 999) {
  I <- Moran(r)
  E_I <- -1/(length(r[!is.na(r[])]) - 1)
  null_I <- vector(length = n_sim)
  for (i in 1:n_sim) {
    r_copy <- r
    r_copy[!is.na(r_copy[])] %<>% sample()
    null_I[[i]] <- Moran(r_copy)
  }
  P_I <- rank(c(I, null_I))[[1]]/(n_sim + 1)
  # Make two-sided
  P_I <- ifelse(P_I > 0.5,
    1 - P_I + (1/(n_sim + 1)),
    P_I
  )
  beepr::beep(8)
  list(
    I      = I,
    E_I    = E_I,
    null_I = list(null_I),
    P_I    = P_I
  )
}

heterogeneity_0.10 <-
  glue("{data_dir}/raster-layers/heterogeneity-0_{var_names_tidy}.tif") %>%
  stack() %>%
  set_names(var_names_tidy)
GCFR_heterogeneity_0.10  <- crop(heterogeneity_0.10, GCFR_border_buffered)
SWAFR_heterogeneity_0.10 <- crop(heterogeneity_0.10, SWAFR_border_buffered)
GCFR_heterogeneity_0.10_Moran <- GCFR_heterogeneity_0.10 %>%
  as.list() %>%
  map(my_Moran) %>%
  set_names(var_names_tidy) %>%
  bind_rows(.id = "variable")
SWAFR_heterogeneity_0.10_Moran <- SWAFR_heterogeneity_0.10 %>%
  as.list() %>%
  map(my_Moran) %>%
  set_names(var_names_tidy) %>%
  bind_rows(.id = "variable")

heterogeneity_QDS <-
  glue("{data_dir}/raster-layers/heterogeneity-QDS_{var_names_tidy}.tif") %>%
  stack() %>%
  set_names(var_names_tidy)
GCFR_heterogeneity_QDS  <- crop(heterogeneity_QDS, GCFR_border_buffered)
SWAFR_heterogeneity_QDS <- crop(heterogeneity_QDS, SWAFR_border_buffered)
GCFR_heterogeneity_QDS_Moran <- GCFR_heterogeneity_QDS %>%
  as.list() %>%
  map(my_Moran) %>%
  set_names(var_names_tidy) %>%
  bind_rows(.id = "variable")
SWAFR_heterogeneity_QDS_Moran <- SWAFR_heterogeneity_QDS %>%
  as.list() %>%
  map(my_Moran) %>%
  set_names(var_names_tidy) %>%
  bind_rows(.id = "variable")

heterogeneity_HDS <-
  glue("{data_dir}/raster-layers/heterogeneity-HDS_{var_names_tidy}.tif") %>%
  stack() %>%
  set_names(var_names_tidy)
GCFR_heterogeneity_HDS  <- crop(heterogeneity_HDS, GCFR_border_buffered)
SWAFR_heterogeneity_HDS <- crop(heterogeneity_HDS, SWAFR_border_buffered)
GCFR_heterogeneity_HDS_Moran <- GCFR_heterogeneity_HDS %>%
  as.list() %>%
  map(my_Moran) %>%
  set_names(var_names_tidy) %>%
  bind_rows(.id = "variable")
SWAFR_heterogeneity_HDS_Moran <- SWAFR_heterogeneity_HDS %>%
  as.list() %>%
  map(my_Moran) %>%
  set_names(var_names_tidy) %>%
  bind_rows(.id = "variable")

heterogeneity_DS <-
  glue("{data_dir}/raster-layers/heterogeneity-DS_{var_names_tidy}.tif") %>%
  stack() %>%
  set_names(var_names_tidy)
GCFR_heterogeneity_DS  <- crop(heterogeneity_DS, GCFR_border_buffered)
SWAFR_heterogeneity_DS <- crop(heterogeneity_DS, SWAFR_border_buffered)
GCFR_heterogeneity_DS_Moran <- GCFR_heterogeneity_DS %>%
  as.list() %>%
  map(my_Moran) %>%
  set_names(var_names_tidy) %>%
  bind_rows(.id = "variable")
SWAFR_heterogeneity_DS_Moran <- SWAFR_heterogeneity_DS %>%
  as.list() %>%
  map(my_Moran) %>%
  set_names(var_names_tidy) %>%
  bind_rows(.id = "variable")

Moran_data <- as_tibble(rbind(
  cbind(region = "GCFR",  scale = 0.10, bind_rows(GCFR_heterogeneity_0.10_Moran)),
  cbind(region = "SWAFR", scale = 0.10, bind_rows(SWAFR_heterogeneity_0.10_Moran)),
  cbind(region = "GCFR",  scale = 0.25, bind_rows(GCFR_heterogeneity_QDS_Moran)),
  cbind(region = "SWAFR", scale = 0.25, bind_rows(SWAFR_heterogeneity_QDS_Moran)),
  cbind(region = "GCFR",  scale = 0.50, bind_rows(GCFR_heterogeneity_HDS_Moran)),
  cbind(region = "SWAFR", scale = 0.50, bind_rows(SWAFR_heterogeneity_HDS_Moran)),
  cbind(region = "GCFR",  scale = 1.00, bind_rows(GCFR_heterogeneity_DS_Moran)),
  cbind(region = "SWAFR", scale = 1.00, bind_rows(SWAFR_heterogeneity_DS_Moran))
))

write_csv(Moran_data[, -6], here("results/heterogeneity-Moran-tests.csv"))
write_rds(Moran_data, here("results/heterogeneity-Moran-tests") )

richness_QDS <- raster(glue("{data_dir}/raster-layers/QDS-richness_QDS.tif"))
GCFR_richness_QDS  <- crop(richness_QDS, GCFR_border_buffered)
SWAFR_richness_QDS <- crop(richness_QDS, SWAFR_border_buffered)
GCFR_richness_QDS_Moran <- my_Moran(GCFR_richness_QDS)
SWAFR_richness_QDS_Moran <- my_Moran(SWAFR_richness_QDS)

richness_HDS <- raster(glue("{data_dir}/raster-layers/HDS-richness_HDS.tif"))
GCFR_richness_HDS  <- crop(richness_HDS, GCFR_border_buffered)
SWAFR_richness_HDS <- crop(richness_HDS, SWAFR_border_buffered)
GCFR_richness_HDS_Moran <- my_Moran(GCFR_richness_HDS)
SWAFR_richness_HDS_Moran <- my_Moran(SWAFR_richness_HDS)

richness_DS <- raster(glue("{data_dir}/raster-layers/DS-richness_DS.tif"))
GCFR_richness_DS  <- crop(richness_DS, GCFR_border_buffered)
SWAFR_richness_DS <- crop(richness_DS, SWAFR_border_buffered)
GCFR_richness_DS_Moran <- my_Moran(GCFR_richness_DS)
SWAFR_richness_DS_Moran <- my_Moran(SWAFR_richness_DS)

Moran_data_richness <- as_tibble(rbind(
  cbind(region = "GCFR",  scale = "QDS", bind_rows(GCFR_richness_QDS_Moran)),
  cbind(region = "SWAFR", scale = "QDS", bind_rows(SWAFR_richness_QDS_Moran)),
  cbind(region = "GCFR",  scale = "HDS", bind_rows(GCFR_richness_HDS_Moran)),
  cbind(region = "SWAFR", scale = "HDS", bind_rows(SWAFR_richness_HDS_Moran)),
  cbind(region = "GCFR",  scale = "DS",  bind_rows(GCFR_richness_DS_Moran)),
  cbind(region = "SWAFR", scale = "DS",  bind_rows(SWAFR_richness_DS_Moran))
))

write_csv(Moran_data_richness[, -5], here("results/richness-Moran-tests.csv"))
write_rds(Moran_data_richness, here("results/richness-Moran-tests"))

par(mfrow = c(3, 2))
for (i in 1:nrow(Moran_data_richness)) {
  hist(
    Moran_data_richness$null_I[[i]],
    main = "",
    xlim = c(-0.6, 0.6),
    xlab = "Moran's I"
  )
  abline(v = Moran_data_richness$I[[i]], lty = "dashed")
  title(paste(
    "Region:", Moran_data_richness$region[[i]], "\n",
    "Scale:",  Moran_data_richness$scale[[i]],  "\n",
    "P =",     Moran_data_richness$P_I[[i]]
  ))
}
par(op)

Moran_data %>%
  filter(P_I > 0.05)
as.data.frame(Moran_data[, -6])

Moran_data %>%
  mutate(
    null_max = map_dbl(null_I, quantile, 0.975),
    null_min = map_dbl(null_I, quantile, 0.025)
  ) %>%
  ggplot() +
    aes(scale, I, colour = region, group = region) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey75") +
    geom_errorbar(
      aes(ymax = null_max, ymin = null_min),
      width = 0, position = position_dodge(width = 0.05)
    ) +
    geom_point() +
    geom_line() +
    labs(x = "Spatial scale (º)", y = bquote("Moran's"~italic("I"))) +
    facet_wrap(~variable) +
    scale_colour_manual(name = "Region", values = c("black", "grey50"))

mean_QDS_richness <- raster(glue(
  "{data_dir}/raster-layers/",
  "mean-QDS-richness_HDS.tif"
))
GCFR_mean_QDS_richness  <- crop(mean_QDS_richness, GCFR_border_buffered)
SWAFR_mean_QDS_richness <- crop(mean_QDS_richness, SWAFR_border_buffered)

mean_HDS_richness <- raster(glue(
  "{data_dir}/raster-layers/",
  "mean-HDS-richness_DS.tif"
))
GCFR_mean_HDS_richness  <- crop(mean_HDS_richness, GCFR_border_buffered)
SWAFR_mean_HDS_richness <- crop(mean_HDS_richness, SWAFR_border_buffered)

GCFR_HDS_turnover_prop <-
  (GCFR_richness_HDS - GCFR_mean_QDS_richness)/
   GCFR_richness_HDS
SWAFR_HDS_turnover_prop <-
  (SWAFR_richness_HDS - SWAFR_mean_QDS_richness)/
   SWAFR_richness_HDS

my_Moran(GCFR_HDS_turnover_prop)
my_Moran(SWAFR_HDS_turnover_prop)

plot(MoranLocal(GCFR_richness_QDS)[] ~ GCFR_richness_QDS[])
plot(MoranLocal(GCFR_richness_HDS)[] ~ GCFR_HDS_turnover_prop[])
plot(MoranLocal(GCFR_richness_DS)[] ~ GCFR_richness_DS[])

MoranLocalP <- function(r, n_sim = 999) {
  I <- MoranLocal(r)
  null_I <- vector("list", length = n_sim)
  for (i in 1:n_sim) {
    r_copy <- r
    r_copy[!is.na(r_copy[])] %<>% sample()
    null_I[[i]] <- MoranLocal(r_copy)
    cat(glue("{i}, "))
  }
  cat("\n\n")
  null_I %<>% stack()
  P <- I
  P[] <- NA
  for (i in 1:ncell(r)) {
    if (!is.na(I[i])) {
      P[i] <- rank(c(I[i], null_I[i]))[[1]]/(n_sim + 1)
    }
    cat(glue("{i}, "))
  }
  cat("\n\n")
  # Make two-sided
  P[!is.na(P[])] %<>% {ifelse(. > 0.5,
    1 - . + (1/(n_sim + 1)),
    .
  )}
  beepr::beep(8)
  P
}

GCFR_richness_QDS_MoranLocalP <- MoranLocalP(GCFR_richness_QDS)
plot(GCFR_richness_QDS_MoranLocalP)
plot(GCFR_richness_QDS_MoranLocalP[] ~ GCFR_richness_QDS[])
plot(MoranLocal(GCFR_richness_QDS))
plot(MoranLocal(GCFR_richness_QDS) * (1 - 2*GCFR_richness_QDS_MoranLocalP))
plot(
  MoranLocal(GCFR_richness_QDS)[] * (1 - 2*GCFR_richness_QDS_MoranLocalP[]) ~
  GCFR_richness_QDS[]
)

QDS_residuals <- raster(glue("{data_dir}/raster-layers/PC1_residual_QDS.tif"))
GCFR_QDS_residual  <- crop(QDS_residuals, GCFR_border_buffered)
SWAFR_QDS_residual <- crop(QDS_residuals, SWAFR_border_buffered)

Moran(GCFR_QDS_residual)
my_Moran(GCFR_QDS_residual)
plot(MoranLocal(GCFR_QDS_residual))
plot(MoranLocal(GCFR_QDS_residual)[] ~ GCFR_QDS_residual[])
MoranLocalP(GCFR_QDS_residual)
plot(GCFR_QDS_residual[][-ncell(GCFR_QDS_residual)] ~ GCFR_QDS_residual[][-1])
m <- lm(residual ~ x + y + I(x^2) + I(y^2), as.data.frame(cbind(
  residual = GCFR_QDS_residual[],
  xyFromCell(GCFR_QDS_residual, 1:ncell(GCFR_QDS_residual))
)))
summary(m)
plot(m)
visreg::visreg(m, xvar = "x", by = "y", overlay = TRUE)
visreg::visreg(m, xvar = "y", by = "x", overlay = TRUE)
