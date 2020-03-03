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

Moran_data %>%
  filter(P_I > 0.05)
as.data.frame(Moran_data[, -6])

Moran_data %>%
  gather(I_type, I, I, E_I) %>%
  mutate(I_type = ifelse(I_type == "I", "Obs.", "Exp.")) %>%
  ggplot() +
    aes(scale, I, colour = I_type) +
    geom_point() +
    geom_line() +
    facet_grid(region ~ variable)

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
    "Scale:", Moran_data_richness$scale[[i]],   "\n",
    "P =", Moran_data_richness$P_I[[i]]
  ))
}
par(op)
