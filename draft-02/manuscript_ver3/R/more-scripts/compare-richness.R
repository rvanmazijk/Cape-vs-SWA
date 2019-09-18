# Comparing regions' species richness (S)
# GCFR vs SWAFR manuscript
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("draft-02/R/setup.R"))

# import-cellular-data
HDS <- read_csv(here("draft-02/outputs/QDS_data_cells.csv"))
QDS <- read_csv(here("draft-02/outputs/EDS_data_cells.csv"))

# Remove cells w/ < 4 sub-cells
HDS %<>% filter(n_QDS == 4)
QDS %<>% filter(n_EDS == 4)

# log(x + 1) & scale roughness values to match PCAs were done on logged data
# (See section 3)
HDS[, str_which(names(HDS), "roughness")] %<>%
  log1p() %>%
  scale()
QDS[, str_which(names(QDS), "roughness")] %<>%
  log1p() %>%
  scale()

# Combine datasets for figures
HDS_QDS <- as_tibble(rbind(
  HDS %>%
    dplyr::select(region, HDS_richness, add_turnover_prop, PC1) %>%
    gather(metric, value, HDS_richness, add_turnover_prop, -PC1),
  QDS %>%
    dplyr::select(region, QDS_richness, PC1) %>%
    gather(metric, value, QDS_richness, -PC1)
))

# plot-richness-and-turnover-maps
#richness_data <- rbind(
#  QDS %>%
#    dplyr::select(lon, lat, region, QDS_richness) %>%
#    rename(response_value = QDS_richness) %>%
#    mutate(response = "italic(S)[QDS]"),
#  HDS %>%
#    dplyr::select(lon, lat, region, HDS_richness) %>%
#    rename(response_value = HDS_richness) %>%
#    mutate(response = "italic(S)[HDS]"),
#  HDS %>%
#    dplyr::select(lon, lat, region, add_turnover_prop) %>%
#    rename(response_value = add_turnover_prop) %>%
#    mutate(response = "italic(T)[QDS]/italic(S)[HDS]")
#)
#richness_data %<>%
#  mutate(response = factor(response, levels = c(
#    "italic(S)[QDS]",
#    "italic(S)[HDS]",
#    "italic(T)[QDS]/italic(S)[HDS]"
#  ))) %>%
#  group_by(response) %>%
#  mutate(response_value = scale(response_value)) %>%
#  filter(response != "italic(S)[QDS]")
#ggplot(richness_data, aes(lon, lat, colour = response_value)) +
#  geom_point() +
#  facet_grid(response ~ region, scales = "free_x", labeller = label_parsed) +
#  scale_colour_viridis_c() +
#  theme(strip.text.y = element_text(angle = 0))

# test-richness-turnover-univariate
HDS_QDS %>%
  mutate(metric = case_when(
    metric == "HDS_richness"      ~ "$S_{\\mathrm{HDS}}$",
    metric == "QDS_richness"      ~ "$S_{\\mathrm{QDS}}$",
    metric == "add_turnover_prop" ~ "$T_{\\mathrm{QDS}} / S_{\\mathrm{HDS}}$"
  )) %>%
  mutate(metric = factor(metric, levels = c(
    "$S_{\\mathrm{HDS}}$",
    "$S_{\\mathrm{QDS}}$",
    "$T_{\\mathrm{QDS}} / S_{\\mathrm{HDS}}$"
  ))) %>%
  group_by(metric) %>%
  summarise(
    P = wilcox.test(value[region == "SWAFR"], value[region == "GCFR"])$p.value,
    CLES_value = CLES(value[region == "SWAFR"], value[region == "GCFR"])
  ) %>%
  mutate_if(is.numeric, ~ifelse(.x < 0.001,
    "$< 0.001$",
    format(round(.x, digits = 3), nsmall = 3)
  )) %>%
  dplyr::select(metric, CLES_value, P) %>%
  knitr::kable(
    caption = paste(
      "Results of Mann-Whitney $U$-tests and the $CLES$ of GCFR vs SWAFR",
      "for various species richness and turnover metrics."
    ),
    col.names = c("Metric", "$CLES$", "$P_U$"),
    align = "lrr"
  )

# test-richness-turnover-normality
# Also show how I tested for metrics deviating from normality
HDS_QDS %>%
  group_by(metric) %>%
  summarise(
    P_GCFR  = shapiro.test(value[region == "GCFR" ])$p.value,
    P_SWAFR = shapiro.test(value[region == "SWAFR"])$p.value
  ) %>%
  mutate_at(c("P_GCFR", "P_SWAFR"), list(sig = ~ . < 0.05))

# visualise-partitions
hist_plots <- map(unique(HDS_QDS$metric), ~ HDS_QDS %>%
  filter(metric == .x) %>%
  ggplot(aes(value, fill = region)) +
    geom_histogram(bins = 20, position = "dodge", colour = "black") +
    scale_fill_manual(name = "Region", values = c("black", "white")) +
    labs(
      x = case_when(
        .x == "HDS_richness" ~
          bquote(italic("S")["HDS"]),
        .x == "QDS_richness" ~
          bquote(italic("S")["QDS"]),
        .x == "add_turnover_prop" ~
          bquote(italic("T")["QDS"]/italic("S")["HDS"])
      ),
      y = case_when(
        .x == "QDS_richness" ~ "No. QDS",
        TRUE                 ~ "No. HDS",
      )
    ) +
    theme(
      legend.position      = c(0.9, 0.9),
      legend.justification = c(1,   1),
      axis.text.y          = element_text(angle = 90, hjust = 0.5)
    )
)
names(hist_plots) <- unique(HDS_QDS$metric)
hist_plots[1:2] %<>% map(~.x + theme(legend.position = "none"))

plot_edges <- HDS %$%
  ceiling(max(c(mean_QDS_richness, add_turnover))) + 10

S_HDS_background <- plot_edges %>%
  {seq(from = 0, to = ., by = 10)} %>%
  {expand.grid(x = ., y = .)} %>%
  mutate(z = x + y)
S_HDS_background_plot <- ggplot(S_HDS_background) +
  lims(x = c(0, plot_edges), y = c(0, plot_edges)) +
  geom_contour(
    mapping     = aes(x, y, z = z),
    binwidth    = 500,
    colour      = "grey90",
    show.legend = TRUE
  ) +
  geom_abline(
    intercept = 0, slope = 1,
    linetype = "dashed", colour = "grey25"
  ) +
  labs(
    x = bquote(italic("T")["QDS"]),
    y = bquote(bar(italic("S"))["QDS"])
  ) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

partition_plot <- S_HDS_background_plot +
  geom_point(
    data    = HDS,
    mapping = aes(add_turnover, mean_QDS_richness, fill = region),
    shape   = 21
  ) +
  scale_fill_manual(values = c("black", "white")) +
  theme(legend.position = "none")

# Add labels to S_HDS contours manually
label1500 <- bquote(italic("S")["HDS"] == 1500)
partition_plot <- partition_plot +
  geom_text(
    data = tibble(
      add_turnover      = c( 10,   10,        60,  360,  860, 1360),
      mean_QDS_richness = c(500, 1000,      1450, 1650, 1650, 1650),
      HDS_richness      = c(500, 1000, label1500, 2000, 2500, 3000)
    ),
    mapping = aes(add_turnover, mean_QDS_richness, label = HDS_richness),
    angle = -45, vjust = -0.5, colour = "grey50", size = 2.5,
    parse = TRUE
  ) +
  # Flip partition plot to get axes to line up across panels (b/o text heights)
  coord_flip()

# Plot panels
hist_plots %$% plot_grid(
  HDS_richness,   QDS_richness,
  partition_plot, add_turnover_prop,
  nrow = 2,
  labels = glue("({letters[1:4]})"),
  label_x = 0.125, label_y = 0.975
)
