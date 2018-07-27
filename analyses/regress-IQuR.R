# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
import_objects(output_paths)
IQ95R_data_numeric <- mutate(IQ95R_data, resolution = case_when(
  resolution == "0.05º" ~ 0.05,
  resolution == "QDS"   ~ 0.25,
  resolution == "HDS"   ~ 0.50,
  resolution == "3QDS"  ~ 0.75
))
IQ95R_data_numeric_95 <- filter(IQ95R_data_numeric, quantile == 0.95)
IQ95R_data_numeric_99 <- filter(IQ95R_data_numeric, quantile == 0.99)
m <- list(
  I95R = lm(IXR ~ resolution, IQ95R_data_numeric_95),
  I99R = lm(IXR ~ resolution, IQ95R_data_numeric_99)
)
visreg::visreg(m$I95R)
visreg::visreg(m$I99R)
