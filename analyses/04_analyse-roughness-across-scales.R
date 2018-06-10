# Analyse environmental roughness varying across spatial scales
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("analyses/compare_samples.R"))
set.seed(1234)

# Data setup -------------------------------------------------------------------

var_names <- c(
    "Elevation",
    "MAP",
    "PDQ",
    "Surface T",
    "NDVI",
    "CEC",
    "Clay",
    "Soil C",
    "pH"
)
GCFR_variables <- list(
    GCFR_elev,
    GCFR_MAP,
    GCFR_PDQ,
    GCFR_MLST,
    GCFR_NDVI,
    GCFR_soils$GCFR_CECSOL_M_250m_std_CRS_0.05,
    GCFR_soils$GCFR_CLYPPT_M_250m_std_CRS_0.05,
    GCFR_soils$GCFR_OCDENS_M_250m_std_CRS_0.05,
    GCFR_soils$GCFR_PHIKCL_M_250m_std_CRS_0.05
)
SWAFR_variables <- list(
    SWAFR_elev,
    SWAFR_MAP,
    SWAFR_PDQ,
    SWAFR_MLST,
    SWAFR_NDVI,
    SWAFR_soils$SWAFR_CECSOL_M_250m_std_CRS_0.05,
    SWAFR_soils$SWAFR_CLYPPT_M_250m_std_CRS_0.05,
    SWAFR_soils$SWAFR_OCDENS_M_250m_std_CRS_0.05,
    SWAFR_soils$SWAFR_PHIKCL_M_250m_std_CRS_0.05
)
GCFR_variables %<>%
    map(crop, GCFR_variables[[4]]) %>%
    map(mask, GCFR_border)
SWAFR_variables %<>%
    map(crop, SWAFR_variables[[4]]) %>%
    map(mask, SWAFR_border)
names(GCFR_variables) <- var_names
names(SWAFR_variables) <- var_names

# Function setup ---------------------------------------------------------------

focal_sd <- function(x, ...) {
    focal(
        x = x,
        w = matrix(1, nrow = 3, ncol = 3),
        function(x, ...) {
            diffs <- vector(length = 8)
            diffs[1] <- (x[5] - x[1]) ^ 2
            diffs[2] <- (x[5] - x[2]) ^ 2
            diffs[3] <- (x[5] - x[3]) ^ 2
            diffs[4] <- (x[5] - x[4]) ^ 2
            diffs[5] <- (x[5] - x[6]) ^ 2
            diffs[6] <- (x[5] - x[7]) ^ 2
            diffs[7] <- (x[5] - x[8]) ^ 2
            diffs[8] <- (x[5] - x[9]) ^ 2
            return(sqrt(mean(diffs)))
        }
    )
}

prep_layer <- function(x, ...) {
    x %<>%
        aggregate(fact = resolution / 0.05) %>%
        focal_sd() %>%
        `[`()
    if (resolution == 0.05) {
        x %<>% base::sample(size = 5000)  # maxi sample size Wilcox test accepts
    }
    return(x)
}

compare_roughness <- function(x, y, resolution, raw = FALSE, ...) {
    x %<>% prep_layer()
    y %<>% prep_layer()
    test <- compare_samples(x, y, "two.sided", ...)$test
    if (raw) {
        return(test)
    } else {
        return(broom::tidy(test))
    }
}

describe_roughness <- function(x, y, resolution, ...) {
    x %<>% prep_layer()
    y %<>% prep_layer()
    return(compare_samples(x, y, "two.sided", ...)$assumptions)
}

# Test 0.05deg, QDS, HDS, 3QDS comparisons -------------------------------------

test_results <- foreach(resolution = list(0.05, 0.25, 0.50, 0.75)) %do% {
    test_results_at_a_res <-
        map2_df(GCFR_variables,
                SWAFR_variables,
                compare_roughness,
                resolution = resolution) %>%
        cbind(variable = var_names, .) %>%
        as_tibble()
}
names(test_results) <- c("0.05deg", "QDS", "HDS", "3QDS")

test_results_summary <- test_results %>%
    map(mutate, sig = p.value < 0.05) %>%
    map(dplyr::select, variable, sig) %$%
    tibble(variable = var_names,
           `0.05`   = .$`0.05deg`$sig,
           `0.25`   = .$QDS$sig,
           `0.50`   = .$HDS$sig,
           `0.75`   = .$`3QDS`$sig)

transformation_results <- map2(
    GCFR_variables, SWAFR_variables,
    describe_roughness
)
# Because mostly had to Mann-Whitney U,
# why not just Mann-Whitney U all to be parsimonious:

test_results <- foreach(resolution = list(0.05, 0.25, 0.50, 0.75)) %do% {
    test_results_at_a_res <-
        map2_df(GCFR_variables,
                SWAFR_variables,
                compare_roughness,
                resolution = resolution,
                force_mann_whitney_u = TRUE) %>%
        cbind(variable = var_names, .) %>%
        as_tibble()
}
names(test_results) <- c("0.05deg", "QDS", "HDS", "3QDS")

test_results_summary <- test_results %>%
    map(mutate, sig = p.value < 0.05) %>%
    map(dplyr::select, variable, sig) %$%
    tibble(variable = var_names,
           `0.05 x 0.05` = .$`0.05deg`$sig,
           `0.25 x 0.25` = .$QDS$sig,
           `0.50 x 0.50` = .$HDS$sig,
           `0.75 x 0.75` = .$`3QDS`$sig)

# Save to disc
write_csv(
    test_results_summary,
    here::here("analyses/07_outputs/test_results_summary.csv")
)

# CLES for those tests ---------------------------------------------------------
# <https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test#Effect_sizes>

test_results_CLES <- foreach(resolution = list(0.05, 0.25, 0.50, 0.75)) %do% {
    out <- foreach(GCFR = GCFR_variables, SWAFR = SWAFR_variables) %do% {
        canprot::CLES(
            na.omit(prep_layer(GCFR)[]),
            na.omit(prep_layer(SWAFR)[])
        )
    }
    names(out) <- var_names
    return(out)
}
names(test_results_CLES) <- c("0.05", "0.25", "0.50", "0.75")

test_results_CLES_for_plot <- test_results_CLES %>%
    map(as_tibble) %$%
    rbind(.$`0.05`, .$`0.25`, .$`0.50`, .$`0.75`) %>%
    cbind(resolution = names(test_results_CLES), .) %>%
    gather(variable, CLES, -resolution) %>%
    as_tibble()

# Save to disc
write_csv(
    test_results_CLES_for_plot,
    here::here("analyses/07_outputs/test_results_CLES_for_plot.csv")
)

# Prep data for violin plot ----------------------------------------------------

data_for_violin_plot <- foreach(resolution = list(0.05, 0.25, 0.50, 0.75)) %do% {
    rbind(
        cbind(region = "GCFR", map2_df(GCFR_variables, resolution, prep_layer)),
        cbind(region = "SWAFR", map2_df(SWAFR_variables, resolution, prep_layer))
    )
}
data_for_violin_plot <- data_for_violin_plot %$%
    rbind(cbind(resolution = "0.05 x 0.05", .[[1]]),
          cbind(resolution = "0.25 x 0.25", .[[2]]),
          cbind(resolution = "0.50 x 0.50", .[[3]]),
          cbind(resolution = "0.75 x 0.75", .[[4]])) %>%
    as_tibble() %>%
    gather(variable, roughness, -resolution, -region) %>%
    na.omit() %>%
    group_by(resolution, variable) %>%
    mutate(z_roughness = scale(roughness))  # Z-scale that shit!
data_for_violin_plot$variable %<>% factor(levels = var_names)

# Save to disc
write_csv(
    data_for_violin_plot,
    here::here("analyses/07_outputs/data_for_violin_plot.csv")
)

# Analyse IQ95R ~ scale --------------------------------------------------------

IQ99R <- function(x) quantile(x, 0.99) - quantile(x, 0.01)
IQ95R <- function(x) quantile(x, 0.95) - quantile(x, 0.05)

IQ95R_data <- data_for_violin_plot %>%
    group_by(resolution, variable) %>%
    mutate(z_roughness = scale(roughness)) %>%  # Z-scale that shit!
    group_by(resolution, region, variable) %>%
    summarise(IQ99R = IQ99R(z_roughness),
              IQ95R = IQ95R(z_roughness)) %>%
    gather(quantile, IXR,
           -resolution, -region, -variable) %>%
    mutate(quantile = ifelse(quantile == "IQ99R",
                             0.99,
                             ifelse(quantile == "IQ95R",
                                    0.95,
                                    NA)))

# Save to disc
write_csv(
    IQ95R_data,
    here::here("analyses/07_outputs/IQ95R_data.csv")
)

# Plot lm fits of 95%-interquantile range ~ scale * region
#ggplot(IXR_data %>%
#           filter(quantile == 0.95),
#       aes(as.numeric(resolution), IXR, col = region)) +
#    geom_point() +
#    geom_smooth(method = "lm")

# Plot difference in GCFR and SWAFR 95%-interquantile ranges ~ scale
#IXR_data_diff <- IXR_data  %>%
#    group_by(region, resolution, variable, quantile) %>%
#    summarise(IXR) %>%
#    spread(region, IXR) %>%
#    mutate(diff = GCFR - SWAFR)
#ggplot(IXR_data_diff %>%
#           filter(quantile == 0.95),
#       aes(resolution, diff,
#           col = variable,
#           #alpha = quantile,
#           group = variable)) +
#    geom_point() +
#    geom_path() +
#    facet_wrap(~ variable) +
#    geom_hline(yintercept = 0, lty = "dashed") +
#    ylab("GCFR - SWAFR")

# Plot lm fit of diff in 95%-interquantile range ~ scale
#ggplot(IXR_data_diff %>%
#           filter(quantile == 0.95),
#       aes(as.numeric(resolution), diff)) +
#    geom_point() +
#    geom_smooth(method = "lm")
