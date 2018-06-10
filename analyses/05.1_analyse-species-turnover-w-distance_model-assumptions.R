# Interrogating assumptions of modelling jaccard's ~ geographic distance
# Cape vs SWA publication
# Ruan van Mazijk

# Setup, import, function def --------------------------------------------------

source(here::here("setup.R"))

species_turnover_geodist_betw_cells_df <- read_csv(here::here("analyses/05_outputs/species_turnover_geodist_betw_cells_df.csv"))
genus_turnover_geodist_betw_cells_df <- read_csv(here::here("analyses/05_outputs/genus_turnover_geodist_betw_cells_df.csv"))
family_turnover_geodist_betw_cells_df <- read_csv(here::here("analyses/05_outputs/family_turnover_geodist_betw_cells_df.csv"))

many_models <- function(data, tau = 0.05, sample_fraction = 0.01) {
    data %<>%
        filter_("geodist" > 0) %>%
        sample_frac(sample_fraction)  # To aid computation time
    linear <- rq("turnover ~ geodist * region", tau, data)
    logged <- rq("turnover ~ log(geodist) * region", tau, data)
    quadratic <- rq("turnover ~ geodist * region + I(geodist  ^ 2)", tau, data)
    log_quadratic <- rq("turnover ~ log(geodist) * region + I(log(geodist) ^ 2)", tau, data)
    out <- list(
        "linear" = linear,
        "logged" = logged,
        "quadratic" = quadratic,
        "log_quadratic" = log_quadratic
    )
    out %<>% map(broom::glance)
    return(out)
}

# DEPRECATED:
if (FALSE) {
    interrogate <- function(data, formula, sample_fraction = 0.1) {
        taxonomic_rank <- deparse(substitute(data)) %>%
            str_split("_") %>%
            map(`[[`, 1) %>%
            as_vector()
        print(taxonomic_rank)
        turnover_geodist_lm <- data %>%
            filter_("geodist" > 0) %>%
            sample_frac(sample_fraction) %>%  # To ease computation time
            lm(formula, data = .)
        pdf(width = 4, height = 4, here::here(glue("
            analyses/05.1_outputs/\\
            {paste(taxonomic_rank, formula)}.pdf
        ")))
        plot(turnover_geodist_lm)
        dev.off()
    }
    interrogate(species_turnover_geodist_betw_cells_df, "turnover ~ geodist")
    interrogate(species_turnover_geodist_betw_cells_df, "turnover ~ log(geodist)")
}

# Find well-fitting models -----------------------------------------------------

many_models(species_turnover_geodist_betw_cells_df)
many_models(genus_turnover_geodist_betw_cells_df)
many_models(family_turnover_geodist_betw_cells_df)

# Results:
#     Log-quadratic is has lowest AIC for species and genus turnover,
#     and is only 4 AIC units away from being better than
#     quadratic alone in family turnover

# Conclusion:
#     Using log-quadratic through out.
#     TODO: Go back to 05.R and change the models!

# What about a t-test though? --------------------------------------------------
# Or at least Mann-Whitney U + CLES

source(here::here("analyses/compare_samples.R"))
compare_samples(
    species_turnover_geodist_betw_cells_df %>%
        filter(region == "GCFR", geodist > 0) %>%
        dplyr::select(turnover) %>%
        sample_n(5000) %>%  # Some functions break w/ too many obs
        as_vector(),
    species_turnover_geodist_betw_cells_df %>%
        filter(region == "SWAFR", geodist > 0) %>%
        dplyr::select(turnover) %>%
        sample_n(5000) %>%
        as_vector(),
    alternative = "two.sided"
)
canprot::CLES(
    species_turnover_geodist_betw_cells_df %>%
        filter(region == "SWAFR", geodist > 0) %>%
        dplyr::select(turnover) %>%
        sample_n(5000) %>%
        as_vector(),
    species_turnover_geodist_betw_cells_df %>%
        filter(region == "GCFR", geodist > 0) %>%
        dplyr::select(turnover) %>%
        sample_n(5000) %>%
        as_vector()
)
species_turnover_geodist_betw_cells_df %>%
    filter(geodist > 0) %>%
    ggplot(aes(region, turnover, col = region)) +
    geom_violin() +
    stat_summary(geom = "point", fun.y = median)
