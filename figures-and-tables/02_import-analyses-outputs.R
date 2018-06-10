# Analyse roughness across scales ----------------------------------------------

test_results_summary <- read_csv(
    here::here("analyses/04_outputs/test_results_summary.csv"))
test_results_CLES_for_plot <- read_csv(
    here::here("analyses/04_outputs/test_results_CLES_for_plot.csv"))
data_for_violin_plot <- read_csv(
    here::here("analyses/04_outputs/data_for_violin_plot.csv"))
IQ95R_data <- read_csv(
    here::here("analyses/04_outputs/IQ95R_data.csv"))

# Analyse species turnover w distance ------------------------------------------

species_turnover_geodist_betw_cells_df <- read_csv(
    here::here("analyses/05_outputs/species_turnover_geodist_betw_cells_df.csv"))
species_turnover_geodist_m <- read_rds(
    here::here("analyses/05_outputs/species_turnover_geodist_m.RDS"))

genus_turnover_geodist_betw_cells_df <- read_csv(
    here::here("analyses/05_outputs/genus_turnover_geodist_betw_cells_df.csv"))
genus_turnover_geodist_m <- read_rds(
    here::here("analyses/05_outputs/genus_turnover_geodist_m.RDS"))

family_turnover_geodist_betw_cells_df <- read_csv(
    here::here("analyses/05_outputs/family_turnover_geodist_betw_cells_df.csv"))
family_turnover_geodist_m <- read_rds(
    here::here("analyses/05_outputs/family_turnover_geodist_m.RDS"))

# Analyse species turnover and richness ----------------------------------------

gamma_beta_alpha <- read_csv(
    here::here("analyses/06_outputs/gamma_beta_alpha.csv"))
gamma_beta_alpha_3QDS <- read_csv(
    here::here("analyses/06_outputs/gamma_beta_alpha_3QDS.csv"))
turnover_richness_HDS_m <- read_rds(
    here::here("analyses/06_outputs/turnover_richness_HDS_m.RDS"))
turnover_richness_3QDS_m <- read_rds(
    here::here("analyses/06_outputs/turnover_richness_3QDS_m.RDS"))
HDS_AIC_table <- read_csv(
    here::here("analyses/06_outputs/HDS_AIC_table.csv"))
threeQDS_AIC_table <- read_csv(
    here::here("analyses/06_outputs/threeQDS_AIC_table.csv"))
