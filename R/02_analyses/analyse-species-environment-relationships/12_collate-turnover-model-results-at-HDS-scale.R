# Collating output files from 1000 replicate runs &
#   999 replicate permuted-response runs of BRTS of
#   plant species richness at QDS-scale
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

rep_output_path <- here(
  "outputs/species-environment-relationships",
  "from-local-machines/HDS-turnover-models_1000-reps"
)

perm_output_path <- here(
  "outputs/species-environment-relationships",
  "from-local-machines/HDS-turnover-models_999-permuted-reps"
)

output_path <- here("outputs/species-environment-relationships/from-local-machines")

# Import BRT summary and variable-contribution tables --------------------------

rep_summaries <- import_replicate_outputs(
  rep_output_path, "summary", "_",
  response = "turnover", scale = "HDS"
)
rep_contribs <- import_replicate_outputs(
  rep_output_path, "contribs", "_",
  response = "turnover", scale = "HDS"
)

perm_summaries <- import_replicate_outputs(
  perm_output_path, "summary", "_permutation-",
  response = "turnover", scale = "HDS"
)
perm_contribs <- import_replicate_outputs(
  perm_output_path, "contribs", "_permutation-",
  response = "turnover", scale = "HDS"
)

# Save collated data -----------------------------------------------------------

summary_data <- rbind(
  cbind(model_type = "replicates", rep_summaries),
  cbind(model_type = "permutations", perm_summaries)
)
write_csv(
  summary_data,
  glue("{output_path}/HDS-turnover-models-summaries.csv")
)

contribs_data <- rbind(
  cbind(model_type = "replicates", rep_contribs),
  cbind(model_type = "permutations", perm_contribs)
)
write_csv(
  contribs_data,
  glue("{output_path}/HDS-turnover-models-contributions.csv")
)
