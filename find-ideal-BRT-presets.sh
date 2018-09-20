#PBS -q UCTlong
#PBS -l nodes=1:ppn=64:series600
#PBS -N find-ideal-BRT-presets

module load software/R-3.5.1

Rscript -e "
  source('Cape-vs-SWA/R/install-pkgs.R');
  source('Cape-vs-SWA/R/02_analyses/analyse-species-environment-relationships/01_collate-data.R');
  source('Cape-vs-SWA/R/02_analyses/analyse-species-environment-relationships/02_check-for-collinearity.R');
  source('Cape-vs-SWA/R/02_analyses/analyse-species-environment-relationships/03_find-ideal-BRT-presets.R');
"
