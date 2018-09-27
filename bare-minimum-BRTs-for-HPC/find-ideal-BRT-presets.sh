#PBS -q UCTlong
#PBS -l nodes=1:ppn=64:series600
#PBS -N find-ideal-BRT-presets

module load software/R-3.5.1

Rscript '01_find-ideal-BRT-presets.R'
