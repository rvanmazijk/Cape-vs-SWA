#PBS -q UCTlong
#PBS -l nodes=1:ppn=1:series600
#PBS -N fit-GCFR-richness-turnover

module load software/R-3.5.1

Rscript 'fit-GCFR-richness-turnover.R'
