#PBS -q UCTlong
#PBS -l nodes=1:ppn=1:series600
#PBS -N fit-final-GCFR-richness

module load software/R-3.5.1

Rscript 'fit-final-GCFR-richness.R'
