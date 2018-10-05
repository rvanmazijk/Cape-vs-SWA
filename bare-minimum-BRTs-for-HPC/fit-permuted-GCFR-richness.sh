#PBS -q UCTlong
#PBS -l nodes=1:ppn=10:series600
#PBS -N fit-permuted-GCFR-richness

module load software/R-3.5.1

Rscript 'fit-permuted-GCFR-richness.R'
