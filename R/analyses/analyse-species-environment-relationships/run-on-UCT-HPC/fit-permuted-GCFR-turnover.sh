#PBS -q UCTlong
#PBS -l nodes=1:ppn=64:series600
#PBS -N fit-permuted-GCFR-turnover

module load software/R-3.5.1

Rscript 'fit-permuted-GCFR-turnover.R'
