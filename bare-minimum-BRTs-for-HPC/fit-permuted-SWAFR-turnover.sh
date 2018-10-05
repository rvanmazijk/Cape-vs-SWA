#PBS -q UCTlong
#PBS -l nodes=1:ppn=10:series600
#PBS -N fit-permuted-SWAFR-turnover

module load software/R-3.5.1

Rscript 'fit-permuted-SWAFR-turnover.R'
