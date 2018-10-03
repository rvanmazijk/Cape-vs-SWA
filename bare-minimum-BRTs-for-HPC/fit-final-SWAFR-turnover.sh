#PBS -q UCTlong
#PBS -l nodes=1:ppn=1:series600
#PBS -N fit-final-SWAFR-turnover

module load software/R-3.5.1

Rscript 'fit-final-SWAFR-turnover.R'
