#PBS -q UCTlong
#PBS -l nodes=1:ppn=8:series600
#PBS -N fit-final-BRTs

module load software/R-3.5.1

Rscript 'fit-final-BRTs.R'
