#PBS -q UCTlong
#PBS -l nodes=1:ppn=1:series600
#PBS -N tc-3_lr-0.001

module load software/R-3.5.1

Rscript 'tc-3_lr-0.001.R'
