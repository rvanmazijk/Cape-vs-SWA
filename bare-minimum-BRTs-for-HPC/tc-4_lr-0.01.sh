#PBS -q UCTlong
#PBS -l nodes=1:ppn=1:series600
#PBS -N tc-4_lr-0.01

module load software/R-3.5.1

Rscript 'tc-4_lr-0.01.R'
