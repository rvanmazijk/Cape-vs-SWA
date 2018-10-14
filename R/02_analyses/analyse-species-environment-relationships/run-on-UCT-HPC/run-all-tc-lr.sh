#!/bin/sh

for tc in {1..5}
do
  for lr in 0.01 0.005 0.001 0.0005 0.0001
  do
    qsub tc-"$tc"_lr-"$lr".sh 
  done
done

