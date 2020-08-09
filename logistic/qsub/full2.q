#!/bin/bash
#$ -N full2
#$ -q LT
#$ -pe smp 2
#$ -wd /Users/ssrivastva/logistic/code/
#$ -m a
#$ -M sanvesh-srivastava@uiowa.edu
#$ -t 1-20
#$ -V
#$ -e /Users/ssrivastva/err/
#$ -o /Users/ssrivastva/out/
#$ -j y

module load stack/2020.1
module load r-stanheaders/2.18.1-10_gcc-9.2.0
module load r-rstan/2.19.2_gcc-9.2.0

R CMD BATCH --no-save --no-restore "--args 2 $SGE_TASK_ID" submit.R samp/full_n1e4_$SGE_TASK_ID.rout
