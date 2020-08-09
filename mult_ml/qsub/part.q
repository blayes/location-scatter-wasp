#!/bin/bash
#$ -N ml_part
#$ -q INFORMATICS
#$ -wd /Users/ssrivastva/mult_ml/code/
#$ -t 1-500
#$ -V
#$ -e /Users/ssrivastva/err/
#$ -o /Users/ssrivastva/out/
#$ -j y

module load stack/2020.1
module load r-stanheaders/2.18.1-10_gcc-9.2.0
module load r-rstan/2.19.2_gcc-9.2.0

R CMD BATCH --no-save --no-restore "--args 1 $SGE_TASK_ID" submit_part.R samp/part_$SGE_TASK_ID.rout
