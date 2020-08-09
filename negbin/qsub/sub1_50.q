#!/bin/bash
#$ -N sub1_50
#$ -q INFORMATICS
#$ -wd /Users/ssrivastva/negbin/code/
#$ -m a
#$ -M sanvesh-srivastava@uiowa.edu
#$ -t 1-1000
#$ -V
#$ -e /Users/ssrivastva/err/
#$ -o /Users/ssrivastva/out/
#$ -j y

module load stack/2020.1
module load r-stanheaders/2.18.1-10_gcc-9.2.0
module load r-rstan/2.19.2_gcc-9.2.0

R CMD BATCH --no-save --no-restore "--args 4 $SGE_TASK_ID" submit.R samp/sub1_50_$SGE_TASK_ID.rout
