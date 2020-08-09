#!/bin/bash
#$ -N comb20
#$ -q INFORMATICS
#$ -pe smp 2
#$ -l h_rt=520:00:00
#$ -l s_rt=520:00:00
#$ -wd /Users/ssrivastva/negbin/code/
#$ -t 1-20
#$ -V
#$ -e /Users/ssrivastva/err/
#$ -o /Users/ssrivastva/out/
#$ -j y

module load R

R CMD BATCH --no-save --no-restore "--args 7 $SGE_TASK_ID" submit.R samp/comb20_$SGE_TASK_ID.rout
