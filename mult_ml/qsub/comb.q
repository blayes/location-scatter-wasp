#!/bin/bash
#$ -N ml_comb
#$ -q INFORMATICS
#$ -pe smp 2
#$ -l h_rt=520:00:00
#$ -l s_rt=520:00:00
#$ -wd /Users/ssrivastva/mult_ml/code/
#$ -t 1-10
#$ -V
#$ -e /Users/ssrivastva/err/
#$ -o /Users/ssrivastva/out/
#$ -j y

module load R

R CMD BATCH --no-save --no-restore "--args 2 $SGE_TASK_ID" submit_part.R samp/comb_$SGE_TASK_ID.rout
