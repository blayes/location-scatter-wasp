* Organization
  - There are three directories, two for the simulations (logistic and negbin) and the third for the real data analysis (mult_ml). 
  - Each directory has two sub directories: code and qsub.
  - Directory 'code' has files of all the code (Stan and R source code) that were used in the experiments. 
  - Directory 'qsub' has SGE files (.q) that were used to submit jobs on a SGE cluster. 

* Brief Description of files
  - 'simulate_data.R' contains the code to simulate/partition the data.
  - 'analyze_result.R' contains the code for analyzing the results of MCMC, Mposterior, and competing methods and making plots/tables.
  - 'submit.R' contains the  code for the R code for submitting a job on the cluster. The files in 'qsub' directory use this file for running simulations.

* Citation:
  If you use the code, then please cite the following paper:
  - ND Shyamalkumar and S Srivastava (2020). An Algorithm for Distributed Bayesian Inference in Generalized Linear Models. 
   
* Contact:
  Please email Sanvesh Srivastava (<sanvesh-srivastava@uiowa.edu>) if you have any questions related to the code.

* Acknowledgment
  - The setup of real data analysis borrow from Perry (2017). 
    Perry, P. O. (2017). Fast moment-based estimation for hierarchical models. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 79(1), 267–291
  - Please email us if you think that we have missed citations to your paper/work. 
  



