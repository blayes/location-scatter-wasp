functions {
  real stoc_negbin_lpmf (int y, real eta, real phi, real nrep) {
    return (nrep * neg_binomial_2_log_lpmf(y | eta, phi));
  }
}

data {
  int<lower=0> n;
  int<lower=0> p;
  matrix[n, p] x;
  int<lower=0> y[n];
  real nrep;
}

parameters {
  vector[p] beta;
  real<lower=0> phi;
}

model {
  beta ~ normal(0, 5);
  phi ~ cauchy(0, 5);

  for (ii in 1:n) {
    y[ii] ~ stoc_negbin(x[ii] * beta, phi, nrep);
  }  

}


