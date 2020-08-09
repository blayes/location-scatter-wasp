data {
  int<lower=0> n;
  int<lower=0> p;
  matrix[n, p] x;
  int<lower=0> y[n];
}

parameters {
  vector[p] beta;
  real<lower=0> phi;
}

model {
  beta ~ normal(0, 5);
  phi ~ cauchy(0, 5);

  for (ii in 1:n) {
    y[ii] ~ neg_binomial_2_log(x[ii] * beta, phi);
  }  
}


