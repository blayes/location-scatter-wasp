functions {
  real stoc_cat_logit_lpmf (int y, vector beta, real nrep) {
    return (nrep * categorical_logit_lpmf(y | beta));
  }
}

data {
  int K;
  int N;
  int D;
  int y[N];
  matrix[N, D] x;
  real nrep;
}

parameters {
  matrix[K - 1, D] beta_raw;
}

transformed parameters {
  matrix[K, D] beta;
  beta = append_row(beta_raw, rep_row_vector(0, D));
}

model {
  matrix[N, K] x_beta = x * beta';

  for (n in 1:N)
    y[n] ~ stoc_cat_logit(x_beta[n]', nrep);
}
