data {
  int K;
  int N;
  int D;
  int y[N];
  matrix[N, D] x;
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
    y[n] ~ categorical_logit(x_beta[n]');
}
