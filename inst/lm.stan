data {
  int<lower=0> N;
  int<lower=0> N_new;
  vector[N] y;
  vector[N] x;
  vector[N_new] x_new;
}
parameters {
  vector[2] beta;
  real<lower=0> sigma;
}
model {
  // priors
  beta ~ normal(0, 3);
  sigma ~ student_t(3, 0, 2);
  
  y ~ normal(beta[1] + beta[2] * x, sigma);
}
generated quantities {
  vector[N_new] y_hat;
  for (i in 1:N_new)
    y_hat = beta[1] + beta[2] * x_new;
}
