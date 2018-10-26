/*  Variable naming:
 obs       = observed
 cen       = (right) censored
 M         = number of predictor variables
 pred      = predictor variables
 N         = number of observations
*/

data {
  int<lower=0> Nobs;
  int<lower=0> Ncen;
  int<lower=0> M_pred;
  vector[Nobs] yobs;
  vector[Ncen] ycen;
  matrix[Nobs, M_pred] Xobs_pred;
  matrix[Ncen, M_pred] Xcen_pred;
}

parameters {
  vector[M_pred] beta;
  real alpha_raw;
  real mu;
}

transformed parameters {
  real alpha;
  alpha = exp(10.0 * alpha_raw);
}

model {
  yobs ~ weibull(alpha, exp(-(mu + Xobs_pred * beta)/alpha));
  target += weibull_lccdf(ycen | alpha, exp(-(mu + Xcen_pred * beta)/alpha));

  beta ~ normal(0.0, 10.0);
  alpha_raw ~ normal(0.0, 1.0);

  mu ~ normal(0.0, 10.0);
}

