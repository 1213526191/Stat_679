// hw01.stan
data
{
  int<lower=1> N; // number of individuals
  int<lower=1> J; // number of groups
  real y[N]; // response variable
  real x[N]; // explanatory variable
  int<lower=1,upper=J> group[N]; // group index
}

parameters
{
  vector[2] beta; // intercept and slope
  vector[J] alpha; // group intercept effects
  real<lower=0> sigma; // individual standard deviation
  real<lower=0> sigmaA; // group standard deviation
}

transformed parameters
{
  vector[N] mu;
  for ( i in 1:N )
  {
    mu[i] = beta[1] + alpha[group[i]] + beta[2]*x[i];
  }
}

model
{
  // prior distributions for parameters
  beta[1] ~ cauchy(0,5);
  beta[2] ~ cauchy(0,5);
  sigma ~ cauchy(0,5);
  sigmaA ~ cauchy(0,5);
  // likelihood model
  alpha ~ normal(0,sigmaA);
  y ~ normal(mu,sigma);  
}
