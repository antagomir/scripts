require(rstan)
poisson <- "
	data {
	  int<lower=1> N;
	  int<lower=0> y[N];
	}
	parameters {
	  real<lower=0> lambda;
	}
	model { 
	  lambda ~ gamma(1,1);
	  for (i in 1:N)
	    y[i] ~ poisson(lambda);
	}"


N <- 100
lambda <- 10
y <- rpois(N,lambda)
data <- list(y=y, N=N)
m <- stan_model(model_code = poisson)
samples <- sampling(m, data=data, iter=1000, chains=1)
lambda <- mean(extract(samples)$lambda)
