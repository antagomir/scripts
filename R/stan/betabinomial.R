require(rstan)
binomial <- "
	data {
	  int<lower=1> N;
	  int<lower=1> s;
	  int<lower=0> y[N];
	}
	parameters {
	  real<lower=0, upper=1> theta;
	}
	model { 
	  theta ~ beta(1,1);
	  for (i in 1:N)
	    y[i] ~ binomial(s,theta);
	}"


N <- 100
s <- 10 #number of trials
theta <- .4
y <- rbinom(N, size, theta)
data <- list(y=y, N=N, s = s)
m <- stan_model(model_code = binomial)
samples <- sampling(m, data=data, iter=1000, chains=1)
theta <- mean(extract(samples)$theta)
