# https://www.cs.helsinki.fi/u/sakaya/tutorial/

# Gaussian with unknown mean
gaussian <- "
	data {
	    int<lower=1> n;
	    vector[n] x;
	}
	parameters {
    	    real mu;
            real<lower=0> sigma;
	}
	model {
	    mu ~ normal(0, 10);
	    x ~ normal(mu, sigma);
   	} "

library("rstan")
n <- 1000
x <- rnorm(n, 5, 10)
data <- list(x=x, n=n)
m <- stan_model(model_code = gaussian)
samples <- sampling(m, data=data, iter=10000, chains=1)
mu <- mean(extract(samples)$mu)
sigma <- mean(extract(samples)$sigma)

