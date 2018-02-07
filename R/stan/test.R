require(rstan)
bernoulli <- "
	data { 
	} 
	parameters {
	} 
	model {
	}"

N <- 100
y <- rbinom(N, 1, .4)
data <- list(y=y, N=N)
m <- stan_model(model_code = bernoulli)
samples <- sampling(m, data=data, iter=1000, chains=1)
mu <- mean(extract(samples)$theta)
