normal <- "
	data {		
		int<lower=0> n; 
		vector[n] y;
	}
	parameters {
		real mu;
	}
	model {
		mu ~ normal(0,1);
		y ~ normal(mu,3);
	}"

mu <- 10
n <- 100
y <- rnorm(N, 10, 3)
data <- list(y=y, n=n)
m <- stan_model(model_code = normal)
samples <- sampling(m, data=data, iter=1000, chains=1)
mu <- mean(extract(samples)$mu)
