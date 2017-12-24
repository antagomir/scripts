normal <- "
	data {		
		int<lower=0> n; 
		vector[n] y;
	}
	parameters {
		real mu;
		real<lower=0> sigma2;
	}
	transformed parameters {
		real<lower=0> sigma;
		sigma = sqrt(sigma2);	
	}
	model {
		mu ~ normal(10,5);
		sigma2 ~ inv_gamma(1,1);
		y ~ normal(mu, sigma);
	}"

mu <- 10
sigma <- 5
n <- 100
y <- rnorm(n, mu, sigma^2)
data <- list(y=y, n=n)
m <- stan_model(model_code = normal)
samples <- sampling(m, data=data, iter=1000, chains=1)
mu <- mean(extract(samples)$mu)
sigma2 <- mean(extract(samples)$sigma2)




normal <- "
	data {		
		int<lower=0> n; 
		vector[n] y;
	}
	parameters {
		real mu;
		real<lower=0> sigma;
	}
	model {
		mu ~ normal(0,1);
        sigma ~ lognormal(0,10);
		y ~ normal(mu, sigma);
	}"

mu <- 10
sigma <- 5
n <- 1000
y <- rnorm(n, mu, sigma)
data <- list(y=y, n=n)
m <- stan_model(model_code = normal)
samples <- sampling(m, data=data, iter=2000, chains=1)
mu <- mean(extract(samples)$mu)
sigma <- mean(extract(samples)$sigma)



normal <- "
	data {		
		int<lower=0> n; 
		vector[n] y;
	}
	parameters {
		real mu;
		real<lower=0> sigma;
	}
	model {
		mu ~ normal(0,1);
        sigma ~ cauchy(0,10);
		y ~ normal(mu, sigma);
	}"

mu <- 10
sigma <- 5
n <- 1000
y <- rnorm(n, mu, sigma)
data <- list(y=y, n=n)
m <- stan_model(model_code = normal)
samples <- sampling(m, data=data, iter=2000, chains=1)
mu <- mean(extract(samples)$mu)
sigma <- mean(extract(samples)$sigma)
