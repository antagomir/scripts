require(rstan)
bayesian_linear <- "
	data {
		int<lower=0> N;
		int<lower=0> D;
		matrix[N,D] X;
		vector[N] y;
	}
	parameters {
		vector[D] w;
		real<lower=0> tau;
	}
	transformed parameters {
		real<lower=0> t_tau;
		t_tau =  1/sqrt(tau);
	}
	model {
		tau ~ gamma(1, 1);
		w ~ normal(0, t_tau);
		y ~ normal(X*w, t_tau);
	}"

tau <- 1
N <- 1000
D <- 10
w <- rnorm(D, sd = tau)
X <- matrix(rnorm(N*D), N, D)
y <- c(X %*% w + rnorm(N))
data <- list(N=N, D=D, X=X, y=y)

m <- stan_model(model_code = bayesian_linear)
samples <- sampling(m, data=data, iter=2000, chains=1)
w <- colMeans(extract(samples)$w)
tau <- mean(extract(samples)$tau)

