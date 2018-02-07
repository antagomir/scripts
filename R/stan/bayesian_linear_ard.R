require(rstan)
bayesian_linear_ard <- "
	data {
		int<lower=0> N;
		int<lower=0> D;
		matrix[N,D] X;
		vector[N] y;
	}
	parameters {
		vector[D] w;
		vector<lower=0>[D] alpha;
		real<lower=0> tau;
	}
	transformed parameters {
		vector<lower=0>[D] t_alpha;
		real<lower=0> t_tau;
		for (d in 1:D) t_alpha[d] = 1/sqrt(alpha[d]);
		t_tau =  1/sqrt(tau);
	}
	model {
		tau ~ gamma(1, 1);
		alpha ~ gamma(1e-3,1e-3);
		w ~ normal(0,  t_alpha);
		y ~ normal(X*w, t_tau);
	}"


tau <- 1
N <- 1000
D <- 10
alpha <- rep(1,D)
alpha[1:5] <- 1e6
w <- sapply(1/sqrt(alpha), function(a) rnorm(1,sd=a))
X <- matrix(rnorm(N*D), N, D)
y <- c(X %*% w + rnorm(N))
data <- list(N=N, D=D, X=X, y=y)

m <- stan_model(model_code = bayesian_linear_ard)
samples <- sampling(m, data=data, iter=2000, chains=1)
w <- colMeans(extract(samples)$w)
alpha <- colMeans(extract(samples)$alpha)
tau <- mean(extract(samples)$tau)

