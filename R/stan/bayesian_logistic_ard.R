require(rstan)
require(boot)
set.seed(100)
bayesian_logistic_ard <- "
	data {
		int<lower=0> N;
		int<lower=0> D;
		matrix[N,D] X;
		int<lower=0,upper=1> y[N];
	}
	parameters {
		vector[D] w;
		vector<lower=0>[D] alpha;
	}
	transformed parameters {
		vector<lower=0>[D] t_alpha;
		for (d in 1:D) t_alpha[d] = 1/sqrt(alpha[d]);
	}
	model {
		alpha ~ gamma(1e-3,1e-3);
		w ~ normal(0, t_alpha);
		y ~ bernoulli_logit(X*w);
	}"


tau <- 1
N <- 1000
D <- 10
alpha <- rep(1,D)
alpha[1:5] <- 1e6
w <- sapply(1/sqrt(alpha), function(a) rnorm(1,sd=a))
X <- matrix(rnorm(N*D), N, D)
y <- rbinom(N,1, inv.logit(c(X %*% w + rnorm(N))))
data <- list(N=N, D=D, X=X, y=y)

m <- stan_model(model_code = bayesian_logistic_ard)
#samples <- sampling(m, data=data, iter=2000, chains=1)
samples <- vb(m, data = data, algorithm = "meanfield")
w <- colMeans(extract(samples)$w)
alpha <- colMeans(extract(samples)$alpha)


