require(rstan)
require(gplots)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(100)
pca <- "
	data {
		int<lower=0> N; // Number of samples
		int<lower=0> D; // The original dimension
		int<lower=0> K; // The latent dimension
		matrix[N, D] X; // The data matrix
	}

	parameters {
		matrix[N, K] Z; // The latent matrix
		matrix[D, K] W; // The weight matrix
		real<lower=0> tau; // Noise term 
		vector<lower=0>[K] alpha; // ARD prior
	}

	transformed parameters{
		vector<lower=0>[K] t_alpha;
		real<lower=0> t_tau;
                t_alpha = inv(sqrt(alpha));
                t_tau = inv(sqrt(tau));
	}
	model {
		tau ~ gamma(1,1);			
		to_vector(Z) ~ normal(0,1);
		alpha ~ gamma(1e-3,1e-3);				
		for(k in 1:K) W[,k] ~ normal(0, t_alpha[k]);
		to_vector(X) ~ normal(to_vector(Z*W'), t_tau);

	} "


N <- 200
D <- 20
K <- 5
Z <- matrix(rnorm(N*K,0,1),N,K)    # Latent components
tau <- 3
alpha <- rep(1,K)    # Component precisions for the two data sets

W <- matrix(0,D,K)   # The weights
for(k in 1:K)  W[,k] <- rnorm(D,0,1/sqrt(alpha[k]))
X <- Z %*% t(W) + matrix(rnorm(N*D,0,1/sqrt(tau)),N,D)   
data <- list(N = N, D = D, K = 10, X = X)

m <- stan_model(model_code = pca)
stan.fit.vb <- vb(m, data = data, algorithm = "meanfield")
W.vb <- apply(extract(stan.fit.vb,"W")[[1]], c(2,3), mean)
alpha.vb <- apply(extract(stan.fit.vb,"alpha")[[1]], c(2), mean)

heatmap.2(W.vb, col = bluered(70), dendrogram='none',trace='none', Rowv = FALSE, Colv = FALSE, key=FALSE)

#stan.fit.sampling <- sampling(m, data = data, chains=1, iter=1000); 
#W.sampling <- t(apply(extract(stan.fit.sampling,"W")[[1]], c(2,3), mean))
#alpha.sampling <- apply(extract(stan.fit.sampling,"alpha")[[1]], c(2), mean)
#heatmap.2(W.sampling, col = bluered(70), dendrogram='none',trace='none', Rowv = FALSE, Colv = FALSE, key=FALSE)

load("UML.RData")
X <- GeneExpression.HL60
N <- dim(X)[1]
D <- dim(X)[2]
K <- 5
data <- list(N = N, D = D, K = K, X = X)

m <- stan_model(model_code = pca)
stan.fit.vb.real <- vb(m, data = data, algorithm = "meanfield", iter = 5000)
alpha.vb.real <- apply(extract(stan.fit.vb.real,"alpha")[[1]], c(2), mean)
Z.vb <- apply(extract(stan.fit.vb.real,"Z")[[1]], c(2,3), mean)
W.vb <- apply(extract(stan.fit.vb.real,"W")[[1]], c(2,3), mean)
