require(rstan)
require(gplots)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(100)
pca <- "
	data {
		int<lower=0> N; 
		int<lower=0> D; 
		int<lower=0> K;
		int<lower=0> L;
		real Y[N,D,L];
	}

	parameters {
		matrix[N, K] X;
		matrix[L, K] U;
		matrix[D, K] W;
		real<lower=0> tau;
		vector<lower=0>[K] alpha;
	}

	transformed parameters{
		vector<lower=0>[K] t_alpha;
		real<lower=0> t_tau;
		t_alpha = inv(sqrt(alpha));
		t_tau = inv(sqrt(tau));
	}
	model {
		real tmp;

		tau ~ gamma(1,1);
		alpha ~ gamma(1e-3,1e-3);	
		to_vector(X) ~ normal(0,1);			
		to_vector(U) ~ normal(0,1);			
		for(k in 1:K) W[,k] ~ normal(0, t_alpha[k]);

		//Terrible idea to introduce so many for loops, but let's keep it easy.
		for (n in 1:N) {
			for (d in 1:D) {
				for (l in 1:L) {
					tmp = 0;
					for (k in 1:K)
						tmp = tmp + X[n,k] * W[d,k] * U[l,k];			
					Y[n,d,l] ~ normal(tmp, t_tau);
				}
			}
		}
	}"


N <- 50
D <- 20
L <- 10
K <- 5

X <- matrix(rnorm(N*K,0,1),N,K)
U <- matrix(rnorm(N*L,0,1),L,K)
tau <- 3
alpha <- rep(1,K) 
W <- matrix(0,D,K)
for(k in 1:K)  W[,k] <- rnorm(D,0,1/sqrt(alpha[k]))

Y <- Reduce('+', lapply(1:K, function(k) (X[,k] %o% W[,k]) %o% U[,k] )) + array(rnorm(N*D*L,0,1/sqrt(tau)),c(N,D,L))
data <- list(N = N, D = D, L = L, K = 10, Y = Y)

m <- stan_model(model_code = pca)
stan.fit.vb <- vb(m, data = data, algorithm = "meanfield")
W.vb <- apply(extract(stan.fit.vb,"W")[[1]], c(2,3), mean)
X.vb <- apply(extract(stan.fit.vb,"X")[[1]], c(2,3), mean)
U.vb <- apply(extract(stan.fit.vb,"U")[[1]], c(2,3), mean)
alpha.vb <- apply(extract(stan.fit.vb,"alpha")[[1]], c(2), mean)

heatmap.2(W.vb, col = bluered(70), dendrogram='none', trace='none', Rowv = FALSE, Colv = FALSE, key=FALSE, lwid=c(0.1,4), lhei=c(0.1,4))
heatmap.2(X.vb, col = bluered(70), dendrogram='none', trace='none', Rowv = FALSE, Colv = FALSE, key=FALSE, lwid=c(0.1,4), lhei=c(0.1,4))
heatmap.2(U.vb, col = bluered(70), dendrogram='none', trace='none', Rowv = FALSE, Colv = FALSE, key=FALSE, lwid=c(0.1,4), lhei=c(0.1,4))
