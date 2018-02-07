require(rstan)
require(gplots)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(100)

cca <- "
	data {
		int<lower=0> N; // Number of samples
		int<lower=0> D; // Sum of the original dimensions of every view
		int<lower=0> M; // The number of views
		int<lower=0> K; // The latent dimension
		int<lower = 0> Dm[M]; //The original dimension for each view

		matrix[N, D] X; // The data matrix
	}

	parameters {
		matrix[N, K] Z; // The latent matrix
		matrix[K, D] W; // The weight matrix. You will be learning *one* single W with structured sparsity - both view-specific components and shared components are captured by this
		vector<lower=0>[M] tau; // View-specific noise terms
		matrix<lower=0>[M,K] alpha; // View-specific ARD prior
	}

	transformed parameters{
		vector<lower=0>[M] t_tau;
		matrix<lower=0>[M,K] t_alpha; 
 		t_alpha = inv(sqrt(alpha));
		t_tau = inv(sqrt(tau));
	}

	model {
		// You will need to loop through 1:Dm[m] for each view m. Increment ind seperately to index the concatenated X and W.
		int ind;
		tau ~ gamma(1,1);			
		to_vector(Z) ~ normal(0,1); // because sampling K dimensional standard normal multivariate is equivalent to sampling from k univariate standard normal distributions.
		to_vector(alpha) ~ gamma(1e-3,1e-3); // stack columns of alpha to a vector and sample quickly.	
		ind = 0;
		// There is a more efficient way to do this with ragged arrays, but the effort spent is hugely disproportionate to the speed-ups obtained.
		for (m in 1:M) {	
    		for (d in 1:Dm[m]) {
        		ind = ind + 1;      
        		W[,ind] ~ normal(0.0, t_alpha[m,]);
        		X[,ind] ~ normal(Z*W[,ind], t_tau[m]);  
       		}
		}
    }"

# The artificial data is produced exactly as specified by the generative model. We give the ARD prior a certain structure that
# has active components over a some subset of views and run our model to find that it is able to recover the structure.

N <- 500
D <- c(15,7)             # Data dimensions
K <- 4 					
M <- 2			 # Because, CCA
Z <- matrix(0,N,K)       # Latent components
Z[,1] <- sin((1:N)/(N/20))
Z[,2] <- cos((1:N)/(N/20))
Z[,3] <- rnorm(N,0,1)
Z[,4] <- as.matrix(2*((1:N)/N-0.5),N,1)
tau <- c(3,6)             # Noise precisions
alpha <- matrix(0,M,4)    # Component precisions for the two data sets
alpha[1,] <- c(1,1,1e6,1) # 1   = active (the value determines the data scale)
alpha[2,] <- c(1,1,1,1e6) # 1e6 = inactive

# Create some random projection vectors and sample data from the model. 
Y <- vector("list",length=2)
W <- vector("list",length=2)
for(view in 1:2) {
  W[[view]] <- matrix(0,D[view],K)
  for(k in 1:K) {
    W[[view]][,k] <- rnorm(D[view],0,1/sqrt(alpha[view,k]))
  }
  Y[[view]] <- Z %*% t(W[[view]]) + matrix(rnorm(N*D[view],0,1/sqrt(tau[view])),N,D[view])
}
Z <- Z[1:N,]

data <- list(N = N, Dm = D, D=sum(D), M=M, K=K, X=do.call(cbind,Y))
m <- stan_model(model_code = cca)
stan.fit.vb <- vb(m, data = data, algorithm = "meanfield",tol_rel_obj =1e-3, iter = 5000, grad_samples=1)

Z.vb <- apply(extract(stan.fit.vb,"Z")[[1]], c(2,3), mean)
alpha.vb <- apply(extract(stan.fit.vb,"alpha")[[1]], c(2,3), mean)

# Plots 
heatmap.2(alpha, col = bluered(100), dendrogram='none',trace='none', Rowv = FALSE, Colv = FALSE, key=FALSE)
heatmap.2(log(alpha.vb), col = bluered(70), dendrogram='none',trace='none', Rowv = FALSE, Colv = FALSE, key=FALSE)

par(mfrow=c(4,1), oma=c(0,0,2,0))
for(i in 1:4) {
  plot(Z[,i],ylim=c(-2,2), xlab = paste0(i,"th component"), ylab = paste0("Z_",i));
}
par(mfrow=c(1,1));
title(main="True latent components",outer=TRUE)

par(mfrow=c(4,1), oma=c(0,0,1,0))
for(i in 1:4) {
  plot(Z.vb[,i],ylim=c(-2,2), xlab = paste0(i,"th component"), ylab = paste0("Z_",i));
}
par(mfrow=c(1,1));
title(main="Estimated latent components",outer=TRUE)


