library(rstan)
library(gplots)
set.seed(100)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(100)

gfa <- " 
	data {
	  int<lower=0> N;
	  int<lower=0> D;
	  int<lower=0> M;
	  int<lower=0> K;
	  
	  int<lower=0> Dm[M]; 
	  matrix[N, D] X;
	}

	parameters {
	 matrix[N, K] Z;
	 matrix[K, D] W; // you will be learning *one* single W with structured sparsity - both view-specific components and shared components are captured by this
	 vector<lower=0>[M] tau;
	 matrix<lower=0>[M, K] alpha;
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
	} 
	"

# The artificial data is produced exactly as specified by the generative model. We give the ARD prior a certain structure that
# has active components over a some subset of views and run our model to find that it is able to recover the structure.

N <- 100
K <- 6
M <- 15 # Number of views
D <- rep(10,M)
Z <- matrix(rnorm(N*K,0,1),N,K)    # Latent components
tau <- rgamma(M,1,1)
alpha <- matrix(1,M,K)    # Component precisions for the two data sets
alpha[c(1:5,11:15),1] <- 1e6
alpha[6:15,2] <- 1e6
alpha[1:11,3] <- 1e6
alpha[11:15,4] <- 1e6
alpha[1:5,5] <- 1e6
alpha[6:10,6] <- 1e6

# Create some random projection vectors and sample data from the model. 
Y <- vector("list",length=M)
Ytest <- vector("list",length=M)
W <- vector("list",length=M)
for(view in 1:M) {
  W[[view]] <- matrix(0,D[view],K)
  for(k in 1:K) {
    W[[view]][,k] <- rnorm(D[view],0,1/sqrt(alpha[view,k]))
  }
  Y[[view]] <- Z %*% t(W[[view]]) + matrix(rnorm(N*D[view],0,1/sqrt(tau[view])),N,D[view])   
}

data <- list(N = N, Dm = D, D=sum(D), M=M, K=K, X=do.call(cbind,Y))

m <- stan_model(model_code = gfa)
stan.fit <- vb(m, data = data, algorithm = "meanfield",tol_rel_obj = 1e-3)
alpha.vb <- apply(extract(stan.fit,"alpha")[[1]], c(2,3), mean)

lmat = rbind(c(0,3),c(2,1),c(0,4))
lwid = c(0.1,4)
lhei = c(0.5,4,1)
heatmap.2(log(alpha), col = bluered(70), dendrogram='none',trace='none', Rowv = FALSE, Colv = FALSE, lmat = lmat, lwid = lwid, lhei = lhei, cexCol = 2, cexRow = 2)
heatmap.2(log(alpha.vb), col = bluered(70), dendrogram='none',trace='none', Rowv = FALSE, Colv = FALSE,lmat = lmat, lwid = lwid, lhei = lhei, cexCol = 2, cexRow = 2)
