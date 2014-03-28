library(rstan)
 
# Create toy data
d <- data.frame(list(wine = rep(c("w1", "w2", "w3"), 100), 
     	             wine_color = sample(rep(c("red", "white"), 150)), 
		     quality = sample(rep(c("good", "bad"), 150)),
		     var1 = rnorm(300),
		     var2 = rnorm(300),
		     var3 = rnorm(300)
))


s.code <- "

data {
int<lower=1> K;
int<lower=0> N;
int<lower=0,upper=1> y[N];
matrix[N,K] X;
}

parameters {
vector[K] beta;
real intercept;
real<lower=0> sigma;
}

model {
sigma ~ cauchy(0, 10.);
beta ~ cauchy(0, sigma);
intercept ~ normal(0, 1000);
y ~ bernoulli_logit(X * beta + intercept);
}

"

# Pick X and Y 
get.y <- function(d) as.numeric(d$wine_color=="red")
get.X <- function(d) d[,-grep("^(wine|quality)", names(d))]
y.train <- get.y(d)
X.train <- get.X(d)
X.train <- data.frame(sapply(X.train, function (v) (v-mean(v))/sd(v)))

# Form Stan data list
sdat <- list(N = nrow(X.train), K=ncol(X.train), X = X.train, y = y.train)

# Fit the Stan model
fit <- stan(model_code = s.code, data = sdat, iter = 1000, chains = 1, thin=5)
samples <- data.frame(fit@sim$samples[[1]])
 
# Test training data performance on the fitted parameters using sample mean
logit <- function (x) 1/(1+exp(-x))
p <- apply(logit(as.matrix(X.train) %*% t(as.matrix(samples[,grep("^beta", names(samples))])) +
outer(rep(1, nrow(X.train)), samples$intercept)), 1, mean)

# Prediction accuracy?
sum((p>0.5)== y.train)/length(y.train)

