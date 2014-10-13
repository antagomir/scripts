# http://quant.stackexchange.com/questions/1260/r-code-for-ornstein-uhlenbeck-process -> Euler-Maruyama method
# http://en.wikipedia.org/wiki/Euler%E2%80%93Maruyama_method
# nu is long run mean, lambda is mean reversion speed
ornstein_uhlenbeck <- function(T,n,nu,lambda,sigma,x0){

  dw  <- rnorm(n, 0, sqrt(T/n))
  dt  <- T/n
  x <- c(x0)
  for (i in 2:(n+1)) {
    x[i]  <-  x[i-1] + lambda*(nu-x[i-1])*dt + sigma*dw[i-1]
  }
  return(x);
}


# Simulating OU process?
# nu is long run mean, lambda is mean reversion speed
T <- seq(0, 1e4, 1)
n <- 1e4
nu <- 20
lambda <- 1000
sigma <- 1
x0 <- 10
ou <- ornstein_uhlenbeck(T,n,nu,lambda,sigma,x0)
plot(ou, pch = ".")

