# Leo 21.8.2007
# Computes inverse gamma function density P(x|a,b)
# with parameters a,b. See Gelman et al, for example.



gam <- function (x,a,b) {(b^a)*(x^(a-1))*exp(-b*x)/gamma(a)}
invgam <- function (x,a,b) {(b^a)*(x^(-(a+1)))*exp(-b/x)/gamma(a)}
