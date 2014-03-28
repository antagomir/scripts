density.invgam <- function (x, alpha, beta) {

  # Density of inverse gamma function
  # (double as quick as MCMCpack dinvgam)
  (beta^alpha / gamma(alpha))*x^(-alpha-1)*exp(-beta/x)

}

