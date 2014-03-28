# R functions for processing lm, glm, and lmer output

# Update 13 June 2006:  added Jennifer Hill's matching() function
# Update 30 May 2006:
#   1.  removed beta.hat() and beta.se()
#   2.  fixed bug in sigma.hat()
#   3.  se.coef() now works for lmer fits
# Update 19 Apr 2006:  mcsamp() altered to work with updated as.bugs.array()
#   function.  as.bugs.array() converts a 3-way array of simulations into a
#   bugs object that can be conveniently displayed using print() and plot().
#   as.bugs.array() currently lives in the file bugs.R and it will soon be
#   part of the R2WinBUGS package.
# Update 21 Mar 2006:  se.coef, se.fixef, and se.ranef functions added.
#   We're trying to switch over to more standard notation (following the
#   suggestions of Martyn Plummer and Bob O'Hara)
# Update 28 Jan 2006:  now (mostly) works with Matrix version 0.995-4
# Update 22 Jan 2006:
#   1.  fixed weird R "paperwork" thing:
#       for some reason, you can't call "vcov" anymore on an lmer object, so
#       I changed calls to vcov to Matrix:::vcov
#   2.  took mcsamp() from the bugs.R file and put it here
#   3.  mcmcsamp (part of lmer in the Matrix package) currently doesn't work
#       with saveb=TRUE but I'm assured this is being fixed.  The function
#       mcsamp will probably need to be debugged when the new mcmcsamp debuts.
# Update 19 Jan 2006:  functions altered to work with Matrix version 0.995-1
# Update 15 Jan 2006:  rwish() copied from MCMCpack
# Update 7 Dec 2006:  fixed bugs in sim()
# Update 6 Dec 2006:  adapted sim() to get simulations of coefs from lmer objects
# Update 2 Dec 2006:  now (mostly) works with lmer
# Update 26 Sep 2005:  added sigma.hat
# Update 5 Sep 2005:  added beta.hat and beta.se
# Update 3 Sep 2005:  lm.all and glm.all removed
# Update 20 Apr 2005:  Jouni's patch to mvrnorm
# Update 8 Feb 2005:  lm.all and glm.all updated
# Update 2 Jan 2004:  new combination functions lm.all and glm.all
# Update 27 Nov 2002:  display dispersion parameter (if any)
# Written 17 Sept 2002 by Andrew Gelman, using the mvrnorm function from
# the MASS library in R

# This file has 3 parts:
#
# (1) "display" and "sim":  R functions for lm and glm output
# (2) Some useful little R functions
# (3) beta.hat, beta.se, sigma.hat:  functions for pulling out estimates and se's

display <- function (object, digits=2){

# display:  display the results of a lm, glm, or lmer object
#
# See also the function sim()
#
# Arguments:
#
#     object:  the output of a call to "lm" or "glm"
#              with n data points and k predictors
#     digits:  number of significant digits to display.  (Note:  R-squared
#              is automatically displayed to 2 digits, and deviances are
#              automatically displayed to 1 digit, no matter what.)
#
# Output are the model, the regression coefficients and standard errors,
# and the residual sd and R-squared (for a linear model),
# or the null deviance and residual deviance (for a generalized linear model).
#
# Example of "display" and "sim":
#
# Linear regression of weights of mesquite bushes
#
#   fit1 <- lm (weight ~ diam1 + diam2 + canopy.height + total.height)
#   display (fit1)
#   fit1.sims <- sim (fit1)
#
# Logistic regression of whether the weight exceeds 500 grams
#
#   threshold <- ifelse (weight>500, 1, 0)
#   fit2 <- glm (threshold ~ diam1 + diam2 + canopy.height + total.height, family=binomial(link="logit"))
#   display (fit2)
#   fit2.sims <- sim (fit2)
#
# Using lmer:

# Here's a simple example of a model of the form, y = a + bx + error, with 10 observations in each of 10 groups, and with both the intercept and the slope varying by group.  First we set up the model and data.
#   
#   group <- rep(1:10, rep(10,10))
#   mu.a <- 0
#   sigma.a <- 2
#   mu.b <- 3
#   sigma.b <- 4
#   rho <- 0
#   Sigma.ab <- array (c(sigma.a^2, rho*sigma.a*sigma.b, rho*sigma.a*sigma.b, sigma.b^2), c(2,2))
#   sigma.y <- 1
#   ab <- mvrnorm (10, c(mu.a,mu.b), Sigma.ab)
#   a <- ab[,1]
#   b <- ab[,2]
#   
#   x <- rnorm (100)
#   y <- rnorm (100, a[group] + b[group]*x, sigma.y)
# 
# Then fit and display a simple varying-intercept model:
# 
#   M1 <- lmer (y ~ x + (1|group))
#   display (M1)
#   M1.sim <- mcsamp (M1)
#   print (M1.sim)
#   plot (M1.sim)
# 
# Then the full varying-intercept, varying-slope model:
# 
#   M2 <- lmer (y ~ x + (1 + x |group))
#   display (M2)
#   M2.sim <- mcsamp (M2)
#   print (M2.sim)
#   plot (M2.sim)

  object.class <- class(object)[[1]]
  if (object.class=="lm" | object.class=="glm"){
    call <- object$call
    summ <- summary (object)
    coef <- summ$coef[,1:2,drop=FALSE]
    dimnames(coef)[[2]] <- c("coef.est","coef.se")
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    print (call)
    pfround (coef, digits)
    if (object.class=="lm")
      cat (paste ("  n = ", n, ", k = ", k,
        "\n  residual sd = ", fround (summ$sigma, digits),
        ", R-Squared = ", fround (summ$r.squared, 2), "\n", sep=""))
    else if (object.class=="glm"){
      cat (paste ("  n = ", n, ", k = ", k,
        "\n  residual deviance = ", fround (summ$deviance, 1),
        ", null deviance = ", fround (summ$null.deviance, 1),
        " (difference = ", fround (summ$null.deviance - summ$deviance, 1), ")",
        "\n", sep=""))
      if (summ$dispersion!=1) 
        cat (paste ("  overdispersion parameter = ",
          fround (summ$dispersion, 1), "\n", sep=""))
    }
  }
  else if (object.class=="lmer"){
    object <- summary (object)
    call <- object@call
    print (call)
    fcoef <- .Call("mer_fixef", object, PACKAGE = "Matrix")
    useScale <- object@useScale
    corF <- Matrix:::vcov(object)@factors$correlation
    coefs <- cbind(fcoef, corF@sd)
    if (length (fcoef) > 0){
      dimnames(coefs) <- list(names(fcoef), c("coef.est", "coef.se"))
      pfround (coefs, digits)
    }
    cat("Error terms:\n")
    vc <- as.matrix.VarCorr (VarCorr (object, useScale=useScale),useScale=useScale,digits)
    print (vc[,c(1:2,4:ncol(vc))], quote=FALSE)
    ngrps <- lapply(object@flist, function(x) length(levels(x)))
    dev <- object@deviance
    devc <- object@devComp
    cat(sprintf("# of obs: %d, groups: ", devc[1]))
    cat(paste(paste(names(ngrps), ngrps, sep = ", "), collapse = "; "))
    cat("\ndeviance =", fround (dev[1], 1), "\n")
    if (!useScale){
      cat("  overdispersion parameter =", fround (.Call("mer_sigma", 
        object, FALSE, PACKAGE = "Matrix"), 1), "\n")}
  }
}

sim <- function (object, n.sims=100){

# sim:  get posterior simulations of sigma and beta from a lm object, or
#       simulations of beta from a glm object, or
#       simulations of beta from a lmer object
#
# See also the functions display() and glm.sim()
#
# Arguments:
#
#     object:  the output of a call to "lm"
#              with n data points and k predictors
#     n.sims:  number of independent simulation draws to create
#
# Output is a list (sigma.sim, beta.sim):
#
#     sigma.sim:  vector of n.sims random draws of sigma
#       (for glm's, this just returns a vector of 1's or else of the
#       square root of the overdispersion parameter if that is in the model)
#     beta.sim:  matrix (dimensions n.sims x k) of n.sims random draws of beta
#
# Example of "display" and "sim":
#
# Linear regression of weights of mesquite bushes
#
#   fit1 <- lm (weight ~ diam1 + diam2 + canopy.height + total.height)
#   display (fit1)
#   fit1.sims <- sim (fit1)
#
# Logistic regression of whether the weight exceeds 500 grams
#
#   threshold <- ifelse (weight>500, 1, 0)
#   fit2 <- glm (threshold ~ diam1 + diam2 + canopy.height + total.height, family=binomial(link="logit"))
#   display (fit2)
#   fit2.sims <- sim (fit2)

  object.class <- class(object)[[1]]
  if (object.class=="lm"){
    summ <- summary (object)
    coef <- summ$coef[,1:2,drop=FALSE]
    dimnames(coef)[[2]] <- c("coef.est","coef.sd")
    sigma.hat <- summ$sigma
    beta.hat <- coef[,1]
    V.beta <- summ$cov.unscaled
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    sigma <- rep (NA, n.sims)
    beta <- array (NA, c(n.sims,k))
    dimnames(beta) <- list (NULL, names(beta.hat))
    for (s in 1:n.sims){
      sigma[s] <- sigma.hat*sqrt((n-k)/rchisq(1,n-k))
      beta[s,] <- mvrnorm (1, beta.hat, V.beta*sigma[s]^2)
    }
    return (list (beta=beta, sigma=sigma))
  }
  else if (object.class=="glm"){
    summ <- summary (object, correlation=TRUE)
    coef <- summ$coef[,1:2,drop=FALSE]
    dimnames(coef)[[2]] <- c("coef.est","coef.sd")
    beta.hat <- coef[,1]
    sd.beta <- coef[,2]
    corr.beta <- summ$corr
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    V.beta <- corr.beta * array(sd.beta,c(k,k)) * t(array(sd.beta,c(k,k)))
    beta <- array (NA, c(n.sims,k))
    dimnames(beta) <- list (NULL, names(beta.hat))
    for (s in 1:n.sims){
      beta[s,] <- mvrnorm (1, beta.hat, V.beta)
    }
    sigma <- rep (sqrt(summ$dispersion), n.sims)
    return (beta=beta, sigma=sigma)
  }
  else if (object.class=="lmer"){
    object <- summary (object)
    useScale <- object@useScale
# simulate unmodeled coefficients
    fcoef <- .Call("mer_fixef", object, PACKAGE = "Matrix")
    corF <- Matrix:::vcov(object)@factors$correlation
    se.unmodeled <- corF@sd
    V.beta <- (se.unmodeled %o% se.unmodeled) * as.matrix(corF)
    beta.unmodeled <- NULL
    if (length (fcoef) > 0){
      beta.unmodeled[[1]] <- mvrnorm (n.sims, fcoef, V.beta)
      names (beta.unmodeled) <- "unmodeled"
    }
# simulate coefficients within groups
    coef <- ranef (object)
    sc <- attr (VarCorr (object, useScale=useScale), "sc")
    vars <- object@bVar
    beta.bygroup <- vars
    n.groupings <- length (vars)
    for (m in 1:n.groupings){
      vars.m <- vars[[m]]
      K <- dim(vars.m)[1]
      J <- dim(vars.m)[3]
      beta.bygroup[[m]] <- array (NA, c(n.sims, J, K))
      bhat <- ranef(object)[[m]]
      for (j in 1:J){
        V.beta <- untriangle(vars.m[,,j])*sc^2
        beta.bygroup[[m]][,j,] <- mvrnorm (n.sims, bhat[j,], V.beta)
      }   
      dimnames (beta.bygroup[[m]]) <- c (list(NULL), dimnames(bhat))
    }
    betas <- c (beta.unmodeled, beta.bygroup)
  }
  return (betas)
}

# some useful little functions

  sd.scalar <- function (x, ...) {sqrt(var(as.vector(x), ...))}
  wmean <- function (x, w, ...) {mean(x*w, ...)/mean(w, ...)}
  logit <- function (x) {log(x/(1-x))}
  invlogit <- function (x) {1/(1+exp(-x))}
  fround <- function (x, digits) {format (round (x, digits), nsmall=digits)}
  pfround <- function (x, digits) {print (fround (x, digits), quote=FALSE)}
  untriangle <- function (x) {x + t(x) - x*diag(nrow(as.matrix(x)))}

# new functions!

se.coef <- function (object){
  object.class <- class(object)[[1]]
  if (object.class=="lm" | object.class=="glm"){
    sqrt (diag(vcov(object)))
  }
  else if (object.class=="lmer"){
    object <- summary (object)
    fcoef <- .Call("mer_fixef", object, PACKAGE = "Matrix")
    useScale <- object@useScale
    corF <- Matrix:::vcov(object)@factors$correlation
    se.unmodeled <- NULL
    se.unmodeled[[1]] <- corF@sd
    names (se.unmodeled) <- "unmodeled"

    sc <- attr (VarCorr (object, useScale=useScale), "sc")
    vars <- object@bVar
    se.bygroup <- vars
    n.groupings <- length (vars)
    for (m in 1:n.groupings){
      vars.m <- vars[[m]]
      K <- dim(vars.m)[1]
      J <- dim(vars.m)[3]
      se.bygroup[[m]] <- array (NA, c(J,K))
      for (j in 1:J){
        se.bygroup[[m]][j,] <- sqrt(diag(as.matrix(vars.m[,,j])))
      }
      se.bygroup[[m]] <- se.bygroup[[m]]*sc
      names.full <- dimnames (ranef(object)[[m]])
      dimnames (se.bygroup[[m]]) <- list (names.full[[1]],
        names.full[[2]])
    }
    ses <- c (se.unmodeled, se.bygroup)
    return (ses)
  }
}


se.fixef <- function (object){
  object <- summary (object)
  fcoef <- .Call("mer_fixef", object, PACKAGE = "Matrix")
  useScale <- object@useScale
  corF <- Matrix:::vcov(object)@factors$correlation
  return (corF@sd)
}

se.ranef <- function (object){
  object <- summary (object)
  useScale <- object@useScale
  sc <- attr (VarCorr (object, useScale=useScale), "sc")
  vars <- object@bVar
  se.bygroup <- vars
  n.groupings <- length (vars)
  for (m in 1:n.groupings){
    vars.m <- vars[[m]]
    K <- dim(vars.m)[1]
    J <- dim(vars.m)[3]
    se.bygroup[[m]] <- array (NA, c(J,K))
    for (j in 1:J){
      se.bygroup[[m]][j,] <- sqrt(diag(as.matrix(vars.m[,,j])))
    }
    se.bygroup[[m]] <- se.bygroup[[m]]*sc
    names.full <- dimnames (ranef(object)[[m]])
    dimnames (se.bygroup[[m]]) <- list (names.full[[1]],
      names.full[[2]])
  }
  return (se.bygroup)
}

# sigma.hat

sigma.hat <- function (object){
  object.class <- class(object)[[1]]
  if (object.class=="lm"){
    sigma <- summary(object)$sigma
    return (sigma)
  }
  else if (object.class=="glm"){
    sigma <- summary(object, correlation=TRUE)$sigma
    return (sigma)
  }
  else if (object.class=="lmer"){
    object <- summary (object)
    fcoef <- .Call("mer_fixef", object, PACKAGE = "Matrix")
    useScale <- object@useScale
    ngrps <- lapply(object@flist, function(x) length(levels(x)))
    n.groupings <- length (ngrps)
    varc <- VarCorr (object, useScale=useScale)
    sc <- attr(varc, "sc")
    recorr <- lapply(varc, function(el) el@factors$correlation)
    reStdDev <- c(lapply(recorr, slot, "sd"), list(Residual = sc))
    sigmas <- as.list (rep (NA, n.groupings+1))
    sigmas[1] <- ifelse (useScale, sc, NA)
    cors <- as.list (rep (NA, n.groupings+1))
    names (sigmas) <- names (cors) <- c ("data", names (varc))
    for (k in 1:n.groupings){
      sigmas[[k+1]] <- reStdDev[[k]]
      cors[[k+1]] <- as.matrix (recorr[[k]])
      if (length (cors[[k+1]]) == 1) cors[[k+1]] <- NA
    }
    return (list (sigma=sigmas, cors=cors))
  }
}

as.matrix.VarCorr <- function (varc, useScale, digits){
# VarCorr function for lmer objects, altered as follows:
#   1.  specify rounding
#   2.  print statement at end is removed
#   3.  reMat is returned
#   4.  last line kept in reMat even when there's no error term
#
                    sc <- attr(varc, "sc")
                  recorr <- lapply(varc, function(el) el@factors$correlation)
                  reStdDev <- c(lapply(recorr, slot, "sd"), list(Residual = sc))
                  reLens <- unlist(c(lapply(reStdDev, length)))
                  reMat <- array('', c(sum(reLens), 4),
                                 list(rep('', sum(reLens)),
                                      c("Groups", "Name", "Variance", "Std.Dev.")))
                  reMat[1+cumsum(reLens)-reLens, 1] <- names(reLens)
                  reMat[,2] <- c(unlist(lapply(reStdDev, names)), "")
#                  reMat[,3] <- format(unlist(reStdDev)^2, digits = digits)
#                  reMat[,4] <- format(unlist(reStdDev), digits = digits)
                  reMat[,3] <- fround(unlist(reStdDev)^2, digits)
                  reMat[,4] <- fround(unlist(reStdDev), digits)
                  if (any(reLens > 1)) {
                      maxlen <- max(reLens)
                      corr <-
                          do.call("rbind",
                                  lapply(recorr,
                                         function(x, maxlen) {
                                             x <- as(x, "matrix")
#                                             cc <- format(round(x, 3), nsmall = 3)
                                             cc <- fround (x, digits)
                                             cc[!lower.tri(cc)] <- ""
                                             nr <- dim(cc)[1]
                                             if (nr >= maxlen) return(cc)
                                             cbind(cc, matrix("", nr, maxlen-nr))
                                         }, maxlen))
                      colnames(corr) <- c("Corr", rep("", maxlen - 1))
                      reMat <- cbind(reMat, rbind(corr, rep("", ncol(corr))))
                  }
#                  if (!useScale) reMat <- reMat[-nrow(reMat),]
          if (!useScale) reMat[nrow(reMat),] <- c ("No residual sd", rep("",ncol(reMat)-1))
          return (reMat)
      }

# rwish and dwish functions stolen from Martin and Quinn's MCMCpack

rwish <- function (v, S){
  if (!is.matrix(S)) 
        S <- matrix(S)
    if (nrow(S) != ncol(S)) {
        stop(message = "S not square in rwish().\n")
    }
    if (v < nrow(S)) {
        stop(message = "v is less than the dimension of S in rwish().\n")
    }
    p <- nrow(S)
    CC <- chol(S)
    Z <- matrix(0, p, p)
    diag(Z) <- sqrt(rchisq(p, v:(v - p + 1)))
    if (p > 1) {
        pseq <- 1:(p - 1)
        Z[rep(p * pseq, pseq) + unlist(lapply(pseq, seq))] <- rnorm(p * 
            (p - 1)/2)
    }
    return(crossprod(Z %*% CC))
}

dwish <- function (W, v, S) {
    if (!is.matrix(S)) 
        S <- matrix(S)
    if (nrow(S) != ncol(S)) {
        stop(message = "W not square in dwish()\n\n")
    }
    if (!is.matrix(W)) 
        S <- matrix(W)
    if (nrow(W) != ncol(W)) {
        stop(message = "W not square in dwish()\n\n")
    }
    if (nrow(S) != ncol(W)) {
        stop(message = "W and X of different dimensionality in dwish()\n\n")
    }
    if (v < nrow(S)) {
        stop(message = "v is less than the dimension of S in  dwish()\n\n")
    }
    k <- nrow(S)
    gammapart <- 1
    for (i in 1:k) {
        gammapart <- gammapart * gamma((v + 1 - i)/2)
    }
    denom <- gammapart * 2^(v * k/2) * pi^(k * (k - 1)/4)
    detS <- det(S)
    detW <- det(W)
    hold <- solve(S) %*% W
    tracehold <- sum(hold[row(hold) == col(hold)])
    num <- detS^(-v/2) * detW^((v - k - 1)/2) * exp(-1/2 * tracehold)
    return(num/denom)
}


# mcsamp function (wrapper for mcmcsamp in lmer())

mcsamp <- function (object, n.chains=3, n.iter=1000, n.burnin=floor(n.iter/2), n.thin=1, saveb=TRUE, make.bugs.object=TRUE){
  # Quick function to run mcmcsamp() [the function for MCMC sampling for
  # lmer objects) and convert to Bugs objects for easy display
  #
  # Example application:
  #
  # x <- rnorm (100)
  # group <- rep(1:10, rep(10,10))
  # mu.a <- 0
  # sigma.a <- 2
  # mu.b <- 3
  # sigma.b <- 4
  # rho <- 0
  # Sigma.ab <- array (c(sigma.a^2, rho*sigma.a*sigma.b, rho*sigma.a*sigma.b, sigma.b^2), c(2,2))
  # sigma.y <- 1
  # ab <- mvrnorm (10, c(mu.a,mu.b), Sigma.ab)
  # a <- ab[,1]
  # b <- ab[,2]
  # y <- rnorm (100, a[group] + b[group]*x, sigma.y)
  # M1 <- lmer (y ~ x + (1|group))
  # M1.sim <- mcsamp (M1)
  # plot (M1.sim)
  #
  # M2 <- lmer (y ~ x + (1 + x |group))
  # M2.sim <- mcsamp (M2)
  # plot (M2.sim)

  require ("R2WinBUGS")
  if (n.chains<2) stop ("n.chains must be at least 2")
  n.keep <- n.iter - n.burnin
  first.chain <- mcmcsamp (object, n.iter, saveb=saveb, trans=TRUE)[(n.burnin+1):n.iter,]
  n.parameters <- ncol(first.chain)
  if (n.thin!=1){
    cat ("Sorry--thinning hasn't been implemented yet!\n")
    n.thin <- 1
  }
  sims <- array (NA, c(n.keep, n.chains, n.parameters))
  par.names <- dimnames(first.chain)[[2]]
  par.names <- par.names[is.na(match(par.names,""))]
  if (saveb){
    b.hat <- beta.hat (object)
    n.groupings <- length(b.hat) - 1
    for (m in 1:n.groupings){
      J <- dim(b.hat[[m+1]])[1]
      K <- dim(b.hat[[m+1]])[2]
      var.names <- paste (names(b.hat)[m+1],
                          unlist (dimnames(b.hat[[m+1]])[2]), sep=".")
      par.names <- c (par.names,
        paste ("eta.", rep(var.names,J), "[", rep(1:J,each=K), "]", sep=""))
    }
  }
  sims[,1,] <- first.chain
  for (k in 2:n.chains){
    sims[,k,] <- mcmcsamp (object, n.iter, saveb=saveb, trans=TRUE)[(n.burnin+1):n.iter,]
  }
  for (j in 1:n.parameters){
    if (par.names[j]=="log(sigma^2)"){
      par.names[j] <- "sigma.y"
      sims[,,j] <- exp (sims[,,j]/2)
    }
    else if (substr(par.names[j],1,4)=="log("){
      par.names[j] <- paste ("sigma.", substr(par.names[j], 5, nchar(par.names[j])-1), sep="")
      sims[,,j] <- exp (sims[,,j]/2)
    }
    else if (substr(par.names[j],1,6)=="atanh("){
      par.names[j] <- paste ("rho.", substr(par.names[j], 7, nchar(par.names[j])-1), sep="")
      sims[,,j] <- tanh (sims[,,j])
    }
    else if (substr(par.names[j],1,4)=="eta."){
    }
    else {
      par.names[j] <- paste ("beta.", par.names[j], sep="")
    }
  }
  dimnames(sims) <- list (NULL, NULL, par.names)
  if (make.bugs.object){
    return (as.bugs.array (sims, program="lmer", n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin))
  }
  else {
    return (sims)
  }
}

matching <- function(z, score){
	# argument z is the vector of indicators for treatment or control #
	# argument score is the vector of the propensity scores in the    #
	# same order as z                                                 #
	# the function returns a vector of indices that the corresponding #
	# unit is matched to. 0 means matched to nothing.                 #
	#                                                                 #
	# now also returns a number for each pair making it easier to     #
	# later recover those pairs
	n <- length(score)
	matched <- rep(0., n)
	pairs <- rep(0., n)
#	if(length(z) != n) print("Error: unequal lengths")
	b <- (sum(z) < n/2.) * 1
	tally <- 0
	for(i in (1:n)[z == b]) {
		available <- (1:n)[(z != b) & (matched == 0)]
		j <- available[order(abs(score[available] - score[i]))[1.]]
		matched[i] <- j
		matched[j] <- i
		tally <- tally + 1.
		pairs[c(i, j)] <- tally
	}
	out <- cbind.data.frame(matched = matched, pairs = pairs)
	out
}
