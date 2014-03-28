#Downloaded 26.6.2006 from 
#http://www.stat.columbia.edu/~gelman/itsim/itsim.R
#Modified such that '_' are replaced by '<-'

# R/S/Splus functions for inference from iterative simulation

# Written 1 July 1991 by Andrew Gelman, Dept. of Statistics, Columbia Univ.
# Function "monitor" added 28 Feb 1994, fixed 20 Jan 1995.
# (df+3)/(df+1) fix on 2 Oct 1995--see Brooks and Gelman (1998) paper.
# Option "keep.all" added on 24 Aug 2002.
#
# Please mail all comments/questions to gelman@stat.columbia.edu

# Software can be freely used for non-commercial purposes and freely
#    distributed.  Submitted to statlib on 29 April 1992.  Current
#    version submitted to statlib on 29 Dec 1995.

# Derivations appear in the articles, "Inference from iterative simulation
#    using multiple sequences," by A. Gelman and D. B. Rubin, in "Statistical
#    Science" 7, 457-511 (1992), and "Some new ideas on inference from
#    iterative simulation using multiple sequences," by S. Brooks and
#    A. Gelman, in "Journal of Computational and Graphical Statistics" (1998).

#                     How to Use These Programs
#
# Preparation:  The results of m multiple sequences, each of length 2n,
#    of iterative simulation (e.g., the Metropolis algorithm or the
#    Gibbs sampler) used to simulate a probability distribution.
#    The starting points of the simulations should be drawn from a
#    distribution that is overdispersed relative to the "target"
#    distribution you are trying to simulate.
#
#    At each iteration, all scalar parameters or summaries of interest
#    should be stored.  The result will be an 2n by m matrix for each
#    scalar summary.
#    (Or, if keep.all=T, the matrix is n by m.)
#
#    The program "gpar" (or "gpar.log" or "gpar.logit", see below) can be
#    used for each scalar summary, or the results from k scalar summaries
#    can be combined into an array of dimensions (2n, m, k) and put into
#    the program "monitor".
#    (Or, if keep.all=T, the array dimensions are (n, m, k).)
#
# To run:  Use gpar (or gpar.log or gpar.logit) to do output analysis
#    on the simulations for each scalar summary of interest.  For
#    each scalar summary r, gpar produces two results:
#
#       a.  Estimates of the distribution of r,
#       b.  The estimated potential scale reduction;
#           i.e., the factor by which the estimated t intervals for r
#           might decrease if the iterative simulation is continued
#           forever.  As n increases, the scale reduction approaches 1.

# The following S functions are included:

# gpar (Gibbs-parallel) takes a matrix r of simulations:  each of
#       m columns is an independent iteratively simulated sequence of length
#       2n.  The first n steps of each sequence are discarded.
#       (If keep.all=T, then each column is of length n, and nothing is discarded.)
#       Output is a list of three vectors:
#
#       post:  (2.5%, 50%, 97.5%) quantiles for the target distribution
#               based on the Student-t distribution
#       quantiles:  (2.5%, 25%, 50%, 75%, 97.5%) empirical quantiles of
#               the mn simulated values.
#       confshrink:  50% and 97.5% quantiles of a rough upper bound on
#               how much the confidence interval of "post" will shrink
#               if the iterative simulation is continued forever.
#
#           If both components of confshrink are not near 1, you should
#           probably run the iterative simulation further.

# gpar.log and gpar.logit are versions of gpar to be used for scalar
#       summaries that are all-positive (gpar.log) or are constrained to lie
#       between 0 and 1 (gpar.logit).

# monitor is a routine for monitoring the convergence of k scalar summaries
#	at once:  the input is an array of dimensions (2n, m, k).
#          (If keep.all=T, the dimensions are (n, m, k).)
#	Output is a k by 7 matrix, with 2.5%, 25%, 50%, 75%, 97.5% quantiles,
#	R-hat, and 97.5% upper bound for R-hat, for eack of the k scalar
#	summaries.
#	Optional additional input is trans:  a vector of length k, each
#	element of which is "", "log", or "logit", corresponding to no
#	transformation, log transformation, or logit transformation for
#	computing R-hat (the transformations have no effect on the quantiles).

#############################################################################

# SUBROUTINES

cov<-function (a,b) {
        m<-length(a)
        ((mean ((a-mean(a))*(b-mean(b)))) * m)/(m-1)}
logit<-function (x) {log(x/(1-x))}
invlogit<-function (x) {1/(1+exp(-x))}

col.means<-function(mat) {
        ones<-matrix(1, nrow = 1, ncol = nrow(mat))
        ones %*% mat/nrow(mat)}
col.vars<-function(mat) {
        means<-col.means(mat)
        col.means(mat * mat) - means * means}

# Chi-squared degrees of freedom estimated by method of moments
#
#       (Assume A has a gamma sampling distribution and varA is an unbiased
#       estimate of the sampling variance of A.)

chisqdf<-function (A, varA) {2*(A^2/varA)}

#############################################################################

# MAIN PROGRAM

gpar<-function (x, keep.all=F) {
        alpha<-.05                     # 95% intervals
        m<-ncol(x)
        if (!keep.all) x<-x [(nrow(x)/2+1):nrow(x),]  # 2nd half of simulated seqs
        n<-nrow(x)

# We compute the following statistics:
#
#  xdot:  vector of sequence means
#  s2:  vector of sequence sample variances (dividing by n-1)
#  W = mean(s2):  within MS
#  B = n*var(xdot):  between MS.
#  muhat = mean(xdot):  grand mean; unbiased under strong stationarity
#  varW = var(s2)/m:  estimated sampling var of W
#  varB = B^2 * 2/(m+1):  estimated sampling var of B
#  covWB = (n/m)*(cov(s2,xdot^2) - 2*muhat*cov(s^2,xdot)):
#                                               estimated sampling cov(W,B)
#  sig2hat = ((n-1)/n))*W + (1/n)*B:  estimate of sig2; unbiased under
#                                               strong stationarity
#  quantiles:  emipirical quantiles from last half of simulated sequences

        xdot<-as.vector(col.means(x))
        s2<-as.vector(col.vars(x))
        W<-mean(s2)
        B<-n*var(xdot)
        muhat<-mean(xdot)
        varW<-var(s2)/m
        varB<-B^2 * 2/(m-1)
        covWB<-(n/m)*(cov(s2,xdot^2) - 2*muhat*cov(s2,xdot))
        sig2hat<-((n-1)*W + B)/n
        quantiles<-quantile (as.vector(x), probs=c(.025,.25,.5,.75,.975))

    if (W > 1.e-8) {            # non-degenerate case

# Posterior interval post.range combines all uncertainties
# in a t interval with center muhat, scale sqrt(postvar),
# and postvar.df degrees of freedom.
#
#       postvar = sig2hat + B/(mn):  variance for the posterior interval
#                               The B/(mn) term is there because of the
#                               sampling variance of muhat.
#       varpostvar:  estimated sampling variance of postvar

        postvar<-sig2hat + B/(m*n)
        varpostvar <-
                (((n-1)^2)*varW + (1+1/m)^2*varB + 2*(n-1)*(1+1/m)*covWB)/n^2
        post.df<-chisqdf (postvar, varpostvar)
        post.range<-muhat + sqrt(postvar) * qt(1-alpha/2, post.df)*c(-1,0,1)

# Estimated potential scale reduction (that would be achieved by
# continuing simulations forever) has two components:  an estimate and
# an approx. 97.5% upper bound.
#
# confshrink = sqrt(postvar/W),
#     multiplied by sqrt(df/(df-2)) as an adjustment for the
###      CHANGED TO sqrt((df+3)/(df+1))
#     width of the t-interval with df degrees of freedom.
#
# postvar/W = (n-1)/n + (1+1/m)(1/n)(B/W); we approximate the sampling dist.
# of (B/W) by an F distribution, with degrees of freedom estimated
# from the approximate chi-squared sampling dists for B and W.  (The
# F approximation assumes that the sampling dists of B and W are independent;
# if they are positively correlated, the approximation is conservative.)

        varlo.df<-chisqdf (W, varW)
        confshrink.range<-sqrt (c(postvar/W,
                (n-1)/n + (1+1/m)*(1/n)*(B/W) * qf(.975, m-1, varlo.df)) *
                (post.df+3)/(post.df+1))

        list(post=post.range, quantiles=quantiles, confshrink=confshrink.range)
    }
    else {      # degenerate case:  all entries in "data matrix" are identical
        list (post=muhat*c(1,1,1), quantiles=quantiles, confshrink=c(1,1))
    }
        }

##############################################################################

gpar.log<-function (r, keep.all=F) {
        gp<-gpar(log(r), keep.all)
        list (post=exp(gp$post), quantiles=exp(gp$quantiles),
              confshrink=gp$confshrink)}

gpar.logit<-function (r, keep.all=F) {
        gp<-gpar(logit(r), keep.all)
        list (post=invlogit(gp$post), quantiles=invlogit(gp$quantiles),
              confshrink=gp$confshrink)}

##############################################################################

monitor<-function (a, trans=rep("",
	ifelse (length(dim(a))<3, 1, dim(a)[length(dim(a))])), keep.all=F) {

# a is a (2n) x m x k matrix:  m sequences of length 2n, k variables measured
# trans is a vector of length k:  "" if no transformation, or "log" or "logit"

	output<-NULL
	nparams<-ifelse (length(dim(a))<3, 1, dim(a)[length(dim(a))])
	if (length(dim(a))==2) a<-array (a, c(dim(a),1)) 
	for (i in 1:nparams){
	    if (trans[i]=="log") gp<-gpar.log(a[,,i], keep.all)
	    else if (trans[i]=="logit") gp<-gpar.logit(a[,,i], keep.all)
	    else gp<-gpar(a[,,i], keep.all)
	    output<-rbind (output, c(gp$quantiles, gp$confshrink))
	}
	dimnames(output)<-list(dimnames(a)[[3]],c("2.5%","25%","50%","75%",
	    "97.5%","Rhat","Rupper"))
        output
}
