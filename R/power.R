# Example on power calculation for comparison of two unpaired groups
# that are assumed to follow normal distribution 
# NOTE: with HITChip this means log10 data !

# Leo Lahti 29.1.2014

# Explanations for these power analyses
# http://www.statmethods.net/stats/power.html
# http://www.ats.ucla.edu/stat/r/dae/t_test_power2.htm

# Load the power calculation library
# install.packages("pwr") 
library(pwr)

# Let's define the requested sample size, alpha and power
n <- 16       # Sample size
alpha <- 0.05 # Accepted false positive rate (ie. p-value threshold)
beta <- 0.2   # False negative rate (power = sensitivity = 1-beta)

# However, testing 130 L2 groups we will have considerable multiple testing. 
# To take this into account, adjust alpha level with Bonferroni:
# (note that Bonferroni is very conservative so this is an overkill but 
# safe bet)
alpha <- 0.05/130

# Then let's see what is the effect size that can be captured
# with these parameters for unpaired t-test assessing differences
# between the groups
d <- pwr.t2n.test(n1 = n, n2 = n, d = NULL, sig.level = alpha, power = 1-beta)$d

# The effect size is calculated as: 
# d <- abs(mu1 - mu2)/sd ; here sd^2 is the joint variance 
# which is calculated from the variances of the two groups as follows:
# sd <- sqrt((sd1^2 + sd2^2)/2) 
# In the case of HITChip, the variance in log10 data is typically <
#  0.76 (98% quantile) for given taxon. 
# So we can assume this is known. For a safe
# conservative estimate, let's assume sd1 = sd2 = 0.76. 
# Also note that variances within homogeneous subject groups are
# likely to be smaller, so again the estimate is conservative
# This gives now
sd1 <- 0.76
sd2 <- 0.76
sd.common <- sqrt((sd1^2 + sd2^2)/2) 

# Now we can calculate the difference between the group means
# that corresponds to the common standard deviation and effect size:
# d <- abs(mu1 - mu2)/sd --> abs(mu1 - mu2) = d * sd
mu <- d * sd.common

# This is the log fold change that we can detect, given
# n, alpha, beta, sd1, sd2
print(mu)

# 1.319234  -> This corresponds to 10^mu =  20.85615
# absolute fold change, fair enough?








