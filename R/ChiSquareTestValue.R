# 
# Compute chi square test value.
# sum_i ((observed_i - expected_i)^2 / expected_i)
# degrees of freedom is N-1 (i = 1...N)
#

# Author: Leo 
# Date: 20.12.2006
# Note: there are many presuppositions for the chi square test to be reliable.

# Input:
# * observed: vector of observed values 
# * expected: vector of expected values 
#
# Output: c(testval, df)
# * testval: chi-square test value
# * df: degrees of freedom


chiSqTestVal <- function (observed,expected) {
	testval<-sum(((observed-expected)^2) / expected)
	df<-length(observed)-1
	out<-list()
	out$df<-df
	out$testval<-testval
	return(out)
}

