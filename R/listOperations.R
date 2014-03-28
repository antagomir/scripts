listPairCombinations <-function(li) {

  # Input: list
  # Output: unique pairs from the list elements
  # Author: Leo Lahti
  # (C) 2006-2012

  li <- unique(li) #unique items
  n <- length(li)

  # all possible index pairs
  z <- expand.grid(1:n, 1:n)

  # remove permutations
  z <- z[z[,1] < z[,2], ]

  # pick elements
  pairlist <- t(apply(z, 1, function (pair) {li[pair]}))
  rownames(pairlist) <- NULL
}




