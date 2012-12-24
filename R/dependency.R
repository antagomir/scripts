



write.correlation.table <- function (cors, fname = NULL, append = FALSE, digits = 3, sep = " ") {

  dfm <- melt(round(cors$qval, 3))
  names(dfm)[[3]] <- "qvalue" 	

  dfm2 <- melt(round(cors$cor, 3))
  names(dfm2)[[3]] <- "correlation" 	

  dfm <- cbind(dfm, correlation = dfm2[["correlation"]])

  # sort by qvalue
  ss <- esort(dfm, qvalue)

  # Write to file
  if (!is.null(file)) {
    message(paste("Writing to file: ", fname))
    write.table(ss, file = fname, row.names = FALSE, quote = FALSE, sep = sep, append = append)
  }

  ss

}


co.occurrence.stats <- function (mat, type = "counts") {

  # mat: binary matrix of detection/absence 1/0
  # investigate for each feature pair (rows)
  # how many times they co-occur across samples
  # FIXME: speedup considerably with vector operations
   
  co <- array(0, dim = c(nrow(mat), nrow(mat)))
  dimnames(co) <- list(rownames(mat), rownames(mat))
  
  for (i in 1:nrow(mat))  {
    for (j in 1:nrow(mat)) {
      if (type == "counts") {
        # number of co-occurrences
        s <- sum(colSums(mat[c(i,j),]) == 2)
      } else if (type == "proportions") {
        # proportion of co-occurrences
        s <- mean(colSums(mat[c(i,j),]) == 2)
      } else if (type == "dependency") {
        # number of co-occurrences
        s <- sum(colSums(mat[c(i,j),]) == 2)        
      } else if (type == "bf") {
        # pvalue for 2x2 contingency table for this pair
        # note: assumes uniform prior, check if this is appropriate with your model
        # number of co-occurrences
        s <- sum(colSums(mat[c(i,j),]) == 2)        
      }
      co[i, j] <- s
    }
  }

  # measure deviation from independent features assumption
  # with Bayes factor measure and uniform priors
  if (type == "bf") {
    require(LearnBayes)
    prior <- matrix(rep(1, prod(dim(co))), nrow(co));
    bf <- ctable(co, prior) 

    for (i in 1:nrow(mat))  {
      for (j in 1:nrow(mat)) {
        # pvalue for 2x2 contingency table for this pair
        # note: assumes uniform prior, check if this is appropriate with your model
        y <- co[c(i,j),c(i,j)]
        prior <- matrix(rep(1,4),2,2)
        bf <- ctable(y, prior)
        co[i, j] <- bf
      }
    }
  } else if (type == "dependency") {
    # proportion of co-occurrences
    rs <- rowSums(co)
    cs <- colSums(co)
    # row and column frequencies and joint freq
    rf <- rs/sum(rs)
    cf <- cs/sum(cs)
    cof <- co/sum(co)
    co <- cof/matrix(rf)%*%t(matrix(cf))                
  }
  
  co
}



find.dependent.pairs <- function (mat, type = "bf") {

  # mat: binary matrix of detection/absence 1/0
  # investigate for each feature pair (rows)
  # how many times they co-occur across samples
  # FIXME: speedup considerably with vector operations

  require(LearnBayes)
  prior <- matrix(rep(1, 4), 2, 2);
  
  co <- array(0, dim = c(nrow(mat), nrow(mat)))
  dimnames(co) <- list(rownames(mat), rownames(mat))
  
  for (i in 1:nrow(mat))  {
    for (j in 1:nrow(mat)) {
      if (type == "bf") {
        # pvalue for 2x2 contingency table for this pair
        # note: assumes uniform prior, check if this is appropriate with your model
        # number of co-occurrences
        ctab <- matrix(NA, 2, 2)
        ctab[1, 1] <- sum(mat[i,] & mat[j,])
        ctab[2, 2] <- sum(!mat[i,] & !mat[j,])
        ctab[1, 2] <- sum(mat[i,] & !mat[j,])
        ctab[2, 1] <- sum(!mat[i,] & mat[j,])                
        bf <- ctable(ctab, prior) 
        co[i, j] <- bf
      }
    }
  }
  
  co
}



corContribution <- function (D1,D2) {

  # D1, D2: samples x features matrices		

  # Author: Leo Lahti
  # Data: 19.3.2007
  # Compute for each sample its contribution to the correlation between the two data sets
  # Comes directly from the equation for Pearson correlation.
	

  if (is.matrix(D1) && is.matrix(D2)) {
    # Ensure that the mean is removed:
    D1m <- apply(D1,2,function(x){(x-mean(x))})
    D2m <- apply(D2,2,function(x){(x-mean(x))})
	
    # Compute scores
    sco <- D1m*D2m/(sd(D1m)*sd(D2m))

  } else if (is.vector(D1) && is.vector(D2)) {
    # Ensure that the mean is removed:
    D1m <- D1-mean(D1)
    D2m <- D2-mean(D2)
	
    # Compute scores
    sco <- D1m*D2m/(sd(D1m)*sd(D2m))
  
  }

  sco

}





