#' Scale data matrix columns to unit variance
#'
#' @param X data matrix
#' @param rm.na logical: remove NAs
#' @param sd.value standard deviation for columns (by default 1)
#'
#' @return scaled data matrix
#' 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export

UnitScale <- function(X, rm.na = TRUE, sd.value = NULL) {

  #remove col means from matrix X
  # FIXME: use centerData function here
  if (rm.na) {
    X2 <- matrix(NA,nrow=nrow(X),ncol=ncol(X))
    for (i in 1:ncol(X)) {
      x <- X[,i]
      nainds <- is.na(x)
      x <- x[!nainds]
      X2[!nainds,i] <- x/sd(x)
    }
  }
  if (!rm.na) {
    X2 <- apply(X,2,function(x){x/sd(x)})
  }
  if (length(sd.value)>0) {
    # Scale to predefined sd
    X2 <- apply(X2,2,function(x){x*sd.value})
  }

  dimnames(X2) <- dimnames(X)
  
  X2
}


#' Shift the data matrix column means to a specified value
#'
#' @param X data matrix
#' @param rm.na logical: remove NAs
#' @param meanvalue mean for columns (by default 0)
#'
#' @return shifted data matrix
#' 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export

CenterData <- function (X, rm.na = TRUE, meanvalue = 0) {

  if (!rm.na) {
    xcenter <- colMeans(X)
    X2 <- X - rep(xcenter, rep.int(nrow(X), ncol(X)))
  } else {	
    X2 <- array(NA, dim = c(nrow(X), ncol(X)), dimnames = dimnames(X))
    for (i in 1:ncol(X)) {
      x <- X[,i]
      nainds <- is.na(x)
      xmean <- mean(x[!nainds])
      X2[!nainds,i] <- x[!nainds] - xmean 	
    }
    dimnames(X2) <- dimnames(X)
  }

  # Shift the data to a specified value
  X2 <- X2 + meanvalue
  
  X2
}

