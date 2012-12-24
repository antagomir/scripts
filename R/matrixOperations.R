
duplicates <- function (mat) {

  # Find duplicated rows	  

  dups <- logical(length = nrow(mat))	   

  cnt <- 0
  i <- 1
  while (i < nrow(mat)) { 
    j <- i + 1
    while (j <= nrow(mat) && all(mat[i,] == mat[j,])) {
      dups[[j]] <- TRUE
      j <- j + 1
    }
    i <- j
    
  } 	   
  dups
}

# Correlation matrices
#http://theatavism.blogspot.com/2009/05/plotting-correlation-matrix-with.html

discretize <- function (vec, br) {
  # discretize the vector vec into bins whose centroids are given in br
  sapply(vec, function(x) {br[which.min(abs(x - br))]})
}

impute <- function (X) {

  # Impute missing values from a Gaussian. This is a standard simple
  # imputation approach.
  # (C) Leo Lahti 2010. License: MIT.
  
  for (i in 1:ncol(X)) {
    x <- X[, i]
    nas <- is.na(x)
    X[nas, i] <- rnorm(sum(nas), mean(x[!is.na(x)]), sd(x[!is.na(x)]))
  }
  
  X
  
}

colVariances <- function (dat) {
  # This is about 5x faster than apply(dat, 2, var)
  #max(abs(colVariances(dat) - apply(dat,2,var)))
  colSums((dat - rep(colMeans(dat), each = nrow(dat)))^2)/(nrow(dat) - 1)
}


lower.triangle<-function (mat) {
	# Get lower triangle of a square matrix 
	# as a numeric vector such that
	# row-by-row, picking elements in the order
	# 2,1;3,1;3,2;4,1,...
	
	# Author: Leo Lahti
	# Date: 19.10.2006

	elements<-c()
	nr=dim(mat)[[1]]
	nc=dim(mat)[[2]]
	for (i in 2:nr) {
		for (j in 1:(i-1)) {
			elements<-c(elements,mat[i,j])
		}
	}	
	elements
}

matrix.sqrt <- function (A) {
	#Solve square root (A^(1/2)) of a diagonalizable nxn-matrix A
	#First diagonalize A, then compute sqrt for the diagonal elements
	#of the diagonalized matrix Adot. Then compute matrix sqrt from these
	#Eigenvectors of A

	#Author: Leo Lahti
	#Date: 13.2.2007

        #Comment: I did not easily find suitable function in R
        #although there might well be a better optimized one.

	e<-eigen(A) 
	V<-e$vector
	#try inverse the eigvec matrix. 
	#if not diagonalizable then some error will occur from try I think
	Vinv<-try(solve(V)) 
	D <- diag(e$values)

	V%*%sqrt(D)%*%Vinv
}


whiten <- function(X) {
	#Remember to remove column means of X before whitening!!!

        #Whitens matrix X to Xw such that cov(Xw)=I
        #where * is matrix product and t() is transpose
        #this is done by whitening matrix W
        #Let  X*t(X)=E*D*t(E) (eigenvalue decomposition)
        #then W=D^(-1/2)*t(E)
        #and Xw=XW is the whitened matrix

	#Author: Leo Lahti
	#Date: 15.2.2007
        #FIXME: exploit SVD trick for more efficient computation if possible?

	#Help:
	epsilon1<-(1e-15)
	if (any(colMeans(X)>epsilon1)) {print("Remove column means before whitening!")}
	else {
		#Eigenvalue decomposition for the covariance matrix
		e<-eigen(cov(X))
		E<-e$vectors
		D<-e$values
		#Whitening matrix
		W<-E%*%diag(D^(-1/2))%*%t(E) 
		#Whitened matrix
		Xw<-X%*%W
		Xw
	}

	#FIXME: is it possible to do whitening with the SVD trick?
	#In this case, e<-eigen(X%*%t(X)) #X'* t(X') = I,
	#Possible SVD trick solution would go along these lines
	#Y<-t(X)/sqrt(N-1)
	#S<-svd(Y)
	#E2<-S$v
	#D2<-S$d
	#E2 is the same than eigen(X%*%t(X))$vectors but D2 is not (it is from the SVD).
	#There seems to be something odd when trying to solve eigenvalues
	
	#WARNING: RE-DE should be zero by definition when E are the eigenvectors 
	#and D the eigenvalues of the matrix R. However, there seems to be some deviation. 
	#e<-eigen(cov(t(X)))
	#E<-e$vectors
	#D<-e$values
	#plot(R%*%E,diag(D)%*%E)

	#max(abs((1/(dim(Mc)[[1]]-1))*(t(Mc)%*%Mc)==cov(Mc))) #within epsilon
	#i.e.
	#max(abs((1/(dim(t(Mc))[[1]]-1))*(Mc%*%t(Mc))==cov(t(Mc))))
}

diagonalize <- function(A){
	#Diagonalize given square matrix if it is diagonalizable
	#Author: Leo Lahti
	#Date: 2.3.2007
	e<-eigen(A) 
	V<-e$vector
	#try inverse the eigvec matrix. 
	#if not diagonalizable then some error will occur from try I think
	Vinv<-try(solve(V)) 
	#diagonalize A
	Adot<-Vinv%*%A%*%V
	Adot
}


projectData <- function (X,M) {
	#Project the data set X on the hyperplane defined by vectors in M
	#make the data orthogonal to M	
	for (k in 1:dim(M)[[1]]) {
		X<-X-outer(as.vector(X%*%M[k,]),M[k,])
	}
	X
}


normalize.basic <- function (X, meanvalue=NULL, sd.value=NULL) {
  #unitscale(centerData(X, rm.na = TRUE), rm.na = TRUE)
  centerData(unitscale(X, rm.na = TRUE, sd.value), rm.na = TRUE, meanvalue)
}




unitscale <- function(X, rm.na = TRUE, sd.value=NULL) {
	# scale each column to unit variance

  #remove col means from matrix X
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



# RETIRED: use function in visualization.R
#plotMatrix.2way = function (mat,mybreaks = c(-rev(c(.95,.98,.99,.995,.999,2)),c(.95,.98,.99,.995,.999,2)),maintext="",xlab="",ylab="",cexlab=1) {
#        require(graphics)
#        lim = floor(max(abs(mat)))+1
#        zlims = c(-lim,lim)
#        Ncol = length(mybreaks)-1
#        mypalette <- colorRampPalette(c("green", "black", "red"),space = "rgb")
#        # transpose and revert row order
#        # this way we plot the original matrix elements in the same coordinates as they appear in its numeric form
#        image(t(mat[rev(seq(nrow(mat))),]),col=mypalette(Ncol),xaxt='n',yaxt='n',zlim=zlims,breaks=mybreaks,main=maintext,xlab=xlab,ylab=ylab,cex.lab=cexlab)##
#
#        #axis(1,seq(0,1,length=length(subsets)),subsets,las=2,cex.axis=.7)
#        # Plot scale image
#        #image(t(as.matrix(seq(-1,1,.01))),col=mypalette(Ncol),zlim=zlims,breaks=mybreaks)#
#
#        return(mypalette)
#}

mcr <- function (x, verbose = TRUE, min.region = 1, remove.noninformative = TRUE) {

  # Leo Lahti 2010-
  # Licence: GNU GPL >= 2
  
  # Detect minimal common regions for a binary matrix (can have -1/0/1)
  # NOTE: non-optimized not-properly tested, non-perfect quick hack

  k <- 1
  regs <- NULL
  startpoint <- endpoint <- 1

  # FIXME: this may leave the last value unchecked, check this
  while (k < (nrow(x) - 2)) {

    k <- k+1
    
    # Require that the column mean always 0 or 1
    # meaning that all row elements are either 0 or 1
    y <- x[startpoint:k,]
    if (!(all(colMeans(y) == 0 | colMeans(y) == 1 | colMeans(y) == (-1)))) {
      if (verbose) {print(k/nrow(x))}
      endpoint <- k
      regs <- rbind(regs, c(startpoint, endpoint-1))
      # Set new starting point
      startpoint <- k
    } 
  }

  # Remove too small regions
  reg.sizes <- regs[,2]-regs[,1] + 1
  inds <- reg.sizes >= min.region
  regs <- regs[inds,]

  regs
}

