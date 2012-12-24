#
# This function takes matrix A as input and returns quantile normalized matrix.
# Author: Leo Lahti 31.7.2006

myQuantiles <- function (A) {
	#A is a matrix of size n x m

	#This function performs quantile normalization for matrix rows

	Anormalized<-matrix(NA,dim(A)[[1]],dim(A)[[2]])
	Aord<-matrix(NA,dim(A)[[1]],dim(A)[[2]])
	for (i in 1:dim(A)[[2]]) {
		Aord[,i]<-sort(A[,i])
	}
	AordRowMeans<-rowMeans(Aord)
	for (i in 1:dim(A)[[2]]) {
		Anormalized[order(A[,i]),i]<-AordRowMeans
	}
	return(Anormalized)	
}

