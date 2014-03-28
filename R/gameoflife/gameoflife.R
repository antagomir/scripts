library(simecol)

gol <- function(N=NA,n=20,m=20,maxs=100) {
require(simecol)
if(!is.matrix(N)) {
N <- sample(c(0,1),n*m,replace=T)
dim(N) <- c(n,m)
}
steps <- 0
image(N,col=c("grey","darkgreen"),axes=F)
while(steps<maxs) {
steps <- steps+1
B <- eightneighbours(N)
N[which(N==1& B!=2 & B!=3)] <- 0
N[which(N==0&B==3)] <- 1
image(N,col=c("grey","darkgreen"),axes=F,add=T)
Sys.sleep(0.15)
}
}

gol()