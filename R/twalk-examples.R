#####################################################################
####  Examples using the t-walk implementation in R
####  see http://www.cimat.mx/~jac/twalk/
####  Author J. Andres Christen

#### We first load the twalk package:
rm(list=ls(all=TRUE)) # remove previous definitions

library(Rtwalk)



#### A ver simple inline example, 4 independent normal N(0,1):
######        dimension,  num of it, -log of objective function besides a const, support,
#info <- Runtwalk( dim=4,  Tr=100000,  Obj=function(x) { sum(x^2)/2 }, Supp=function(x) { TRUE },
#	x0=runif(4, min=20, max=21), xp0=runif(4, min=20, max=21)) 
####  and two (intentionally bad) initial points

### One can plot some histograms:
#PlotHist(info, par=3)
### Or time series of the parameters
#TS(info)
### or plot the log of the objective
#PlotLogObj(info)
### and remove the burn-in
#PlotLogObj(info, from=2000)
#PlotHist(info, par=3, from=2000)
#TS( info, from=2000)
### And do some basic autocorrelation analysis
#Ana( info, from=2000)

### And save the output as columns in a table
#SaveOutput( info, file="Tsttwalk.dat")
### SaveOutput is simply a wraper to the write.table function



########### A more complex Objective,
########### the posterior of alpha (shape) and beta (rate) in gamma sampling
########### The prior for alpha is U( 1, 4) and for beta is Exp(1)

### a initialization function
GaSamInit <- function(sample.size=100) {
	
	### Set the dimension as the global variable npars
	npars <<- 2 ## alpha and beta 	
	
	### sample 100 gammas with the true parameters 2.5 and 3
	m <<- sample.size ### sample size, now global variable m
	smpl <- rgamma( sample.size, shape=2.5, rate=3)
	
	### calculate the suff. statistics 
	r1 <<- sum(smpl)
	r2 <<- sum(log(smpl))
}

### This is the -log of the posterior, -log of the objective
GaSamU <- function(x) {

	al <- x[1]
	be <- x[2]
	
	### It is VERY advisable to try to do the calculations inside -log post:
	-1*m*al*log(be) + m*lgamma(al) + (1-al)*r2 + be*(1+r1) 
}

### This is the support:
GaSamSupp <- function(x) {

	(((0 < x[1]) & (x[1] < 4)) & (0 < x[2]))	
}

### Is also very advisable to have a function that generates initial (random?) points
### abything within the same galaxy of the objective most probabbly work
### for example, sample from the prior
GaSamX0 <- function(x) { c( runif(1, min=1, max=4), rexp(1,rate=1)) }

### The twalk is run with
### Don't forget to initialize the problem:
#GaSamInit()
#info <- Runtwalk( dim=npars,  Tr=100000,  Obj=GaSamU, Supp=GaSamSupp, x0=GaSamX0(), xp0=GaSamX0()) 

### For backward compatibility (depreciated!) this also works, value of dim taken from the global var n
#n <- npars
#info <- Runtwalk( Tr=100000,  Obj=GaSamU, Supp=GaSamSupp, x0=GaSamX0(), xp0=GaSamX0()) 



### For illustration purposes, and for dim=2, we can see the twalk running
### over countors of the posterior:
PlotObjGaSam <- function(add=TRUE)
{
	x1 <- x2 <- seq( 1, 4, length=100);
	dat <- NULL;
	for (y in x2)
		for (x in x1)
			dat <- append( dat, exp(-GaSamU(c(x,y))) );
	z <- matrix( dat, nrow=length(x1));
	contour(x1,x2,z,add=add);
}

#info <- Runtwalk( dim=npars,  Tr=100000,  Obj=GaSamU, Supp=GaSamSupp, x0=GaSamX0(), xp0=GaSamX0(), PlotObj= PlotObjGaSam)







######################################################################
#################### Multivariate Normal
############### centered at zero, precision matrix from "PresMat.dat"
############### a very strongy correlated matrix in the first two components.


### A highly correlated precision matrix:
PresMat <- matrix(
c(         20,           19,            0,            0,            0,          
           19,           20,            0,            0,            0,          
            0,            0,            1,            0,            0,          
            0,            0,            0,            1,            0,          
            0,            0,            0,            0,            1), ncol=5, byrow=TRUE)          
 

###  This loads the precision matrix, the inverse of the cov matrix.
InitParsMN<- function()
{
	### PresMat is defined below	 
	### The dimesion is the global variable n and most be set before Runtwalk is called
	n <<- dim(PresMat)[1]
}

###  Runtwalk takes U=-log Objective(x)
MNU <- function(x)
{
	0.5*(x %*% PresMat %*% x);
}

### And also takes the definition of the support.  Since in this case it is all
### R^n it is always true.
SuppMN <- function(x) { TRUE; } 

### To run the twalk do
#### > InitParsMN()                                    ## two initial points
#### > info <- Runtwalk( Tr=50000, Obj=MNU, Supp=SuppMN, x0=rep(1,n), xp0=rep(0,n), PlotObj=FALSE)
#### > Ana(info)  ## to analyse the output
#### > PlotHist( info,par=2) ## to plot the second parameter 
 
#### More examples below.

##############################################################
#### objective function
#### Bivariate normal with mean (0,0) and marginal std sig1 sig2 and correlation rho


log2pi <<- log(2*pi);

InitParsBN <- function()
{
	n <<- 2; ##Dimension
	sig1 <<- 2;
	sig2 <<- 3;
	m1 <<- -12;
	m2 <<- 12;
	rho <<- 0.9;
	r2 <<- 1-rho^2;
	mnLogConst <<- log2pi + log(sig1) + log(sig2) + 0.5*log(r2);
}

### U = -log of the objective function

BNU <- function(x)
{
	#mnLogConst + 1/(2*r2)*( (x[1]/sig1)^2 - 2*rho*(x[1]/sig1)*(x[2]/sig2) + (x[2]/sig2)^2);
	
	mnLogConst + 1/(2*r2)*( ((x[1]-m1)/sig1)^2 - 2*rho*((x[1]-m1)/sig1)*((x[2]-m2)/sig2) + ((x[2]-m2)/sig2)^2);
	
}

BNU2 <- function( x, m1, m2, sig1, sig2, rho)
{
	
	mnLogConst + 1/(2*r2)*( ((x[1]-m1)/sig1)^2 - 2*rho*((x[1]-m1)/sig1)*((x[2]-m2)/sig2) + ((x[2]-m2)/sig2)^2);
	
}

PlotObjBN <- function(add=TRUE)
{
	sig <- max(sig1,sig2);
	x1 <- x2 <- seq( -3.2*sig, 3.2*sig, length=100);
	x1 <- m1 + x1;
	x2 <- m2 + x2;
	dat <- NULL;
	for (y in x2)
		for (x in x1)
			dat <- append( dat, exp(-BNU(c(x,y))) );
	z <- matrix( dat, nrow=length(x1));
	contour(x1,x2,z,add=add);
}

SuppBN <- function(x) { TRUE; } 


### Run the twalk with:
### InitParsBN()
### info <- Runtwalk( Tr=5000, Obj=BNU, Supp=SuppBN, x0=c(1,2), xp0=c(0,3), PlotObj=PlotObjBN)



##################### More Examples
##################### The necessary functions are defined further below


################# Rosenbrok example
RBExample <- function( Tr=10000, k=(1/20), ...)
{
	InitParsRB(k);
	info <- Runtwalk(Tr=Tr, Obj=RBU, x0=c(0,10), xp0=c(2,15), Supp=SuppRB, PlotObj=PlotObjRB, dynty="p", ...);
	info;
}



RaExample <-
 function( Tr=10000, pathcol="red", rate=0.1, cent1=c(15,-15), cent2=c(11.5,-11.5), plrange=0.1, add=TRUE)
{
	InitParsRa(rate=rate, cent1=cent1, cent2=cent2, plrange=plrange)
	info <- Runtwalk(Tr=Tr, Obj=RaU, Supp=SuppRa, x0=c(0,0), xp0=c(1,1), pathcol=pathcol);
	cat("BiDou, m=(20,-10): Accepted", info$acc, ",", round(100*info$acc/Tr), "%", "\n");
	UU <- append( UU, -info$Us);
	info;
}



################### Example for figure 2 in the paper
Example1 <- function(Tr)
{
	plot( 0, 0, xlim=c(-20, 20), ylim=c(-20, 20), pch=".", main="", xlab="", ylab="");
	lines(c(-25, 30), c(0,0))
	lines(c(0,0),c(-20, 20))
	
###########We use Obj: RaU
	
	InitParsRa(rate=0.01, cent1=c(-15.5,-15.5), cent2=c(-9,-14), plrange=0.03)
	info <- Runtwalk(Tr=Tr, Obj=RaU, Supp=SuppRa, x0=c(0,0), xp0=c(1,1), PlotObj= PlotObjRa,
		add=TRUE, pathcol="blue");
	
	InitParsRa(rate=0.1, cent1=c(15,-15), cent2=c(11.5,-11.5), plrange=0.1)
	info <- Runtwalk(Tr=Tr, Obj=RaU, Supp=SuppRa, x0=c(0,0), xp0=c(1,1), PlotObj= PlotObjRa,
		add=TRUE, pathcol="red");
	
	InitParsRa(rate=0.001,cent1=c(4,6), cent2=c(15,10), plrange=0.01)
	info <- Runtwalk(Tr=Tr, Obj=RaU, Supp=SuppRa, x0=c(0,0), xp0=c(1,1), PlotObj= PlotObjRa,
		add=TRUE, pathcol="green");
		
	InitParsRa(rate=1000,cent1=c(-8, 1), cent2=c(-8.1, 1.3), plrange=1000)
	info <- Runtwalk(Tr=Tr, Obj=RaU, Supp=SuppRa, x0=c(0,0), xp0=c(1,1), PlotObj= PlotObjRa,
		add=TRUE, pathcol="black");
	
	
############ And the bivariate normal.
		
	InitParsBN();
	info <- Runtwalk(Tr=Tr, Obj=BNU, Supp=SuppBN, x0=c(0,0), xp0=c(1,1),
		PlotObj=PlotObjBN, add=TRUE, pathcol="grey")
		
 
}



ExampleTwoModes <- function(Tr, x0=c(5, 15), xp0=c(6,14))
{
	InitParsTwoModes();
	
	plot( 0, 0, xlim=c(-5, 15), ylim=c(-15, 15), pch=".", type="n", main="", xlab="", ylab="");

	info <- Runtwalk(Tr=Tr, Obj=TwoModesU, Supp=SuppTwoModesU, PlotObj=PlotObjTwoModes,  dynty="p", x0=x0, xp0=xp0, add=TRUE)
	cat("TwoModes, m=(-12,12): Accepted", info$acc, ",", round(100*info$acc/Tr), "%", "\n");
	info;
}

ExampleBN <- function(Tr, corr, ...)
{
	n <<- 2; ##Dimension
	sig1 <<- 1;
	sig2 <<- 1;
	m1 <<- 0;
	m2 <<- 0;
	rho <<- corr;
	r2 <<- 1-rho^2;
	mnLogConst <<- log2pi + log(sig1) + log(sig2) + 0.5*log(r2);
	
	Runtwalk(Tr=Tr, Obj=BNU, Supp=SuppBN, PlotObj=PlotObjBN, ...);
}






################################################################################
##########  Product of bivariate normals
############ objective
###  Radial

InitParsRa <- function(dm=2, rate=10, cent1=c(0,0), cent2=c(1,1), plrange=4.0)
{
	n <<- dm;   ##Dimension
	tau <<- rate; ##
	m1 <<- cent1; ##centre 1
	m2 <<- cent2; ##centre 2
	rng <<- plrange; ##for plotting
}

### U = -log of the objective function

IntProd <- function(x) { sum(x*x)  } ## square of the norm.

RaU <- function(x)
{
	#Old waky circular
	#((x[1]-m[1])/d + (x[2]-m[2])/d)^2/(1 + IntProd((x - m)/d)) +
	#(1/(2*(d/10)^2))*(d - sqrt(IntProd(x - m)))^2;
	
	tau*IntProd(x - m1)*IntProd(x - m2);
	
}


PlotObjRa <- function(add=FALSE)
{
	if (n == 2)
	{
		x1 <- seq(min(m1[1],m2[1])-(rng/tau), max(m1[1],m2[1])+(rng/tau), length=100);
		x2 <- seq(min(m1[2],m2[2])-(rng/tau), max(m1[2],m2[2])+(rng/tau), length=100);

		dat <- NULL;
		for (y in x2)
			for (x in x1)
				dat <- append( dat, exp(-RaU(c(x,y))) );
		z <- matrix( dat, nrow=length(x1));
		contour(x1,x2,z, add=add);
	}
}

SuppRa <- function(x) { TRUE; }







##########################################################################################
### Two modes at different scales

#InitParsTwoModes <- function( w=0.7, m1=c(6,0), s1=c(4,5), rho1=0.8, m2=c(0,0), s2=c(1,1), rho2=0.1) {
InitParsTwoModes <- function( w=0.7, m1=c(6,0), s1=c(4,5), rho1=0.8, m2=c(-3,10), s2=c(1,1), rho2=0.1) {
	
	n <<- 2; 

	w <<- 0.7;
	m1 <<- m1;
	s1 <<- s1;
	rho1 <<- rho1;
	m2 <<- m2;
	s2 <<- s2;
	rho2 <<- rho2;
}
	

TwoModesU <- function(x)
{
	### 	mnLogConst <<- log2pi + log(sig1) + log(sig2) + 0.5*log(r2);

	-log(
	    w*(1/(2*pi*s1[1]*s1[2]*sqrt(1-rho1^2)))*
	    exp(-( 1/(2*(1-rho1^2))*( ((x[1]-m1[1])/s1[1])^2 - 2*rho1*((x[1]-m1[1])/s1[1])*((x[2]-m1[2])/s1[2]) + ((x[2]-m1[2])/s1[2])^2))) 
		+
	    (1-w)*(1/(2*pi*s2[1]*s2[2]*sqrt(1-rho2^2)))*
	    exp(-( 1/(2*(1-rho2^2))*( ((x[1]-m2[1])/s2[1])^2 - 2*rho2*((x[1]-m2[1])/s2[1])*((x[2]-m2[2])/s2[2]) + ((x[2]-m2[2])/s2[2])^2))) 
		);
	
}

SuppTwoModesU <- function(x) { TRUE; }


PlotObjTwoModes <- function(add=TRUE)
{
	x1 <- seq( -5, 15, length=100);
	x2 <- seq(-15, 15, length=100);
	dat <- NULL;
	for (y in x2)
		for (x in x1)
			dat <- append( dat, exp(-TwoModesU(c(x,y))) );
	z <- matrix( dat, nrow=length(x1));
	contour(x1,x2,z, levels=seq( 0.0001, 0.04, length=20), add=add);
}




#############################################################################
#######################  Objective, Rosenbrok: super correlated hook shaped

InitParsRB <- function(k=(1/20))
{
	RBk <<- k;
	n <<- 2;
}

RBU <- function(x) { RBk*(100*(x[2]-x[1]^2)^2 + (1-x[1])^2); }

SuppRB <- function(x) { TRUE; }

PlotObjRB <- function(add=TRUE, length=300)
{
	x1 <- seq( -3, 4, length=length);  ### full -6 - 8
	x2 <- seq( -0.7, 15, length=length);  ### -0.7 to 55
	dat <- NULL;
	for (y in x2)
		for (x in x1)
			dat <- append( dat, exp(-RBU(c(x,y))) );
	z <- matrix( dat, nrow=length(x1));
	contour( x1, x2, z, drawlabels=FALSE, nlevels=5, add=add);
}

PlotFrameRB <- function(add=FALSE)
{
	if (!add)
		plot( 0,0, xlim=c(-2.5,4.5),ylim=c(-0.5,18),type="n",xlab="x",ylab="y");
}



