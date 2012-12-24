#########################

# Task: compute values of the logistic equation 
#       for the given number of iterations

# Input: x0 = initial value
#         r = parameter
#         n = number of iterations
#

# Output: vector of values

# Background: logistic equation x_n+1 = r * x_n * (1 - x_n)

# Authors: Karoline Faust, Didier Gonze

###########################

logisticEq<-function(x0 = 0.1, r = 2, n = 100, plot = FALSE){
x = x0
outvec = c(x)
for(i in 1:n){
	x = x * r * (1-x)
	outvec = c(outvec,x)	
}
if(plot){
	plot(1:length(outvec),outvec,main="logistic equation",xlab="number of iterations",ylab="value",type="l")	
}	
outvec
}

#############################

bifurc<-function(rmin=0, rmax=4, rint=0.01){

nr=(rmax-rmin)/rint
plot(rmin:rmax,main="bifurcation diagram", xlab="r",ylab="maxima of x",type="n", ylim=c(0,1),xlim=c(0,4))
for(j in 0:nr){
	r = rmin+j*rint
    data=logisticEq (r = r, plot = FALSE)
    maxima = c()
	for(k in 50:(length(data)-1)){
		if((data[k] > data[k-1]) && (data[k] > data[k+1])){
    		maxima = c(maxima,data[k])
		}
	}
	if(length(maxima) > 0){ 
		lines(rep(r,length(maxima)),maxima,type="p", pch=".")
	}
}	
}
