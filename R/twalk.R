library(Rtwalk)

info <- Runtwalk( dim=4,  Tr=100000,  Obj=function(x) { sum(x^2)/2 }, Supp=function(x) { TRUE }, x0=runif(4, min=20, max=21), xp0=runif(4, min=20, max=21)) 

#info$output

### or plot the log of the objective
### and remove the burn-in
#PlotLogObj(info, from=2000)
#PlotHist(info, par=3, from=2000)
#TS( info, from=2000) ### time series of the parameters
### And do some basic autocorrelation analysis
#Ana( info, from=2000)
### And save the output as columns in a table
#SaveOutput( info, file="Tsttwalk.dat")
### SaveOutput is simply a wraper to the write.table function


##################################

# Playing with 1D case


library(Rtwalk)

k<-1

info <- Runtwalk( dim=k,  Tr=11000,  Obj=function(x) { sum(x^5 + x^4-2x^2 +3) }, Supp=function(x) { TRUE }, x0=runif(k, min=-5, max=5), xp0=runif(k, min=-5, max=5)) 

info$output[-seq(1000)][seq(1, 10000, 100)]

##############################


