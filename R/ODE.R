install.packages(c("deSolve", "RCurl","stringr", "ggplot2", "plyr"))

library(deSolve)
library(ggplot2)
library(plyr)
library(RCurl)    # For getURL() and curl handler / cookie / google login
library(stringr)  # For str_trim() to trip whitespace from strings

# Credits/Inspiration
# http://christophriedl.net/2013/08/22/google-trends-with-r/
# http://www.samsi.info/

# Google account settings
#username <- "YOUR_NAME@gmail.com"
#password <- "YOUR_PASSWORD"

# URLs
loginURL 		<- "https://accounts.google.com/accounts/ServiceLogin"
authenticateURL <- "https://accounts.google.com/accounts/ServiceLoginAuth"
trendsURL 		<- "http://www.google.com/trends?"

## This gets the GALX cookie which we need to pass back with the login form
getGALX <- function(curl) {
  txt = basicTextGatherer()
  curlPerform( url=loginURL, curl=curl, writefunction=txt$update, header=TRUE, ssl.verifypeer=FALSE )
  
  tmp <- txt$value()
  
  val <- grep("Cookie: GALX", strsplit(tmp, "\n")[[1]], val = TRUE)
  strsplit(val, "[:=;]")[[1]][3]
  
  return( strsplit( val, "[:=;]")[[1]][3]) 
}


## Function to perform Google login and get cookies ready
gLogin <- function(username, password) {
  ch <- getCurlHandle()
  
  ans <- (curlSetOpt(curl = ch,
                     ssl.verifypeer = FALSE,
                     useragent = getOption('HTTPUserAgent', "R"),
                     timeout = 60,         
                     followlocation = TRUE,
                     cookiejar = "./cookies",
                     cookiefile = ""))
  
  galx <- getGALX(ch)
  authenticatePage <- postForm(authenticateURL, .params=list(Email=username, Passwd=password, GALX=galx, PersistentCookie="yes", continue="http://www.google.com/trends"), curl=ch)
  
  authenticatePage2 <- getURL("http://www.google.com", curl=ch)
  
  if(getCurlInfo(ch)$response.code == 200) {
    print("Google login successful!")
  } else {
    print("Google login failed!")
  }
  return(ch)
}

## Function to query a search string
gQuery <-function(username, password, queryString){
  ch <- gLogin( username, password )
  authenticatePage2 <- getURL("http://www.google.com", curl=ch)
  res <- getForm(trendsURL, q=queryString, content=1, export=1, graph="all_csv", curl=ch)
  #str(res)
  # Check if quota limit reached
  if( grepl( "You have reached your quota limit", res ) ) {
    stop( "Quota limit reached; You should wait a while and try again lateer" )
  }
  
  # Parse resonse and store in CSV
  # We skip ther first few rows which contain the Google header; we then read 500 rows up to the current date
  x <- try( read.table(text=res, sep=",", col.names=c("Week", "TrendsCount"), skip=32, nrows=500) )
  return(x)
}

myspaceData <-gQuery(username, password, "myspace")
qplot(Week,TrendsCount,data=myspaceData)
fbData<-gQuery(username, password, "facebook")
qplot(Week,TrendsCount,data=fbData)

Propagation<-myspaceData$TrendsCount
#first need an R function defining the SIR model

SIRmodel <- function(t, x, params) {
  with(as.list(c(params, x)), {
    dS <- -b*S*I
    dI <- b*S*I - g*I
    dR <- g*I
    res <- c(dS, dI, dR)
    list(res)
  })
}

## Parameters
## (Just pick some numbers)
params <- c(b=0.5, g=0.5)
## vector of time steps
times <- seq(1, 500, by=1)
## Initial conditions
IC <- c(S=99.99, I=0.01, R=0)

## Solving ODE
Output <- lsoda(IC, times, SIRmodel, params)
plot(Output)

#function that measures distance between solution of SIRmodel and actual incidence data
SIRdist <- function(x){
  #x should have 3 entries
  #x[1]=b
  #x[2]=g
  #x[3]=Initial number of infected O to 100
  
  GhostParameters <- c(b=x[1], g=x[2])
  InitialCondition <- c(S=100-.01, I=.01, R=0)
  
  Ghost<-lsoda(InitialCondition,times,SIRmodel,GhostParameters)
  
  a<-Ghost[,"I"]
  
  
  Dist<-sum((Propagation-a)^2)
  
  Dist
}

#Optim has the tendency to find a local minimum instead of global, hence zeroing in on the solution space first before running optim
beta.in <- seq(0.000, 0.0019, by=.0001)
gamma.in <- seq(0.004, 0.0059, by=.0001)

n<-1
SIRDistMat<-array(1:400, dim=c(20,20))
for (i in beta.in){
  m<-1
  for(j in gamma.in){
    SIRDistMat[n,m] <- SIRdist(c(i,j, 0.01))
    m<-m+1
  }
  n<-n+1
}

newStartingPoint <- arrayInd(which.min(SIRDistMat), dim(SIRDistMat))


# New Starting Point
y <- c(beta.in[newStartingPoint[1]],gamma.in[newStartingPoint[2]])
# Use trace<-3 if you'd like to see what's going on
X<-optim(y,SIRdist)

a<-X$par
OptimalParams <- c(b=a[1], g=a[2])

IC <- c(S=99.99, I=.01, R=0) # same IC as in SIRdist()

Solution <- lsoda(IC,times, SIRmodel, OptimalParams)
df <- data.frame(t(rbind(myspaceData$Week,myspaceData$TrendsCount,Solution[,"I"] )))
names(df)[1] <-"Week"
names(df)[2] <-"Actual"
names(df)[3] <- "Model"

ggplot() +
  geom_point(data = df, aes(Week, Actual)) +
  geom_point(data = df, aes(Week, Model), colour = "red") +ylab("Model V/s Actual")


