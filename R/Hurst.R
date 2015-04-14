library(Rwave)
library(fractal)
library(fractaldim)
library(pracma)
x <- df$tax
#hurst.est(x, 1:50, 5)
hurstexp(x, d = 16)
RoverS(x)
#hurstSpec(x)                    
hurstBlock(x, method="aggAbs")  
hurstBlock(x, method="aggVar")  
#hurstBlock(x, method="diffvar") 
#hurstBlock(x, method="higuchi") 
 
library(fractal)
x <- df$tax
RoverS(x)

# -----------------------------------

library(Rwave)
#hurst.est(df$tax, 1:50, 5)
wnoise <- rnorm(8192)
plot.ts(wnoise)
spwnoise <- fft(wnoise)
spwnoise <- Mod(spwnoise)
spwnoise <- spwnoise*spwnoise
plot(spwnoise[1:4096], log="xy", type="l")
lswnoise <- lsfit(log10(1:4096), log10(spwnoise[1:4096]))
abline(lswnoise$coef)
cwtwnoise <- DOG(wnoise, 10, 5, 1, plot=FALSE)
mcwtwnoise <- Mod(cwtwnoise)
mcwtwnoise <- mcwtwnoise*mcwtwnoise
wspwnoise <- tfmean(mcwtwnoise, plot=FALSE)
wspec.pl(wspwnoise, 5)
hurst.est(wspwnoise, 1:50, 5)
hurst.est(wspec, range, nvoice, plot=TRUE)

# --------------------------------

##  Computing the Hurst exponent
library(fractal)
library(fractaldim)
library(pracma)
data(brown72)
hurstexp(brown72, d = 128)
hurst.est(df$tax, 1:50, 5)

##  Compare with other implementations
library(fractal)
x72 <- brown72                          #  H = 0.72
xgn <- rnorm(1024)                      #  H = 0.50
xlm <- numeric(1024); xlm[1] <- 0.1     #  H = 0.43
for (i in 2:1024) xlm[i] <- 4 * xlm[i-1] * (1 - xlm[i-1])
 
x <- x72
hurstSpec(x)                    # 0.776   # 0.720
RoverS(x)                       # 0.717
hurstBlock(x, method="aggAbs")  # 0.648
hurstBlock(x, method="aggVar")  # 0.613
hurstBlock(x, method="diffvar") # 0.714
hurstBlock(x, method="higuchi") # 1.001
 
x <- xgn
hurstSpec(x)                    # 0.538   # 0.500
RoverS(x)                       # 0.663
hurstBlock(x, method="aggAbs")  # 0.463
hurstBlock(x, method="aggVar")  # 0.430
hurstBlock(x, method="diffvar") # 0.471
hurstBlock(x, method="higuchi") # 0.574
 
x <- xlm
hurstSpec(x)                    # 0.478   # 0.430
RoverS(x)                       # 0.622
hurstBlock(x, method="aggAbs")  # 0.316
hurstBlock(x, method="aggVar")  # 0.279
hurstBlock(x, method="diffvar") # 0.547
hurstBlock(x, method="higuchi") # 0.998
## End(Not run)

# -------------------------------

#http://www.r-bloggers.com/exploring-the-market-with-hurst/
require(quantmod)
require(PerformanceAnalytics)

#get DJIA since 1896 from St. Louis Fed Fred
getSymbols("DJIA",src="FRED")

#do monthly to shorten the lengthy calculation
#on my old computer
DJIA <- to.monthly(DJIA)[,4]
retDJIA <- ROC(DJIA,n=1,type="discrete")

#paper HURST EXPONENT AND FINANCIAL MARKET PREDICTABILITY by
#Bo Qian and Khaled Rasheed use 1024 days or 4 years
#or 208 weeks assuming 52 weeks/year
#I first tried HurstIndex from PerformanceAnalytics
hurst.48 <- apply.rolling(retDJIA, FUN="HurstIndex", width=48)
chart.TimeSeries(hurst.208,colorset=c("cadetblue"),
	main = "Hurst Index (48 Month) for DJIA
	1896-2011")
#my results are very different from the research paper whether I use
#daily, weekly, or monthly
#so I must be doing something wrong
hurst.avg <- apply.rolling(retDJIA, FUN="HurstIndex", width=12)
hurst.avg <- runMean(hurst.avg,n=48)
hurst <- merge(hurst.48,hurst.avg)
colnames(hurst) <- c("Hurst 48 Month","Hurst 4y Avg of 12 Month")
chart.TimeSeries(hurst,colorset=c("cadetblue","darkolivegreen3"),
	legend.loc="bottomright",
	main = "Hurst Index Comparison for DJIA
	1896-2011")
abline(h=0.5) #this represents no trend or mean reversion


#now let's try a different package
require(FGN)
#get DJIA since 1896 from St. Louis Fed Fred
getSymbols("DJIA",src="FRED")
#will do daily; takes about 2 hours
#on my old computer
DJIA <- DJIA
retDJIA <- ROC(DJIA,n=1)
#about 2 hours for a result on my old computer
hurstK <- apply.rolling(retDJIA, FUN="HurstK", width=1024)
chart.TimeSeries(hurstK,colorset="cadetblue",
	main = "HurstK Calculation of Hurst Exponent (1024 days) for DJIA
	1896-2011")

#######system-building time
# do monthly just to test more quickly
DJIA <- to.monthly(DJIA)[,4]
index(DJIA) <- as.Date(index(DJIA))
retDJIA<-ROC(DJIA,n=1,type="discrete")
index(retDJIA) <- as.Date(index(retDJIA))
hurstKmonthly <- apply.rolling(retDJIA, FUN="HurstK", width = 12)
colnames(hurstKmonthly) <- "HurstK.monthly"
index(hurstKmonthly) <- as.Date(index(hurstKmonthly))
serialcorr <- runCor(cbind(coredata(retDJIA)),cbind(index(retDJIA)),n=12)
serialcorr <- as.xts(serialcorr,order.by=index(retDJIA))
autoreg <- runCor(retDJIA,lag(retDJIA,k=1),n=12)
colnames(serialcorr) <- "SerialCorrelation.monthly"
colnames(autoreg) <- "AutoRegression.monthly"
#check for correlation of potential signals
chart.Correlation(merge(hurstKmonthly,serialcorr,autoreg))
#try a sum to enter in strong trends
signal <- hurstKmonthly+serialcorr+autoreg
colnames(signal) <- "HurstCorrelationSum"
chart.TimeSeries(signal)
signal <- lag(signal,k=1)
retSys <- ifelse(signal > 0.1, 1, 0) * retDJIA
#charts.PerformanceSummary(retSys,ylog=TRUE)
#ok performance but let's see if we can enter
#only strong trends up and reduce the drawdown
signalUpTrend <- runMean(hurstKmonthly+serialcorr+autoreg,n=6) + (DJIA/runMean(DJIA,n=12)-1)*10
chart.TimeSeries(signalUpTrend)
signalUpTrend <- lag(signalUpTrend,k=1)
retSys <- merge(retSys,ifelse(signalUpTrend > 1, 1, 0) * retDJIA,retDJIA)
colnames(retSys) <- c("DJIA Hurst System","DJIA HurstUp System",
	"DJIA")
charts.PerformanceSummary(retSys,ylog=TRUE,cex.legend=1.25,
	colorset=c("cadetblue","darkolivegreen3","gray70"))

#now let's take it out of sample to see how it works
getSymbols("^N225",from="1980-01-01",to=format(Sys.Date(),"%Y-%m-%d"))
N225 <- to.monthly(N225)[,4]
index(N225) <- as.Date(index(N225))
retN225<-ROC(N225,n=1,type="discrete")
index(retN225) <- as.Date(index(retN225))
hurstKmonthly <- apply.rolling(retN225, FUN="HurstK", width = 12)
colnames(hurstKmonthly) <- "HurstK.monthly"
index(hurstKmonthly) <- as.Date(index(hurstKmonthly))
serialcorr <- runCor(cbind(coredata(retN225)),cbind(index(retN225)),n=12)
serialcorr <- as.xts(serialcorr,order.by=index(retN225))
autoreg <- runCor(retN225,lag(retN225,k=1),n=12)
colnames(serialcorr) <- "SerialCorrelation.monthly"
colnames(autoreg) <- "AutoRegression.monthly"
signalUpTrend <- runMean(hurstKmonthly+serialcorr+autoreg,n=6) + (N225/runMean(N225,n=12)-1)*10
chart.TimeSeries(signalUpTrend)
signalUpTrend <- lag(signalUpTrend,k=1)
retSys <- merge(ifelse(signalUpTrend > 1, 1, 0) * retN225,retN225)
colnames(retSys) <- c("Nikkei 225 HurstUp System","Nikkei 225")
charts.PerformanceSummary(retSys,ylog=TRUE,cex.legend=1.25,
	colorset=c("cadetblue","darkolivegreen3"))
###########################