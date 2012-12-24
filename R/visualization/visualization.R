

plot.correlation.heatmap <- function (cors, qth = 0.05, cex.xlab = 3, cex.ylab = 3, xangle = 0) {

  # cors <- cors.dietwise[[diet]]; qth = 0.05; cex.xlab = 6; cex.ylab = 4; xangle = 0

  library(ggplot2)

  hc <- hclust(as.dist(cor(1-cors$cor)))
  cind <- hc$order

  hc <- hclust(as.dist(cor(1-t(cors$cor))))
  rind <- hc$order

  df <- melt(cors$cor) # melt correlation matrix
  df$qvalue <- melt(cors$qval)$value # melt qvalue matrix

  # Important: change the order in all dfs to be plotted
  df$X1 <- factor(df$X1, levels = rownames(cors$cor))
  df$X2 <- factor(df$X2, levels = rev(colnames(cors$cor)))

  p <- ggplot(df, aes(x = X1, y = X2, fill = value))
  p <- p + geom_tile() 
  limit <- 1
  p <- p + scale_fill_gradientn("Correlation", breaks=seq(from=limit, to=-limit, by=-0.1), colours=c("darkblue", "blue", "white", "red", "darkred"), limits=c(-limit,limit)) 
  p <- p + theme_bw() 
  p <- p + opts(axis.text.x=theme_text(angle = 90, size = cex.xlab)) + opts(axis.text.y=theme_text(size = cex.ylab))

  # Merkkaa merkitsevat tahdella
  signif <- subset(df, qvalue < qth)
  if (nrow(signif) > 0) {
    p <- p + geom_text(data = signif, aes(x = X1, y = X2, label = "+"), col = "white", size = 3)
  }

  p <- p + xlab("") + ylab("")
  p <- p + opts(axis.text.x=theme_text(angle=xangle)) 

  p

}

project.data <- function (amat, type = "PCA") {

  # Amat: samples x features

  if (type == "PCA") {
    if (nrow(amat) < ncol(amat)) {

      message("More samples than features, using sparse PCA")
      #x <- t(amat)
      #library(elasticnet)
      #pca <- arrayspc(x, K = 2, para = rep(0.01, 2), use.corr=FALSE); # Sparse PCA
      #scores <- x%*%pca$loadings
      ## Spca example: we are selecting 50 variables on each of the PCs
      library(mixOmics)
      #pcatune(amat, ncomp = 10, center = TRUE, scale. = FALSE) # optimal number of components 
      result <- spca(amat, ncomp = 2, center = TRUE, scale. = TRUE, keepX = rep(50, 2))
      scores <- result$x
    } else {
      message("PCA")
      pca <- princomp(amat) # Classical PCA
      scores <- pca$scores
    }
    tab <- data.frame(scores[,1:2])
    rownames(tab) <- rownames(amat)
  } else if (type == "Sammon") {
    library(MASS)
    d <- as.dist(1-cor(t(amat)))
    fit <- sammon(d, k = 2) # This gave the clearest visualization. Tuning magic parameter could still improve. Try for instance magic = 0.05.
    # Plot solution 
    tab <- data.frame(list(Comp.1 = fit$points[,1], Comp.2 = fit$points[,2]))
    rownames(tab) <- rownames(amat)
  } 
  # TODO MDS
  #fit <- cmdscale(d, eig=TRUE, k=2) # classical MDS
  #fit <- isoMDS(d, k=2)             # nonmetric MDS

#library(MASS)
#d <- as.dist(1-cor(t(amat)))
#fit <- cmdscale(d, eig=TRUE, k=2) # classical MDS
#fit <- isoMDS(d, k=2)             # nonmetric MDS
#fit <- sammon(d, k = 2) # This gave the clearest visualization. Tuning magic parameter could still improve. Try for instance magic = 0.05.
# Plot solution 
#x <- fit$points[,1]
#y <- fit$points[,2]
#s <- rownames(amat)
#s <- c(ns,rs)
#par(mar = c(5,5,1,1))
#plot(x[s], y[s], xlab="Coordinate 1", ylab="Coordinate 2", type="n", cex.lab = 1.8, las = 1, cex.axis = 1.3)
#lab <- gsub("FLN","",gsub("_","",annot[s,]$sampleID))
#char <- "W"; lab <- sapply(strsplit(lab, char), function (x) {if (length(x)>1) {gsub(" ","",paste(x[[1]], char, collapse = ""))} else {x}})
#char <- "R"; lab <- sapply(strsplit(lab, char), function (x) {if (length(x)>1) {gsub(" ","",paste(x[[1]], char, collapse = ""))} else {x}})
#char <- "S"; lab <- sapply(strsplit(lab, char), function (x) {if (length(x)>1) {gsub(" ","",paste(x[[1]], char, collapse = ""))} else {x}})
#char <- "M"; lab <- sapply(strsplit(lab, char), function (x) {if (length(x)>1) {gsub(" ","",paste(x[[1]], char, collapse = ""))} else {x}})
#char <- "N"; lab <- sapply(strsplit(lab, char), function (x) {if (length(x)>1) {gsub(" ","",paste(x[[1]], char, collapse = ""))} else {x}})
#cols <- c("purple","green","royalblue","darkorange","darkgreen","red","black","magenta","brown","forestgreen","darkblue","blue")
#text(x[s], y[s], labels = lab, cex=1.3, col = cols[as.numeric(annot[s,]$Donor)])


# TODO Kernel-PCA
#library(kernlab)
#kpc <- kpca(~., data=as.data.frame(x.train), kernel="rbfdot", features = 2)
#Print the principal component vectors
#pcv(kpc)
#Plot the data projection on the components
#par(mfrow=c(2,2))
#plot(rotated(kpc), col = as.integer(as.factor(ann[rownames(x.train),"time"])), xlab="1st Principal Component", ylab="2nd Principal Component")
#plot(rotated(kpc), col = as.integer(as.factor(ann[rownames(x.train),"lipids.group"])), xlab="1st Principal Component", ylab="2nd Principal Component")
#embed remaining points 
#emb <- predict(kpc, x.test)
#plot(rotated(kpc), col = as.integer(as.factor(ann[rownames(x.train),"lipids.group"])), xlab="1st Principal Component", ylab="2nd Principal Component")
#points(emb, col = as.integer(as.factor(ann[rownames(x.train),"lipids.group"])))
    
  colnames(tab) <- c("Comp.1","Comp.2")

  tab
}



boxplotFactor <- function (v) {
	#Input: vector with named elements
	#Action: produce plot such that each unique name has its own column in the plot
	uninames<-unique(names(v))
	values<-vector(length=length(uninames),mode="list")
	names(values)=uninames
	for (nam in uninames) {
		values[[nam]]<-v[names(v)==nam]
	}
	boxplot(values)
	values
}

dotplotFactor <- function (v) {
	#Input: vector with named elements
	#Action: produce plot such that each unique name has its own column in the plot
	uninames<-unique(names(v))
	values<-vector(length=length(uninames),mode="list")
	names(values)=uninames
	plot(rep(1,sum(names(v)==uninames[[1]])),v[names(v)==uninames[[1]]],xlim=c(1,length(uninames)),ylim=range(v))
	values[[1]]<-v[names(v)==uninames[[1]]]
	for (i in 2:length(uninames)) {
		nam<-uninames[[i]]
		values[[nam]]<-v[names(v)==nam]
		points(rep(i,sum(names(v)==uninames[[i]])),v[names(v)==uninames[[i]]])
	}
	values
}


set.breaks <- function (mat, interval=.1) {
  if (max(abs(mat))>1) {
    m <- floor(max(abs(mat)))
  } else {
    
    m <- round(max(abs(mat)),nchar(1/interval)-1)
  }

  mm <- m + interval/2
  vals <- seq(interval/2,mm,interval)
  # Note: the first and last values mimic infinity
  mybreaks  <- c(-(m+1e6),c(-rev(vals),vals),m+1e6)
  mybreaks
}


gray.palette <- function (int) {
  gray(seq(0,1,length=int))
}


plotMatrix.2way <- function (...) {
  library(sorvi)
  PlotMatrix(...)
}



plotScale <- function (mybreaks, mycolors = NULL, m = NULL, cex.axis=1.5, label.step = 2, interval=.1, two.sided = TRUE, label.start = 1.00, Nlab = 3, palette.function = NULL, ndigits = 5) {

  # Plot color scale 
  # m: overrides mybreaks, mypalette and plots scale that ranges (-m,m)
  # two.sided: symmetric two-way color-scheme around zero
  # TODO: palette.function usage compare rainbow, gray  

  require(graph)
  require(RBGL)
  require(Rgraphviz)
  require(graphics)

  if (two.sided) {
    
    if (length(m)>0) {
      mybreaks <- set.breaks(m, interval)
      image(t(as.matrix(seq(-mm, mm, length = 100))), col = mycolors, xaxt = 'n', yaxt = 'n', zlim = range(mybreaks), breaks=mybreaks)
    } else {
      image(t(as.matrix(mybreaks)), col = mycolors, xaxt = 'n',yaxt = 'n', zlim = range(mybreaks), breaks = mybreaks)
    }
  
    mm <- abs(mybreaks[[2]])
    
    tmp <- unlist(strsplit(as.character(mm),"\\."))
    if (length(tmp)>1) { ndigits <- nchar(tmp[[2]]) } else {ndigits <- 1}
    digit.step <-10^(-ndigits)
    labs <- round(seq(-mm, mm, by = digit.step), ndigits)
    start.position <- which.min(abs(round(labs, ndigits) - (-label.start)))
    end.position <- which.min(abs(round(labs, ndigits) - (label.start)))
    inds <- seq(start.position,end.position,length=Nlab)
      
    axis(2, at = seq(0,1,length=Nlab), labels = labs[inds], cex.axis = cex.axis, las=2)
  }

  if (!two.sided) {

    mm <- max(mybreaks) + 1e6 # infty
    m <- max(mybreaks)
 
    labs = seq(0,m,label.step)
    #inds = sapply(labs,function(lab){min(which(lab<=mybreaks))})
    start.position <- which.min(abs(round(labs, ndigits) - (-label.start)))
    end.position <- which.min(abs(round(labs, ndigits) - (label.start)))
    inds <- seq(start.position,end.position,length=Nlab)  

    image(t(as.matrix(seq(0, m, length = 100))), col = mycolors, xaxt='n',yaxt='n', zlim=range(mybreaks), breaks=mybreaks)
    
    axis(2, at = seq(0, 1, length=Nlab), labels=labs[inds], cex.axis=cex.axis, las=2)
  }
  
}


plot.axis <- function (mat, my.colors, my.breaks, N = 2) {

  color.scale <- t(seq(0,max(mat),length=1e2))
  image(color.scale, col = my.colors, breaks = my.breaks, yaxt='n')
  # interval between consecutive color breakpoints
  interval  <- my.breaks[[2]] - my.breaks[[1]]
  # position axis labels between the breakpoints
  # leave off the last value (it exceeds the limits)
  break.centers <- (my.breaks + interval/2)[-length(my.breaks)]
  # axis labels (use the values as names)
  axis.labels <- as.character(break.centers)
  # positions for the tickmarks
  tick.positions <- break.centers / max(my.breaks)
  # show every N's label only 
  #N <- 2
  inds <- seq(1,length(break.centers),N)
  axis(2, at = tick.positions[inds], labels=break.centers[inds], las = 2)

}


###############

#http://www.quantmod.com/examples/chartSeries3d/chartSeries3d.alpha.R
`chartSeries3d0` <-
function(Z, theta=30, r=10, col=c("yellow","red"), smoother=1, border=NA, shade=.3, ltheta=20,  x.cex=.75, srt=0,...) {
cnames <- colnames(Z)
yred <- colorRampPalette(col)
par(mar=c(3,1,1,1))
time.axis <- axTicksByTime(Z)
if(smoother > 1)
Z <- as.xts(t(apply(Z,1,function(x) spline(as.vector(coredata(x)), n=smoother*length(x))$y)))
pm <- persp(z=Z,
            x=(1:NROW(Z))/length(time.axis),
            y=(1:NCOL(Z))/smoother,
            shade=shade, ltheta=ltheta,
            r=r,
            theta=theta,
            col=rep(rep(yred(NCOL(Z)/smoother),each=smoother),each=(NROW(Z)-1)),
            scale=F, border=border,box=FALSE,...)

x_axis <- seq(1, NROW(Z), length.out=length(time.axis))/length(time.axis)
y_axis <- seq(1, NCOL(Z), length.out=NCOL(Z)/smoother)/smoother

# x-axis
xy0 <- trans3d(x_axis,y_axis[1],0,pm)
xy1 <- trans3d(x_axis,y_axis[1]-0.3,0,pm)
lines(trans3d(x_axis,y_axis[1],0,pm),col="#555555")
segments(xy0$x,xy0$y,xy1$x,xy1$y, col="#555555")
#text(xy1$x, xy1$y, labels=as.character(format(index(Z)[x_axis*10],"%m/%d/%y")), pos=1, offset=.25,cex=x.cex, srt=srt)
text(xy1$x, xy1$y, labels=names(time.axis), pos=1, offset=.25,cex=x.cex, srt=srt)

# y-axis
xy0 <- trans3d(x_axis[length(x_axis)], y_axis, 0, pm)
xy1 <- trans3d(x_axis[length(x_axis)]+.3, y_axis, 0, pm)
yz0 <- trans3d(x_axis[length(x_axis)], y_axis, coredata(Z)[NROW(Z),seq(1,NCOL(Z),by=smoother)], pm) # vertical y
lines(trans3d(x_axis[length(x_axis)], y_axis, 0, pm),col="#555555")
segments(xy0$x,xy0$y,xy1$x,xy1$y,col="#555555")
text(xy1$x, xy1$y, labels=cnames, pos=4, offset=.5,cex=x.cex)

segments(xy0$x,xy0$y,yz0$x,yz0$y, col="#555555") # y-axis vertical lines

# z-axis
z_axis <- seq(trunc(min(Z,na.rm=TRUE)), round(max(Z, na.rm=TRUE)))
xy0 <- trans3d(x_axis[length(x_axis)], y_axis[length(y_axis)], z_axis, pm)
xy1 <- trans3d(x_axis[length(x_axis)]+0.3, y_axis[length(y_axis)], z_axis, pm)
lines(trans3d(x_axis[length(x_axis)], y_axis[length(y_axis)], z_axis, pm))
segments(xy0$x,xy0$y,xy1$x,xy1$y)
text(xy1$x, xy1$y, labels=paste(z_axis,'%',sep=''), pos=1, offset=-.5,cex=x.cex)

title("Yield Curve 2008 --- Daily")
par(mar=c(5.1,4.1,4.1,3.1))
return(invisible(pm))
}

`getUSTreasuries` <-
function(year="2008") {
 if(missing(year)) {
     year <- NULL # current year
   } else year <- paste("_",year,sep="")
   curr.year <- paste("_",as.POSIXlt(Sys.Date())$year+1900,sep="")
   YC <- character(0)
   for(y in year) {
     treas.gov <- paste("http://www.ustreas.gov/offices/domestic-finance/debt-management/interest-rate/yield_historical",y,".shtml",sep="")
     if(y == curr.year)
       treas.gov <- "http://www.ustreas.gov/offices/domestic-finance/debt-management/interest-rate/yield_historical.shtml"
     cat(treas.gov,'\n')
     YC <- c(YC,readLines(treas.gov))
   }
   date <- as.Date(gsub('<.*?>','',YC[grep("headers=\"1\"",YC)],perl=TRUE),format="%m/%d/%y")
   .onemonth <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"2\"",YC)],perl=TRUE))
   .threemonth <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"3\"",YC)],perl=TRUE))
   .sixmonth <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"4\"",YC)],perl=TRUE))
   .oneyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"5\"",YC)],perl=TRUE))
   .twoyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"6\"",YC)],perl=TRUE))
   .threeyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"7\"",YC)],perl=TRUE))
   .fiveyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"8\"",YC)],perl=TRUE))
   .sevenyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"9\"",YC)],perl=TRUE))
   .tenyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"10\"",YC)],perl=TRUE))
   .twentyyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"11\"",YC)],perl=TRUE))
   .thirtyyear <- as.numeric(gsub("N/A",NA_character_,gsub('<.*?>','',YC[grep("headers=\"12\"",YC)],perl=TRUE)))
   T <- xts(matrix(c(.onemonth,.threemonth,.sixmonth,.oneyear,.twoyear,.threeyear,.fiveyear,.sevenyear,.tenyear,.twentyyear,.thirtyyear),nc=11), date)
   colnames(T) <- unlist(strsplit('1mo 3mo 6mo 1yr 2yr 3yr 5yr 7yr 10yr 20yr 30yr', ' '))
   T
}


boxplotFactor <- function (v) {
	#Input: vector with named elements
	#Action: produce plot such that each unique name has its own column in the plot
	uninames<-unique(names(v))
	values<-vector(length=length(uninames),mode="list")
	names(values)=uninames
	for (nam in uninames) {
		values[[nam]]<-v[names(v)==nam]
	}
	boxplot(values)
	values
}

dotplotFactor <- function (v) {
	#Input: vector with named elements
	#Action: produce plot such that each unique name has its own column in the plot
	uninames<-unique(names(v))
	values<-vector(length=length(uninames),mode="list")
	names(values)=uninames
	plot(rep(1,sum(names(v)==uninames[[1]])),v[names(v)==uninames[[1]]],xlim=c(1,length(uninames)),ylim=range(v))
	values[[1]]<-v[names(v)==uninames[[1]]]
	for (i in 2:length(uninames)) {
		nam<-uninames[[i]]
		values[[nam]]<-v[names(v)==nam]
		points(rep(i,sum(names(v)==uninames[[i]])),v[names(v)==uninames[[i]]])
	}
	values
}


set.breaks <- function (mat, interval=.1) {
  if (max(abs(mat))>1) {
    m <- floor(max(abs(mat)))
  } else {
    
    m <- round(max(abs(mat)),nchar(1/interval)-1)
  }

  mm <- m + interval/2
  vals <- seq(interval/2,mm,interval)
  # Note: the first and last values mimic infinity
  mybreaks  <- c(-(m+1e6),c(-rev(vals),vals),m+1e6)
  mybreaks
}


gray.palette <- function (int) {
  gray(seq(0,1,length=int))
}


plotMatrix.2way <- function (...) {
  library(sorvi)
  PlotMatrix(...)
}



plotScale <- function (mybreaks, mycolors = NULL, m = NULL, cex.axis=1.5, label.step = 2, interval=.1, two.sided = TRUE, label.start = 1.00, Nlab = 3, palette.function = NULL, ndigits = 5) {

  # Plot color scale 
  # m: overrides mybreaks, mypalette and plots scale that ranges (-m,m)
  # two.sided: symmetric two-way color-scheme around zero
  # TODO: palette.function usage compare rainbow, gray  

  require(graph)
  require(RBGL)
  require(Rgraphviz)
  require(graphics)

  if (two.sided) {
    
    if (length(m)>0) {
      mybreaks <- set.breaks(m, interval)
      image(t(as.matrix(seq(-mm, mm, length = 100))), col = mycolors, xaxt = 'n', yaxt = 'n', zlim = range(mybreaks), breaks=mybreaks)
    } else {
      image(t(as.matrix(mybreaks)), col = mycolors, xaxt = 'n',yaxt = 'n', zlim = range(mybreaks), breaks = mybreaks)
    }
  
    mm <- abs(mybreaks[[2]])
    
    tmp <- unlist(strsplit(as.character(mm),"\\."))
    if (length(tmp)>1) { ndigits <- nchar(tmp[[2]]) } else {ndigits <- 1}
    digit.step <-10^(-ndigits)
    labs <- round(seq(-mm, mm, by = digit.step), ndigits)
    start.position <- which.min(abs(round(labs, ndigits) - (-label.start)))
    end.position <- which.min(abs(round(labs, ndigits) - (label.start)))
    inds <- seq(start.position,end.position,length=Nlab)
      
    axis(2, at = seq(0,1,length=Nlab), labels = labs[inds], cex.axis = cex.axis, las=2)
  }

  if (!two.sided) {

    mm <- max(mybreaks) + 1e6 # infty
    m <- max(mybreaks)
 
    labs = seq(0,m,label.step)
    #inds = sapply(labs,function(lab){min(which(lab<=mybreaks))})
    start.position <- which.min(abs(round(labs, ndigits) - (-label.start)))
    end.position <- which.min(abs(round(labs, ndigits) - (label.start)))
    inds <- seq(start.position,end.position,length=Nlab)  

    image(t(as.matrix(seq(0, m, length = 100))), col = mycolors, xaxt='n',yaxt='n', zlim=range(mybreaks), breaks=mybreaks)
    
    axis(2, at = seq(0, 1, length=Nlab), labels=labs[inds], cex.axis=cex.axis, las=2)
  }
  
}


plot.axis <- function (mat, my.colors, my.breaks, N = 2) {

  color.scale <- t(seq(0,max(mat),length=1e2))
  image(color.scale, col = my.colors, breaks = my.breaks, yaxt='n')
  # interval between consecutive color breakpoints
  interval  <- my.breaks[[2]] - my.breaks[[1]]
  # position axis labels between the breakpoints
  # leave off the last value (it exceeds the limits)
  break.centers <- (my.breaks + interval/2)[-length(my.breaks)]
  # axis labels (use the values as names)
  axis.labels <- as.character(break.centers)
  # positions for the tickmarks
  tick.positions <- break.centers / max(my.breaks)
  # show every N's label only 
  #N <- 2
  inds <- seq(1,length(break.centers),N)
  axis(2, at = tick.positions[inds], labels=break.centers[inds], las = 2)

}


###############

#http://www.quantmod.com/examples/chartSeries3d/chartSeries3d.alpha.R
`chartSeries3d0` <-
function(Z, theta=30, r=10, col=c("yellow","red"), smoother=1, border=NA, shade=.3, ltheta=20,  x.cex=.75, srt=0,...) {
cnames <- colnames(Z)
yred <- colorRampPalette(col)
par(mar=c(3,1,1,1))
time.axis <- axTicksByTime(Z)
if(smoother > 1)
Z <- as.xts(t(apply(Z,1,function(x) spline(as.vector(coredata(x)), n=smoother*length(x))$y)))
pm <- persp(z=Z,
            x=(1:NROW(Z))/length(time.axis),
            y=(1:NCOL(Z))/smoother,
            shade=shade, ltheta=ltheta,
            r=r,
            theta=theta,
            col=rep(rep(yred(NCOL(Z)/smoother),each=smoother),each=(NROW(Z)-1)),
            scale=F, border=border,box=FALSE,...)

x_axis <- seq(1, NROW(Z), length.out=length(time.axis))/length(time.axis)
y_axis <- seq(1, NCOL(Z), length.out=NCOL(Z)/smoother)/smoother

# x-axis
xy0 <- trans3d(x_axis,y_axis[1],0,pm)
xy1 <- trans3d(x_axis,y_axis[1]-0.3,0,pm)
lines(trans3d(x_axis,y_axis[1],0,pm),col="#555555")
segments(xy0$x,xy0$y,xy1$x,xy1$y, col="#555555")
#text(xy1$x, xy1$y, labels=as.character(format(index(Z)[x_axis*10],"%m/%d/%y")), pos=1, offset=.25,cex=x.cex, srt=srt)
text(xy1$x, xy1$y, labels=names(time.axis), pos=1, offset=.25,cex=x.cex, srt=srt)

# y-axis
xy0 <- trans3d(x_axis[length(x_axis)], y_axis, 0, pm)
xy1 <- trans3d(x_axis[length(x_axis)]+.3, y_axis, 0, pm)
yz0 <- trans3d(x_axis[length(x_axis)], y_axis, coredata(Z)[NROW(Z),seq(1,NCOL(Z),by=smoother)], pm) # vertical y
lines(trans3d(x_axis[length(x_axis)], y_axis, 0, pm),col="#555555")
segments(xy0$x,xy0$y,xy1$x,xy1$y,col="#555555")
text(xy1$x, xy1$y, labels=cnames, pos=4, offset=.5,cex=x.cex)

segments(xy0$x,xy0$y,yz0$x,yz0$y, col="#555555") # y-axis vertical lines

# z-axis
z_axis <- seq(trunc(min(Z,na.rm=TRUE)), round(max(Z, na.rm=TRUE)))
xy0 <- trans3d(x_axis[length(x_axis)], y_axis[length(y_axis)], z_axis, pm)
xy1 <- trans3d(x_axis[length(x_axis)]+0.3, y_axis[length(y_axis)], z_axis, pm)
lines(trans3d(x_axis[length(x_axis)], y_axis[length(y_axis)], z_axis, pm))
segments(xy0$x,xy0$y,xy1$x,xy1$y)
text(xy1$x, xy1$y, labels=paste(z_axis,'%',sep=''), pos=1, offset=-.5,cex=x.cex)

title("Yield Curve 2008 --- Daily")
par(mar=c(5.1,4.1,4.1,3.1))
return(invisible(pm))
}

`getUSTreasuries` <-
function(year="2008") {
 if(missing(year)) {
     year <- NULL # current year
   } else year <- paste("_",year,sep="")
   curr.year <- paste("_",as.POSIXlt(Sys.Date())$year+1900,sep="")
   YC <- character(0)
   for(y in year) {
     treas.gov <- paste("http://www.ustreas.gov/offices/domestic-finance/debt-management/interest-rate/yield_historical",y,".shtml",sep="")
     if(y == curr.year)
       treas.gov <- "http://www.ustreas.gov/offices/domestic-finance/debt-management/interest-rate/yield_historical.shtml"
     cat(treas.gov,'\n')
     YC <- c(YC,readLines(treas.gov))
   }
   date <- as.Date(gsub('<.*?>','',YC[grep("headers=\"1\"",YC)],perl=TRUE),format="%m/%d/%y")
   .onemonth <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"2\"",YC)],perl=TRUE))
   .threemonth <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"3\"",YC)],perl=TRUE))
   .sixmonth <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"4\"",YC)],perl=TRUE))
   .oneyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"5\"",YC)],perl=TRUE))
   .twoyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"6\"",YC)],perl=TRUE))
   .threeyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"7\"",YC)],perl=TRUE))
   .fiveyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"8\"",YC)],perl=TRUE))
   .sevenyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"9\"",YC)],perl=TRUE))
   .tenyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"10\"",YC)],perl=TRUE))
   .twentyyear <- as.numeric(gsub('<.*?>','',YC[grep("headers=\"11\"",YC)],perl=TRUE))
   .thirtyyear <- as.numeric(gsub("N/A",NA_character_,gsub('<.*?>','',YC[grep("headers=\"12\"",YC)],perl=TRUE)))
   T <- xts(matrix(c(.onemonth,.threemonth,.sixmonth,.oneyear,.twoyear,.threeyear,.fiveyear,.sevenyear,.tenyear,.twentyyear,.thirtyyear),nc=11), date)
   colnames(T) <- unlist(strsplit('1mo 3mo 6mo 1yr 2yr 3yr 5yr 7yr 10yr 20yr 30yr', ' '))
   T
}

#' PlotModesToAnnotations
#' 
#' For a given subnet in NetResponseModel object and sample annotation vector (factors)
#' plot a heatmap visualization of mode-factorlevel associations
#' 
#' @param model NetResponseModel object 
#' @param subnet.id subnet.id
#' @param ann.vec sample annotation vector corresponding to the model
#'
#' @return ggplot2 object
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @keywords utilities
#' @export
#' @examples #
#' 
PlotModesToAnnotations <- function (model, subnet.id, ann.vec) {

    s2r <- sample2response(model, subnet.id, mode = "hard")
    df <- data.frame(list(sample = names(s2r), mode = as.vector(s2r)))
    df <- cbind(df, vnam = ann.vec)
    df2 <- df[, c("mode", "vnam")]
    keep <- apply(df2, 1, function (x) { !any(is.na(x)) })
    df2 <- df2[keep, ]
    df2$mode <- droplevels(df2$mode)
    df2$vnam <- as.factor(as.character(df2$vnam))

    # Factors
    if (nrow(df2) > 0) {
      p <- ggplot(df2, aes(x = mode)) + geom_histogram(aes(fill = vnam), binwidth = 0.5, position = "fill")
      p <- p + xlab("Mode") + ylab(vnam)
    } else {
      p <- NULL
    }

    p

}


#' PlotAnnotationsToModes
#' 
#' For a given subnet in NetResponseModel object and sample annotation vector
#' plot a heatmap visualization of annotation-mode associations
#' 
#' @param model NetResponseModel object 
#' @param subnet.id subnet.id
#' @param ann.vec sample annotation vector corresponding to the model
#'
#' @return ggplot2 object
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @keywords utilities
#' @export
#' @examples #
#' 
PlotAnnotationsToModes <- function (model, subnet.id, ann.vec) {

    s2r <- sample2response(model, subnet.id, mode = "hard")
    df <- data.frame(list(sample = names(s2r), mode = as.vector(s2r)))
    df <- cbind(df, vnam = ann.vec)
    df2 <- df[, c("mode", "vnam")]
    keep <- apply(df2, 1, function (x) { !any(is.na(x)) })
    df2 <- df2[keep, ]

    # Continuous
    if (nrow(df2) > 0) {
 
      p <- ggplot(df2, aes(x = vnam)) + geom_histogram(aes(fill = mode), binwidth = 5, position = "fill")
      p <- p + xlab(vnam) + ylab("Frequency")
    } else {
      p <- NULL
    }
    p

}


gray.palette <- function (int) {
  gray(seq(0,1,length=int))
}


plotMatrix.2way <- function (...) {
  library(sorvi)
  PlotMatrix(...)
}



plotScale <- function (mybreaks, mycolors = NULL, m = NULL, cex.axis=1.5, label.step = 2, interval=.1, two.sided = TRUE, label.start = 1.00, Nlab = 3, palette.function = NULL, ndigits = 5) {

  # Plot color scale 
  # m: overrides mybreaks, mypalette and plots scale that ranges (-m,m)
  # two.sided: symmetric two-way color-scheme around zero
  # TODO: palette.function usage compare rainbow, gray  

  require(graph)
  require(RBGL)
  require(Rgraphviz)
  require(graphics)

  if (two.sided) {
    
    if (length(m)>0) {
      mybreaks <- set.breaks(m, interval)
      image(t(as.matrix(seq(-mm, mm, length = 100))), col = mycolors, xaxt = 'n', yaxt = 'n', zlim = range(mybreaks), breaks=mybreaks)
    } else {
      image(t(as.matrix(mybreaks)), col = mycolors, xaxt = 'n',yaxt = 'n', zlim = range(mybreaks), breaks = mybreaks)
    }
  
    mm <- abs(mybreaks[[2]])
    
    tmp <- unlist(strsplit(as.character(mm),"\\."))
    if (length(tmp)>1) { ndigits <- nchar(tmp[[2]]) } else {ndigits <- 1}
    digit.step <-10^(-ndigits)
    labs <- round(seq(-mm, mm, by = digit.step), ndigits)
    start.position <- which.min(abs(round(labs, ndigits) - (-label.start)))
    end.position <- which.min(abs(round(labs, ndigits) - (label.start)))
    inds <- seq(start.position,end.position,length=Nlab)
      
    axis(2, at = seq(0,1,length=Nlab), labels = labs[inds], cex.axis = cex.axis, las=2)
  }

  if (!two.sided) {

    mm <- max(mybreaks) + 1e6 # infty
    m <- max(mybreaks)
 
    labs = seq(0,m,label.step)
    #inds = sapply(labs,function(lab){min(which(lab<=mybreaks))})
    start.position <- which.min(abs(round(labs, ndigits) - (-label.start)))
    end.position <- which.min(abs(round(labs, ndigits) - (label.start)))
    inds <- seq(start.position,end.position,length=Nlab)  

    image(t(as.matrix(seq(0, m, length = 100))), col = mycolors, xaxt='n',yaxt='n', zlim=range(mybreaks), breaks=mybreaks)
    
    axis(2, at = seq(0, 1, length=Nlab), labels=labs[inds], cex.axis=cex.axis, las=2)
  }
  
}


plot.axis <- function (mat, my.colors, my.breaks, N = 2) {

  color.scale <- t(seq(0,max(mat),length=1e2))
  image(color.scale, col = my.colors, breaks = my.breaks, yaxt='n')
  # interval between consecutive color breakpoints
  interval  <- my.breaks[[2]] - my.breaks[[1]]
  # position axis labels between the breakpoints
  # leave off the last value (it exceeds the limits)
  break.centers <- (my.breaks + interval/2)[-length(my.breaks)]
  # axis labels (use the values as names)
  axis.labels <- as.character(break.centers)
  # positions for the tickmarks
  tick.positions <- break.centers / max(my.breaks)
  # show every N's label only 
  #N <- 2
  inds <- seq(1,length(break.centers),N)
  axis(2, at = tick.positions[inds], labels=break.centers[inds], las = 2)

}

