arimaSelect <- function(
  serie,                # the time serie to fit
  order,                # see stats::arima
  include.mean = FALSE, # idem
  alpha = 0.05          # risk
  ){

   n_max <- sum( order[ c(1,3) ] )
   n     <- length(serie)
   
   #! the matrix of coeffs, each row for each step
   coeff     <- matrix( NA , nrow = n_max , ncol = n_max )
   
   #! same with t-stat p-value's
   pval      <- coeff
   aic       <- rep(NA, n_max)
   liste     <- rep( list(NULL) , n_max )
   fixed     <- aic
   
   go <- TRUE
   i  <- 1
   while(go){
     
     arima.out <- arima(serie,
                        order        = order,
                        include.mean = include.mean,
                        method       = "ML", 
                        fixed        = fixed)
     liste[[i]] <- arima.out
     
     .coeff <- arima.out$coef
     .sd    <- rep(0, n_max)
     .sd[arima.out$mask] <- sqrt(diag(arima.out$var.coef))

     .pval  <- round( 2 * (1 - pt(abs( .coeff / .sd  ),
                                  df = n - n_max - 1 ) ) , 5)

     coeff[i,] <- .coeff
     pval[i,]  <- .pval
     aic[i]    <- arima.out$aic
     fixed[which.max(.pval)] <- 0
     go     <- any(.pval > alpha, na.rm = TRUE)
     i      <- i+1

     
   }
   
   
res <- list(coeff     = coeff,
            pval      = pval,
            aic       = aic, 
            listArima = liste)
class(res) <- "arimaSelect"
res
            
}

plot.arimaSelect <- function(x, choix, ...){
	noms <- names(x$listArima[[1]]$coef)
	coeff <- x$coeff
	k <- min(which(is.na(coeff[,1])))-1
	coeff <- coeff[1:k,]
	pval  <- x$pval[1:k,]
	aic   <- x$aic[1:k]
	coeff[coeff==0] <- NA
	n <- ncol(coeff)
	if(missing(choix)) choix <- k
	
	layout(matrix(c(1,1,1,2,
	                3,3,3,2,
			3,3,3,4,
			5,6,7,7),nr=4),
               widths=c(10,35,45,15),
	       heights=c(30,30,15,15))
	couleurs <- rainbow(75)[1:50]#(50)
	ticks <- pretty(coeff)
	
	### graph AIC
	par(mar=c(1,1,3,1))
	plot(aic,k:1-.5,type="o",pch=21,bg="blue",cex=2,axes=FALSE,lty=2,xpd=NA)
	points(aic[choix],k-choix+.5,pch=21,cex=4,bg=2,xpd=NA)
	#axis(3)
	title("aic",line=2)
	
	par(mar=c(3,0,0,0))	
	plot(0,axes=FALSE,xlab="",ylab="",xlim=range(ticks),ylim=c(.1,1))
	rect(xleft  = min(ticks) + (0:49)/50*(max(ticks)-min(ticks)),
	     xright = min(ticks) + (1:50)/50*(max(ticks)-min(ticks)),
	     ytop   = rep(1,50),
	     ybottom= rep(0,50),col=couleurs,border=NA)
	axis(1,ticks)
	rect(xleft=min(ticks),xright=max(ticks),ytop=1,ybottom=0)
	text(mean(coeff,na.rm=T),.5,"coefficients",cex=2,font=2)
	
	
	par(mar=c(1,1,3,1))
	image(1:n,1:k,t(coeff[k:1,]),axes=FALSE,col=couleurs,zlim=range(ticks))
	for(i in 1:n) for(j in 1:k) if(!is.na(coeff[j,i])) {
		if(pval[j,i]<.01)                            symb = "green"
		else if( (pval[j,i]<.05) & (pval[j,i]>=.01)) symb = "orange"
		else if( (pval[j,i]<.1)  & (pval[j,i]>=.05)) symb = "red"
		else                                         symb = "black"
		polygon(c(i+.5   ,i+.2   ,i+.5   ,i+.5),
		        c(k-j+0.5,k-j+0.5,k-j+0.8,k-j+0.5),
			col=symb)
		
		#points(i+.4,k-j+.6,pch=21,bg=symb)
		#text(i+.5,k-j+.8,round(pval[j,i],2),pos=2,cex=.8)
		if(j==choix)  {
			rect(xleft=i-.5,
			     xright=i+.5,
			     ybottom=k-j+1.5,
			     ytop=k-j+.5,
			     lwd=4)
			text(i,
			     k-j+1,
			     round(coeff[j,i],2),
			     cex=1.2,
			     font=2)
		}
		else{
			rect(xleft=i-.5,xright=i+.5,ybottom=k-j+1.5,ytop=k-j+.5)
			text(i,k-j+1,round(coeff[j,i],2),cex=1.2,font=1)
		}
	}
	axis(3,1:n,noms)


	par(mar=c(0.5,0,0,0.5))	
	plot(0,axes=FALSE,xlab="",ylab="",type="n",xlim=c(0,8),ylim=c(-.2,.8))
	cols <- c("green","orange","red","black")
	niv  <- c("0","0.01","0.05","0.1")
	for(i in 0:3){
		polygon(c(1+2*i   ,1+2*i   ,1+2*i-.5   ,1+2*i),
		        c(.4      ,.7      , .4        , .4),
			col=cols[i+1])
		text(2*i,0.5,niv[i+1],cex=1.5)	
		}
	text(8,.5,1,cex=1.5)
	text(4,0,"p-value",cex=2)
	box()
	
	residus <- x$listArima[[choix]]$res
	
	par(mar=c(1,2,4,1))
	acf(residus,main="")
	title("acf",line=.5)
	
	par(mar=c(1,2,4,1))
	pacf(residus,main="")
	title("pacf",line=.5)
	
	par(mar=c(2,2,4,1))
	qqnorm(residus,main="")
	title("qq-norm",line=.5)
	
}

