#https://stat.ethz.ch/pipermail/r-help/2008-May/161202.html
rgl.plot3d<-function(z, x, y, cols="red",axes=T,new=T)
{xr<-range(x)
x01<-(x-xr[1])/(xr[2]-xr[1])
yr<-range(y)
y01<-(y-yr[1])/(yr[2]-yr[1])
zr<-range(z)
z01<-(z-zr[1])/(zr[2]-zr[1])

if(new) rgl.clear()
if(axes)
	{xlab<-pretty(x)
	ylab<-pretty(y)
	zlab<-pretty(z)
	xat<-(xlab-xr[1])/(xr[2]-xr[1])
	yat<-(ylab-yr[1])/(yr[2]-yr[1])
	zat<-(zlab-zr[1])/(zr[2]-zr[1])
	rgl.lines(c(0,1.1),0,0)
	rgl.lines(0,c(0,1.1),0)
	rgl.lines(0,0,c(0,1.1))	
	rgl.texts(xat,-.05,-.05,xlab)
	rgl.texts(-.05,yat,-.05,ylab)
	rgl.texts(-.05,-.05,zat,zlab)
	rgl.texts(c(0.5,-.15,-.15),c(-.15,.5,-.15),c(-.15,-.15,.5),
		c(deparse(substitute(x)),deparse(substitute(y)),deparse(substitute(z))))
	}

rgl.spheres(x01,y01,z01,.01,color=cols)
}

#and here is how you call it
library(rgl)
data(iris)

iris.pc<-prcomp(iris[,1:4],scale=T)
rgl.plot3d(iris.pc$x[,1],iris.pc$x[,2],iris.pc$x[,3])
# different colors
rgl.plot3d(iris.pc$x[,1],iris.pc$x[,2],iris.pc$x[,3],col=unclass(iris[,5])+1)
