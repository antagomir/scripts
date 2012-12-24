\name{A2Rplot.hclust}

\alias{A2Rplot.hclust}

\title{Customised plot function for class hclust}

\description{The A2Rplot method for class hclust can be used
  to set different colors to each cluster for a given number
  of classes (\code{k}) ...}

\synopsis{
  A2Rplot.hclust(x, k=2, col.up="black", col.down=rainbow(k), 
                 lty.up = 2, lty.down=1, lwd.up=1, lwd.down=2,
                 type     = c("rectangle","triangle"),
                 knot.pos = c("mean","bary","left","right","random")
}

\arguments{
  \item{x}{An \link[stats]{hclust} object to plot}
  \item{k}{The number of clusters}
  \item{col.up}{Color for the upper part. See \code{\link[grDevices]{colors}}}
  \item{col.down}{Vector of colors of length k. One colour per cluster. 
    Default to \code{\link[grDevices]{rainbow}(k)}}
  \item{lty.up}{line type for the upper part. See \code{\link[graphics]{par}}.}
  \item{lty.down}{line type for the clusters part. See \code{\link[graphics]{par}}.}
  \item{lwd.up}{line width for the upper part. See \code{\link[graphics]{par}}.}
  \item{lwd.down}{line width for the clusters part. See \code{\link[graphics]{par}}.}
  \item{type}{type of link. \code{"rectangle"} or \code{"triangle"}.}
  \item{knot.pos}{position of the knots :  
    \code{"mean"} mean between the two tree sons,
    \code{"bary"} \emph{weighted} mean relative to the 
       number of observations in the left and the right branch,
    \code{"left"}, \code{"right"},
    \code{"random"} just for fun actually
     }
  \item{criteria}{Vector of a criteria to draw on the left of the tree.}
  \item{fact.sup}{A factor to categorize the observations.}
  \item{show.labels}{boolean. TRUE if the labels should be drawn.}
  \item{only.tree}{boolean. TRUE if only the tree should be drawn. Use that to
    include the tree in a more complicated layout.}
  \item{boxes}{boolean. TRUE to draw the bow around the plots}
  \item{members}{members of each terminal node. See \code{\link[stats]{hclust}} for more details.}
}

\seealso{\code{\link{A2Rplot}}, \code{\link[stats]{plot.hclust}}}
      
\author{Romain Francois \email{francoisromain@free.fr}}

\section{R Graph Gallery}{
  \url{http://addictedtor.free.fr/graphiques/RGraphGallery.php?graph=98}
  \url{http://addictedtor.free.fr/graphiques/RGraphGallery.php?graph=79}  
  }

\examples{    
  #example with iris
  require("fpc") # to use the cluster.stats function
  d <- dist(iris[,1:4],method="euc")
  h <- hclust(d)
  Species <- iris[,5]
  cs <- rep(0, 10)
  for(i in 1:11){
    cs[i] <- cluster.stats(d, cutree(h,k=i+1), 
                           silhouette=FALSE, 
                           G2=FALSE, G3=FALSE)$hubertgamma
  }
  
  A2Rplot(h, k=3, fact.sup=Species, knot.pos="bary", criteria=cs, show.labels=FALSE)
  
  
  # examples with state.x77
  d77 <- dist(state.x77)
  h77 <- hclust(d77)
  A2Rplot(h77, k=4, knot.pos="mean", type="tri")
  A2Rplot(h77, k=4, lty.up=1,lwd.down=1,
          col.down=c("purple","black","green3","orange"),
          col.up="gray", boxes=FALSE)
          
  A2Rplot(h77, k=4, knot.pos="left", type="tri")
  
  
  # example showing how to include this in 
  # an other layout with only.tree
  op <- par(no.readonly=TRUE)
  par(mfrow = c(3,3))
  par(mar=c(3,3,3,3))  
  plot(rnorm(50)) # one plot
  plot(rnorm(50)) # one plot
  plot(rnorm(50)) # one plot
  plot(rnorm(50)) # one plot
  par(mar=c(1,1,1,1))
  A2Rplot(h77, k=4, only.tree=TRUE, boxes=FALSE)
  par(mar=c(3,3,3,3))  
  plot(rnorm(50)) # one plot
  plot(rnorm(50)) # one plot
  plot(rnorm(50)) # one plot
  plot(rnorm(50)) # one plot
  par(op)

  ## example using members
  
  #--------------------- from stats::hclust help
  hc <- hclust(dist(USArrests)^2, "cen")
  memb <- cutree(hc, k = 10)
  cent <- NULL
  for(k in 1:10){
    cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
    }
  hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
  #-----------------------------------------------------------
  hc1$labels <- paste('g',1:10)
  A2Rplot(hc1, 
         members = table(memb), 
         k=4, 
         lwd.up   =  2, lty.up=1, col.up = "gray",
         lwd.down =  1, lty.down='twodash', 
         col.down = c("orange", "brown", "green3", "royalblue"),
         knot.pos = "bary"
         )

  
}

\keyword{hplot}
\keyword{cluster}
