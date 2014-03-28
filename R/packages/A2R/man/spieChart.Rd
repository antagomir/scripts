\name{spie}

\alias{spie}
\alias{plot.spie}

\title{Spie charts}
\description{Comparing partitions with Spie Charts}

\synopsis{
  spie(firstPartition, secondPartition)
  plot.spie(x, multi, col=rainbow(length(x$radii)),...)
  }

\arguments{
  
  \item{firstPartition}{a vector of positive quantities. 
    The values in \code{firstPartition} are displayed 
    as the areas of the first pie (the one with radius one).}
  \item{secondPartition}{a vector of positive quantities of same
    length than \code{firstPartition}. 
    Angles are the same than those used for the first pie but
    radii change according to the values in \code{secondPartition}.}
  \item{multi}{A vector of multipliers. Circles with radii
    proportionnal to the square root of multi are displayed.}
  \item{col}{Colors of the slices}
  \item{...}{Not used}
  }

\seealso{
  \code{\link[stats]{pie}}
  }

\author{Romain Francois \email{francoisromain@free.fr}}

\references{
  D. G. Feitelson (2003), "Comparing Partitions with Spie Charts". 
  School of Computer Science and Engineering, 
  The Hebrew University of Jerusalem. 
  \url{http://www.cs.huji.ac.il/~feit/papers/Spie03TR.pdf}
  
  Michael Friendly (2000), Gallery of Data Vizualisation :
  The Best and Worst of Statistical Graphics.
  \url{http://www.math.yorku.ca/SCS/Gallery/}
  }

\examples{
  
  pie.sales  <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
  pie.sales2 <- c(0.06, 0.15, 0.52, 0.14, 0.08, 0.05)
  
  spie.sales <- spie(pie.sales, pie.sales2)
  plot(spie.sales, multi=c(.5, 1, 1.5, 2))
  
}

\keyword{hplot}
