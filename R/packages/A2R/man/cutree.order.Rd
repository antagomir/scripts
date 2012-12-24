\name{cutree.order}
\alias{cutree.order}

\title{Cut a tree into groups of data in the order of the tree}

\description{Cut a tree (result 
             from \code{\link[stats]{hclust}}) into groups
             of data. Groups are in the order of the tree 
             leafs}

\synopsis{
  cutree.order(hclu, k=NULL, h=NULL)
}

\arguments{
  \item{hclu}{An \code{\link[stats]{hclust}} object}
  \item{k}{A scalar with the desired number of groups}
  \item{h}{A scalar with height where the tree is to be cut}
}

\seealso{
  \code{\link[stats]{cutree}}, \code{\link{A2Rplot.hclust}}
}

\examples{
  d77 <- dist(state.x77)
  h77 <- hclust(d77)
  (ct.o <- cutree.order(h77, k=4))
  (ct.n <- cutree(h77,k=4))
  
  cbind(ct.o, ct.n)
}

\keyword{cluster}
