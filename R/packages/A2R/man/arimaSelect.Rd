\name{arimaSelect}
\alias{arimaSelect}
\alias{plot.arimaSelect}
\title{selection for arima models}
\description{
 A graphic visualisation of the 
 backward selection steps in an arima model
  }
\usage{
arimaSelect(serie, order, include.mean = FALSE, alpha = 0.05)
}
\arguments{
  \item{serie}{ a time series }
  \item{order}{ see \link[stats]{arima} }
  \item{include.mean}{ see \link[stats]{arima} }
  \item{alpha}{ second order risk (default 0.05) }
}
\value{
An object of class arimaSelect (to be more detailled)
}
\references{ 
  Romain Francois (2005). Backward selection in arima models. 
  \emph{The R Graph Gallery}. \strong{29}.
  }
\author{
  Romain Francois }

\examples{
  
  \dontrun{
  data(fret)
  sel <- arimaSelect(fret,order=c(7,0,3))
  plot(sel,choix=4)
  }
}

\keyword{ ts }% at least one, from doc/KEYWORDS

