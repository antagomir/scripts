esort <- function(x, sortvar, ...) {
  #Sort data frame dd by columns like: esort(dd, -z, b)

  attach(x)
  x <- x[with(x,order(sortvar,...)),]
  return(x)
  detach(x)
}
