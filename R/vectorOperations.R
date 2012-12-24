
interpolate <- function (v) {

  # Fill start and end if NAs (using the first real value from each end)
  if (is.na(v[[1]])) {ind <- which(!is.na(v))[[1]]; v[1:(ind-1)] <- v[[ind]]}
  if (is.na(v[[length(v)]])) {ind <- rev(which(!is.na(v)))[[1]]; v[(ind+1):length(v)] <- v[[ind]]}

  nas <- which(is.na(v))
  ll <- locs[!is.na(v)]
  vals <- v[!is.na(v)]
  ints <- vector(length = length(nas))
  for (j in 1:length(nas)) {
    ind <- nas[[j]]
    loc.na <- locs[[ind]]
    i1 <- max(which(loc.na > ll))
    i2 <- min(which(loc.na < ll))
    d1 <- loc.na - ll[[i1]]
    d2 <- ll[[i2]] - loc.na
    s1 <- vals[[i1]]
    s2 <- vals[[i2]]
    ints[[j]] <- sum(c(d1, d2) * c(s1, s2))/sum(c(d1, d2))
  }
  v[nas] <- ints

  v
}


get.ranks <- function (v, decreasing = FALSE) {
 # (C) Leo Lahti 2011. License: FreeBSD.

  # if values in v are identical give them also identical rank
  # this is to complement 'order' function which does not take into
  # account identical values
  # leave out NAs	
  v.unique.sorted <- sort(unique(na.omit(v)), decreasing = decreasing, na.last = TRUE)
  o <- match(v, v.unique.sorted)
  names(o) <- names(v)
  o
	    
}
	    
	    

slideAverage <- function (dat,interval,step=1) {
	#Compute sliding average over observations
	#dat: 1D vector
	#interval defines the radius for computing the average
	#step defines the distance between consecutive center points for the radius
	slidemean<-c()
	for (ind in seq(interval+1,length(dat)-interval,step)) {
		slidemean<-c(slidemean,mean(dat[(ind-interval):(ind+interval)]))
	}
	slidemean
}


slideAverage2 <- function (dat,interval,step=1) {
	#Compute sliding average over observations
	# also return side results, e.g. step centers
	#dat: 1D vector
	#interval defines the radius for computing the average
	#step defines the distance between consecutive center points for the radius
	slidemean<-c()
	stepcenters <- seq(interval+1,length(dat)-interval,step)
	for (ind in stepcenters) {
		slidemean<-c(slidemean,mean(dat[(ind-interval):(ind+interval)]))
	}
	list(slidemean=slidemean,stepcenters=stepcenters)
}

unitize <- function(vec) {
  # normalize vector to unit length
  u <- vec/sqrt(sum(vec^2))
  names(u) <- names(vec)
  u

}

pick.continuous.regions <- function (v, min.dist = 1, return.names = FALSE) {

  # Given integer vector of
  # ordered values, list the continuou (i.e. 5,6,7,8; 21,22,23; etc)
  # stretches

  # Detect breakpoints (include also the first and last point if it is not in the list)
  bps <- unique(c(0, which(diff(v) > min.dist), length(v)))

  elements <- list()
  for (i in 1:(length(bps)-1)) {
    sp <- bps[[i]] + 1
    ep <- bps[[i+1]]
    elements[[i]] <- v[sp:ep]

  }

  if (return.names) {elements <- lapply(elements, names)}
  
  elements
  
}

