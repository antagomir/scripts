"spie" <- function(firstPartition, secondPartition){
  
  if(length(firstPartition)!=length(secondPartition)) 
    stop("'firstPartition' and 'secondPartition' have different lengths")
  
  if(sum(firstPartition)!=sum(secondPartition))
    stop("'firstPartition' and 'secondPartition' doesn't sum the same")
  
  angles <- cumsum(c(0, 2 * pi * firstPartition / sum(firstPartition)))
  
  radii  <- sqrt( (secondPartition / sum(secondPartition)) /
                  (firstPartition  / sum(firstPartition)) )
  
  namesSlices <- if(is.null(names(firstPartition))) 
                   1:length(firstPartition) 
                 else 
                   names(firstPartition)  
  
  structure(list(angles=angles, 
                 radii=radii, 
                 firstPartition=firstPartition,
                 secondPartition=secondPartition, 
                 namesSlices = namesSlices), 
            class="spie")
}

"plot.spie" <- 
  function(
  x, 
  multi, 
  col = rainbow(length(x$radii)),
  ...){
  
    maxRadii <- max(x$radii)
  
  grid.newpage()
  
  pushViewport(viewport(layout=grid.layout(1,1,respect=TRUE)))
  pushViewport(dataViewport(maxRadii*c(-1.1,1.1), 
                            maxRadii*c(-1.1,1.1),
                            layout.pos.col=1,
                            layout.pos.row=1))
    
  
  if(!missing(multi)){
    
      grid.circle(x=0, y=0, r=sqrt(multi), gp=gpar(col="gray"), default.units="native")
    
  
  }  
  
  
  for(i in 1:length(x$radii)){
    
    theta <- seq(x$angles[i], x$angles[i+1], length=100)
    
    grid.polygon(x   = c(0,  cos(theta) ,0),
                 y   = c(0,  sin(theta) ,0) , 
                 gp  = gpar(fill=col[i]),
                 default.units="native")    

    grid.polygon(x   = c(0, x$radii[i] * cos(theta) ,0),
                 y   = c(0, x$radii[i] * sin(theta) ,0) , 
                 gp  = gpar(fill=col[i], lwd=2),
                 default.units="native")    
           
                 
    angleAnn <- mean(x$angles[i+0:1])
    maxx <- max(1, x$radii[i])+maxRadii/10
    
    
    grid.rect( x = cos(angleAnn)*maxx,
               y = sin(angleAnn)*maxx, 
               width = 1.5*stringWidth(x$namesSlices[i]),
               height = 1.5*stringHeight(x$namesSlices[i]),
               default.units="native",
               gp = gpar(col=col[i], fill="white", lwd=2))

    
    grid.text(x$namesSlices[i], 
              x=cos(angleAnn)*maxx, 
              y=sin(angleAnn)*maxx,
              default.units="native")
    
  }
  
  
  
  
  if(!missing(multi)){ 
    
      grid.lines(x=unit(0,"native"), 
                 y=unit(c(0, max(sqrt(multi))), "native"), gp=gpar(col="gray"))
    
    for(i in multi){
      st <- paste("x", i)
      sw <- stringWidth(st)
      sh <- stringHeight(st)
      

      grid.rect( x = unit(0, "native"),
               y = unit(sqrt(i),"native"), 
               width = 1.5*sw,
               height = 1.5*sh , gp=gpar(fill="white", col="gray"))
               
      grid.text( st , 0, sqrt(i), default.units="native")
      
    }
    
  }
  
  upViewport(2)
 
}
