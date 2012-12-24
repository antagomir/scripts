
# Multiple ggplot2 images in one window
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

arrange <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
 dots <- list(...)
 n <- length(dots)
 if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
 if(is.null(nrow)) { nrow = ceiling(n/ncol)}
 if(is.null(ncol)) { ncol = ceiling(n/nrow)}
        ## NOTE see n2mfrow in grDevices for possible alternative
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
  ii.p <- 1
  for(ii.row in seq(1, nrow)){
  ii.table.row <- ii.row 
  if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
    for(ii.col in seq(1, ncol)){
      ii.table <- ii.p
      if(ii.p > n) break
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
      ii.p <- ii.p + 1
    }
  }
}


theme_L_border <- function(colour = "black", size = 1, linetype = 1) {
  # use with e.g.: ggplot(...) + opts( panel.border=theme_L_border() ) + ...
  structure(
    function(x = 0, y = 0, width = 1, height = 1, ...) {
      polylineGrob(
        x=c(x+width, x, x), y=c(y,y,y+height), ..., default.units = "npc",
        gp=gpar(lwd=size, col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}

theme_bottom_border <- function(colour = "black", size = 1, linetype = 1) {
  # use with e.g.: ggplot(...) + opts( panel.border=theme_bottom_border() ) + ...
  structure(
    function(x = 0, y = 0, width = 1, height = 1, ...) {
      polylineGrob(
        x=c(x, x+width), y=c(y,y), ..., default.units = "npc",
        gp=gpar(lwd=size, col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}

theme_left_border <- function(colour = "black", size = 1, linetype = 1) {
  # use with e.g.: ggplot(...) + opts( panel.border=theme_left_border() ) + ...
  structure(
    function(x = 0, y = 0, width = 1, height = 1, ...) {
      polylineGrob(
        x=c(x, x), y=c(y,y+height), ..., default.units = "npc",
        gp=gpar(lwd=size, col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}

theme_border_numerictype <- function(type, colour = "black", size = 1, linetype = 1) {
  # use with e.g.: ggplot(...) + opts( panel.border=theme_border(type=9) ) + ...
  structure(
    function(x = 0, y = 0, width = 1, height = 1, ...) {
      # numerical types from: library(gridExtra); example(borderGrob)
      # 1=none, 2=bottom, 3=right, 4=top, 5=left, 6=B+R, 7=T+R, 8=T+L, 9=B+L, 10=T+B, 11=L+R, 12=T+B+R, 13=T+L+R, 14=T+B+L, 15=B+L+R, 16=T+B+L+R
      xlist <- c()
      ylist <- c()
      idlist <- c()
      if (type==2 || type==6 || type==9 || type==10 || type==12 || type==14 || type==15 || type==16) { # bottom
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y, y))
        idlist <- append(idlist, c(1,1))
      }
      if (type==4 || type==7 || type==8 || type==10 || type==12 || type==13 || type==14 || type==16) { # top
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y+height, y+height))
        idlist <- append(idlist, c(2,2))
      }
      if (type==5 || type==8 || type==9 || type==11 || type==13 || type==14 || type==15 || type==16) { # left
        xlist <- append(xlist, c(x, x))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(3,3))
      }
      if (type==3 || type==6 || type==7 || type==11 || type==12 || type==13 || type==15 || type==16) { # right
        xlist <- append(xlist, c(x+width, x+width))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(4,4))
      }
      if (type==1) { # blank; can't pass absence of coordinates, so pass a single point and use an invisible line
        xlist <- c(x,x)
        ylist <- c(y,y)
        idlist <- c(5,5)
        linetype <- "blank"
      }
      polylineGrob(
        x=xlist, y=ylist, id=idlist, ..., default.units = "npc",
        gp=gpar(lwd=size, col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}

theme_border <- function(type = c("left", "right", "bottom", "top", "none"), colour = "black", size = 1, linetype = 1) {
  # use with e.g.: ggplot(...) + opts( panel.border=theme_border(type=c("bottom","left")) ) + ...
  type <- match.arg(type, several.ok=TRUE)
  structure(
    function(x = 0, y = 0, width = 1, height = 1, ...) {
      xlist <- c()
      ylist <- c()
      idlist <- c()
      if ("bottom" %in% type) { # bottom
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y, y))
        idlist <- append(idlist, c(1,1))
      }
      if ("top" %in% type) { # top
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y+height, y+height))
        idlist <- append(idlist, c(2,2))
      }
      if ("left" %in% type) { # left
        xlist <- append(xlist, c(x, x))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(3,3))
      }
      if ("right" %in% type) { # right
        xlist <- append(xlist, c(x+width, x+width))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(4,4))
      }
      if (length(type)==0 || "none" %in% type) { # blank; can't pass absence of coordinates, so pass a single point and use an invisible line
        xlist <- c(x,x)
        ylist <- c(y,y)
        idlist <- c(5,5)
        linetype <- "blank"
      }
      polylineGrob(
        x=xlist, y=ylist, id=idlist, ..., default.units = "npc",
        gp=gpar(lwd=size, col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}

# Examples:
# library(ggplot2)
# df = data.frame( x=c(1,2,3), y=c(4,5,6) )
# ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() + opts( panel.border = theme_border_numerictype(9) ) 
# ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() + opts( panel.border = theme_border(c("bottom","left")) ) 
