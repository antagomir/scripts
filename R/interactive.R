# SVG / D3 / R / Interactive
# http://timelyportfolio.github.io/gridSVG_intro/

    #get the latest version of gridSVG
    #install.packages("gridSVG", repos="http://R-Forge.R-project.org")
    require(ggplot2)
    set.seed(955)
    # Make some noisily increasing data
    dat <- data.frame(cond = rep(c("A", "B"), each=10),
    xvar = 1:20 + rnorm(20,sd=3),
    yvar = 1:20 + rnorm(20,sd=3))
    # cond xvar yvar
    # A -4.252354091 3.473157275
    # A 1.702317971 0.005939612
    # ...
    # B 17.793359218 19.718587761
    # B 19.319909163 19.647899863
    g4 <- ggplot(dat, aes(x=xvar, y=yvar)) +
    geom_smooth() + #we'll see why order is important
    geom_point(shape=19, aes(color = cond), size=5)
    g4

    require(gridSVG)
    #print our ggplot2 graphic again
    g4
    #export to SVG file and R object
    #grid.export deprecates the older gridToSVG
    g4.svg <- grid.export("plot1.svg",addClasses=TRUE)
    #print our newly exported SVG inline
    cat(saveXML(g4.svg$svg))
