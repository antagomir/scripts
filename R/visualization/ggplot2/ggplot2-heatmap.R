    difm <- melt(dif)
    df <- ddply(difm, .(X1), transform, rescale = scale(value))
    p <- ggplot(df, aes(X1, X2)) 
    p <- p + geom_tile(aes(fill = rescale), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
    base_size <- 9
    p <- p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) 
    p <- p + opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = "grey50"))

