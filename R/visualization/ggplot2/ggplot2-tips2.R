
# Change ggplot2 background color
th = theme_bw()
th$panel.background
theme_rect(fill = "white", colour = NA)
th$panel.background = theme_rect(fill = "white", colour = NA)
theme_set(th)



# change the order in all dfs to be plotted
#df$X1 <- factor(df$X1, levels = rownames(mat))
#df$X2 <- factor(df$X2, levels = rev(colnames(mat)))



# Vaihda v‰rikoodin otsikko
#12:49 <@ouzo> labs(colour="otsikko")
#12:49 <@ouzo> scale_colour_discrete/gradient(name="otsikko")
#12:49 <@ouzo> labs(x="X", y="Y", colour="Colour") on k‰tsy jos fiksaa monta noista

library(gridExtra)
grid.arrange(piclist[[1]], piclist[[2]], nrow = 1)

####################################################



# Voit sit arrangeGrob:lla tai grid.arrange:lla yhdist‰‰ ne saamas plotit yhteen kuvaan (ton for-luupin j‰lkeen)

# Jata osa axis-labeleista pois
my_breaks <- seq(min(dfm$value), max(dfm$value), 1)
#my_labs <- seq(min(dfm$value), max(dfm$value), 1)
my_labs <- ggplot2::interleave(seq(min(dfm$value), max(dfm$value), 2), "")
scale_x_continuous(breaks=my_breaks, labels=my_labs, size = 2) + 

##########################################

# Custom theme like this:

theme_min = function (size=10, font=NA, face='plain', 
    panelColor=backgroundColor, axisColor='#999999', 
    gridColor=gridLinesColor, textColor='black') 
{
    theme_text = function(...)
        ggplot2::theme_text(family=font, face=face, colour=textColor, 
            size=size, ...)

opts(
    axis.text.x = theme_text(),
    axis.text.y = theme_text(),
    axis.line = theme_blank(),
    axis.ticks = theme_segment(colour=axisColor, size=0.25),
    panel.border = theme_rect(colour=backgroundColor),
    legend.background = theme_blank(),
    legend.key = theme_blank(),
    legend.key.size = unit(1.5, 'lines'),
    legend.text = theme_text(hjust=0),
    legend.title = theme_text(hjust=0),
    panel.background = theme_rect(fill=panelColor, colour=NA),
    panel.grid.major = theme_line(colour=gridColor, size=0.33),
    panel.grid.minor = theme_blank(),
    strip.background = theme_rect(fill=NA, colour=NA),
    strip.text.x = theme_text(hjust=0),
    strip.text.y = theme_text(angle=-90),
    plot.title = theme_text(hjust=0),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'lines'))
}

##Create a custom font type. Could be 'F', 'TEST', whatever
windowsFonts(F = windowsFont('Wide Latin'))

##and insert this line of code into the original code I list above: 
+ theme_min(font='F', size=10) 



# Wide variety of image tuning options:

ggplot(dat, aes(x = factor(time), y = volume)) + geom_boxplot() + geom_jitter(aes(colour = id)) + labs(x = "time", y = volume) + BaseThemeX90()

BaseThemeX90 <- function(base_size = 10) {

	structure(list(
					axis.line =         theme_blank(),
					axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, colour = "grey50", hjust = 1, angle = 90),
					axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, colour = "grey50", hjust = 1),
					axis.ticks =        theme_segment(colour = "grey50"),
					axis.title.x =      theme_text(size = base_size),
					axis.title.y =      theme_text(size = base_size, angle = 90),
					axis.ticks.length = unit(0.15, "cm"),
					axis.ticks.margin = unit(0.1, "cm"),
					
					legend.background = theme_rect(colour=NA), 
					legend.key =        theme_rect(fill = "grey95", colour = "white"),
					legend.key.size =   unit(1.2, "lines"),
					legend.text =       theme_text(size = base_size * 0.7),
					legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
					legend.position =   "right",
					
					panel.background =  theme_rect(fill = "grey90", colour = NA), 
					panel.border =      theme_blank(), 
					panel.grid.major =  theme_line(colour = "white"),
					panel.grid.minor =  theme_line(colour = "grey95", size = 0.25),
					panel.margin =      unit(0.25, "lines"),
					
					strip.background =  theme_rect(fill = "grey80", colour = NA), 
					strip.label =       function(variable, value) value, 
					strip.text.x =      theme_text(size = base_size * 0.8),
					strip.text.y =      theme_text(size = base_size * 0.8, angle = -90),
					
					plot.background =   theme_rect(colour = NA),
					plot.title =        theme_text(size = base_size * 1.2),
					plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
			), class = "options")


}


