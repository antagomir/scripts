# Leo 11/2009
# Very nice hierarchical clustering visualizations
# See for more : http://ape.mpl.ird.fr/

require(ape)
tr <- rtree(10)
layout(matrix(1:9, 3, 3))
par(mar = rep(0, 4))
plot(tr)
plot(tr, "c")
plot(tr, "u")
plot(tr, "p", FALSE)
plot(tr, "c", FALSE)
plot(tr, "u", FALSE)
plot(tr, "r")
plot(tr, "f")
plot(tr, "f", FALSE) 


############

# For hclust results:
require(ape)
nrow = 30
ncol = 10
mat = array(rnorm(nrow*ncol),dim=c(nrow,ncol))
hc = hclust(dist(t(mat)))
tr = as.phylo(hc)
plot(tr, "f", FALSE)
plot(tr, "f", FALSE, edge.color = c(rep(1,5),rep(2,5)))

############

# Circular diagrams with ape
# http://cran.r-project.org/web/packages/ape/index.html
# kohtuullisen hyvä paketti, ainakin pienemmille aineistoille
# For screenshots, see
# http://ape.mpl.ird.fr/

library(ape)
#######################

# Layouts
tr <- rtree(10)
layout(matrix(1:9, 3, 3))
par(mar = rep(0, 4))
plot(tr)
plot(tr, "c")
plot(tr, "u")
plot(tr, "p", FALSE)
plot(tr, "c", FALSE)
plot(tr, "u", FALSE)
plot(tr, "r")
plot(tr, "f")
plot(tr, "f", FALSE)

########################

# Hclust + barplots

tr <- rcoal(30)
x <- rnorm(30)
plot(tr, x.lim = 5) # <- adjust x.lim
offset <- 4 # <- adjust offset
barplot(c(NA, x), add = TRUE, horiz = TRUE, space = 0, offset =  offset, axes = FALSE)
lab <- pretty(x)
axis(1, lab + offset, labels = lab)
# check that all is lined up:
abline(h=1:30, col = "red", lty = 3)

#The trick is to add NA to the x vector to lift the barplot by one 
#unit. This requires to adjust manually two parameters: x.lim and 
#offset. You may also adjust 'lab' and/or rescale x to make it look 
#nicer.

########################

# Other tools 

# iTol: monipuolinen (http://itol.embl.de/). Sitä ei voi suoraan käyttää R:stä

# SeqinR:  Sekvenssidatan käsittelyyn aika hyvä paketti 
#(http://cran.r-project.org/web/packages/seqinr/index.html).
