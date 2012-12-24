# 
# Leo Lahti (2009)
# License: Artistic 2.0
# http://www.opensource.org/licenses/artistic-license-2.0.php
#

# Produce html annotation for your probesets.
# Example with probesets from hgu95av2 platform:

library("annaffy")
library("annotate")
data(aafExpr)
anncols <- c("Probe", "Symbol", "Description", "Cytoband", "PubMed", "Pathway")
probeids <- featureNames(aafExpr)[1:3]
anntable <- aafTableAnn(probeids, "hgu95av2.db", anncols)
saveHTML(anntable, "ex1.html", title = "Example Table without Data")
