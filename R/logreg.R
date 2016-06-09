# Different ways to visualize log reg

par(mfrow = c(3,4))
for (tax in as.character(tab.final$tax)) {
  d <- cbind(sampleinfo.full, meta.full)
  d$abundance <- dat.full[tax,]
  fit <- glm(group ~ abundance, family = binomial(logit), data = d)

  # Log reg curve with response histograms
  #library(popbio)
  #logi.hist.plot(d$abundance, as.numeric(d$group)-1, boxp=FALSE, type="hist", col="gray", main = tax)

  # Standard curve with rugs
  #plot(d$abundance, as.numeric(d$group)-1, xlab="abundance", ylab="Disease Probability", main = tax, las = 1, pch = "|", cex = 2)
  #curve(predict(fit, data.frame(abundance=x), type="resp"), add=TRUE, col="red")
  
  # Log odds curve
  library(visreg)
  #visreg(fit, "abundance", xlab=paste(tax),ylab="Log odds (disease)", rug = 2)
  # Response probabilities
  # Wald conﬁdence intervals based on standard errors returned by predict.glm
  visreg(fit, "abundance", scale="response", partial=FALSE, xlab= tax,ylab="P(disease)", rug = 2)
}

# ----------------------------------------

