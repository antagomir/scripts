dta <- data.frame(val = sample(t,1000), g = gl(4, 250, labels=c("A", "B", "C", "D")) , G2 = gl(2,1, labels=c("XX", "YY")))
boxplot( val ~ G2 + g, data=dta)
boxplot( val ~ g + G2, data=dta, at = 0.8*c(1,2,3,4,6,7,8,9), boxwex=0.4)
