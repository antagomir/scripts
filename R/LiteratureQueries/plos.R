install.packages("rplos")
require(rplos)

out <- plosword(list('monkey','Helianthus','sunflower','protein'), key = )
out$plot

plot_throughtime(list('drosophila','monkey'), 500)

plot_throughtime(list('drosophila','monkey'), 500, gvis = 'TRUE')

