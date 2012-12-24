#http://www.omegahat.org/RGoogleTrends/
#http://www.omegahat.org/RGoogleTrends/RGoogleTrends_0.2-1.tar.gz

library("RGoogleTrends")
#option(GooglePassword = c(lmlahti = "ase1star11sunta"))
#ans <- getGTrends("Lahti", c(lmlahti = "ase1star11sunta"))
g <- googleSignIn(c(lmlahti = "ase1star11sunta"))
ans <- getGTrends("microbiota, curl = g)

