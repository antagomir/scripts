#http://www.omegahat.org/RGoogleTrends/
#http://www.omegahat.org/RGoogleTrends/RGoogleTrends_0.2-1.tar.gz

library("RGoogleTrends")
g <- googleSignIn("lmlahti", "tahan.passu")
ans <- getGTrends("microbiota", curl = g)

