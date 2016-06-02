# Maakunnat suomi-englanti
tab <- read.csv("../data.sorvi.maakunnat.suomi.english.txt", "\t", header = FALSE)
fi <- as.character(tab[,1])
en <- as.character(tab[,2])
names(fi) <- en
fi.en.maakunnat <- fi
save(fi.en.maakunnat, file = "~/tmp/translations.rda")
