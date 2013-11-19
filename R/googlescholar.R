library(scholar)


id.leolahti <- "mjjV-AoAAAAJ"
leolahti <- get_profile(id.leolahti)
leolahti$name # Prints out his name

## Predict h-index
predict_h_index(id.leolahti)


# Compare multiple scholars
ids <- c(aklami = "v8PeLGgAAAAJ",
         #jparkki = "4d5VBfkAAAAJ",
	 #tsuvitai = "SGgP0VQAAAAJ",
	 leolahti = "mjjV-AoAAAAJ",
	 #jpaana = "wJS3f4wAAAAJ",
	 ihuopa = "LDs1AHkAAAAJ",
	 #croos = "7Nf13YoAAAAJ",
	 jsa = "GmWC5ZsAAAAJ",
	 #vos = "SbTINjMAAAAJ",
	 jhollmen = "E6RpqW8AAAAJ",
	 ali = "df7T2gwAAAAJ",
	 jpelto = "WFYU6DkAAAAJ"
	 )

# Compare their career trajectories, based on year of first citation
df <- compare_scholar_careers(ids)

library(ggplot2)
p <- ggplot(df, aes(x=career_year, y=cites)) 
p <- p + geom_line(aes(col=name)) + theme_bw()
print(p)
