library(mia)
library(dplyr)
library(tidyr)

# Get example data
library(microbiomeDataSets)
# tse <- LahtiMData() 
x <- tse

# Agglomerate to Genus level
x <- agglomerateByRank(x, "Genus")

# CLR transform
x <- transformSamples(x, method="clr", abund_values="counts", pseudocount=1)

# Specify one taxonomic group to look at 
tax <- "Dialister"

# Speficy colData fields that determine the condition and sample pairs
pair_field <- "subject"
group_field <- "time"

# Pick abundance signal
colData(x)$Signal <- assay(x, "clr")[tax,]

# Rename the pair and group fields for routine handling
colData(x)$pair <- colData(x)[[pair_field]]
colData(x)$group <- as.factor(colData(x)[[group_field]])
colData(x)$sample <- rownames(colData(x))

# Indicate increase/decrease in signal
cdat <- colData(x) %>% as.data.frame() %>%
		       arrange(group) %>%
		       group_by(pair) %>%
		       mutate(change=diff(Signal)) %>%
		       DataFrame()
rownames(cdat) <- as.character(cdat$sample)

# Pick change to colData
# (note that row sorting had changed in cdat so we need to pick rows by name)
colData(x)$change <- cdat[rownames(colData(x)), "change"]

# Arrange to case-control pairs and rough check significance 
tab <- colData(x) %>% as.data.frame() %>%
                        dplyr::select(pair, group, Signal) %>%
			pivot_wider(names_from="group", values_from="Signal")
colnames(tab) <- c("pair", "control", "case")			
tab <- tab %>% mutate(Difference=case-control)			
			
# Table
knitr::kable(tab,digits=1)

library(scater)
p <- plotColData(x, "Signal", x="group") +
       geom_line(data=as.data.frame(colData(x)),
                   aes(x=group, y=Signal, group=pair, color=change)) +
       scale_color_gradient2(midpoint=0,
                             low="blue", mid="white", high="red",
                             space ="Lab") +
       labs(title=paste(tax),
            color="Change",
	    y="Value",
	    ) +
       geom_jitter(width=0) +
       theme_bw(20)
print(p)       
