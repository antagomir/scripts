PF$factors  # This summarizes what happened at each factor. The first factor split a 40-member monophyletic clade from a 250-member monophylic clade.
### Phylofactorization is based on regression, so the output contains a series of glms corresponding to the regressions on ILR transforms along edges in the tree that maximized our choice function.
### We can summarize those like this:
coefs <- matrix(sapply(PF$glms,FUN=function(gg) gg$coefficients),ncol=2,byrow=T)
colnames(coefs) <- names(PF$glms[[1]]$coefficients)
cbind(PF$factors,coefs)

print("What happened in the first factor?") #####
FactorSummary <- phylofactor.summary(PF,Taxonomy,factor=1)
td <- pf.tidy(FactorSummary)
td$`group1, Monophyletic`
td$`group2, Monophyletic`
# Here, we see that Actinobacteria, and Proteobacterial classes alpha, beta, gamma and delta were split from Bacteroidetes, Firmicutes, Fusobacteria, Verrucomicrobia, and Proteobacterial class Epsilonproteobacteria.
# Before going further, let's look at some other features of our phylofactor object.

print("PhyloFactor is a dimensionality-reducing technique. It allows us to collapse the tree into mostly")
# monophyletic groups which maximize our objective function. The default above is choice='var', which# minimizes residual variance.
#[LL] plot(PF$ExplainedVar)
#[LL] plot(cumsum(PF$ExplainedVar))
min(which(cumsum(PF$ExplainedVar)>0.1))
#this is an output when choice='var'. It is the ratio of residual to total variance in the clr-transformed data when regressing out each clade.
# Another option is choice='F' to maximize the F-statistic for regression on the ilr coordinate (i.e. the corrected ratio of explained to residual variance; as opposed to choice='var' which maximizes the difference between the null deviance and the residual deviance), which
# extracts clades whose own variation can be best predicted with by the formula (input frmla) and independent variable X.
# New objective functions will be incorporated over time. 

print("Another way of looking at this factorization is by a feature of")
# PhyloFactor's output: bins.  bins are the groups which remain
# unsplit by the factors. Predictions of OTU abundances for a given
# phylofactorization will be equal for all OTUs within the same bin.
NewOTUs <- PF$bins
binsize <- unlist(lapply(NewOTUs,FUN = length))
# Notice: there is one more bin than the nclades=4 inut above. That's becuase we've chosen 10 clades,
# and an additional attom is all the remaining taxa not split by the previous 10 clades.

PF$terminated
### Phylofactor stopped early...
PF$nfactors
### and identified 127 factors
PF$bin.sizes
### From here, we see that 104 bins are singletons, and the rest are in clades.

print("which bins are monophyletic?")
Monophyletic <- which(names(PF$bins)=='Monophyletic')
### 106 of these 127 bins are monophyletic.
Monophyletic.clades <- PF$Monophyletic.clades
### There were 46 clades identified, 11 of which are monophyletic.


################################# how this comapred to MHT ###################
binsize <- PF$bin.sizes

print("##### Which bins are single OTUs identified as significant? #############")
##### and how does that compare to phylofactor's resluts? ################
SingleOTUs <- rownames(OTUTable)[unlist(NewOTUs[binsize==1])]
sum(sigOTUs.BF %in% SingleOTUs)/length(sigOTUs.BF)
sum(SingleOTUs %in% sigOTUs.BF)/length(SingleOTUs)
## 45% of the significant OTUs in a Bon-Ferroni correction were identified as singletons in phylfactorization.
## The rest of the OTUs identified by BF corrections are best modelled as being in clades.
## 82% of the Single OTUs in our bins were identified with the BF
sum(sigOTUs.Q %in% SingleOTUs)/length(sigOTUs.Q)
sum(SingleOTUs %in% sigOTUs.Q)/length(SingleOTUs)
## 37.7% of the significant OTUs in a Bon-Ferroni correction were identified as singletons in phylfactorization.
## The rest of the OTUs identified by BF corrections are best modelled as being in clades.
## 91% of our singletons in phylofactorizatoin were identified as significant when using MHT at an FDR of 1%


print("######### These are the taxa in our bins: #############################")
### The following function extracts the taxonomic IDs for all the elements of each bin.
Taxa <- phylofactor.TaxaIDs(PF,Taxonomy,tree,nfactors=PF$nfactors,common.name=T,uniques=T)
### Let's look at the taxonomic description of the taxa which were not Single-OTUs:
Taxonomy.of.clades <- Taxa[Monophyletic.clades]
zz=unlist(Taxonomy.of.clades)
names(zz)=NULL
zz

print("### The main, monophyletic clades distinguishing the gut from the tongue are:")
# [1] "k__Bacteria; p__Firmicutes; c__Bacilli; o__Lactobacillales; f__Carnobacteriaceae; g__"                                       
# [2] "k__Bacteria; p__Proteobacteria; c__Epsilonproteobacteria; o__Campylobacterales; f__Campylobacteraceae; g__Campylobacter; s__"
# [3] "k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__Veillonellaceae; g__"                                        
# [4] "k__Bacteria; p__Bacteroidetes; c__Bacteroidia; o__Bacteroidales; f__[Paraprevotellaceae]; g__[Prevotella]; s__"              
# [5] "k__Bacteria; p__Bacteroidetes; c__Flavobacteriia; o__Flavobacteriales; f__"                                                  
# [6] "k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__Lachnospiraceae; g__Lachnospira; s__"                        
# [7] "k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__"                                                            
# [8] "k__Bacteria; p__Actinobacteria; c__Actinobacteria; o__Actinomycetales; f__Micrococcaceae; g__Rothia; s__"                    
# [9] "k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__Lachnospiraceae; g__; s__"                                   
# [10] "k__Bacteria; p__Fusobacteria; c__Fusobacteriia; o__Fusobacteriales; f__Fusobacteriaceae; g__Fusobacterium; s__"              
# [11] "k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__Lachnospiraceae; g__; s__" 



print("######## And their preferred environments: #############################")
predictedData <- phylofactor.predict(PF,newdata=data.frame(X=c('feces','tongue')))
rownames(predictedData) <- rownames(OTUTable)
colnames(predictedData) <- c('feces','tongue')
phylo.heatmap(tree,t(clr(t(predictedData))))


atms <- PF$bins
binsamples <- lapply(atms,FUN = function(x) x[1])
FecalTongueratios <- unlist(lapply(binsamples,FUN= function(x,predictedData) predictedData[x,'feces']/predictedData[x,'tongue'],predictedData=predictedData))
names(FecalTongueratios) <- unlist(lapply(as.list(1:length(atms)),FUN = function(x,y) paste(y,x),y='bin'))
sort(FecalTongueratios)

Taxa <- phylofactor.TaxaIDs(PF,Taxonomy,tree,nfactors=PF$nfactors,common.name=T)
Taxonomy.of.clades <- Taxa[Monophyletic.clades]
data.frame('est.Fecal.Tongue.ratio'=unlist(FecalTongueratios[Monophyletic.clades]),'Clade.Taxonomy'=unlist(Taxonomy.of.clades))

#               est.Fecal.Tongue.ratio                                                                      Clade.Taxonomy
# bin 3              0.12046926                                        k__Bacteria; p__Firmicutes; c__Bacilli; o__Lactobacillales; f__Carnobacteriaceae; g__
# bin 7              0.01900048 k__Bacteria; p__Proteobacteria; c__Epsilonproteobacteria; o__Campylobacterales; f__Campylobacteraceae; g__Campylobacter; s__
# bin 9              0.10137152                                         k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__Veillonellaceae; g__
# bin 10             0.13342493               k__Bacteria; p__Bacteroidetes; c__Bacteroidia; o__Bacteroidales; f__[Paraprevotellaceae]; g__[Prevotella]; s__
# bin 11             0.06569599                                                   k__Bacteria; p__Bacteroidetes; c__Flavobacteriia; o__Flavobacteriales; f__
# bin 13             2.09950006                         k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__Lachnospiraceae; g__Lachnospira; s__
# bin 15             3.03992358                                                             k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__
# bin 23             0.05837176                     k__Bacteria; p__Actinobacteria; c__Actinobacteria; o__Actinomycetales; f__Micrococcaceae; g__Rothia; s__
# bin 28             1.16264678                                    k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__Lachnospiraceae; g__; s__
# bin 30             0.09964988               k__Bacteria; p__Fusobacteria; c__Fusobacteriia; o__Fusobacteriales; f__Fusobacteriaceae; g__Fusobacterium; s__
# bin 31             1.00897201                                    k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__Lachnospiraceae; g__; s__



print("################ how to project our data using Phylofactor? #####################")
BPU <- binProjection(PF,prediction = F)
Reduced_OTUTable <- BPU$Data
# This can be used to factor any new dataset according to old phylofactors, provided the rows of the two datasets correspond to the same OTUs.

print("### Let's do some ordination-visualization:")
phylofactor.visualize(PF)
phylofactor.visualize(PF,dimension=3)


print("### here's what happens with ordination-visualization: we project the data into ILR space:")
OTUTable_3 <- phylofactor.ILRprojection(PF,nfactors=2)


print("## And then plot the ILR coorindates.")
legend(0,2,legend=as.list(unique(X)))
par(mfrow=c(1,1))
par(mar=c(5,4,4,1))
lms <- c(min(OTUTable_3),max(OTUTable_3))
plot(OTUTable_3[1,X=='feces'],OTUTable_3[2,X=='feces'],col='brown',xlab='PF 1',ylab='PF 2',pch=17,xlim=lms, ylim=lms,main='Phylofactor Ordination-Visualization')
points(OTUTable_3[1,X=='tongue'],OTUTable_3[2,X=='tongue'],col='green',pch=19)
legend(x=-5,y=5,legend = list('Feces','Tongue'),col=c('brown','green'),pch=c(17,19))


print("################# More summary of  TAXONOMIC INFORMATION OF OUR PHYLOFACTORS #################")
#what are the bins for a lower-dimensional factorization?
nfactors <- 3

#[LL] atms <- bins(PF$basis[,factors])              #contains list taxa in each bin formed by the partitions in "clades"
Taxa <- phylofactor.TaxaIDs(PF,Taxonomy,tree,nfactors=nfactors)  # The default doesn't trim the taxonomic IDs into the longest common prefix within bins. common.name=T will do that.
Taxa <- phylofactor.TaxaIDs(PF,Taxonomy,tree,nfactors=nfactors,common.name=T)  #lists of taxonomic IDs formed by bins in clades.
### Not all of these are monophyletic. The elements of Taxa correspond to atms,
### but one element - the first one - corresponds to the bin of "all other taxa", the set of all groups not split by Phylofactor.

print("### What happened at the third factor?")
FactorSummary <- phylofactor.summary(PF,Taxonomy,factor=3)
TidySummary <- pf.tidy(FactorSummary)
# Here's a breakdown of taxa that were split:
TidySummary$`group1, Monophyletic`
TidySummary$`group2, Paraphyletic`
### With a parsimony assumption, we may be interested in the taxa in Group 1, as they are monophyletic


print("### For more complete info, here are all the taxa pulled out of our third factor - the Group and its Complemenet")
FactorSummary$group1$IDs
FactorSummary$group2$IDs    


print("### Our summary.node object also contains the predicted relative abundances of taxa in the group and its complement:")
FactorSummary$group1$PF.prediction
### an important detail: the default prediction in summary.node is based on all factors up to the node we chose.


print("################# How to make predictions based on Phylofactor? ############################")
txa <- PF$bins[[1]] #What is the prediction for the 1st bin?
phylofactor.predict(PF,newdata=data.frame(X='feces'))[txa]
