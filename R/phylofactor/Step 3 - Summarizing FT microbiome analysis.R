library(phylofactor)
library(ape)
library(compositions)
library(magrittr)

data(FTmicrobiome)
PF <- FTmicrobiome$PF
PF.F <- FTmicrobiome$PF.Fstat
tree <- PF$tree
taxonomy <- FTmicrobiome$taxonomy

# PF - choice='var' -------------------------------------------------------


print("#################### First Factor ###########################")
smry <- phylofactor.summary(PF,taxonomy,factor=1)
L=smry %>% pf.tidy
L

otus1 <- smry$group1$IDs[,1]
otus1 <- sapply(as.list(otus1),FUN=toString)

Edgs <- 1:Nedge(tree)
edg1 <- extractEdges(tree,otus1,type=3)

ecols <- rep('green',Nedge(tree))
ecols[edg1] <- 'red'

#[LL] Lg <- ColorTaxa(tree,taxonomy,show.tip.label=F,type='unrooted',legend=T,outputlegend = T)
#[LL] lims <- par('usr')
#[LL] legend(lims[1],lims[4]-.07,legend=Lg$Taxa,fill=Lg$colors)


plot.phylo(tree,type='unrooted',edge.color=ecols,edge.width = 2,show.tip.label = F,main='First Factor')
lims <- par('usr')
legend(lims[1],lims[4]-.1,legend=L[[2]],cex = .5,lty=1,lwd=2,col='green')
legend(lims[1]*.5+lims[2]*.5,lims[3]+.2,legend=L[[1]],cex = .5,lty=1,lwd=2,col='red')

print("###################################### Second Factor #################################################")
smry <- phylofactor.summary(PF,taxonomy,factor=2)
L <- smry %>% pf.tidy

otus1 <- smry$group1$IDs[,1]
otus1 <- sapply(as.list(otus1),FUN=toString)

otus2 <- smry$group2$IDs[,1]
otus2 <- sapply(as.list(otus2),FUN=toString)

Edgs <- 1:Nedge(tree)
edg1 <- extractEdges(tree,otus1,type=3)
edg2 <- extractEdges(tree,otus2,type=3)

ecols <- rep('black',Nedge(tree))
ecols[edg1] <- 'red'
ecols[edg2] <- 'green'


plot.phylo(tree,type='unrooted',edge.color=ecols,edge.width = 2,show.tip.label = F,main='Second Factor')
lims <- par('usr')
legend(lims[1],lims[4]-.1,legend=L[[2]],cex = .5,lty=1,lwd=2,col='green')
legend(lims[1]*.6+lims[2]*.4,lims[4]-.05,legend=L[[1]],cex = .5,lty=1,lwd=2,col='red')


print("################################ Third Factor #############################################")
smry <- phylofactor.summary(PF,taxonomy,factor=3)
L <- smry %>% pf.tidy

otus1 <- smry$group1$IDs[,1]
otus1 <- sapply(as.list(otus1),FUN=toString)

otus2 <- smry$group2$IDs[,1]
otus2 <- sapply(as.list(otus2),FUN=toString)

Edgs <- 1:Nedge(tree)
edg1 <- extractEdges(tree,otus1,type=3)
edg2 <- extractEdges(tree,otus2,type=3)

ecols <- rep('black',Nedge(tree))
ecols[edg1] <- 'red'
ecols[edg2] <- 'green'


plot.phylo(tree,type='unrooted',edge.color=ecols,edge.width = 2,show.tip.label = F,main='Third Factor')
lims <- par('usr')
legend(lims[1],lims[4]-.1,legend=L[[2]],cex = .5,lty=1,lwd=2,col='green')
legend(lims[1]*.4+lims[2]*.6,lims[4]-.05,legend=L[[1]],cex = .5,lty=1,lwd=2,col='red')

print("####################")

atms <- bins(PF$basis[,1:3])
otus <- lapply(atms,FUN = function(x,names) names[x],names=tree$tip.label)
taxa <- lapply(otus,FUN=OTUtoTaxa,Taxonomy=taxonomy,common.name=F,uniques=T)

legendNames <- binTaxa(PF,3,taxonomy)


L = binPhyloPlot(PF,3,legend.return=T,edge.width=2,rotate=25)
lims=par('usr')
legend(lims[1],lims[4]-.05,legend=legendNames[[1]],fill=L$Colors[1],cex=.7)
legend(lims[2]-.4,lims[4]-.4,legend=legendNames[[2]],fill=L$Colors[2],cex=.7)
legend(lims[1]+.06,lims[4]-.35,legend=legendNames[[3]],fill=L$Colors[3],cex=.7)
legend(lims[2]-.5,lims[4]-.01,legend=legendNames[[4]],fill=L$Colors[4],cex=.7)


print("########### other factors #############")
factor=33
phylofactor.summary(PF,taxonomy,factor=factor) %>% pf.tidy


print("########### relationship between effect size and distance to tips ################")
# are basal effects more faint and often over-ridden by distal effects?

print("############################# Visualize #########################")
# The first two factors separate body sites pretty well:
phylofactor.visualize(PF,2)
phylofactor.summary(PF,taxonomy,1) %>% pf.tidy
phylofactor.summary(PF,taxonomy,2) %>% pf.tidy

DF <- data.frame(PF$Data)
Ldata <- log(PF$Data)
CLData <- (Ldata - rowMeans(Ldata))/apply(Ldata,MARGIN=1,FUN=sd)
pp <- prcomp(CLData,center=T,scale=T)
#[LL] library(ggbiplot)
#[LL] ggbiplot(pp)

sv <- svd(CLData)


print("#################################################### here")

# PF.F choice='F' --------------------------------------------------------------------

PF.F$factors[1:40,]

factor=7
smry <- phylofactor.summary(PF.F,taxonomy,factor)
smry %>% pf.tidy
## The first split separates one Bacteroidetes g__Prevotella from the rest as being 522x more abundant than the rest in the feces
## The second split separates seven g__streptococcus from the rest as being 12x more abundant than the rest in the tongue.
## The third split separates one streptococcus from the rest as being 197x more abundant than the rest in the feces.
## Then, f__Pasteurellaceae g__Haemophilus was separated from the rest as being 262x more abundant in the feces
## Factor 7 corresponds to something like factor 1 in PF - separating Firmicutes & Fusobacteria from the rest as being .5x abundant in the feces, 2x abundant in tongue
## Factor 8 pulls out some Clostridialies (two undescribed families, 7 undescribed genera in family Lachnospiraceae, and 3 undescribed species one in genus Lachnobacterium and two in genus Roseburia)
## as being 4x as abundant in the feces, 0.4x abundant in tongue.
## Factor 13 pulls out some g_Blautia, g_Dorea,  and other o_Clostridiales as being .4x feces, 1.8x tongue
## Factor 14 pulls out the Fusobacteria from the Firmicutes - indicating that Fusobacteria are 0.4x in feces, 3.7x in tongue!!

fset <- c(2,7,8,9,13,14)
for (aa in 1:152){
  binPhyloPlot(PF.F,factor=aa,n=156,edge.width=2,rotate=-90)
}

PF.F$bin.sizes


print("## At the end of phylofactorization, we're left with 157 bins, 36 are clades, 15 of which are monophyletic clades.")
clds <- sapply(PF.F$bins,FUN=function(x) length(x)>1)
monophy <- (names(PF.F$bins)=='Monophyletic')
MonophyleticClades <- which(clds & monophy)
sapply(PF.F$bins[MonophyleticClades],FUN=length)
## All of the monophyletic clades contain only two members.


print("## The predictions, combining all factors:")
dat <- phylofactor.predict(PF.F,factors=100)
image(clr(t(dat)))
image(clr(t(PF.F$Data)))

image(clr(t(PF.F$Data))-clr(t(dat)))

var(c(clr(t(PF.F$Data))))
var(c(clr(t(PF.F$Data))-clr(t(dat))))

print("# Are there an unusually large number of tips/clades in our factorization?")
## We can use a two-tailed hypergeometric test to determine whether there is an over-representation of tips/basal edges in our set.
TipTest(PF)

binPhyloPlot(PF,1)
