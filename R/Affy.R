get.set2gid <- function (chip) {

  # find GeneIDs for probesets
  if (chip == "hgu95a") {
    require("hgu95a.db")
    x <- hgu95aENTREZID                                    
  }

  if (chip == "hgu133a") {
    require("hgu133a.db")
    x <- hgu133aENTREZID                                    
  }
  
  if (chip == "mgu74a") {
    # find GeneIDs for probesets
    require("mgu74a.db")
    x <- mgu74aENTREZID                                    
  }

  mapped_probes <- mappedkeys(x)
  set2gid <- as.list(x[mapped_probes])
  
  set2gid
}


gid2set.pairing.hgu133plus2 = function (my.seed=2562, mysets=NULL) {

	# Author: Leo Lahti 2007

        set.seed(my.seed)

        # Select exactly one probeset for each GeneID to avoid systematic bias from this source for the
        # genes located near-by in the chromosome
        # Get the probe identifiers that are mapped to an ENTREZ Gene ID

        # if mysets is given, probesets are searched for the GeneIDs only from among those

        require(hgu133plus2.db)
        x <- hgu133plus2ENTREZID
        xx <- as.list(x[mappedkeys(x)])

        source("/share/mi/scripts/Affy.R")
        set.seed(my.seed)
        gids = c()
        sets = c()
        unigids = unique(unlist(xx))
        for (i in 1:length(unigids)) {
                print(i/length(unigids))

                gid = unigids[[i]]

                # probesets for this geneid that are found in the data
                gidsets = names(which(xx == gid))

                if (length(mysets)>0) {
                        data = gidsets[gidsets %in% mysets]
                }

                if (length(gidsets)>0) {
                        set = selectAtSets(gidsets)
                        gids = c(gids,gid)
                        sets = c(sets,set)
                }
        }
        names(sets) = gids
        gid2set = sets
        gid2set

}


selectAtSets <- function (sets,remAFFX=TRUE) {

	# Author: Leo Lahti 2007	

	#From given gene list, choose the unique '_at' set. 
	#If multiple '_at' sets exist, pick one of them randomly. 
	#If no '_at' sets exists, pick one of those randomly
	#This is suited for selecting a representative probeset for a GeneID
	#when multiple probesets correspond to one GeneID.

	#Pick endings (remove AFFX sets)
	if (remAFFX) {sets<-removeAFFX(sets)}
	ends<-lapply(lapply(lapply(sets,function(x){strsplit(as.character(x),split="_")}),function(x){x[[1]][-1]}),function(x){paste(x,collapse="_")})
	if ("at" %in% ends) {
		atinds<-which(ends=="at")
		rind<-atinds[sample(length(atinds))][[1]]
		set<-sets[rind]
	} else {
		rind<-sample(length(sets))[[1]]
		set<-sets[rind]
	}

	return(set)
} 

removeAFFX <- function (sets) {
	#Remove AFFX control sets from set list
	sets<-sets[!findAFFX(sets)]
	sets
}


findAFFX <- function (sets) {
	#Find AFFX control sets from the given set list
	affx.set<-logical(length(sets))
	for (i in 1:length(sets)) {
		affx.set[[i]]<-(substr(sets[[i]],1,4)=="AFFX")
	}
	affx.set
}


plotSet <- function (set,abatch,log=FALSE,ylims=FALSE) {
	#Plot raw data (PM) of a given set for affybatch
	probeindices<-pmindex(abatch,set)[[1]]
	Np<-length(probeindices)
	exp<-exprs(abatch)[probeindices,]
	if (log) {exp<-log2(exp)}
	if (ylims==FALSE) {ylims<-range(exp)}
	plot(c(1,2,3),type='n',main=paste(set),xlim=c(1,dim(exp)[[2]]),ylim=ylims,xlab="treatments",ylab="signal",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
	for (i in 1:Np) {
	        lines(exp[i,],col="gray")
	}
	exp #return probe-level expression matrix
}


plotSet2 <- function (set,abatch,expMatrix,lines=T) {
	#Plot probe-level profiles of a given probeset from raw data
	#Now use user-preprocessed expressions (expMatrix)
	#set: name of the probeset
	#abatch: AffyBatch object
	#expMatrix: raw data preprocessed by user. Probes should correspond to those in abatch.

	require(affy)

	probeindices<-pmindex(abatch,set)[[1]]
	Np<-length(probeindices)
	
	exp<-expMatrix[probeindices,]
	Nt<-dim(exp)[[2]]

	ylims<-range(exp)
	plot(c(1,2,3),xlim=c(1,Nt),type='n',main=set,ylim=ylims,xlab="Treatments",ylab="Signal log-ratio",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
	if (!lines) { 	
		for (tr in 1:Nt) {
	        	points(rep(tr,Np),exp[,tr])
	        	lines(rep(tr,Np),exp[,tr])
		} 
	} else {
		for (i in 1:Np) {
	        	lines(exp[i,],col="gray")
		}
	}
	exp
}

annotateAffySets = function (sets, platform="hgu133plus2",savefile=NA) { 
	require(affy)
	require(annaffy)
	#require(hgu133plus2)
	tab<-aafTableAnn(sets,platform)
	if (!is.na(savefile)) {
		saveHTML(tab,file=savefile,title=savefile)
	}
	tab
}


xy2seq <- function (x, y, tab) {
  # tab is for example hgu133aprobe object from require(hgu133aprobe)
  tab[tab[,"x"] %in% x & tab[,"y"] %in% y, "sequence"]  
}

set2seq <- function (set, abatch, probe.anns) {

  # List probes in a probeset
  # and then their probe sequences in the same order
  # probe.anns comes from require(hgu133aprobe) etc.
  # i.e. use probe.anns = hgu133aprobe or analogous for other platforms
  inds <- indexProbes(abatch, which="pm", genenames = set)
  xy <- as.array(lapply(inds, function (ind) {indices2xy(ind, abatch = abatch)})[[1]])

  seq <- c()
  for (i in 1:nrow(xy)) {
    seq[[i]] <- xy2seq(xy[i, "x"], xy[i, "y"], probe.anns)
  }

  seq
}


