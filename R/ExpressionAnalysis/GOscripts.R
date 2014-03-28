# Scripts for handling Gene Ontology information

getOffspring <- function (id, rank = 1)
{
    #Modified from 'CustomEndNodeList' function of goTools package 
    #May require package 'goTools' due to use of 'goChildren' function 
 
    #Author: Leo Lahti
    #Date: 22.2.2007
    #Comment: for given gene ontology id ('id'), get offspring terms up to given rank
    #	      for each tree level, save a separate list of unique terms at this level

   cust <- id
    res <- numeric(0)
    rankIDs<-vector(length=rank,mode="list")
    for (i in 1:rank) {
        print(paste("rank=", i))
        cust <- unique(unlist(lapply(cust, goChildren)))
	#remove NAs
	cust <- cust[!is.na(cust)]
	#save IDs unique for this rank-level
	if (i==1) {
		rankIDs[[i]]<-cust
		novelIDs<-cust
	} else {
		previousIDs<-unlist(rankIDs[-seq(i-1)])
		novelIDs<-cust[!(cust %in% previousIDs)]
		#Include only those IDs that were not listed at previous levels
		rankIDs[[i]]<-novelIDs
	}
	cust<-novelIDs
    }
    rankIDs
}

getLeaves <- function(id,rank=1) {
    #Author: Leo Lahti
    #Date: 22.2.2007
    #Comment: for given gene ontology id ('id'), get offspring terms up to given rank
    #	      for each tree level. If certain terms at higher levels do not have children
    # 		add them to leaves. I.e. get all terms that are at rank level
    # 		with or without children, and preceding levels without children.

    leaves<-c()
    cust <- id
    res <- numeric(0)
    rankIDs<-vector(length=rank,mode="list")
    for (i in 1:rank) {
        print(paste("rank=", i))
	children<-lapply(cust, goChildren)
	leaves<-c(leaves,cust[is.na(children)])
        cust <- unique(unlist(children))
	#remove NAs
	cust <- cust[!is.na(cust)]
	#save IDs unique for this rank-level
	if (i==1) {
		rankIDs[[i]]<-cust
		novelIDs<-cust
	} else {
		previousIDs<-unlist(rankIDs[-seq(i-1)])
		novelIDs<-cust[!(cust %in% previousIDs)]
		#Include only those IDs that were not listed at previous levels
		rankIDs[[i]]<-novelIDs
	}
	cust<-novelIDs
	leaves<-unique(c(leaves,rankIDs[[rank]]))

    }
    leaves
}


