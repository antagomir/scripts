# function to read a Newick string with node labels & (possible) singles
# written by Liam J. Revell 2013

read.newick<-function(file="",text){
	# check to see if reading from file
	if(file!="") text<-scan(file,sep="\n",what="character")
	if(length(text)>1){
		tree<-lapply(text,newick)
		class(tree)<-"multiPhylo"
	} else tree<-newick(text)
	return(tree)
}

# main Newick string function
# written by Liam J. Revell 2013
newick<-function(text){
	text<-unlist(strsplit(text, NULL))
	tip.label<-vector(mode="character")
	node.label<-vector(mode="character") 
	edge<-matrix(c(1,NA),1,2) 
	edge.length<-vector()
	currnode<-1
	Nnode<-currnode
	i<-j<-k<-1
	while(text[i]!=";"){
		if(text[i]=="("){
			if(j>nrow(edge)) edge<-rbind(edge,c(NA,NA))
			edge[j,1]<-currnode
			i<-i+1
			# is the next element a label?
			if(is.na(match(text[i],c("(",")",",",":",";")))){
				temp<-getLabel(text,i)
				tip.label[k]<-temp$label
				i<-temp$end
				edge[j,2]<--k
				k<-k+1
				# is there a branch length?
				if(text[i]==":"){
					temp<-getEdgeLength(text,i)
					edge.length[j]<-temp$edge.length
					i<-temp$end
				}	
			} else if(text[i]=="("){
				Nnode<-Nnode+1 # creating a new internal node
				currnode<-Nnode
				edge[j,2]<-currnode # move to new internal node
			}
			j<-j+1
		} else if(text[i]==")"){
			i<-i+1
			# is the next element a label?
			if(is.na(match(text[i],c("(",")",",",":",";")))){
				temp<-getLabel(text,i)
				node.label[currnode]<-temp$label
				i<-temp$end
			}
			# is there a branch length?
			if(text[i]==":"){
				temp<-getEdgeLength(text,i)
				if(currnode>1){ 
					ii<-match(currnode,edge[,2])
					edge.length[ii]<-temp$edge.length
				} else root.edge<-temp$edge.length
				i<-temp$end
			}	
			if(currnode>1) currnode<-edge[match(currnode,edge[,2]),1] # move down the tree
		} else if(text[i]==","){
			if(j>nrow(edge)) edge<-rbind(edge,c(NA,NA))
			edge[j,1]<-currnode
			i<-i+1
			# is the next element a label?
			if(is.na(match(text[i],c("(",")",",",":",";")))){
				temp<-getLabel(text,i)
				tip.label[k]<-temp$label
				i<-temp$end
				edge[j,2]<--k
				k<-k+1
				# is there a branch length?
				if(text[i]==":"){
					temp<-getEdgeLength(text,i)
					edge.length[j]<-temp$edge.length
					i<-temp$end
				}
			} else if(text[i]=="("){
				Nnode<-Nnode+1 # creating a new internal node
				currnode<-Nnode
				edge[j,2]<-currnode # move to internal node
			}
			j<-j+1
		}
	}
	Ntip<-k-1
	edge[edge>0]<-edge[edge>0]+Ntip
	edge[edge<0]<--edge[edge<0]
	edge.length[is.na(edge.length)]<-0
	node.label[is.na(node.label)]<-""
	if(length(node.label)==0) node.label<-NULL
	# assemble into "phylo" object
	tree<-list(edge=edge,Nnode=as.integer(Nnode),tip.label=tip.label,edge.length=edge.length,node.label=node.label)
	class(tree)<-"phylo"
	return(tree)
}

# function gets label
# written by Liam J. Revell 2011-2013
getLabel<-function(text,start,stop.char=c(",",":",")")){
	i<-0
	label<-vector()
	while(is.na(match(text[i+start],stop.char))){
		label[i+1]<-text[i+start]
		i<-i+1
	}
	return(list(label=paste(label,collapse=""),end=i+start))
}

# function gets branch length
# written by Liam J. Revell 2011-2013
getEdgeLength<-function(text,start){
	i<-start+1; m<-1
	temp<-vector()
	while(is.na(match(text[i],c(",",")",";")))){
		temp[m]<-text[i]
		i<-i+1
		m<-m+1
	}
	return(list(edge.length=as.numeric(paste(temp,collapse="")),end=i))
}
