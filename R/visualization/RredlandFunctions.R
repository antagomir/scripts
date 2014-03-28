# Functions for interpreting Rredland objects
# Author: Leo Lahti, Antti Ajanki
# Last modification date: 24.5.2007

# NOTE: this is still in a preliminary stage. It is somewhat expected
# that later versions of the Rredland package will contain sufficient
# tools for handling all necessary things. These scripts can be used
# meanwhile.

just.endings <- function(items) {
	#Split objects at "#" and pick the latter part
	ends<-c()
	for (i in seq(length(items))) {
		if ("#" %in% unlist(strsplit(items[[i]],""))) {
			ends[[i]]<-unlist(strsplit(as.character(items[[i]]),"#"))[[2]]
		} else {ends[[i]]<-NA}
	}
	ends
}

list.classes <- function(world=rdf) {
	#Input: rdf: as(readRDF(...),"data.frame") i.e. RDF object as data frame
	#Output: list of (unique) classes in the inspected OWL
	unique(rdf[grep("type$",world[,2]),3])

}

list.pathways <- function (world=rdf,short=FALSE) {
	#Input: rdf: as(readRDF(...),"data.frame") i.e. RDF object as data frame	#short: TRUE if you want only brief names (after '#') listed
	#Output: list of pathways in the inspected OWL
	typefields<-grep("#type",world[,2])
	pwfields<-grep("#pathway$",world[,3]) #"$" is to ensure that we do not pick "pathwayStep" fields here 
	if (short) {nams<-unlist(lapply(pws,function(x){unlist(strsplit(as.character(x),"\\#"))[[2]]}))} else {nams<-unique(rdf[,1][intersect(typefields,pwfields)])} #return
	nams
}

list.proteins <- function (world=rdf) {
	#List all protein entities in the data
	typefields<-grep("#type",world[,2])
	predfields<-grep("#protein$",world[,3])
	as.character(unique(rdf[,1][intersect(typefields,predfields)]))
}

list.xrefs <- function (world=rdf) {
	#List all protein entities in the data
	typefields<-grep("#XREF",world[,2])
	as.character(unique(rdf[typefields,3]))
}

list.uniprots <- function (world=rdf) {
	#List all uniprots in the data
	xreffields<-grep("#XREF",world[,2])
	inds = grep("UniProt",rdf[xreffields,3])
	ups <- as.character(rdf[xreffields[inds],3])
	uniprots <- unique(unlist(lapply(ups,function(x){strsplit(x,"UniProt_")[[1]][[2]]})))
	uniprots
}


list.dnas <- function (world=rdf) {
	#List all dna entities in the data
	typefields<-grep("#type",world[,2])
	predfields<-grep("#dna$",world[,3])
	as.character(unique(rdf[,1][intersect(typefields,predfields)]))
}

list.rnas <- function (world=rdf) {
	#List all rna entities in the data
	typefields<-grep("#type",world[,2])
	predfields<-grep("#rna$",world[,3])
	as.character(unique(rdf[,1][intersect(typefields,predfields)]))
}

list.complexes <- function (world=rdf) {
	#List all complex entities in the data
	typefields<-grep("#type",world[,2])
	predfields<-grep("#complex$",world[,3])
	as.character(unique(rdf[,1][intersect(typefields,predfields)]))
}

list.small.molecules <- function (world=rdf) {
	#List all complex entities in the data
	typefields<-grep("#type",world[,2])
	predfields<-grep("#smallMolecule$",world[,3])
	as.character(unique(rdf[,1][intersect(typefields,predfields)]))
}

list.physical.entities <- function (world=rdf) {
	#According to BioPAX2 manual, physicalEntity has 5 subclasses:
	#complex,dna,protein,rna,smallMolecule
	#List these
	proteins<-list.proteins(world)
	dnas<-list.dnas(world)
	rnas<-list.rnas(world)
	complexes<-list.complexes(world)
	smallMolecules<-list.small.molecules(world)
	list(protein=proteins,dna=dnas,rna=rnas,complex=complexes,smallMolecule=smallMolecules)
}


list.physical.entity.predicates <- function (world=rdf,unique=TRUE) {
	#Pick PHYSICAL-ENTITY predicate fields
	pred.inds<-grep("#PHYSICAL-ENTITY$",world[,2])
	#Pick corresponding objects
	entities<-rdf[pred.inds,3]
	if (unique) {entities<-unique(entities)}
	entities
}



list.pathway.steps <- function (world=rdf) {
	#List all pathway step entities in the data
	typefields<-grep("#type",world[,2])
	predfields<-grep("#pathwayStep$",world[,3])
	as.character(unique(rdf[,1][intersect(typefields,predfields)]))
}

pick.subject.fields <- function (item,world=rdf) {
	#Input: -rdf: 	
	#	-item:	item for which we want to identify subject fields
	#Output -inds:	indices of the subject field corresponding to our item

	# "$" ensures that the field name ends at correct place
	# This prevents inclusion of fields with a name with similar beginning	
	grep(paste(item,"$",sep=""),world[,1]) #->inds
}

step.interactions<- function (pw,world=rdf) {
	#Pick STEP-INTERACTIONS fields for given PW step
	#Input: - rdf: as(readRDF(...),"data.frame") i.e. RDF object as data frame
	#	- pw: name of the pathway as listed in the subjects field of the inspected data
	#Output:
	#	- indices of the step interaction fields for this pathway 
	stepintact.fields<-grep("#STEP-INTERACTIONS",world[,2])
	mypwfield.inds<-pick.subject.fields(pw,world)
	inds<-intersect(mypwfield.inds,stepintact.fields)	
	obs<-as.character(rdf[inds,3])
	list(inds=inds,obs=obs)
}


this.pathway.components<- function (pw,world=rdf) {
	#Pick PATHWAY-COMPONENTS fields for given PW
	#Input: - rdf: as(readRDF(...),"data.frame") i.e. RDF object as data frame
	#	- pw: name of the pathway as listed in the subjects field of the inspected data
	#Output:
	#	- indices of the pathway components fields for this pathway 
	comp.fields<-grep("#PATHWAY-COMPONENTS",world[,2])
	mypwfield.inds<-pick.subject.fields(pw,world)
	inds<-intersect(mypwfield.inds,comp.fields)
	nams<-rdf[inds,3]
	output<-list(inds=inds,nams=nams)
	return(output)
}

check.type <- function (item,world=rdf) {
	#Check type of given item
	#Input: - rdf: as(readRDF(...),"data.frame") i.e. RDF object as data frame
	#	- item: name of the subject to check
	#Output: - type of the checked item
	if (length(item)==1) {
		inds<-pick.subject.fields(item,world)
		obs<-as.character(rdf[,3])
		type.inds<-grep("#type$",world[,2])
		type<-unlist(strsplit(obs[intersect(inds,type.inds)],"#"))[[2]]
	} else if (length(item)>1) {
		type<-vector(length=length(item),mode="character")
		for (i in seq(length(item))) {
			inds<-pick.subject.fields(item[[i]],world)
			obs<-as.character(rdf[,3])
			type.inds<-grep("#type$",world[,2])
			type[[i]]<-unlist(strsplit(obs[intersect(inds,type.inds)],"#"))[[2]] #type
		}
	}
	return(type)
}	

get.predicates <- function (subject,world=rdf,just.ends=FALSE) {
	#unique(unlist(lapply(rdf[,2],function(x){unlist(strsplit(as.character(x),"#"))[[2]]})))
	#For a given subject term, give its predicates
	predicates<-rdf[pick.subject.fields(subject,world),2]
	if (just.ends) {predicates<-just.endings(predicates)}
	predicates
}

get.objects <- function (subject,world=rdf,just.ends=FALSE) {
	#For a given subject term, give its objects
	objects<-rdf[pick.subject.fields(subject,world),3]
	if (just.ends) {objects<-just.endings(objects)}
	objects
}

get.name <- function (subject,world=rdf) {
	#Find the name for given subject if it exists
	sub.inds<-pick.subject.fields(subject,world)
	name.inds<-grep("#NAME$",world[,2])
	as.character(rdf[intersect(sub.inds,name.inds),3])
}


get.left <- function (subject,world=rdf) {
	#Find the LEFT objects for given subject if it exists
	sub.inds<-pick.subject.fields(subject,world)
	pred.inds<-grep("#LEFT$",world[,2])
	as.character(rdf[intersect(sub.inds,pred.inds),3])
}

get.right <- function (subject,world=rdf) {
	#Find the RIGHT objects for given subject if it exists
	sub.inds<-pick.subject.fields(subject,world)
	pred.inds<-grep("#RIGHT$",world[,2])
	as.character(rdf[intersect(sub.inds,pred.inds),3])
}

get.participants <- function (subject,world=rdf) {
	#Find the PARTICIPANTS objects for given subject if it exists
	sub.inds<-pick.subject.fields(subject,world)
	pred.inds<-grep("#PARTICIPANTS$",world[,2])
	as.character(rdf[intersect(sub.inds,pred.inds),3])
}

get.LR.participants <- function (subject,world=rdf) {
	#Find the LEFT-RIGHT participant objects for given subject if it exists
	sub.inds<-pick.subject.fields(subject,world)
	left.inds<-grep("#LEFT$",world[,2])
	right.inds<-grep("#RIGHT$",world[,2])
	pred.inds<-union(left.inds,right.inds)
	as.character(rdf[intersect(sub.inds,pred.inds),3])
}



get.physical.entity <- function (subject,world=rdf) {
	#Find the PHYSICAL-ENTITY objects for given subject(s)
	ents<-c()
	for (i in seq(length(subject))) {
		sub.inds<-pick.subject.fields(subject[[i]],world)
		pred.inds<-grep("#PHYSICAL-ENTITY$",world[,2])
		ents[[i]]<-as.character(rdf[intersect(sub.inds,pred.inds),3])
	}
	ents
}

simple.table <- function (subject,world=rdf) {
	#Print table with endings from the predicate and object fields
	preds<-get.predicates(subject,just.ends=T)
	obs<-get.objects(subject,just.ends=T)
	cbind(preds,obs)
}

full.table <- function (subject,world=rdf) {
	#Print table with full fields from the predicate and object fields
	preds<-get.predicates(subject,just.ends=F)
	obs<-get.objects(subject,just.ends=F)
	cbind(preds,obs)
}

get.xref <- function (subject,world=rdf,parse=FALSE) {
	#Find the XREF objects for given subject if it exists
	sub.inds<-pick.subject.fields(subject,world)
	pred.inds<-grep("#XREF$",world[,2])
	xrefs = as.character(rdf[intersect(sub.inds,pred.inds),3])
	xrefs.parsed = c()
	if (parse) {
		for (xref in xrefs) {
			xrefs.parsed<-c(xrefs.parsed,strsplit(xref,split="#")[[1]][[2]])
		}
		xrefs <- xrefs.parsed
	}
	xrefs
}



get.uniprot <- function (subject,world=rdf) {
	#Pick the UniProt references from the
	#XREF objects for given subject
	if (length(subject)==1) {
		xrefs<-get.xref(subject,world=rdf)
		#Now pick just those where we have UniProt identifier 
		unlist(lapply(as.character(xrefs[grep("#UniProt",xrefs)]),function(x){unlist(strsplit(x,"#UniProt_"))[[2]]}))
	} else if (length(subject)>1) {
		uprotlist<-list()
		for (k in seq(length(subject))) {
			xrefs<-get.xref(subject[[k]],world=rdf)
			uprotlist[[k]]<-unlist(lapply(as.character(xrefs[grep("#UniProt",xrefs)]),function(x){unlist(strsplit(x,"#UniProt_"))[[2]]}))
		}
		uprotlist
	}
}

get.class <- function (subject,world=rdf,short=TRUE) {
	if (length(subject)>0) {
		#Get clas(ses) for the given subject(s)
		class.inds<-grep("type$",world[,2])
		classes<-c()
		for (i in seq(length(subject))) {
			sub.inds<-grep(paste(subject[[i]],"$",sep=""),world[,1])
			classes[[i]]<-as.character(rdf[intersect(class.inds,sub.inds),3])
		}
		if (short) {classes<-just.endings(classes)}
	} else {classes<-list()}
	classes
}

get.components <- function (subject,world=rdf){
	#Get COMPONENTS objects
	sub.inds<-pick.subject.fields(subject,world)
	pred.inds<-grep("#COMPONENTS$",world[,2])
	as.character(rdf[intersect(sub.inds,pred.inds),3])	
}

next.step <- function (subject,world=rdf) {
	#Fot pathwayStep, get NEXT-STEP(s)
	pred.inds<-grep("#NEXT-STEP$",world[,2])
	sub.inds<-pick.subject.fields(subject,world)
        as.character(rdf[intersect(sub.inds,pred.inds),3])
}


pw.component.interactions <- function (pw.components,world=rdf) {
	pw.components.interactions<-vector(length=length(pw.components),mode="list")
	names(pw.components.interactions)<-pw.components
	for (i in seq(length(pw.components))) {
		comp<-pw.components[[i]]
		#comp.class<-get.class(comp,world) #I think all should be pathwayStep
		#List interactions for pwstep
		pw.components.interactions[[i]]<-step.interactions(comp,world)$obs
	}
	pw.components.interactions
}


pw.component.next.step <- function (pw.components,world=rdf) {
	pw.components.next.steps<-vector(length=length(pw.components),mode="list")
	names(pw.components.next.steps)<-pw.components

	for (i in seq(length(pw.components))) {
		comp<-pw.components[[i]]
		#comp.class<-get.class(comp,world) #I think all should be pathwayStep
		#List next steps
		pw.components.next.steps[[i]]<-next.step(comp,world)
	}
	pw.components.next.steps
}

get.members <- function (comp,world=rdf) {
	#Identify members of the "comp" 
	#Input: item from the complex class

	participants<-get.LR.participants(comp)
	components<-get.components(comp)
	comps<-unique(c(participants,components))
	
	prots<-c()
	uniprots<-c()
	uniprots.self<-c()
	complexes<-c()
	dnas<-c()
	rnas<-c()
	smallMolecules<-c()

	if (length(comps)>0) {
		classes<-get.class(get.physical.entity(comps))
	
		#pick proteins (with id of this data)
		#pick uniprots
		prots<-vector(length=length(comps),mode="list")
		uniprots<-vector(length=length(comps),mode="list")
		if ("protein" %in% classes) {
			inds<-which(classes=="protein")
			for (ind in inds) {
				prots[[ind]]<-get.physical.entity(comps[[ind]])
				uniprots[[ind]]<-get.uniprot(get.physical.entity(comps[[ind]]))
			}
		}

		#I noticed that a complex can itself have a UniProt ID
		#Check if this component itself has uniprots and add them
		uniprots.self<-get.uniprot(comp)	

		#pick complexes
		complexes<-vector(length=length(comps),mode="list")
		if ("complex" %in% classes) {
			inds<-which(classes=="complex")
			complexes[inds]<-get.physical.entity(comps[inds])
		}
		
		#pick dna
		dnas<-vector(length=length(comps),mode="list")
		if ("dna" %in% classes) {
			inds<-which(classes=="dna")
			dnas[inds]<-get.physical.entity(comps[inds])
		}
	
		#pick rna
		rnas<-vector(length=length(comps),mode="list")
		if ("rna" %in% classes) {
			inds<-which(classes=="rna")
			rnas[inds]<-get.physical.entity(comps[inds])
		}

		if ("smallMolecule" %in% classes) {
			inds<-which(classes=="smallMolecule")
			smallMolecules[inds]<-get.physical.entity(comps[inds])
		}

		#check other classes
		if (any(is.na(match(classes,c("protein","complex","dna","rna","smallMolecule"))))) {print("Check complex members. There are additional classes to the checked ones.")}

	} else {
		#Here the 'comp' complex does not have additional components (comps)
		#However, I noticed that a complex can still have a UniProt ID
		#For a complex with no components but uniprots, pick the uniprots
		uniprots.self<-get.uniprot(comp)
	} 
	list(proteins=prots,uniprots=uniprots,uniprots.self=uniprots.self,complexes=complexes,dnas=dnas,rnas=rnas,smallMolecules=smallMolecules)
}






get.control.members <- function (comp,world=rdf) {
	#Identify members of the "comp" 
	#Input: item from the complex class

	controls<-get.controls(comp)
	#components<-get.components(comp)
	comps<-unique(controls)
	
	prots<-list()

	if (length(comps)>0) {
		classes<-get.class(comps)

		if ("biochemicalReaction" %in% classes) {
			inds<-which(classes=="biochemicalReaction")
			for (ind in inds) {
				prots[[ind]]<-get.members.uniprots(get.members(comps[[ind]]))
			}
		}

		if ("sequenceParticipant" %in% classes) {
			inds<-which(classes=="sequenceParticipant")
			for (ind in inds) {
				prots[[ind]]<-get.uniprot(get.physical.entity(comps[[ind]]))
			}
		}

		if ("conversion" %in% classes) {
			inds<-which(classes=="conversion")
			for (ind in inds) {
				prots[[ind]]<-get.members.uniprots(get.members(comps[[ind]]))
			}

		}

		if ("physicalEntityParticipant" %in% classes) {
			inds<-which(classes=="physicalEntityParticipant")
			for (ind in inds) {
				prots[[ind]]<-get.members.uniprots(get.members(comps[[ind]]))
			}
		}


		if ("transport" %in% classes) { # subclass of 'conversion', use same code for this
			inds<-which(classes=="transport")
			for (ind in inds) {
				prots[[ind]]<-get.members.uniprots(get.members(comps[[ind]]))
			}
		}


		#check other classes
		if (any(is.na(match(classes,c("biochemicalReaction","sequenceParticipant","conversion","physicalEntityParticipant","transport"))))) {print("Check control members. There are additional classes to the checked ones.")}

	} 

	list(proteins=unique(unlist(prots)))
}


complex.proteins.recursive <- function (comp,world=rdf) {
	#Identify proteins in the complex, including subcomplexes
	membs<-get.members(comp,world)
	prots<-unique(unlist(membs$proteins))
	complexes<-unique(unlist(membs$complexes))
	uniprots.self<-unique(unlist(get.uniprot(comp)))

	complexes.empty<-FALSE
	while (!complexes.empty) {
		#Loop until no subcomplexes any more
		membslist<-list()	
		for (comp2 in complexes) {
			membslist<-c(membslist,get.members(comp2,world))
		}
		protslist<-unique(unlist(lapply(membslist$proteins,function(x){unlist(x)})))
		complexlist<-unique(unlist(lapply(membslist$complexes,function(x){unlist(x)})))
		uniprots.self.list<-unique(unlist(lapply(membslist$uniprots.self,function(x){unlist(x)})))


		#If complex is member for itself then remove it from the subcomplex list to avoid infinite loop
		loop<-match(unlist(membslist$complexes),complexes)
		if (length(loop)>0) {complexes<-complexes[-loop]}

		prots<-unique(c(prots,protslist))
		uniprots.self<-unique(c(uniprots.self,uniprots.self.list))
		complexes<-unique(c(complexes,complexlist))
		complexes.empty<-(length(complexlist)==0)
	}
	list(proteins=prots,uniprots.self=uniprots.self)
}

control.proteins.recursive <- function (subject,world=rdf) {
	#Identify proteins in the control, including subcontrols
	membs<-get.control.members(subject,world)
	prots<-unique(unlist(membs$proteins))
	controls<-unique(unlist(membs$controls))
	uniprots.self<-unique(unlist(get.uniprot(subject)))

	controls.empty<-FALSE
	while (!controls.empty) {
		#Loop until no subcontrols any more
		membslist<-list()	
		for (subject2 in controls) {
			membslist<-c(membslist,get.control.members(comp2,world))
		}
		protslist<-unique(unlist(lapply(membslist$proteins,function(x){unlist(x)})))
		controllist<-unique(unlist(lapply(membslist$controls,function(x){unlist(x)})))
		uniprots.self.list<-unique(unlist(lapply(membslist$uniprots.self,function(x){unlist(x)})))

		prots<-unique(c(prots,protslist))
		uniprots.self<-unique(c(uniprots.self,uniprots.self.list))
		controls<-unique(c(controls,controllist))
		controls.empty<-(length(controllist)==0)
	}
	list(proteins=prots,uniprots.self=uniprots.self)
}


get.interaction.uniprots <- function (intact) {	

	itype = check.type(intact)

	if (itype == 'complexAssembly') {
		# List members of the assembly
		lefts <- get.left(intact)
		rights <- get.right(intact)
		entities <- get.physical.entity(c(lefts,rights))
		# List proteins for the entities
		uniprots <- get.entity.prots(entities)

	}
	if (itype == 'control') {
		cps <- control.proteins.recursive(intact)
		uniprots <- unique(c(cps$proteins,cps$uniprots.self))
	}

	if (itype == 'conversion') {
		prots<-unique(unlist(get.members(intact)$proteins))
		uniprots <- unique(c(unlist(get.uniprot(prots$proteins)),prots$uniprots.self))
	}

	if (itype == 'biochemicalReaction') {
		# List members of the assembly
		lefts <- get.left(intact)
		rights <- get.right(intact)
		entities <- get.physical.entity(c(lefts,rights))
		uniprots <- get.entity.prots(entities)
	}

	if (itype == 'catalysis') {
		controllers <- get.physical.entity(get.controller(intact))
		controller.uniprots <- get.entity.prots(controllers)

		controlled <- get.controlled(intact)
		controlled.prots<-c()
		if (length(controlled)>0) {	
			for (cont in controlled) {
				cps <- control.proteins.recursive(cont)
				controlled.prots <- c(controlled.prots,unique(c(cps$proteins,cps$uniprots.self)))
			}
		}
		controlled.prots <- unique(controlled.prots)
		uniprots <- unique(c(controller.uniprots,controlled.prots))
	}

	if (itype == 'transport') { # subclass of 'conversion', use same code for this
		uniprots <- get.members.uniprots(get.members(intact))
		#This probably should do the same?
		#prots<-unique(unlist(get.members(intact)$proteins))
		#uniprots <- unique(c(unlist(get.uniprot(prots$proteins)),prots$uniprots.self))
	}

	uniprots <- unique(uniprots)
	if (length(uniprots)==0) {uniprots = "No UniProts"}

	uniprots
}


get.protein.complex.friends <- function (protein,world=rdf) {
	#Check if the protein belongs to a complex
	#If yes, identify other protein members of the complex
	#Check only complexes where protein is directly. Do not check mediated assignments from subcomplexes.
	
	#Pick object fields for our protein
	obj.inds<-grep(protein,world[,3])
	pred.inds<-grep("#COMPONENTS$",world[,2])
	#subjects for which our protein is a COMPONENT
	subs<-as.character(rdf[intersect(obj.inds,pred.inds),1])
	if (length(subs)>0) {
		#Make a list of complexes with this protein 
		complexes<-subs[which(get.class(subs)=="complex")]
		#List proteins in each complex
		complex.proteins<-list()
		for (i in seq(length(complexes))) {
			complex.proteins[[i]]<-unique(unlist(get.members(complexes[[i]])$proteins))
		}
		names(complex.proteins)<-complexes
	} else {complex.proteins<-list()}
	complex.proteins
}

list.major.complexes <- function (complexes,world=rdf,all=FALSE) {
	if (all) {complexes<-list.complexes(rdf)} #list all complexes in the data
	# List the complexes in the data that are not members of other complexes
	majors<-vector(length=length(complexes),mode="logical")
	for (i in seq(length(complexes))) {
		#identify entities that have this complex as a COMPONENT
		obj.inds<-grep(complexes[[i]],world[,3])
		pred.inds<-grep("#COMPONENTS$",world[,2])
		#is none of those entities another complex?
		if (!any(get.class(rdf[intersect(obj.inds,pred.inds),1])=="complex")) {
			majors[[i]]<-TRUE
		}
	}
	majors
}

get.controls <- function (control,world=rdf) {
	#Input: "control" object
	#Pick "CONTROLLED" and "CONTROLLER"
	#Check their contents
	sub.inds<-grep(control,world[,1])
	pred.inds<-unique(c(grep("#CONTROLLED$",world[,2]),grep("#CONTROLLER$",world[,2])))
	as.character(rdf[intersect(sub.inds,pred.inds),3])
}

get.members.uniprots <- function (membs) {
	uprots<-list()
	Nmembscomps <- max(length(membs$proteins),length(membs$uniprots),length(membs$uniprots.self),length(membs$complexes))
	if (Nmembscomps>0) {
		for (i in seq(Nmembscomps)) {
			prots<-get.uniprot(membs$proteins[[i]])
			uniprots<-membs$uniprots[[i]]
			uniprots.self<-membs$uniprots.self[[i]]
			#Then list proteins from the complexes
			cup <- c()
			for (com in membs$complexes) {
				cprot <- complex.proteins.recursive(com)
				cup <- c(cup,get.uniprot(cprot$proteins),get.uniprot(cprot$uniprots.self))
			}
			uprots[[i]]<-unique(c(prots,uniprots,uniprots.self,cup))
		}
	} else {uprots <- c()}
	uprots
}

get.control.proteins <- function (control,world=rdf) {
	#Recursively get proteins of a control object
	controls<-get.controls(control,world)
	uprots<-list()
	for (i in seq(length(controls))) {
		con<-controls[[i]]
		if (get.class(con)=="complexAssembly") {
			prots<-complex.proteins.recursive(con)
			uprots<-unique(c(unlist(get.uniprot(prots$proteins)),prots$uniprots.self))
			step.prots[[j]]<-uprots
		} else if (get.class(con)=="conversion") {
			step.prots[[j]]<-unique(unlist(get.members.uniprots(get.members(con))))
		} else if (get.class(con)=="transport") {
			step.prots[[j]]<-unique(unlist(get.members.uniprots(get.members(con))))
		} else if (get.class(con)=="sequenceParticipant") {
			uprots[[i]]<-get.uniprot(get.physical.entity(con))
		} else if (get.class(con)=="control") {
			step.prots[[j]]<-control.proteins.recursive(con)
		} else {print("Check classes 3")}

	}
	unique(unlist(uprots))
}


get.proteins <- function (subject,world=rdf) {
	#Recursively get proteins of a control object
	uprots<-list()

	if (get.class(subject)=="control") {
		items<-get.controls(subject,world)
	} else (items<-get.LR.participants(subject,world))

	if (length(items)>0) {
		step.prots<-c()
		for (j in seq(length(items))) {
			subject<-items[[j]]
			if (get.class(subject)=="complexAssembly") {
				prots<-complex.proteins.recursive(subject,world)
				uprots<-unique(c(unlist(get.uniprot(prots$proteins,world)),prots$uniprots.self))
				step.prots[[j]]<-uprots
			} else if (get.class(subject,world)=="conversion") {
				step.prots[[j]]<-unique(unlist(get.members.uniprots(get.members(subject,world))))
			} else if (get.class(subject,world)=="transport") {
				step.prots[[j]]<-unique(unlist(get.members.uniprots(get.members(subject,world))))
			} else if (get.class(subject,world)=="sequenceParticipant") {
				uprots[[i]]<-get.uniprot(get.physical.entity(subject,world))
			} else if (get.class(subject,world)=="control") {
				step.prots[[j]]<-get.proteins(subject,world)
			} else if (get.class(subject,world)=="catalysis") {
				step.prots[[j]]<-get.proteins(subject,world)
			} else if (get.class(subject,world)=="physicalEntityParticipant") {
				#FIXME: tee oma kokonaishaku physicalenttyParticipantille
				if (get.class(get.physical.entity(subject,world))=="complex") {
					step.prots[[j]]<-get.uniprot(complex.proteins.recursive(get.physical.entity(subject,world)))
				} else  {print("Check classes 4")}
			} else {print("Check classes 3")}
		}
	}
	unique(unlist(uprots))
}


interaction.uniprots <- function (intacts,world=rdf) {
	#Input: intacts: list of INTERACTION objects
	sub.pathways<-list()
	intact.uprots<-vector(length=length(intacts),mode="list")
	for (i in seq(length(intacts))) {
		prots<-c()
		#print(get.class(intacts[[i]]))
		if (get.class(intacts[[i]],world)=="complexAssembly"){
			prots<-complex.proteins.recursive(intacts[[i]],world)
			uprots<-unique(c(unlist(get.uniprot(prots$proteins)),prots$uniprots.self))
		} else if (get.class(intacts[[i]],world)=="transport"){
			membs<-get.members(intacts[[i]],world)
			prots<-unique(unlist(membs$proteins))
			uniprots<-unique(unlist(membs$uniprots))
			uniprots.self<-unique(unlist(membs$uniprots.self))
			uprots<-unique(c(unlist(get.uniprot(prots)),uniprots,uniprots.self))
		} else if (get.class(intacts[[i]],world)=="conversion"){
			prots<-unique(unlist(get.members(intacts[[i]],world)$proteins))
			uprots<-unique(c(unlist(get.uniprot(prots$proteins)),prots$uniprots.self))
		} else if (get.class(intacts[[i]],world)=="pathway"){
			sub.pathways<-c(sub.pathways,intacts[[i]])
			uprots<-NULL
		} else if (get.class(intacts[[i]])=="control") {
			uprots<-get.proteins(intacts[[i]])
		} else if (get.class(intacts[[i]])=="catalysis") {
			uprots<-get.proteins(intacts[[i]])
		} else if (get.class(intacts[[i]])=="biochemicalReaction") {
			uprots<-get.proteins(intacts[[i]])			
		} else {uprots<-get.proteins(intacts[[i]]); print("Check classes?")}
		
		if (length(prots)==0) {uprots<-list()}
		intact.uprots[[i]]<-uprots
	}
	list(sub.pathways=sub.pathways,interaction.uniprotsnnnnnnnnnnn=intact.uprots)
}


get.entity.prots <- function(entities,world = rdf) {
		assembly.uniprots <- c()
		for (ent in entities) {
			if (check.type(ent)=='protein') {
				#nams <- strsplit(as.character(get.objects(ent)[grep("#NAME",get.predicates(ent))]),split="\\^")[[1]][[1]] 
				#SHORT-NAME
				# We could check also nams and short-names
				# But now just take xrefs (these include UniProts)
				#xrefs <- get.xref(ent,parse=TRUE)
				# Even more restricted: get just UniProts
				uniprots <- get.uniprot(ent)
				# add to list
				assembly.uniprots <- c(assembly.uniprots,uniprots)
			}
			if (check.type(ent)=='smallMolecule') {
				# Ignore currently. 
				# Any bioactive molecule that is not a peptide, DNA, or RNA.
				# So no direct connections to microarray data sets
			}
			if (check.type(ent)=='complex') {
				#Ignore in members: rnas, smallMolecules, dnas
				# List proteins of the complex.
				# Note that complex may consist of complexes,
				# Hence recursive loop.
				up1 <- get.uniprot(complex.proteins.recursive(ent)$proteins)
				up2 <- get.uniprot(complex.proteins.recursive(ent)$uniprots.self)
				# Add to the list
				assembly.uniprots <- c(assembly.uniprots,up1,up2)
			}
		}
		assembly.uniprots <- unique(assembly.uniprots)
}

get.controller <- function (control,world=rdf) {
	#Input: "control" object
	#Pick "CONTROLLER"
	#Check their contents
	sub.inds<-grep(control,world[,1])
	pred.inds<-grep("#CONTROLLER$",world[,2])
	as.character(rdf[intersect(sub.inds,pred.inds),3])
}

get.controlled <- function (control,world=rdf) {
	#Input: "control" object
	#Pick "CONTROLLER"
	#Check their contents
	sub.inds<-grep(control,world[,1])
	pred.inds<-grep("#CONTROLLED$",world[,2])
	as.character(rdf[intersect(sub.inds,pred.inds),3])
}

sanitize.w3.string <- function(str) {
  # Remove W3 string identifier from str if it is there, otherwise
  # leave str as is.

  w3.string <- "^^<http://www.w3.org/2001/XMLSchema#string>\""
  if ((substr(str, 1, 1) == "\"") &&
      (substr(str, nchar(str)-nchar(w3.string)+1, nchar(str)) == w3.string)) {
    return(substr(str, 2, nchar(str)-nchar(w3.string)))
  } else {
    return(str)
  }
}

get.xref.db.and.id <- function(xref,world=rdf) {
  # Returns DB and ID fields of a given XREF
  sub.inds<-pick.subject.fields(xref,world)
  db.inds<-grep("#DB$",world[,2])
  id.inds<-grep("#ID$",world[,2])
  list(db=sanitize.w3.string(as.character(world[intersect(sub.inds,db.inds),3])),
       id=sanitize.w3.string(as.character(world[intersect(sub.inds,id.inds),3])))
}

get.controller.proteins <- function(controller, world=rdf) {
  # Search recursively for proteins in the given CONTROLLER

  # controller is a "physicalEntityParticipant" (or its subclass
  # "sequenceParticipant"). Get its PHYSICAL-ENTITY field.
  pe <- get.physical.entity(controller, world)

  # get protein names recursively
  get.physical.entity.participant.proteins.recursively(pe, world)
}

get.physical.entity.participant.proteins.recursively <- function(pe, world=rdf) {
  # Get protein name for given physicalEntity recursively
  protein <- NULL

  pe.type <- get.class(pe, world)
  for (i in 1:length(pe)) {
    # valid types subtypes of physicalEntity: complex, protein, rna,
    # dna, smallMolecule
    #
    # Reactome databases seem to contains some plain physicalEntity
    # objects also, even though the manual says that they should never
    # be created.

    if (pe.type[i] == "protein") {
      protein <- c(protein, pe[i])
    } else if (pe.type[i] == "complex") {
      x <- get.physical.entity.participant.proteins.recursively(get.physical.entity(get.components(pe[i])))
      protein <- c(protein, x)
    } else {
      stopifnot(pe.type[i] %in% c("rna", "dna", "smallMolecule", "physicalEntity"))
    }
  }
  
  return(protein)
}

get.step.catalysts <- function(pw.steps, world=rdf) {
  # Returns a list of proteins that function as catalysts in the given
  # pathway steps
  catalysts <- vector("list", length(pw.steps))

  for (i in 1:length(pw.steps)) {
    step.ints <- step.interactions(pw.steps[i], world)
    interaction.types <- get.class(step.ints$obs, world)
    is.catalysis <- interaction.types == "catalysis"
    for (j in (1:length(step.ints$obs))[is.catalysis]) {
      controller <- get.controller(step.ints$obs[j], world)
      prot <- get.controller.proteins(controller, world)
      if (length(prot) > 0)
        catalysts[[i]] <- c(catalysts[[i]], prot)
    }
  }
  
  return(catalysts)
}


get.next.steps <- function(pw.steps, world=rdf) {
  # Returns the next-step objects for given pathway steps
  nexts <- vector("list", length(pw.steps))
  for (i in 1:length(pw.steps)) {
    nexts[[i]] <- next.step(pw.steps[i], world)
  }
  return(nexts)
}


get.catalyst.network <- function(rdf) {
  # Returns a protein-protein network. Two proteins are connected if
  # they catalyze successive reactions.

  proteins <- list.proteins(rdf)
  pw.steps <- list.pathway.steps(rdf)
  nexts <- get.next.steps(pw.steps, rdf)
  catalysts <- get.step.catalysts(pw.steps, rdf)

  # use indexes instead of protein names
  protein.inds <- vector("list", length(catalysts))
  for (i in 1:length(catalysts))
    protein.inds[[i]] <- match(catalysts[[i]], proteins)

  links <- NULL
  for (i in 1:length(pw.steps)) {
    if ((length(nexts[[i]]) == 0) || (length(catalysts[[i]]) == 0)) next
    this.proteins <- protein.inds[[i]]
    
    for (j in 1:length(nexts[[i]])) {
      next.i <- match(nexts[[i]][[j]], pw.steps)
      next.proteins <- protein.inds[[next.i]]
      if (length(next.proteins) == 0) next

      # set link between all protein pairs
      for (k in 1:length(this.proteins)) {
        for (l in 1:length(next.proteins)) {
          links <- rbind(links, c(this.proteins[[k]], next.proteins[[l]]))
        }
      }
    }
  }

  # Make unique. Consider links (a,b) and (b,a) equal.
  m <- max(links)
  u <- NULL
  uniqlinks <- NULL
  for (i in 1:dim(links)[1]) {
    a <- min(links[i,])
    b <- max(links[i,])

    x <- b+m*a
    if (!(x %in% u)) {
      u <- c(u, x)
      uniqlinks <- rbind(uniqlinks, links[i,])
    }
  }
  
  
  return(list(links=uniqlinks, proteins=proteins))
}

links.to.flat.file <- function(L, rdf, linkfile="net.links", namefile="net.names") {
  # Writes link list returned by get.catalyst.network() into two
  # files. The links in the network are be written to linkfile. Each
  # line will contain endpoints indexes of a (undirected) link. The
  # indexes refer to proteins that are saved in namefile. Each row
  # will tell the name of the protein and the name of the database.
  
  uniq.prot.inds <- unique(as.vector(L$links))

  # reindex and write links to a file
  new.links <- matrix(0, dim(L$links)[1], dim(L$links)[2])
  for (i in 1:dim(L$links)[1]) {
    a <- which(uniq.prot.inds == L$links[i, 1])
    b <- which(uniq.prot.inds == L$links[i, 2])
    new.links[i, ] <- c(a, b)
  }
  write.table(new.links, file=linkfile, sep="\t", row.names=FALSE, col.names=FALSE)

  # write proteins and databases to a file
  names <- vector("character", length(uniq.prot.inds))
  databases <- vector("character", length(uniq.prot.inds))
  for (i in 1:length(uniq.prot.inds)) {
    xref <- get.xref(L$proteins[uniq.prot.inds[i]], rdf)
    if (length(xref) > 1) {
      # Use only the first xref if there is more than one (they should
      # be synonyms).
      warning("More than one xref (",  L$proteins[uniq.prot.inds[i]], ")")
      xref <- xref[1]
    }
    data <- get.xref.db.and.id(xref, rdf)
    names[i] <- data$id
    databases[i] <- data$db
  }
  write.table(data.frame(names, databases), file=namefile, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE)
}
