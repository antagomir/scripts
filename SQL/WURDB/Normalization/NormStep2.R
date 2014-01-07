## Load libraries
require(RODBC)
require(tcltk)
tclRequire("Img")
tclRequire("Tktable")
source.file <- "D:\\Backup\\Source\\Scripts\\source4normstep2.R"

## Make connection
con<-odbcDriverConnect(connection="SERVER=localhost;
	DRIVER=MySQL ODBC 5.1 Driver;
	DATABASE=phyloarray;
	UID=root;
	PWD=array;
	case=tolower")

## Functions:
# Get projects
getprojects <- function(f) {
	query <- "SELECT DISTINCT projectID,projectName FROM project"
	if(f==TRUE) {query <- paste(query," JOIN sample USING (projectID) WHERE normalisationFinished=0;",sep="")}
	project.rs <<- sqlQuery(con,query)
	projects <<- as.character(unique(project.rs$projectName))
	}

# Get samples + projectID's
getsamplessimple <- function(f) {
	query <- "SELECT sampleID,projectID FROM sample"
	if(f==TRUE) {query <- paste(query," WHERE normalisationFinished=0;",sep="")}
	samples.rs <<- sqlQuery(con,query)
	}

# Get samples + file locations
getsamplesextended <- function(f) {
	query <- paste("SELECT sampleID,extractionID,reproducibility,normalisationFinished,noReproCheck,",
		"extractionName,noSampleNormalisation,dye,i.ipath AS repimage,ii.ipath AS spaimage FROM sample s ",
		"JOIN hybridisation h USING (sampleID) ",
		"JOIN featureextraction f USING (hybridisationID) ",
		"JOIN image i ON s.imageID=i.imageID ",
		"JOIN image ii ON f.imageID=ii.imageID",sep="")
	if(f==TRUE) {query <- paste(query," WHERE normalisationFinished=0;",sep="")}
	samples.rf <<- sqlQuery(con,query)
	}

# Select samples from project
f.project <- function (ret=FALSE) {
	require(tcltk)
	prj <- tclvalue(prj.def)
	id  <- project.rs$projectID[which(project.rs$projectName==prj)]
	samples <- as.character(samples.rs$sampleID[which(samples.rs$projectID==id)])
	samp <- samples[j]
	tkconfigure(samp.label, text=samp)
	if(ret){return(samples)}
	}

filter.do <- function() {
	if(filt==FALSE){
		#tkconfigure(filter.label,text="Filter is switched on \n(not-normalised samples only!)")
		filt<<-TRUE
		txtfil<<-"Filter is switched on \n(not-normalised samples only!)"
		bgcol<<-"green"
		tkdestroy(tt)
		j <<- 1; n<<- 1; i<<-1
		redo <<- 1
		source(source.file)
		} else {
			#tkconfigure(filter.label,text="Filter is switched off\n(showing all samples)")
			filt<<-FALSE
			txtfil<<-"Filter is switched off\n(showing all samples)"
			bgcol<<-"white"
			tkdestroy(tt)
			j <<- 1; n<<- 1; # i<<-1
			redo <<- 1
			source(source.file)
			}
	}

# End of ... message (beginning)
begin.message <- function() {
	tkmessageBox(message="First one !!!",icon="error",type="ok")
	}
# End of ... message (end)
end.message <- function() {
	tkmessageBox(message="Last one !!!",icon="error",type="ok")
	}
	
changeprj <- function() {
	i <<- 2
	prj <<- tk_select.list(projects,multiple=F,title="Select project",preselect=projects[1])
	tkdestroy(tt)
	j <<- 1; n<<- 1
	source(source.file)
	# tkmessageBox(title="Test project", message = "Done")
	}
	
changesamp.prev <- function() {
	if(j-1<1){begin.message()} else {
			j<<- j-1
			tkdestroy(tt)
			source(source.file)
			}
	}
	
changesamp.next <- function() {
	if(j+1>length(samples)){end.message()} else {
			j<<- j+1
			tkdestroy(tt)
			source(source.file)
			}
	}

viewnorm <- function() {
	id <- extractions[n,1]
	picname <- as.character(extractions[n,5])
	dye <- extractions[n,2]
	norep <- extractions[n,3]
	nosamp <- extractions[n,4]
	name.ex <- extractions[n,6]
	
	gg <<- tktoplevel()
	tkwm.title(gg, paste("Spatial normalisation of --->",samp,"<--- (",name.ex,dye,")"))
	tkwm.geometry(gg, "1600x430+1+600")

	imageSpa <- tclVar()
	tcl("image","create","photo",imageSpa,file=picname)
	imgAsLabelx <- tklabel(gg,image=imageSpa,bg="white")
	tkgrid(imgAsLabelx,row=1,column=1,rowspan=16,columnspan=32)
	
	spa.norm.next.but <- tkbutton(gg,text="  >  ",command=changenorm.next,bg="lightblue")
	spa.norm.prev.but <- tkbutton(gg,text="  <  ",command=changenorm.prev,bg="lightblue")
	tkgrid(spa.norm.next.but,row=17,column=17)
	tkgrid(spa.norm.prev.but,row=17,column=16)
	
	## NoRpro and NoSampleNorm functions
	# Create labels
	norep.label 	<- tklabel(gg, text="Include for reproducibilty calculations?") 
	nosamp.label 	<- tklabel(gg, text="Include for quantile normalisation (step 4)?")
	if(norep==0){norep.def <- "Yes"} else {norep.def <- "No"}
	if(nosamp==0){nosamp.def <- "Yes"} else {nosamp.def <- "No"}
	norep.stat <- tklabel(gg, text=norep.def, bg="white")  
	nosamp.stat <- tklabel(gg, text=nosamp.def, bg="white")  
	tkgrid(norep.label,row=1,column=33)
	tkgrid(norep.stat,row=2,column=33)
	tkgrid(nosamp.label,row=4,column=33)
	tkgrid(nosamp.stat,row=5,column=33)
	# the functions
	resetnormalisation <- function(sample) {
	query <- paste("UPDATE sample s ",
					"JOIN hybridisation h USING (sampleID) ",
					"JOIN featureExtraction fe USING (hybridisationID) ",
					"SET normalisationFinished=0, normAlgVersion=0, hasReproCheck=0, spatNormAlgVersion=0 ",
					"WHERE sampleID='",sample,"'",sep="")
	tkmessageBox(title = "Check", message = query, icon = "info", type = "ok")
	sqlQuery(con,query)
	}
	savenorepro <- function() {
		nono <- paste("UPDATE featureextraction SET noReproCheck=1 WHERE extractionID=",id,sep="")
		tkconfigure(norep.stat,text="No")
		sqlQuery(con,nono)
		resetnormalisation(samp)
		tkdestroy(tt)
		redo<<-1
		source(source.file)
		}
	saverepro <- function() {
		nono <- paste("UPDATE featureextraction SET noReproCheck=0 WHERE extractionID=",id,sep="")
		tkconfigure(norep.stat,text="Yes")		
		sqlQuery(con,nono)
		resetnormalisation(samp)
				tkdestroy(tt)
		redo<<-1
		source(source.file)
		}	
	savenosampnorm <- function() {
		nono <- paste("UPDATE featureextraction SET noSampleNormalisation=1 WHERE extractionID=",id,sep="")
		tkconfigure(nosamp.stat,text="No")
		sqlQuery(con,nono)
		resetnormalisation(samp)
				tkdestroy(tt)
		redo<<-1
		source(source.file)
		}
	savesampnorm <- function() {
		nono <- paste("UPDATE featureextraction SET noSampleNormalisation=0 WHERE extractionID=",id,sep="")
		tkconfigure(nosamp.stat,text="Yes")	
		sqlQuery(con,nono)
		resetnormalisation(samp)
				tkdestroy(tt)
		redo<<-1
		source(source.file)
		}
		
	save.repro.but <- tkbutton(gg,text=" Use it ",command=saverepro,bg="lightgreen")
	nosave.repro.but <- tkbutton(gg,text=" Don't use it ",command=savenorepro,bg="red")
	tkgrid(save.repro.but,row=2,column=34)
	tkgrid(nosave.repro.but,row=2,column=35)
	
	save.sampnorm.but <- tkbutton(gg,text=" Use it ",command=savesampnorm,bg="lightgreen")
	nosave.sampnorm.but <- tkbutton(gg,text=" Don't use it ",command=savenosampnorm,bg="red")
	tkgrid(save.sampnorm.but,row=5,column=34)
	tkgrid(nosave.sampnorm.but,row=5,column=35)
	}

changenorm.prev <- function() {
		if(n-1<1){begin.message()} else {
			n<<- n-1
			tkdestroy(gg)
			viewnorm()
			}
	}

changenorm.next <- function() {
		if(n+1>length(extractions[,1])){end.message()} else {
			n<<- n+1
			tkdestroy(gg)
			viewnorm()
			}
	}

redo<-1
filt<-FALSE					# filter
i <- 1 						# project
j <- 1 						# sample (within project)
n <- 1 						# featureextraction normalisation result
txtfil <- "NO"
bgcol <- "magenta"
source(source.file) # main form

