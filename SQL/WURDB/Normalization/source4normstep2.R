
if(redo==1) {
	getprojects(f=filt)
	getsamplessimple(f=filt)
	getsamplesextended(f=filt)
	redo<-0
	}

#### Main form

tt <- tktoplevel()											# Start tk form
tkwm.title(tt, "Reproducibility check tool")				# insert title
tkwm.geometry(tt, "1700x800+1+1")							# determine shape and position of form
samp.label <- tklabel(tt, text="Sample", bg="lightblue")	# sample label, needs to be dynamically available

pr.but <- tkbutton(tt,text="Change project",command=changeprj)

# Filter for not-normalised samples
filter.label <- tklabel(tt, text=txtfil,bg=bgcol)
filter.but <- tkbutton(tt,text=" Filter ",command=filter.do,bg="yellow")
tkgrid(filter.label,row=2,column=8)
tkgrid(filter.but,row=1,column=8)


# Create label for project dropdown-box
project.label 	<- tklabel(tt, text="Selected project: ") 

# Create list for dropdown-box + fill up the boxes
# projects <- as.character(project.rs$projectName)
if(i==1){prj <- projects[1]} #else {cat("Project:",prj,"\n")}
prj.def <- tclVar(prj)
project.combo <- tklabel(tt, textvariable=prj.def, bg="white")  
# Enter on form
tkgrid(project.label, row=1, column=1)
tkgrid(project.combo, row=1, column=2) 

# Sample selection area on form
samples <- f.project(ret=TRUE)
samp <- samples[j]
tkgrid(pr.but,row=1,column=3)
tkgrid(tklabel(tt, text="Sample: "), row = 1, column = 4)
tkgrid(tklabel(tt, text=" < "), row = 1, column = 5)
tkgrid(samp.label, row = 1, column = 6)				
tkgrid(tklabel(tt, text=" > "), row = 1, column = 7)
prev.but <- tkbutton(tt,text="  <  ",command=changesamp.prev,bg="green")
next.but <- tkbutton(tt,text="  >  ",command=changesamp.next,bg="green")
tkgrid(prev.but,row=1,column=5)
tkgrid(next.but,row=1,column=7)
fin <- unique(samples.rf[which(samples.rf$sampleID==samp),"normalisationFinished"])
if(fin==1){tkgrid(tklabel(tt, text=paste("Sample normalisation is Finished!"),bg="lightgreen"), row = 2, column = 1, columnspan=7)}
if(fin==0){tkgrid(tklabel(tt, text=paste("Sample is not yet normalised!"),bg="red"), row = 2, column = 1, columnspan=7)}

# Bind function (fills in the rest of the form with first sample of selected project)
# tkbind(project.combo, "<<ComboboxSelected>>", f.project)  

scr <- tkscrollbar(tt, repeatinterval=5,
            command=function(...)tkyview(txt,...))
txt <- tktext(tt,bg="white",font="Arial",yscrollcommand=function(...)tkset(scr,...))
tkgrid(txt,row=3,column=8,rowspan=8)
tkgrid(scr,row=3,column=9,rowspan=8)
tkgrid.configure(scr,sticky="ns")	
	
#Repro picture
image0 <- tclVar()
rep.image <- as.character(unique(samples.rf$repimage[which(samples.rf$sampleID==samp)]))
tcl("image","create","photo",image0,file=rep.image)
imgAsLabel <- tklabel(tt,image=image0,bg="white")
tkgrid(imgAsLabel,row=3,column=1,columnspan=7,rowspan=16)
		
#Repro text
rep.txt <- as.character(unique(samples.rf$reproducibility[which(samples.rf$sampleID==samp)]))
#cat("Sample:",samp,"\nRep text:",rep.txt,"\n")
reps <- strsplit(rep.txt,"\n")[[1]]
reps.end <- length(reps)
tclarray0 <- tclArray()
tclarray0[[0,0]] <- "Reproducibility results"
for (i in 1:reps.end){	tclarray0[[i,0]] <- reps[i] }
table0 <- tkwidget(txt,"table",variable=tclarray0,rows=reps.end+1,cols=1,titlerows=1,colwidth=135,selectmode="extended",background="white")
tkgrid(table0,row=1,column=1)
tkconfigure(txt, state="disabled")
		
#Measurement table
tclarray <- tclArray()
tclarray[[0,0]] <- "Measurement"
tclarray[[0,1]] <- "NOT used for Reproducibility"
tclarray[[0,2]] <- "NOT used for Quantile normalisation"
m.tab <- samples.rf[which(samples.rf$sampleID==samp),c("extractionName","dye","noReproCheck","noSampleNormalisation")]	#data needed
end<-length(m.tab[,1])
for (i in 1:end){
	tclarray[[i,0]] <- paste(m.tab[i,1],m.tab[i,2])
	tclarray[[i,1]] <- m.tab[i,3]
	tclarray[[i,2]] <- m.tab[i,4]
	}

table1 <- tkwidget(tt,"table",variable=tclarray,rows=end+1,cols=3,titlerows=1,colwidth=45,selectmode="extended",background="white")
tkgrid(table1,row=9,column=8,rowspan=8)

#View spatial normalisation
extractions <- samples.rf[which(samples.rf$sampleID==samp),c("extractionID","dye","noReproCheck","noSampleNormalisation","spaimage","extractionName")]


spa.norm.view.but <- tkbutton(tt,text="\nView & Accept or Reject\n",command=viewnorm,bg="grey")
tkgrid(spa.norm.view.but,row=9,column=10,rowspan=3,columnspan=2)

