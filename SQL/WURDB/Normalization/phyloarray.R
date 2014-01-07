.packageName <- "phyloarray"
setClass("sampleOutliers",
   representation("list")
)

setClass("sampleReproducibility",
   representation("list")
)

setClass("extractionList",
   representation("list")
)
# database utilities for package-internal use only

# _phyloarrayConnection
# Tests whether the database connection is a phyloarray connection.
# Argument: con, a MySQL database connection.
# Value TRUE when the test succeeds. Otherwise a program halt.
".phyloarrayConnection" <- function (con) {
   if (!(class(con)=='MySQLConnection')) {
      stop('Input must be a DBI connection to a phyloarray database')
   }
   essential <- c("array",
                  "arraydesign",
                  "arrayfeature",
                  "arrayhybridisations",
                  "featureextraction",
                  "featuremeasurement",
                  "hybridisation",
                  "image",
                  "oligo",
                  "oligoclass",
                  "oligotargetpair",
                  "phylogeny",
                  "project",
                  "sample",
                  "slide",
                  "target",
                  "taxon",
                  "taxonlevel",
                  "taxtotax")
   if (!(length(intersect(dbListTables(con),essential))==length(essential))) {
      stop('Essential tables missing in the connected database. Not a phyloarray database?')
   }
   return(TRUE)
}

# Expands one element (one "field", "value" pair list) from a list of "field", "value" pair lists
".expandElement" <- function (elm) {
   if (is.list(elm)) {
      if (is.null(elm$field)) {
         stop("Missing 'field' field in condition")
      }
      if (!is.vector(elm$field)) {
         stop("Field 'field' must be vector with length 1")
      }
      if (length(elm$field)>1) {
         stop("Field 'field' must be vector with length 1")
      }
      if (!is.vector(elm$value)) {
         stop("'value' field must be vector")
      }
      if (is.null(elm$value)) {
         stop("Missing 'value' field in condition")
      }
      if (is.character(elm$value)) {
         elm$value <- paste("'",elm$value,"'",sep='')
      }
       return(paste("(",paste(elm$field,elm$value,sep='=',collapse=' OR '),")",sep=''))
   }
   else {
      stop("Argument 'condition' must be a list of lists with 'field' and 'value' pairs")
   }
}  

# Expands a list of "field", "value" pair lists into an SQL condition
".expandCondition" <- function (condition) {
   if (is.null(condition)) {
      return(NULL)
   }
   else {
      if (is.list(condition)) {
         return(paste(" WHERE",paste(lapply(condition, .expandElement),collapse=' AND ')))
      }
      else {
         stop("Argument 'condition' must be a list of lists with 'field' and 'value' pairs")
      }
   }  
}

"fetch.projects" <- function (con, condition=NULL) {
   if (.phyloarrayConnection(con)) {
      stm <- paste("SELECT * FROM project", .expandCondition(condition), sep='')
      rs <- dbSendQuery(con, stm)
      prjs <- fetch(rs, n=-1)
      return(prjs)
   }
}

"fetch.samples" <- function (con, condition=NULL) {
   if (.phyloarrayConnection(con)) {
      stm <- paste("SELECT * FROM sample", .expandCondition(condition), sep='')
      rs <- dbSendQuery(con, stm)
      smps <- fetch(rs, n=-1)
      return(smps)
   }
}

"fetch.extractions" <- function (con, condition=NULL) {
   if (.phyloarrayConnection(con)) {
      stm <- paste("SELECT arrayID, barcode, array, sampleID, dye, isDiscarded,",
                   "featureExtraction.* FROM array JOIN hybridisation USING",
                   "(arrayID) JOIN featureExtraction USING (hybridisationID)")
      stm <- paste(stm, .expandCondition(condition), sep='')
      rs <- dbSendQuery(con, stm)
      extrs <- fetch(rs, n=-1)
      return(new("extractionList",list(extractions=extrs))) 
   }
}

"fetch.measurements" <- function (con, extrList, which.data=c("raw","rawBGcor","spatnorm","qnorm"), transformation="none") {
   if (.phyloarrayConnection(con)) {
      # Only one type of tranformation allowed
      tr = match.arg(transformation,c("none","avg","log","avglog"),several.ok=FALSE)
      which.data = match.arg(which.data,several.ok=TRUE)
      what = list("none"=list("id"=c("af.featureID","oligoID","fm.isOutlier"),
                              "raw"="FGsignal AS raw",
                              "rawBGcor"="(FGsignal-BGsignal) AS rawBGcor",
                              "spatnorm"="spatNormSignal",
                              "qnorm"="normSignal"),
                  "avg"=list("id"="oligoID",
                             "raw"="avg(FGsignal) AS avgSignal, std(FGsignal) AS stdevSignal",
                             "rawBGcor"="avg(FGsignal-BGsignal) AS avgSignalBGcor, std(FGsignal-BGsignal) AS stdevSignalBGcor",
                             "spatnorm"="avg(spatNormSignal) AS avgSpatNormSignal, std(spatNormSignal) AS stdevSpatNormSignal",
                             "qnorm"="avg(normSignal) AS avgNormSignal, std(normSignal) AS stdevNormSignal"),
                  "log"=list("id"=c("af.featureID","oligoID","fm.isOutlier"),
                             "raw"="log(FGsignal) AS lgRaw",
                             "rawBGcor"="log(FGsignal-BGsignal) AS lgRawBGcor",
                             "spatnorm"="log(spatNormSignal) AS lgSpatNormSignal",
                             "qnorm"="log(normSignal) AS lgNormSignal"),
                  "avglog"=list("id"="oligoID",
                                "raw"="avg(log(FGsignal)) AS avgLgSignal, std(log(FGsignal)) AS stdevLgSignal",
                                "rawBGcor"="avg(log(FGsignal-BGsignal)) AS avgLgSignalBGcor, std(log(FGsignal-BGsignal)) AS stdevLgSignalBGcor",
                                "spatnorm"="avg(log(spatNormSignal)) AS avgLgSpatNormSignal, std(log(spatNormSignal)) AS stdevLgSpatNormSignal",
                                "qnorm"="avg(log(normSignal)) AS avgLgNormSignal, std(log(normSignal)) AS stdevLgNormSignal"),          
                  )
      fields = paste(c(c("sampleID","fe.extractionID"),
                       what[[tr]]$id,unlist(what[[tr]][which.data])), collapse=", ")
      grouping = ifelse((tr=="avg" | tr=="avglog"),"GROUP BY oligoID, extractionID","")
      condition = .expandCondition(list(list(field='extractionID',value=extrList$extractions$extractionID)))
      stm <- paste("SELECT",fields,"FROM hybridisation h ",
                   "JOIN featureExtraction fe USING (hybridisationID)",
                   "JOIN featureMeasurement fm USING (extractionID)",
                   "JOIN arrayFeature af USING (featureID)",
                   "JOIN probe p USING (probeID)",condition, grouping)
      rs <- dbSendQuery(con, stm)
      data <- fetch(rs, n=-1)
      return(data) 
   }   
}

"choose.projects" <- function (con, multi=TRUE, title='Select studies:', condition=NULL) {
   prjs <- fetch.projects(con, condition=condition)
   projects <- select.list(prjs$projectName, multiple=multi, title=title)
   prjs <- fetch.projects(con, condition=list(list(field='projectName',value=projects)))
   return(prjs)
}

"choose.samples" <- function (con, multi=TRUE, title='Select samples:', condition=NULL) {
   smps <- fetch.samples(con, condition=condition)
   samples <- select.list(smps$sampleID, multiple=multi, title=title)
   smps <- fetch.samples(con, condition=list(list(field='sampleID',value=samples)))
   return(smps)
}

#"choose.extractions" <- function (con, multi=TRUE, title='Select extractions:', condition=NULL) {
#   etrs <- fetch.extractions(con, condition=condition)
#   extractions <- select.list(extrs$sampleID, multiple=multi, title=title)
#   extrs <- fetch.extractions(con, condition=list(list(field='sampleID',value=extractions)))
#   return(smps)
#}

# Generic method for submission of calculated data to the database
"submitDB" <- function(x) {
   if (!is.list(x)) {
      stop("Argument 'x' must be a list")
   }
   if (!is.null(x$dbcon)) {
      dbSendQuery(x$dbcon,'DROP TABLE IF EXISTS rinput')
   }
   else {
      stop("Missing 'dbcon' field in argument 'x'")
   }
   UseMethod("submitDB")
}

"submitDB.sampleOutliers" <- function(x) {
   # Delete previous outlier detection results for these samples. Also set
   # the reproducibility-check flag to 0 for all featureextractions belonging
   # to these samples.
   statement <- paste(
      "UPDATE featuremeasurement fm, featureextraction fe, hybridisation h ",
      "SET fm.isOutlier=0, fe.hasReproCheck=0 WHERE h.sampleID='",x$sampleID,"' ",
      "AND fm.extractionID=fe.extractionID ",
      "AND fe.hybridisationID=h.hybridisationID",
      sep='')
   rs <- dbSendQuery(x$dbcon,statement)
   # Put the outliers in a temporary table and transfer the data to the permanent tables
   if (dim(x$spots)[1]>0) {
      dbWriteTable(x$dbcon,'rinput',row.names=FALSE,x$spots,append=TRUE)
      dbSendQuery(x$dbcon,'ALTER TABLE rinput ADD INDEX (featureID)')
      dbSendQuery(x$dbcon,'ALTER TABLE rinput ADD INDEX (extractionID)')
      dbSendQuery(x$dbcon,'UPDATE featuremeasurement f, rinput r SET f.isOutlier=1 WHERE f.featureID=r.featureID and f.extractionID=r.extractionID')
   }
}
# General functions
###################
# Choosing (and creating) a directory
chooseDir <- function (...) {
  choice = ''
  while (choice == '') {
    choice <- guiDlgDir(dir='',...)
  }
  # if it doesn't exist, it must be made
  if (!file.exists(choice)) {
    dir.create(choice,recursive=TRUE)
  }
  return(choice)
}

panel.stability <- function (x, y, scale=c("linear","logarithmic"), ...) {
   scale <- match.arg(scale)
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(0, 1, 0, 1))
   ## our x and y are already logarithms of signals
   if (scale=="logarithmic") {
      r <- abs(cor(x, y))
      m <- rsd(x, y)*100
   }
   if (scale=="linear") {
      r <- abs(cor(exp(x), exp(y)))
      m <- rsd(exp(x), exp(y))*100
   }
   txt <- paste('Pearson=',format(c(r, 0.123456789), digits=5)[1],sep='')
   txt <- paste(txt,paste('RSD=',format(c(m,0.123456789), digits=2)[1],'%',sep=''),sep="\n")
   text(0.5, 0.5, txt, cex=1.5)
}

# General HTML report functions
###############################
# header part
HTMLReportBegin = function (file="report.html",title="Report Title") {
   cat(paste("<html><head><title>",
   title, "</title></head>",
   "<body>",
   sep = ""), file=file, append=TRUE)
}

# generic report part
HTMLReport = function (x, file=paste(file.path(x$directory,x$filename),'html',sep='.'), CSSfile='R2HTML.css') {
   HTMLReportBegin(file)
   file.copy(file.path(system.file(package='R2HTML'),'output','R2HTML.css'),file.path(x$directory,'R2HTML.css'))
   HTMLCSS(file=file, CSSfile=CSSfile)
   HTML(x,file=file)
   HTMLReportEnd(file)
   return(file)
}

# end part
HTMLReportEnd = function (file="report.html") {
   cat("<hr size=1></body></html>",
       file=file, append=TRUE)
}
minmax <- function (x) {
   if (is.vector(x) & class(x)=='numeric') {
      ifelse (abs(max(x)-mean(x))<abs(min(x)-mean(x)),which.min(x),which.max(x))
   }
   else {
      stop('Input for minmax must be a numerical vector')
   }
} 

"outlierPvalue" <- function (x,avgvar) {
   test <- chisq.out.test(x,variance=avgvar)
   return(test$p.value)
}

quantnorm <- function (data,margin="extractionID",signal="signal") {
  # testing data integrity
  if (!is.data.frame(data)) {
    stop("Expecting a dataframe as the argument 'data'")
  }
  if (!(length(setdiff(c(margin,signal),names(data)))==0)) {
    stop(paste("Missing columns in data:",paste(setdiff(c(margin,signal),names(data)),collapse=" and ")))
  }
  # calculation 
  extrIDs <- unique(data[[margin]])
  if (length(extrIDs) > 1) {
    equallength = TRUE
    vlength = length(data[[signal]][data[[margin]]==1])
    minlength = vlength 
    for (i in extrIDs) {
      equallength <- equallength & length(data[[signal]][data[[margin]]==i])==vlength
      if (length(data[[signal]][data[[margin]]==i])<minlength) {
        minlength = length(data[[signal]][data[[margin]]==i])
      }  
    }
    if (minlength > 1) {         
      if (equallength) {
        # mode 1: equal length vectors, averaging per row of sorted vectors
        m <- data[[signal]][data[[margin]]==extrIDs[1]]
        for (i in extrIDs[2:length(extrIDs)]) {
          m <- cbind(m,data[[signal]][data[[margin]]==i])
        }
        colnames(m)=extrIDs
        m.ord <- apply(m,2,order)
        m.sort <- apply(m,2,sort)
        m.avg <- apply(m.sort,1,mean)
        for (i in extrIDs) {
          data[[signal]][data[[margin]]==i] = m.avg[order(m.ord[,as.character(i)])]
        }
      }
      else {
        if (minlength > 3) {
          # mode 2: vectors with unequal length, averaging of splines that fit 
          # sorted signals to relative index position. Spline fits Require
          # vectors with at least length 4
          splinefits <- list()
          for (i in 1:length(extrIDs))  {
            s1 <- data[[signal]][data[[margin]]==extrIDs[i]]
            splinefits[[i]] <- spl<-smooth.spline((0:(length(s1)-1))/(length(s1)-1),sort(s1))
          }
          for (i in extrIDs) {
            s1 <- data[[signal]][data[[margin]]==i]
            vb <- lapply(splinefits,predict,(rank(s1)-1)/(length(s1)-1))
            data[[signal]][data[[margin]]==i]=apply(matrix(unlist(vb),ncol=length(vb))[(length(s1)+1):(2*length(s1)),],1,mean)    
          }   
        }
      }   
    }
  }
  return(data)
}
rsd <- function(x,y=NULL) {
   if (is.data.frame(y)) {
      y <- as.matrix(y)
   } else {
      stopifnot(is.atomic(y))
   }
   if (is.data.frame(x)) {
      x <- as.matrix(x)
   } else {
      stopifnot(is.atomic(x))
      if (!is.matrix(x)) {
         if (is.null(y))
            stop("Supply both 'x' and 'y' or a matrix-like 'x'")
         x <- as.vector(x)
      }
   }
   if (!is.null(y)) {
      x <- cbind(x,y)
   }
   RSD <- matrix(,nrow=dim(x)[2],ncol=dim(x)[2],dimnames=list(colnames(x),colnames(x)))
   for (i in 1:dim(x)[2]) {
      for (j in 1:dim(x)[2]) {
         RSD[i,j]=mean(sqrt(pi)*abs(x[,i]-x[,j])/abs(x[,i]+x[,j]),na.rm=TRUE)
      }
   }
   #if (dim(x)[2]==2) {
   #   RSD <- RSD[1,2]
   #}
   return(RSD)
}
sampleOutliers <- function (con, sampleID, significance=0.001) {
   if (.phyloarrayConnection(con)) {
      if ((!(is.vector(sampleID) & length(sampleID)==1)) | (!is.character(sampleID))) {
         stop("Argument 'sampleID' must be a character vector of length 1")
      }
      out.extractionIDs <- c()
      out.featureIDs <- c()
      # Only ssRNA probes are selected, not control probes etc.

      statement <- paste(
      
         # AS signal did not work (LL); name reassigned below
    	 #"SELECT o.oligoID, fm.extractionID, fm.featureID, log(fm.spatNormSignal) AS signal ",

    	 "SELECT o.oligoID, fm.extractionID, fm.featureID, log(fm.spatNormSignal) ",
         "FROM featuremeasurement fm, oligo o, probe p, arrayfeature af, hybridisation h, featureextraction fe ",
         "WHERE h.sampleID='",sampleID, "' AND NOT h.isDiscarded ",
         "AND fe.hybridisationID=h.hybridisationID AND NOT fe.noReproCheck ",
         "AND fm.extractionID=fe.extractionID AND fm.featureID=af.featureID ",
         "AND af.probeID=p.probeID AND p.oligoID=o.oligoID AND o.class='ssRNA' ",
         "ORDER BY fm.extractionID, fm.featureID",
         sep='')
      rs <- dbSendQuery(con,statement)
      sig <- fetch(rs, n=-1)

      names(sig)[[which(names(sig) == "log(fm.spatNormSignal)")]] <- "signal"


      sig <- quantnorm(sig)
      avgvar<-mean(tapply(sig$signal,sig$oligoID,var))
      haveoutlier <- tapply(sig$signal,sig$oligoID,outlierPvalue,avgvar)
      haveoutlier <- haveoutlier[haveoutlier<significance]
      if (length(haveoutlier)>0) {
         for (j in names(haveoutlier)) {
            out.row <- sig[sig$oligoID==j,][minmax(sig[sig$oligoID==j,]$signal),]
            out.extractionIDs <- c(out.extractionIDs,out.row$extractionID)
            out.featureIDs <- c(out.featureIDs,out.row$featureID)
         }
      }
      return(new("sampleOutliers",list(spots=data.frame(extractionID=out.extractionIDs,featureID=out.featureIDs), dbcon=con, sampleID=sampleID)))
   }
}
"sampleReproducibility" <- function(con, sampleID) {
   if (.phyloarrayConnection(con)) {
      if ((!(is.vector(sampleID) & length(sampleID)==1)) | (!is.character(sampleID))) {
         stop("Argument 'sampleID' must be a character vector of length 1")
      }
      extr <- fetch.extractions(con,condition=list(list(field='sampleID',value=sampleID)))
      extr <- extr[extr$isDiscarded==0 & extr$noReproCheck==0 & extr$noSampleNormalisation==0,]
      
   }
}
spatnorm <- function(data, x="x", y="y", signal="signal", method=c("loess"), span=0.03, degree=2, family="symmetric", subset=1==1) {
   method <- match.arg(method)
   # data integrity testing
   data <- as.data.frame(data)
   if (!is.data.frame(data)) {
      stop("Input should be a matrix or dataframe")
   }
   if (!(length(setdiff(c(x,y,signal),names(data)))==0)) {
    stop(paste("Missing columns in data:",paste(setdiff(c(x,y,signal),names(data)),collapse=" and ")))
   }
   # calculation
   fit <- loess(as.formula(paste(signal,"~",x,"*",y)),data,span=span,degree=degree,normalize=FALSE,family=family,control=loess.control(iterations=8,cell=0.07),subset=subset)
   pr.signal <- predict(fit,data)
   corr.signal <- data[[signal]] - pr.signal + min(pr.signal)
   corr.signal[corr.signal<0.1]=0.1
   outp <- list(fit=fit,pr.signal=pr.signal,corr.signal=corr.signal)
   invisible(outp)
}
