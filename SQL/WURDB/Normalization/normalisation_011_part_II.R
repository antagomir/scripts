# Spatial and sample normalisation and outlier detection
#
# Script version 0.11
# for Phyloarray database version 0.8 or 0.9
#

### ADAPT THIS SECTION FOR YOUR OWN LOCAL DB(s)
# --------------------------------------------
# Storage of images
rep.i.path <- "D:\\Arrays\\ChickChip\\Reproducibility_images\\"
spa.i.path <- "D:\\Arrays\\ChickChip\\Spatial_normalisation_images\\"
spa.i.sql  <- gsub("\\\\","/",spa.i.path)
rep.i.sql  <- gsub("\\\\","/",rep.i.path)
# Using DM's 'phyloarray' package on R version > 2.10
source("C:\\Program Files\\R\\R-2.12.2\\library\\phyloarray\\R\\phyloarray")
# --------------------------------------------

# library(phyloarray) # replaced by the source command above
library(fields)
library(outliers)
library(RMySQL)
library(DBI)
library(R2HTML)
library(svDialogs)

# DATABASE ACCESS
dbuser <- 'root'
dbpwd <- 'array'
drv <- dbDriver("MySQL")
con <- dbConnect(drv,username=dbuser,password=dbpwd,dbname='chickchipdb')

####################################################################################################################
#                                                                                                                  #
#                                              ALGORITHM VERSIONS                                                 #
#                                                                                                                  #
####################################################################################################################

# Spatial normalization algorithm version
spatNormAlgv <- '1.1'

# Sample normalisation algorithm version
normAlgv<- '1.1'

# ========= END OF:  ALGORITHM VERSIONS ===========================================================================


####################################################################################################################
#                                                                                                                  #
#                          SPATIAL NORMALIZATION ON SINGLE ARRAYS / SINGLE CHANNELS                                #
#                                                                                                                  #
####################################################################################################################
# Retrieving arrays (in fact single channel measurements) that still have to be spatially normalized
# Only those array-channels are retrieved that have not been normalized or were normalized
# using a previous version of the normalization algorithm. Also discarded arrays are spatially
# normalized

# To avoid normalisation problems with arrays that have the main part of their spots in the lower signal
# region we define an approximate background level. If normalisation pictures do not look good, or
# if normalisation gets stuck this level should be changed.

bgLevel <- 60

statement <- paste("SELECT DISTINCT s.sampleID, f.extractionID, extractionName, dye, spatNormAlgVersion",
                   "FROM featureMeasurement fm, featureExtraction f, hybridisation h, sample s",
                   "WHERE h.sampleID=s.sampleID",
                   "AND f.hybridisationID=h.hybridisationID",
                   "AND fm.extractionID=f.extractionID",
                   "AND NOT normalisationFinished",
                   "AND (spatNormAlgVersion IS NULL OR spatNormAlgVersion <",spatNormAlgv,")")
rs <- dbSendQuery(con,statement)
fe <- fetch(rs, n = -1)

# Correct all arrays using the within array spatial cosmetics procedure
# and store the data in the field "spatNormSignal" of table "featureMeasurement.
if (length(fe)>0) {
   fe$extractionName <- as.factor(fe$extractionName)
   fe$dye <- as.factor(fe$dye)
   fe$sampleID <- as.factor(fe$sampleID)
   workdir <- getwd()
   for (i in fe$extractionID) {
      dbSendQuery(con,'DROP TABLE IF EXISTS rinput')
      # delete old images connected with this extraction
      rs <- dbSendQuery(con,paste("SELECT imageID FROM featureExtraction WHERE extractionID=",i,sep=''))
      id <- fetch(rs, n=-1)
      if (!is.na(id$imageID)) {
         id <- id$imageID
         dbSendQuery(con,paste("UPDATE featureExtraction SET imageID=NULL WHERE extractionID=",i,sep=''))
         dbSendQuery(con,paste("DELETE FROM image WHERE imageID=",id,sep=''))
      }
      cat("Normalising extractionID",i,"\n")
      statement <- paste(
         "SELECT m.featureID, m.extractionID, FGsignal, BGsignal, arrayCol, arrayRow ",
         "FROM arrayFeature a, featureMeasurement m ",
         "WHERE a.featureID=m.featureID ",
         "AND extractionID='",i,"' ORDER BY arrayRow, arrayCol",sep='')
      rs <- dbSendQuery(con,statement)
      d <- fetch(rs, n = -1)
      cols <- sort(unique(d$arrayCol))
      rows <- sort(unique(d$arrayRow))
      upplim <- ifelse(quantile(d$FGsignal,c(0,0.70))[2]<bgLevel,bgLevel,quantile(d$FGsignal,c(0,0.70))[2])
      spatfit <- loess(FGsignal~arrayCol*arrayRow,d[d$FGsignal<=upplim,],span=0.03,degree=2,normalize=FALSE,family="symmetric",control=loess.control(iterations=8,cell=0.07))
      FG <- d$FGsignal
      pr.FG <- predict(spatfit,d)
      corr.FG <- FG - pr.FG + min(pr.FG)
      corr.FG[corr.FG<0.1]=0.1 #To get values >0
	tmp.name <- paste(spa.i.path,"extractionID_",i,".jpg",sep="")
	tmp.name.sql <- paste(spa.i.sql,"extractionID_",i,".jpg",sep="")
      jpeg(file=tmp.name,width=1200,height=400,quality=100)
      layout(matrix(c(1,2,3),nrow=1),respect=TRUE)
      zlim <- quantile(FG,c(0,0.50))
      use <- which(FG >= zlim[1] & FG <= zlim[2])
      img <- as.image(FG[use],ind=as.matrix(cbind(d$arrayRow[use],d$arrayCol[use])))
      image.plot(img,legend.mar=7.1,main='Original signals, lower 50%')
      img <- as.image(pr.FG,ind=as.matrix(cbind(d$arrayRow,d$arrayCol)))
      image.plot(img,legend.mar=7.1,main='Fitted signals, lower 70%')
      zlim <- quantile(corr.FG,c(0,0.50))
      use <- which(corr.FG >= zlim[1] & corr.FG <= zlim[2])
      img <- as.image(corr.FG[use],ind=as.matrix(cbind(d$arrayRow[use],d$arrayCol[use])))
      image.plot(img,legend.mar=7.1,main='Corrected signals, lower 50%')            
      dev.off()
      dbSendQuery(con,'INSERT INTO image(image) values(NULL)')
      rs <- dbSendQuery(con,'SELECT last_insert_id() AS lid')
      id <- fetch(rs, n=-1)
      id <- id$lid
      #dbSendQuery(con,paste("UPDATE image SET image=LOAD_FILE('",workdir,"/tmpimg.jpg') WHERE imageID=",id,sep='')) # replaced by next line
	dbSendQuery(con,paste("UPDATE image SET ipath='",tmp.name.sql,"' WHERE imageID=",id,sep=''))
      dbSendQuery(con,paste("UPDATE featureextraction SET imageID=",id," WHERE extractionID='",i,"'",sep=''))
      dbSendQuery(con,paste("UPDATE featureextraction SET spatNormAlgVersion=",spatNormAlgv,", hasReproCheck=0 WHERE extractionID='",i,"'",sep=''))
      tab <- data.frame(featureID=d$featureID,extractionID=d$extractionID,spatcorsig=corr.FG)
      dbWriteTable(con,'Rinput',row.names=FALSE,tab,append=FALSE)
      dbSendQuery(con,'ALTER TABLE rinput ADD INDEX (featureID)')
      dbSendQuery(con,'ALTER TABLE rinput ADD INDEX (extractionID)') 
      dbSendQuery(con,'UPDATE featuremeasurement f, rinput r SET f.spatNormSignal=r.spatcorsig WHERE f.featureID=r.featureID and f.extractionID=r.extractionID')
   }
   #unlink(tmp.name)
   dbSendQuery(con,'DROP TABLE IF EXISTS rinput')
}
# ========= END OF: SPATIAL NORMALIZATION ON SINGLE ARRAYS / SINGLE CHANNELS =======================================



####################################################################################################################
#                                                                                                                  #
#                                             OUTLIER DETECTION                                                    #
#                                                                                                                  #
####################################################################################################################
# The routine tests for the presence of one outlier in all measurements for the same 
# oligonucleotide and the same sample. Retrieves for each sample all spatially normalized 
# array-channel data and does the chi-square outlier test from the 'outliers' package.
# Only those samples are considered that have a scan that has not yet been checked for
# reproducibility (see next section).
#
#
#             IMPORTANT NOTICE BEFORE YOU CONTINUE !!!!!!!
#
# You should discard very bad hybridisations or individual scans of a 
# hybridisation before performing outlier detection & reproducibility test, 
# otherwise, these bad scans will have a negative effect on outlier detection.
# ON THE OTHER HAND, if you conclude after reproducibility testing that a scan
# was very bad you can always discard it then and repeat the outlier detection 
# and reproducibility tests

significance <- 0.001
# A rather strict outlier detection p-value of 0.001 ensures that in a set of 
# ten-thousands of spots not too many false-positives will be indicated as outlier

statement <- paste(
   "SELECT DISTINCT s.sampleID",
   "FROM sample s, hybridisation h, featureextraction f",
   "WHERE s.sampleID=h.sampleID",
   "AND h.hybridisationID=f.hybridisationID",
   "AND NOT normalisationFinished AND NOT h.isDiscarded",
   "AND NOT f.noReproCheck",
   "AND NOT hasReprocheck",  # ST: this prevents samples with >2 measurements to be used again
   "AND f.spatNormAlgVersion >=",spatNormAlgv,
   "GROUP BY h.sampleID HAVING COUNT(f.extractionID)>1")  #ST: this prevent using samples <2 measurements to be used again
rs <- dbSendQuery(con,statement)
fe <- fetch(rs, n = -1)

for (i in fe$sampleID) {
   cat("Outlier detection for sample: ",i,"\n")
   outl <- sampleOutliers(con, i, significance)
   submitDB(outl)
}

## ========= END OF: OUTLIER DETECTION ==============================================================================



####################################################################################################################
#                                                                                                                  #
#                                                 REPRODUCIBILITY TEST                                             #
#                                                                                                                  #
####################################################################################################################
# Retrieve for each sample all spatially normalized array-channel data and normalize
# them. Then calculate the Pearson correlation coefficient. Only non-outlier spots
# are considered. Signals are averaged per oligonucleotide and scan 

# Selecting the samples to be tested. Only those which are not discarded, and have the 
# flags hasReproCheck=0 and not noReprocheck=0 are selected
# 
statement <- paste(
   "SELECT s.sampleID FROM sample s, hybridisation h, featureextraction f",
   "WHERE s.sampleID=h.sampleID AND h.hybridisationID=f.hybridisationID",
   "AND NOT h.isDiscarded AND NOT f.noReproCheck AND NOT hasReprocheck",
   "AND f.spatNormAlgVersion >=",spatNormAlgv,
   "GROUP BY h.sampleID HAVING COUNT(f.extractionID)>1")
rs <- dbSendQuery(con,statement)
fe <- fetch(rs, n = -1)
   
workdir <- getwd()
for (i in fe$sampleID) {
   cat("Reproducibility calculation for sample",i,"\n")
   # delete old images connected with this extraction
   rs <- dbSendQuery(con,paste("SELECT imageID FROM sample WHERE sampleID='",i,"'",sep=''))
   id <- fetch(rs, n=-1)
   if (!is.na(id$imageID)) {
      id <- id$imageID
      dbSendQuery(con,paste("UPDATE sample SET imageID=NULL WHERE sampleID='",i,"'",sep=''))
      dbSendQuery(con,paste("DELETE FROM image WHERE imageID=",id,sep=''))
   }
   statement <- paste(
      "SELECT f.extractionID, h.arrayID, f.extractionName, h.dye ",
      "FROM featureextraction f, hybridisation h ",
      "WHERE NOT h.isDiscarded AND NOT f.noReproCheck ",
      "AND h.sampleID='",i,"' AND f.spatNormAlgVersion >= ",spatNormAlgv,
      "AND h.hybridisationID=f.hybridisationID",sep='')
   rs <- dbSendQuery(con,statement)
   extr <- fetch(rs, n=-1)
   # Only ssRNA probes are selected and spots that are not outliers
   statement <- paste(
      "SELECT o.oligoID, fm.extractionID, avg(log(fm.spatNormSignal)) AS signal ",
      "FROM hybridisation h, featuremeasurement fm, featureextraction fe, arrayfeature af, probe p, oligo o ",
      "WHERE h.sampleID='",i,"' AND h.hybridisationID=fe.hybridisationID ",
      "AND fe.extractionID=fm.extractionID AND fm.featureID=af.featureID ",
      "AND af.probeID=p.probeID AND p.oligoID=o.oligoID AND o.class='ssRNA' ",
      "AND NOT isOutlier AND NOT h.isDiscarded AND NOT fe.noReproCheck ",
      "AND fe.spatNormAlgVersion >= ",spatNormAlgv," ",
      "GROUP BY o.oligoID, fm.extractionID ORDER BY o.oligoID, fm.extractionID",sep='')
   rs <- dbSendQuery(con,statement)
   sig <- fetch(rs, n=-1)
   sig <- quantnorm(sig)
   A <- c()
   for (j in extr$extractionID) {
      dbSendQuery(con,paste("UPDATE featureExtraction SET hasReproCheck=1 WHERE extractionID=",j,sep=''))
      A <- cbind(A,sig$signal[sig$extractionID==j])
   }
   colnames(A) <- paste(extr$extractionName,extr$dye,sep=":")
   reptab <- cor(A)
   mrdtab <- rsd(A)
   reptabtxt <- c()
   for (s1 in 1:(dim(reptab)[1]-1)) {
      for (s2 in (s1+1):dim(reptab)[1]) {
         cc <- paste(rownames(reptab)[s1]," x ",colnames(reptab)[s2],": ",format(reptab[s1,s2],digits=5)," (Pearson), ",
                     format(mrdtab[s1,s2]*100,digits=3),"% (RSD)",sep='')
         reptabtxt <- c(reptabtxt,cc)
      }
   }
   reptabtxt <- paste(c(reptabtxt),sep='',collapse="\n")
   colnames(A) <- paste(extr$extractionName,extr$dye,sep="\n")
   iii<-gsub("/","_",i)
   tmp.name <- paste(rep.i.path,iii,".jpg",sep="")
   tmp.name.sql <- paste(rep.i.sql,iii,".jpg",sep="")
   jpeg(file=tmp.name,width=700,height=700,quality=100)
   pairs(A,pch=19,cex=0.1,lower.panel=panel.stability,,scale="logarithmic")
   dev.off()
   dbSendQuery(con,'INSERT INTO image(image) values(NULL)')
   rs <- dbSendQuery(con,'SELECT last_insert_id() AS lid')
   id <- fetch(rs, n=-1)
   id <- id$lid
   # dbSendQuery(con,paste("UPDATE image SET image=LOAD_FILE('",workdir,"/tmpimg.jpg') WHERE imageID=",id,sep='')) #replaced by next line ST
   dbSendQuery(con,paste("UPDATE image SET ipath='",tmp.name.sql,"' WHERE imageID=",id,sep=''))
   dbSendQuery(con,paste("UPDATE sample SET imageID=",id," WHERE sampleID='",i,"'",sep=''))
   dbSendQuery(con,paste("UPDATE sample SET reproducibility=\"",reptabtxt,"\" WHERE sampleID='",i,"'",sep=''))
}
#unlink(tmp.name)

# ========= END OF: REPRODUCIBILITY TEST ===========================================================================


####################################################################################################################
#                                                                                                                  #
#                               SAMPLE QUANTILE NORMALISATION OF SELECTED SCANS, SPOTS                             #
#                                                                                                                  #
####################################################################################################################
#
#             IMPORTANT NOTICE BEFORE YOU CONTINUE !!!!!!!
#
# You should discard scans that you do not want to include in the sample 
# normalisation before performing quantile normalisation.
# In principle, only one scan per hybridisation should be included 
# for a sample normalisation. Sample normalisation will only be performed
# on samples having more than 1 scan

# Only those sample measurements are retrieved that have been spatially normalized, and have 
# not yet been normalised or were normalized using a previous version of the normalisation 
# algorithm and and have 2 or more scans per sample that have not been discarded (the "isDiscarded" 
# field has a value >=1)
statement <- paste(
   "SELECT s.sampleID FROM sample s, hybridisation h, featureextraction f",
   "WHERE s.sampleID=h.sampleID AND h.hybridisationID=f.hybridisationID",
   "AND NOT h.isDiscarded AND NOT f.noReproCheck AND NOT f.noSampleNormalisation",
   "AND NOT s.normalisationFinished AND (normAlgVersion <",normAlgv,
   "OR normAlgVersion IS NULL) AND spatNormAlgVersion>=",spatNormAlgv,
   "GROUP BY sampleID HAVING COUNT(f.extractionID)>1")
rs <- dbSendQuery(con,statement)
fe <- fetch(rs, n = -1)

# Between array, within sample, quantile normalisation
if (length(fe)>0) {
   samples <- fe$sampleID
   for (i in samples) {
      cat('Normalising sample "',i,'"\n')
      dbSendQuery(con,'DROP TABLE IF EXISTS rinput')  
      statement <- paste(
      "SELECT fm.extractionID, fm.featureID, log(fm.spatNormSignal) AS signal ",
      "FROM hybridisation h, featuremeasurement fm, featureextraction fe, arrayfeature af, probe p, oligo o ",
      "WHERE h.sampleID='",i,"' AND h.hybridisationID=fe.hybridisationID ",
      "AND fe.extractionID=fm.extractionID AND fm.featureID=af.featureID ",
      "AND af.probeID=p.probeID AND p.oligoID=o.oligoID AND o.class='ssRNA' ",
      "AND NOT isOutlier AND NOT h.isDiscarded AND NOT fe.noReproCheck ",
      "AND NOT fe.noSampleNormalisation AND fe.spatNormAlgVersion >= ",spatNormAlgv," ",
      "ORDER BY fm.featureID, fm.extractionID",sep='')
      rs <- dbSendQuery(con,statement)
      sig <- fetch(rs, n=-1)
      sig <- quantnorm(sig)
      tab <- data.frame(featureID=sig$featureID,extractionID=sig$extractionID,normSignal=exp(sig$signal))
      dbWriteTable(con,'Rinput',row.names=FALSE,tab,append=FALSE)
      dbSendQuery(con,'ALTER TABLE rinput ADD INDEX (featureID)')
      dbSendQuery(con,'ALTER TABLE rinput ADD INDEX (extractionID)')
      dbSendQuery(con,'UPDATE featuremeasurement f, rinput r SET f.normSignal=r.normSignal WHERE f.featureID=r.featureID and f.extractionID=r.extractionID')
      dbSendQuery(con,paste("UPDATE sample SET normAlgVersion=",normAlgv," WHERE sampleID='",i,"'",sep=''))
      dbSendQuery(con, paste("UPDATE sample SET normalisationFinished=1 WHERE sampleID='",i,"'",sep=''))
   }
   dbSendQuery(con,'DROP TABLE IF EXISTS rinput')
}
# ========= END OF: SAMPLE QUANTILE NORMALISATION OF SELECTED SCANS, SPOTS==========================================

####################################################################################################################
#                                                                                                                  #
#                                                   VERSION HISTORY                                                #
#                                                                                                                  #
####################################################################################################################
#
# Version 0.10
# 17-Jun-2011
# by Sebastian Tims (previous ones by Douwe Molenaar)
# 1. Added check to prevent samples to be normalised (over and over again) at the outlier 
#    detection part. The criteria prevent using samples that have:
#	- >2 measurements (hasReprocheck = 1 is such samples)
#	- only 1 f.extractionID (these are not grouped and therfore not taken along anymore)
#
# Version 0.09
# 12-Aug-2007
# 1. Adaptation of spatial normalisation routine to handle arrays with 
#    empty spots. Requires "fields" library.
#
# Version 0.08
# 24-Oct-2006
# 1. Replacement of explicitly defined functions by the 'phyloarray' library:
#    - minmax
#    - outlier
#    - quantnorm
#    - mrd
# 21-Dec-2006
# 2. Added constant bgLevel to spatial normalisation procedure to avoid problems
# with arrays containing mainly spots with very low signals
#
# Version 0.07
# 23-Oct-2006
# 1. Modified the first query under Spatial Normalisation to exclude samples that have no array 
#    data attached (which cause a fatal error further on in the normalisation algorithm)
#
# Version 0.06
# 14-Oct-2006
# 1. Modifications to Reproducibility Test and Sample Quantization method 
#    to prevent these methods from repeating calculations that were already
#    performed before.
#
# Version 0.05
# 4-Oct-2006
# 1. Test correction error message:
# > Normalising extractionID 336 
# > Error in image.default(rows, cols, matrix(FG, ncol = length(cols), byrow = TRUE),  : 
# >        increasing 'x' and 'y' values expected
#
# Version 0.04
# 6-Aug-2006
# 1. Added rsd (relative standard deviation) function to reproducibility test
# 2. Updated quantile normalisation script so as to be able to handle normalisation
#    on vectors of unequal length.
# 3. Added sample quantile normalisation routine
# 4. Modified spatial normalisation routine so that featureMeasurement is up-
#    dated per sample instead of after all samples have been normalised. Main
#    reason is that index generation on large 'rinput' table is very time-
#    consuming when this table is large.
#
# Version 0.03
# 3-Aug-2006
# 1. Corrected error in reproducibility test
# 