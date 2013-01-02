#FetchHITChipAtlas <- function (allowed.projects, dbuser, dbpwd, dbname, 
#		     	       result.path,
#		     	       my.scaling = "minmax", 
#			       mc.cores = 3, 
#			       remove.nonspecific.oligos = FALSE, host = NULL, port = NULL) 
#{

library(microbiome);
fs <- list.files("~/Rpackages/microbiome/microbiome/R/", full.names = T); for (f in fs) {source(f)};
source("~/Rpackages/scripts/R/HITChip/atlas.R")
allowed.projects <- ListAtlasProjects();                                        
#allowed.projects <- sample(ListAtlasProjects(), 5);
dbuser <- 'root';
#dbpwd <- 'fidipro';
dbpwd <- ''
dbname <- 'phyloarray';
host <- NULL
port <- NULL
#host <- '127.0.0.1'
#port <- 3307
result.path <- "/Volumes/h901/fidipro/HITChip/Leo/";
my.scaling <- "minmax";
mc.cores <- 3;
remove.nonspecific.oligos <- FALSE

# ------------------------------------------------------

  # Install new MySQL dump of the database with: 
  # mysql -u"dbuser" -p"dbpwd" dbname < dump.sql
  require(parallel)

  if (!require(RMySQL)) {
    install.packages("RMySQL")
    require(RMySQL)
  }

  # Probes and species to exclude
  rm.phylotypes <- phylotype.rm.list("HITChip")

  # Define the chip type to use
  chiptype   <- "Agilent-016089" 

  full.data.file <- paste(result.path, "/atlas.full.RData", sep = "")
  parameter.data.file <- paste(result.path, "/atlas.parameters.RData", sep = "")
  a <- try(save(chiptype, file = parameter.data.file))
  if (!is.null(a)) { stop("Create result directory in advance!") }

  message("Extract sample information from the HITChip database")
  project.info <- fetch.sample.info(allowed.projects, chiptype, dbuser, dbpwd, dbname, host = host, port = port)

  phylogeny.info <- get.phylogeny.info(phylogeny = "16S", rm.phylotypes$oligos, dbuser, dbpwd, dbname, remove.nonspecific.oligos = remove.nonspecific.oligos, host = host, port = port)

  message("Get probe-level data for the selected hybridisations")
  #tmp <- get.probedata(unique(project.info[["hybridisationID"]]), 
  #    	 		rm.phylotypes$oligos, dbuser, dbpwd, dbname, mc.cores = mc.cores, host = host, port = port)  

#get.probedata <- function (hybridization.ids, rmoligos, dbuser, dbpwd, dbname, mc.cores = 1, host = NULL, port = NULL) {
hybridization.ids <- unique(project.info[["hybridisationID"]])
rmoligos <- rm.phylotypes$oligos

  if (!require(RMySQL)) {
    install.packages("RMySQL")
    require(RMySQL)
  }

  # hybridization.ids <- unique(project.info[["hybridisationID"]]); rmoligos <- rm.phylotypes$oligos; mc.cores = 1

  # List unique hybridisations for the selected samples
  hids <- mysql.format(hybridization.ids)
                      
  require(RMySQL)
  drv <- dbDriver("MySQL")
  if (!(is.null(host) && is.null(port))) {
    con <- dbConnect(drv, username = dbuser, password = dbpwd, dbname = dbname, host = host, port = port)
  } else { 
    con <- dbConnect(drv, username = dbuser, password = dbpwd, dbname = dbname)
  }  

  rs <- dbSendQuery(con, statement = paste("SELECT featureID,extractionID,fe.hybridisationID,spatNormSignal,isOutlier
      		FROM featuremeasurement 
		JOIN featureextraction fe USING (extractionID)
		JOIN hybridisation h USING (hybridisationID)
                JOIN arrayfeature af USE INDEX (PRIMARY) USING (featureID)
		WHERE fe.hybridisationID IN", hids))
  rawdata <- fetch(rs, n = -1)

  ## Check if there is any data
  if(nrow(rawdata) == 0) {
    stop("No data found for these samples (perhaps they are not normalized yet?).\n\n")
  }

  message("Remove outliers")
  rawdata$spatNormSignal[as.logical(rawdata$isOutlier)] <- NA

  message("Split data into arrays") # SLOW - FIXME - speedups?
  rawdata.esplit <- split(rawdata, rawdata$hybridisationID)

  message("Remove NAs")
  na.inds <- sapply(rawdata.esplit, function (x) all(is.na(x$spatNormSignal)))
  rawdata.esplit <- rawdata.esplit[!na.inds]

  # Get probeID - featureID - oligoID mappings
  rs <- dbSendQuery(con, "SELECT fe.featureID,p.probeID,p.oligoID,fe.arrayCol,fe.arrayRow FROM arrayfeature fe JOIN probe p USING (probeID)")
  probes <- fetch(rs, n = -1) 
  
  # Remove specified oligos
  probes <- probes[!probes$oligoID %in% rmoligos,]

  ftab.info <- data.frame(list(featureID = unique(rawdata$featureID)))
  ftab.info[["probeID"]] <- probes$probeID[match(ftab.info$featureID, probes$featureID)]
  ftab.info[["oligoID"]] <- probes$oligoID[match(ftab.info$probeID, probes$probeID)]

  message("Remove NA oligos")
  keep <- !is.na(ftab.info$oligoID)
  ftab.info <- ftab.info[keep, ]
  rownames(ftab.info) <- ftab.info$featureID

  # LL 4.4.2012. With HITChip atlas we encountered some cases where the arrays had different number of entries
  # due to duplicates on some arrays. Now added automated handling here to avoid problems with other array types
  # that may have different natural number of elements on the array.
  if (length(table(sapply(rawdata.esplit, nrow))) == 2) {
    ntmp <- max(sapply(rawdata.esplit, nrow))
    message(paste("Remove elements containing duplicated entries (", round(100*mean(!sapply(rawdata.esplit, nrow) == ntmp), 2), "%)", sep = ""))
    
    # ntmp == !10799 encountered with HITChip atlas, not yet elsewhere
    rawdata.esplit <- rawdata.esplit[!sapply(rawdata.esplit, nrow) == ntmp]
  } else if (length(table(sapply(rawdata.esplit, nrow))) > 2) {
    stop("Error 10799. Arrays are not comparable. Contact R package admins.")
  }

  # Form features x hybridizations matrix 
  inds <- match(rownames(ftab.info), rawdata.esplit[[1]]$featureID)
  ftab <- matrix(NA, nrow = nrow(ftab.info), ncol = length(rawdata.esplit))
  rownames(ftab) <- rownames(ftab.info)
  colnames(ftab) <- names(rawdata.esplit)
  for (hid in names(rawdata.esplit)) { ftab[, hid] <- I(rawdata.esplit[[hid]][inds, "spatNormSignal"]) }

  # Close MySQL connection
  dbDisconnect(con)

  # Clean up memory
  gc()

  tmp <- list(data = ftab, info = ftab.info)
#}



  fdat.orig <- tmp$data      # features x hybs, original non-log scale
  fdat.oligoinfo <- tmp$info # oligoinfo

  # Annotations for selected hybridisations
  fdat.hybinfo <- project.info[match(colnames(fdat.orig), project.info$hybridisationID), ]  	       	                    
  rownames(fdat.hybinfo) <- colnames(fdat.orig)

  ## Discard the hybs that contain only NAs
  onlyNA <- colMeans(is.na(fdat.orig)) == 1
  naHybs <- colnames(fdat.orig)[onlyNA]
  if(sum(onlyNA) > 0){
    message("Removing the following hybs, because they contain only NAs:\n")
    message(naHybs,"\n\n")
    fdat.orig <- fdat.orig[, !onlyNA]
    fdat.hybinfo <- fdat.hybinfo[, !onlyNA]
  }

  # calculate quantile points in original scale 
  # hard-code to unify all analyses; these values were calculated manually from the HITChip atlas with 3200 samples, 
  # and rounded to 3 significant digits
  minmax.points <- c(30, 133000) 

  # Normalize (input required in log-scale)
  fdat <- ScaleProfile(fdat.orig, method = my.scaling, minmax.points = minmax.points)

  # Summarize probes into oligos and hybridisations into samples
  oligo.log10 <- summarize.rawdata(log10(fdat), fdat.hybinfo, fdat.oligoinfo, 
  	     		oligo.ids = sort(unique(phylogeny.info$oligoID)))
	 
  # First produce full preprocessed data matrices
  data.matrices.full <- list(oligo = oligo.log10)
  for (level in c("species", "L1", "L2")) {
    for (method in c("sum", "rpa")) { 
      message(paste(level, method))
      data.matrices.full[[level]][[method]] <- summarize.probesets(phylogeny.info, oligo.log10, method, level, rm.phylotypes = rm.phylotypes)
    }
  }

  # Sample annotation matrix
  sample.info.full <- fdat.hybinfo[match(colnames(oligo.log10), 
  		                         fdat.hybinfo$sampleID),]
  rownames(sample.info.full) <- as.character(sample.info.full$sampleID)

  # -----------------------------------------------------------

  message(paste("Saving full data matrix in ", full.data.file), sep = "")
  atlas <- data.matrices.full
  atlas.sampleinfo <- sample.info.full 
  save(atlas, atlas.sampleinfo, file = full.data.file, compress = "xz")

  # Save parameters
  params <- list(dbuser = dbuser, dbpwd = NA, dbname = dbname, host = host, port = port,
  	         my.scaling = my.scaling, # minmax.quantiles = minmax.quantiles, 
		 minmax.points = minmax.points, 
		 result.path = result.path, 
		 allowed.projects = allowed.projects, 
		 rm.oligos = rm.phylotypes$oligos, rm.phylotypes = rm.phylotypes, 
		 files = list(full.data.file, parameter.data.file),
		 session.info = sessionInfo(), minmax.points = minmax.points, date = date())

  # Save version info
  save(phylogeny.info, params, file = parameter.data.file, compress = "xz")

  res <- list(
       full.data = data.matrices.full, 
       sampleinfo = sample.info.full, 
       phylogeny.info = phylogeny.info, 
       parameters = params)



