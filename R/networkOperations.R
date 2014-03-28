printSIF <- function (mynet, sif.file, sep = " ") {
  
  write("", file=sif.file, append = FALSE)

  # Print network in SIF format
  # NOTE: print only upper triangle of the matrix
  # Reason: since the edges are not directed, we would have two links for each 
  # pair of linked nodes which makes the figure unclear and is redundant
  # in cytoscape figures anyway
  for (i in 1:nrow(mynet)) {
        gid<-rownames(mynet)[[i]]
        inds = which(as.logical(mynet[i,]))
        # keep only upper triangle of the network matrix
        inds = inds[inds>i]
        neighgids<-colnames(mynet)[inds]
	
        for (ng in neighgids) {
                # 'link' is arbitrary user-chosen name
                # of the interaction type.
                # here we just have this one type.
                write(paste(gid,"link",ng, sep = sep), file=sif.file, append = TRUE)
        }
   }
  print(paste("Network printed into",sif.file))
}



