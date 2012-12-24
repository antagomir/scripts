
MUOKKAA LOPPUUN


# Recall: Check fraction of all positives among top-n
       recall <- list()
          # Precision: Check fraction of true positivies among top-n findings
    precision <- list()

	         # Number of genes for this method
		            ng <- length(res[[ds]]$ordered.genes[[nam]])
			    

				       
				                hits <-
na.omit(res[[ds]]$ordered.genes[[nam]] %in% res[[ds]]$cancerGenes)
         precision[[nam]] <- sapply(1:length(hits), function (n) { mean(hits[1:n])})
	          recall[[nam]] <- sapply(ns, function (n) {
sum(hits[1:n])/sum(hits) })


					
					