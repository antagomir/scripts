# Functions for ROC analysis

roc <- function (ordered.results, P) {
	#ordered results: best to worst
	#P: known positives
	#output: true positive rate and false positive rate
	
	#Check that all known positives are included in the original analysis i.e. ordered results list
	#if (!all(P %in% ordered.results)) {print("Warning: not all known positives are in the results list. Only included positives are used.")}
	positives<-P[P %in% ordered.results]	
	
	#Number of retrieved known cytobands
	N<-length(ordered.results) #total number of samples
	Np<-length(positives) #number of positives
	Nn<-N-Np #number of negatives

	TP<-cumsum(ordered.results %in% positives)
	FP<-cumsum(!(ordered.results %in% positives))
	tpr<-TP/Np #TP/(TP + FN) = TP.simCCA / P
	fpr<-FP/Nn #FP/(FP + TN) = FP.simCCA / N.simCCA

	list(tpr=tpr,fpr=fpr)
}

roc.plot <- function(ordered.results, P,line=F,maintext="") {
	res<-roc(ordered.results, P)
	plot(res$fpr,res$tpr,lty=1,type='l',xlab="False positive rate",ylab="True positive rate",xlim=c(0,1),ylim=c(0,1),main=maintext)
	if (line) {
		#Draw 45 angle line
		lines(c(0,1),c(0,1))
	}
}

roc.auc <- function (ordered.results, P) {

  # ordered results: best to worst
  # P: known positives

  # Compute area under curve
  rates <- roc(ordered.results, P)
  # integration: Compute step intervals and compute weighted sum of true positive rates in each interval.
  # note that interval length can be 0 if fpr does not change
  as.numeric(t(rates$fpr[-1]-rates$fpr[-length(rates$fpr)])%*%rates$tpr[-length(rates$fpr)])
}
