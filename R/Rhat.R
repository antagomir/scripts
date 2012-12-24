# MAIN PROGRAM


gparRhat<-function (x, keep.all=F,n=0) {
        m<-ncol(x)
        if (!keep.all) x<-x [(nrow(x)/2+1):nrow(x),]  # 2nd half of simulated seqs
        #n<-nrow(x)
        xdot<-as.vector(col.means(x))
        s2<-as.vector(col.vars(x))
        W<-mean(s2)
        B<-n*var(xdot)
        #muhat<-mean(xdot)
        varW<-var(s2)/m
        varB<-B^2 * 2/(m-1)
        covWB<-(n/m)*(cov(s2,xdot^2) - 2*mean(xdot)*cov(s2,xdot))
        sig2hat<-((n-1)*W + B)/n
    if (W > 1.e-8) {            # non-degenerate case
        postvar<-sig2hat + B/(m*n)
        varpostvar <-(((n-1)^2)*varW + (1+1/m)^2*varB + 2*(n-1)*(1+1/m)*covWB)/n^2
        post.df<-chisqdf (postvar, varpostvar)
	Rhat<-sqrt((postvar/W)*(post.df+3)/(post.df+1))
    }
    else {      # degenerate case:  all entries in "data matrix" are identical
	Rhat<-1
    }
    Rhat
}

Rhat<-function (a, trans=rep("",ifelse (length(dim(a))<3, 1, dim(a)[length(dim(a))])), keep.all=F,nparams=ifelse (length(dim(a))<3, 1, dim(a)[length(dim(a))])) {
	# a is a (2n) x m x k matrix:  m sequences of length 2n, k variables measured
	# trans is a vector of length k:  "" if no transformation, or "log" or "logit"
	gp<-vector(mode='numeric',length=nparams)
	if (length(dim(a))==2) a<-array (a, c(dim(a),1)) 
	n=dim(a)[[1]]
	for (i in 1:nparams){
	    if (trans[i]=="log") gp[i]<-gparRhat(log(a[,,i]), keep.all,n=n) 
	    else if (trans[i]=="logit") gp[i]<-gparRhat(logit(a[,,i]), keep.all,n=n)
	    else gp[i]<-gparRhat(a[,,i],keep.all,n=n)
	}
        gp
}
