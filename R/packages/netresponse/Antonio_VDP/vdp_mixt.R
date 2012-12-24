vdp_mixt <- function(mydat){
#  Modified from Agglomerative Independent Variable
#  Group Analysis package:
#  Copyright (C) 2001-2007 Esa Alhoniemi, Antti Honkela, Krista Lagus,
#  Jeremias Seppa, Harri Valpola, and Paul Wagner
#
#  This file is based on the Variational Dirichlet Process
#  Gaussian Mixture Model implementation,
#  Copyright (C) 2007 Kenichi Kurihara. All rights reserved.
#  
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2, or (at your option)
#  any later version.
#
#  The source code was further modified by Leo Lahti, 2009
#  Conversion from matlab to R by António Gusmão & Leo Lahti, 2009

# INPUT: mydat::
#       Each Row is an observation.
#       Each Column is a variable.
#
# OUTPUT:
#    list(hp_prior, opts, free_energy, hp_posterior, K);
#
#    * hp_prior: prior info
#         - hp_prior$q_of_z: prior on observation labels
#         - Mu_mu: centroids, 
#         - S2_mu: variance,
#
#    * opts: option list used in training
#
#    * free_energy: free energy of mixture model found.
#
#    * hp_posterior = templist$hp_posterior
#
#    * K: Number of mixture components (clusters)
#  
#
################  ALGORITHM SUMMARY  ################
# This code implements Gaussian mixture models with diagonal covariance matrices. 
# The following greedy iterative approach is taken in order to obtain the number
# of mixture models and their corresponding parameters:
#
# 1. Start from one cluster, $T = 1$.
# 2. Select a number of candidate clusters according to their values of 
#    "Nc" = \sum_{n=1}^N q_{z_n} (z_n = c) (larger is better).
# 3. For each of the candidate clusters, c: 
#     3a. Split c into two clusters, c1 and c2, through the bisector of its 
#         principal component. Initialise the responsibilities 
#         q_{z_n}(z_n = c_1) and q_{z_n}(z_n = c_2). 
#     3b. Update only the parameters of c1 and c2 using the observations that
#         belonged to c, and determine the new value for the free energy, F{T+1}.
#     3c. Reassign cluster labels so that cluster 1 corresponds to the largest 
#         cluster, cluster 2 to the second largest, and so on.
# 4. Select the split that lead to the maximal reduction of free energy, F{T+1}.
# 5. Update the posterior using the newly split data.
# 6. If FT - F{T+1} < \epsilon then halt, else set T := T +1 and go to step 2.
#
# The loop is implemented in the function greedy(...)




dyn.load("vdp_mk_log_lambda.so");
dyn.load("vdp_mk_hp_posterior.so")
dyn.load("vdp_softmax.so");
dyn.load("vdp_sumlogsumexp.so");

  opts <- list(
    #Prior parameters
    prior_alpha     = 1,    # default = 1; Remark: result is quite insensible to this variable
    prior_alpha_ksi = 0.01, # default = 0.01; smaller -> less clusters (and big!) -> quite sensitive
    prior_beta_ksi  = 0.01, # default = 0.01; larger -> less clusters (and big!)
 
    # Max_PCA_on_Split: when a candidate cluster, C, is split to generate two 
    # new clusters, it is split by mapping the data onto the first principal 
    # component of the data in C and then splitting that in half. To speed up, 
    # one can compute an approximate first principal component by considering
    # a randomly selected subset of the data belonging to C, and computing its
    # first principal component.
    # No Speedup: Max_PCA_on_Split = Inf;
    Max_PCA_on_Split = Inf, # default = Inf;

    do_sort = TRUE,     # When true, q_of_z will be sorted in decreasing fashion based on colSums(q_of_z)
    threshold = 1.0e-5, # Defines the minimal free energy improvement that stops the algorithm
    initial_K = 1,      # Number of clusters to start from
    ite = Inf,          # used on update_posterior. Defines maximum number of iterations
    implicit_noisevar = 0, # Adds implicit noise. used in vdp_mk_log_lambda.so and vdp_mk_hp_posterior.so
    c_max = 10          # maximum number of candidates to consider in find_best_splitting. Candidates are chosen
                        # based on their Nc value (larger = better). Nc = colSums(q_of_z)
  );



  # data.given_data.X1 = mydat;
  data <- list();
  data[["given_data"]]<-list();
  data[["given_data"]][["X1"]] <- mydat;

  # The hyperparameters of priors:
  hp_prior <- mk_hp_prior(data, opts);
  q_of_z   <- rand_q_of_z(data, opts$initial_K);


  # Get posteriors
  hp_posterior <- mk_hp_posterior(data, q_of_z, hp_prior, opts);
  templist <- greedy(data, hp_posterior, hp_prior, opts);

  templist$hp_prior <- c(templist$hp_prior, list(q_of_z = q_of_z));

  # Save Results:
  results <- list(
    hp_prior     = templist$hp_prior,
    opts         = opts,
    free_energy  = templist$free_energy,
    hp_posterior = templist$hp_posterior,
    K            = dim(templist$hp_posterior$Mu_bar)[1]
  );

  results
}


################################################################################
#INPUT:   data     - matrix with data vectors
#         opts     - list with algorithm options
#OUTPUT:  hp_prior - list with prior information
#
#DESCRIPTION: Prior for the mean     = mean of data
#             Prior for the variance = variance of data

mk_hp_prior <- function(data, opts){

  X1      <- data$given_data$X1; # real-valued. Data to be clustered.
  n       <- dim(X1)[1];         # number of data points (#rows of X1)
  dim1    <- dim(X1)[2];         # dimensionality (#columns of X1)
  Mean    <- colMeans(X1);       # mean of each dimension
  Var     <- colSums((X1-rep(Mean,each=n))^2)/n; # Variance of each dimension

  # priors for distribution of codebook vectors Mu ~ N(Mu_Mu, S2_Mu)..
  hp_prior <- list(Mu_mu = Mean, S2_mu = Var, U_p = Inf);

  # priors for data variance Ksi ~ Gamma(Alpha_ksi, Beta_ksi)
  hp_prior <- c(hp_prior, list(Alpha_ksi = rep(opts$prior_alpha_ksi,dim1),
           Beta_ksi = rep(opts$prior_beta_ksi,dim1), alpha = opts$prior_alpha));
  
  hp_prior
}

################################################################################
#INPUT:   data   - matrix with data vectors
#         K      - number of clusters
#OUTPUT:  q_of_z - matrix of size N*(K+1).
#DESCRIPTION: This function assigns data randomly to K clusters by drawing cluster
#             membership values from a uniform distribution.

rand_q_of_z <- function(data, K){

  N <- dim(data[["given_data"]][["X1"]])[1];
  q_of_z <- matrix(runif(N*(K+1)),N,K+1);
  q_of_z[,K+1] <- 0;
  q_of_z <- q_of_z/rowSums(q_of_z); #normalize, each row should sum up to 1.
  
  q_of_z
}


################################################################################
#INPUT:   data: structure with data matrix
#         hp_posterior: posterior information for the current mixture model
#         hp_prior: prior information
#         opts: options list.
#
#OUTPUT:  list(free_energy, hp_posterior, hp_prior, data);
#
#DESCRIPTION: Read the main description on the beginning of the file.

greedy <- function(data, hp_posterior, hp_prior, opts){
  
  free_energy = mk_free_energy(data, hp_posterior, hp_prior, opts)$free_energy;

  while(1){
    # ALGORITHM STEP 2-4.
    templist <- find_best_splitting(data, hp_posterior, hp_prior, opts);
    new_free_energy  <- templist$free_energy;
    new_hp_posterior <- templist$hp_posterior;
    c                <- templist$c;
    
    if(c== -1) break; # infinite free energy

    # ALGORITHM STEP 5.
    dummylist <- update_posterior(data, new_hp_posterior, hp_prior, opts, opts$ite, 1);
    new_free_energy  <- dummylist$free_energy;
    new_hp_posterior <- dummylist$hp_posterior;
    
    if(is.infinite(new_free_energy))
      stop("Free energy is not finite, please consider adding implicit noise or not updating the hyperparameters");

    # ALGORITHM STEP 6.
    if(free_energy_improved(free_energy,new_free_energy,0,opts) == 0)
      break; #free_energy didn't improve , greedy search is over.

    free_energy  <- new_free_energy;
    hp_posterior <- new_hp_posterior;
  }

  out <- list(free_energy=free_energy, hp_posterior = hp_posterior,
       hp_prior = hp_prior, data = data);

  out
}


################################################################################
#INPUT:   data, hp_posterior, hp_prior, opts
#OUTPUT:  free_energy: value of mixture model's free energy
#         log_lambda: Used for posterior of labels q_of_z <- softmax(log_lambda);
#DESCRIPTION: ...
mk_free_energy <- function(data, hp_posterior, hp_prior, opts, fc="unknown", log_lambda="unknown"){
  
  if(fc=="unknown" || log_lambda=="unknown"){
    fc <- mk_E_log_q_p_eta(data, hp_posterior, hp_prior, opts); # 1*K
    log_lambda <- mk_log_lambda(data, hp_posterior, hp_prior, opts); # N*K
  }
 
  E_log_p_of_V <- lgamma(colSums(hp_posterior$gamma)) -
      lgamma(1+hp_prior$alpha) -
      colSums(lgamma(hp_posterior$gamma)) +
      lgamma(hp_prior$alpha) +
      ( (hp_posterior$gamma[1,]-1) * 
        (digamma(hp_posterior$gamma[1,]) - digamma(colSums(hp_posterior$gamma))) ) +
      ( (hp_posterior$gamma[2,] - hp_prior$alpha) *
        (digamma(hp_posterior$gamma[2,]) - digamma(colSums(hp_posterior$gamma))) );

  extra_term <- sum(E_log_p_of_V);

  free_energy <- extra_term + sum(fc) - sumlogsumexp(log_lambda);
  
  returnvalues <- list(free_energy=free_energy, log_lambda= log_lambda);
  returnvalues
}

################################################################################
#INPUT:   data, hp_posterior, hp_prior, opts
#OUTPUT:  matrix [1xk]: used to compute the free_energy formula. 
#DESCRIPTION: Regards the gaussian model's parameters.
mk_E_log_q_p_eta <- function(data, hp_posterior, hp_prior, opts){
  
  # returns E [ log q(eta)/p(eta) ]_q
  # fc: 1 by k

  N  <- dim(data$given_data$X1)[1];
  M1 <- dim(data$given_data$X1)[2];
  K  <- dim(hp_posterior$Mu_bar)[1];
  l_codebook <- - M1/2 * matrix(1,K);

  Ksi_log = (digamma(hp_posterior$Ksi_alpha) - log(hp_posterior$Ksi_beta));

  
  for (j in 1:M1) {
    l_codebook = l_codebook +
      .5 * ( log(hp_prior$S2_mu[j] / hp_posterior$Mu_tilde[,j]) +
             ( (hp_posterior$Mu_bar[,j] - hp_prior$Mu_mu[j])^2 +
                hp_posterior$Mu_tilde[,j] 
             ) / hp_prior$S2_mu[j]
           ) +
      lgamma(hp_prior$Alpha_ksi[j]) - 
      lgamma(hp_posterior$Ksi_alpha[,j]) +
      hp_posterior$Ksi_alpha[,j] * log(hp_posterior$Ksi_beta[,j]) -
      hp_prior$Alpha_ksi[j] * log(hp_prior$Beta_ksi[j]) + 
      (hp_posterior$Ksi_alpha[,j] - hp_prior$Alpha_ksi[j]) * Ksi_log[,j] +
      (hp_prior$Beta_ksi[j] - hp_posterior$Ksi_beta[,j]) *
        (hp_posterior$Ksi_alpha[,j] / hp_posterior$Ksi_beta[,j]);
  }
  fc <- t(l_codebook);
  fc
}

################################################################################
#INPUT:   data, hp_posterior, hp_prior, opts
#OUTPUT:  list(free_energy,hp_posterior,data,c)
#       * free_energy: free energy of the best split found
#       * hp_posterior: posterior info of the best split found
#       * c: index of the cluster that resulted in the best split found
#
#DESCRIPTION: Implements the VDP algorithm steps 2 to 4.

find_best_splitting <- function(data, hp_posterior, hp_prior, opts){
  
  c_max <- opts$c_max; # maximum number of candidates to consider.
  K     <- dim(hp_posterior$Mu_bar)[1]; # number of clusters
  
  # ALGORITHM STEP 2
  candidates <- which(hp_posterior$Nc > 2); # Select candidates.

  if (length(candidates)==0) 
    c <- 1;
  
  q_of_z <- mk_q_of_z(data, hp_posterior, hp_prior, opts);
  
  new_free_energy <- rep(Inf,max(candidates));

  fc <- mk_E_log_q_p_eta(data, hp_posterior, hp_prior, opts);
  log_lambda <- mk_log_lambda(data, hp_posterior, hp_prior, opts);

  #Initialize:
  new_q_of_z_list <- list();

  # ALGORITHM STEP 3 (3a,3b,3c)
  #for each candidate
  for (c in candidates[1:min(c_max,length(candidates))]) {
    #Splitting

    # ALGORITHM STEP 3a. split the candidate cluster
    templist <- split(c, data, q_of_z , hp_posterior, hp_prior, opts);
    new_q_of_z <- templist$new_q_of_z;
    new_c <- templist$new_c;
    

    relating_n <- which(rowSums(new_q_of_z[,c(c,new_c)]) > 0.5); 
    # relating_n has the indexes of data points that belonged to the candidate cluster prior
    # to splitting (that's why it is the sum over the now 2 clusters (after splitting).
    # REMARK: Is this 0.5 correct? when there are lots of clusters it is natural to assume points will have less than 0.5 for any cluster.


    if (length(relating_n)==0){
      stop('length relating_n == 0'); # this stop is not required, but I think this should not happen unless something is going wrong...
      next;
    }

    new_K = dim(new_q_of_z)[2];
    sub_q_of_z = new_q_of_z[relating_n, c(c, new_c, new_K)];

    sub_data = data;
    sub_data$given_data$X1= data$given_data$X1[relating_n,]; # for 1D data this ...
    # transforms a column vector into an array. Next if corrects that...
    if(dim(data$given_data$X1)[2]==1){
      sub_data$given_data$X1 = matrix(sub_data$given_data$X1);
    }
    sub_data$given_data$data = sub_data$given_data$X1;

    # ALGORITHM STEP 3b and 3c
    # update the posterior of the split clusters for a small number of iter. (10)
    # update_posterior sorts the clusters by size.
    sub_hp_posterior = mk_hp_posterior(sub_data, sub_q_of_z, hp_prior, opts);
    dummylist <- update_posterior(sub_data, sub_hp_posterior, hp_prior, opts, 10, 0);
    sub_f            <- dummylist$free_energy;
    sub_hp_posterior <- dummylist$hp_posterior;
    sub_q_of_z       <- dummylist$q_of_z;



    if(dim(sub_q_of_z)[2] < 3){
      next;
    }

    if(length(which(colSums(sub_q_of_z) < 1.0e-10)) > 1){
      next;
    }
    
    sub_log_lambda <- mk_log_lambda(data, sub_hp_posterior, hp_prior, opts);
    insert_indices <- c(c, new_c, new_K:(new_K + dim(sub_q_of_z)[2]-3));

    
    aux_max <- max(insert_indices);
    if(aux_max > dim(log_lambda)[2]){
      new_log_lambda <- cbind(log_lambda,matrix(0,dim(log_lambda)[1]));
    }
    else{
      new_log_lambda <- log_lambda;
    }
    new_log_lambda[,insert_indices] <- sub_log_lambda;


    new_fc <- fc;
    new_fc[insert_indices] <- mk_E_log_q_p_eta(sub_data, sub_hp_posterior, hp_prior, opts);   
    new_free_energy[c] <- mk_free_energy(data, sub_hp_posterior, hp_prior, opts, new_fc, new_log_lambda)$free_energy;

    new_q_of_z[relating_n,] <- 0;
    new_q_of_z[relating_n,insert_indices] <- sub_q_of_z;
    new_q_of_z_list[[c]] <- new_q_of_z;
  }
  
  free_energy <- min(new_free_energy);
  c <- which(new_free_energy == free_energy);

  if(is.infinite(free_energy)){
    c <- -1;
    return(list(free_energy=free_energy,hp_posterior= hp_posterior,data= data,c= c));
  }

  hp_posterior <- mk_hp_posterior(data, new_q_of_z_list[[c]], hp_prior, opts);

  list(free_energy=free_energy,hp_posterior= hp_posterior,data= data,c= c)
}

################################################################################
mk_q_of_z <- function(data, hp_posterior, hp_prior, opts, log_lambda=NULL){
  if(is.null(log_lambda)==1){
    log_lambda = mk_log_lambda(data, hp_posterior, hp_prior, opts);
  }
  
  q_of_z <- softmax(log_lambda);
}


################################################################################
#INPUT:   data, q_of_z, hp_posterior, hp_prior, opts
#OUTPUT:  list(new_q_of_z, new_c);
#            * new_q_of_z: posterior over labels including the split clusters.
#            * new_c: index of the newly created cluster.
#DESCRIPTION: Implements the VDP algorithm step 3a.

split <- function(c, data, q_of_z, hp_posterior, hp_prior, opts){

  # compute the first principal component of the candidate cluster, 
  # not the whole data set.
  cluster_assignments=apply(q_of_z, 1, which.max);
  indices = which(cluster_assignments == c);

  candidate_cluster_datapoints <- data$given_data$X1[indices,];

  N <- dim(candidate_cluster_datapoints)[1]; 
  #Center data
  mydc <- (candidate_cluster_datapoints - matrix(rep(colMeans(candidate_cluster_datapoints),N),N,byrow=TRUE));

  if(dim(mydc)[1] > opts$Max_PCA_on_Split){
    # Max_PCA_on_Split: when a candidate cluster, C, is split to generate two 
    # new clusters, it is split by mapping the data onto the first principal 
    # component of the data in C and then splitting that in half. To speed up, 
    # one can compute an approximate first principal component by considering
    # a randomly selected subset of the data belonging to C, and computing its
    # first principal component.
    # No Speedup: Max_PCA_on_Split = Inf;

    pcadata <- mydc[sample(dim(mydc)[1],opts$Max_PCA_on_Split),];
    principal_component <- - loadings(princomp(pcadata));  # Do PCA
  }else{
    principal_component <- - loadings(princomp(mydc));  # Do PCA
  }
  class(principal_component) <- "matrix";             # Process output of princomp
  principal_component <- unname(principal_component); # remove names


  # project on the first principal component to get assignments:
  dir <- mydc%*%principal_component[,1];

  q_of_z_c1 <- matrix(0,dim(q_of_z)[1]);
  q_of_z_c2 <- q_of_z[,c];

# OLD I <- which(dir >= 0);
  I <- indices[which(dir >= 0)];
  q_of_z_c1[I] <- q_of_z[I,c];
  q_of_z_c2[I] <- 0;
  
  new_q_of_z <- matrix(0,dim(q_of_z)[1], dim(q_of_z)[2]+1);
  new_c <- dim(new_q_of_z)[2] -1;
  new_q_of_z[,- new_c] <- q_of_z;
  new_q_of_z[,c] <- q_of_z_c1;
  new_q_of_z[, new_c] <- q_of_z_c2;

  cluster_assignments=apply(new_q_of_z, 1, which.max);

  return(list(new_q_of_z=new_q_of_z, new_c=new_c));
}

################################################################################
#INPUT:   data, hp_posterior, hp_prior, opts, ite, do_sort
#             * do_sort: TRUE/FALSE: indicates whether the clusters should be 
#                        sorted by size or not.
#             * ite: number of update iterations.
#OUTPUT:  Updated parameters: list(free_energy, hp_posterior, q_of_z)
#DESCRIPTION: Updates the posterior of the mixture model. if do_sort=true it also
#             sorts the cluster labels by size. (i.e. cluster 1 = largest cluster)
update_posterior <- function(data, hp_posterior, hp_prior, opts, ite=Inf, do_sort=1){

  free_energy <- Inf;

  i          <- 0;
  last_Nc    <- 0;
  start_sort <- 0;


  while(1){
    i <- i+1;

    templist <- mk_free_energy(data, hp_posterior, hp_prior, opts);
    new_free_energy <- templist$free_energy;
    log_lambda <- templist$log_lambda;

    if(is.infinite(new_free_energy))
      stop("Free energy is not finite, please consider adding implicit noise or not updating the hyperparameters");

    if ( (is.finite(ite) && i>= ite) ||
         (is.infinite(ite) && 
          free_energy_improved(free_energy, new_free_energy, 0, opts) == 0)){

      free_energy <- new_free_energy;
      if( do_sort && opts$do_sort && (!start_sort) ){
        start_sort <- 1;
      }else{
        break;
      }
    }

    last_Nc <- hp_posterior$Nc;

    free_energy <- new_free_energy;

    q_of_z <- mk_q_of_z(data, hp_posterior, hp_prior, opts, log_lambda);

    # check if the last component is small enough: If not, add a new one with value 0
    if(sum(q_of_z[,dim(q_of_z)[2]]) > 1.0e-20){
      q_of_z <- cbind(q_of_z,0);
    }

    if(start_sort){
      q_of_z <- sort_q_of_z(q_of_z);
    }

    if(sum(q_of_z[,dim(q_of_z)[2]-1]) < 1.0e-10){
      q_of_z <- q_of_z[, -dim(q_of_z)[2] -1];
    }
    
    hp_posterior <- mk_hp_posterior(data, q_of_z, hp_prior, opts);
  }

  list(free_energy=free_energy, hp_posterior = hp_posterior, q_of_z = q_of_z)
}

################################################################################
#INPUT: "old" free_energy value, "new" free_energy value, options
#OUTPUT: bool: 0 if the there was no significant improvement.
#              1 if new_free_energy is smaller than free_energy (more than opts$threshold).
free_energy_improved <- function(free_energy, new_free_energy, warn_when_increasing, opts){
  diff <- new_free_energy - free_energy;

  if(is.nan(abs(diff/free_energy)) || abs(diff/free_energy) < opts$threshold){
    bool <- 0;
  }else{ 
    if(diff >0){
      if(warn_when_increasing){
        if( abs(diff/free_energy) > 1.0e-3){
          stop(c("the free energy increased. The diff is ", toString(diff)));
        }else{
          warning(c("the free energy increased. The diff is ", toString(diff)));
        }
      }
      bool <- 0;
    }else{
      if(diff == 0){
        bool <- 0;
      }else{
        bool <- 1;
      }
    }
  }
  
  

  bool
}


################################################################################
#INPUT: matrix (q_of_z)
#OUTPUT: matrix
#DESCRIPTION: Sorted matrix in decreasing fashion based on the value of colSums.
#             Remark: The last column of the matrix is kept in place (it is not sorted).
sort_q_of_z <- function(q_of_z){
  Nc <- colSums(q_of_z);
  I  <- sort(Nc[-length(Nc)],decreasing=TRUE, index.return=TRUE)[2];
  I  <- c(unlist(I),length(Nc));
  q_of_z <- q_of_z[,I];
  q_of_z
}

################################################################################
mk_hp_posterior <- function(data, q_of_z, hp_prior, opts){
  # Compatibility variables not needed for the current functionality #
  tmp_realS <- 0.0;
  X2        <- 0.0;
  dimX2     <- 0.0;

  out <- .Call("m_hp_post",data$given_data$X1,dim(data$given_data$X1)[2], dim(data$given_data$X1)[1],
      X2,dimX2,
      tmp_realS, opts$implicit_noisevar,
      hp_prior$Mu_mu, hp_prior$S2_mu, hp_prior$Alpha_ksi, hp_prior$Beta_ksi,
      hp_prior$U_p, hp_prior$alpha,
      q_of_z,
      dim(q_of_z)[2]);

hp_posterior <- list(
  Mu_bar    = matrix(out$Mu_bar,   dim(q_of_z)[2]),
  Mu_tilde  = matrix(out$Mu_tilde, dim(q_of_z)[2]),
  Ksi_alpha = matrix(out$Ksi_alpha,dim(q_of_z)[2]),
  Ksi_beta  = matrix(out$Ksi_beta, dim(q_of_z)[2]),
  gamma     = matrix(out$gamma,    2),
  Nc        = out$Nc,
  true_Nc   = out$true_Nc,
  q_of_z    = matrix(out$q_of_z, dim(q_of_z)[1]),
  Uhat      = out$Uhat);

  hp_posterior
}


################################################################################
mk_log_lambda <- function(data, hp_posterior, hp_prior, opts){
  # Compatibility variables not needed for the current functionality #
  tmp_realS <- 0.0;
  X2        <- 0.0;
  dimX2     <- 0.0;

  out <- .Call("m_log_lambda",data$given_data$X1,dim(data$given_data$X1)[2], dim(data$given_data$X1)[1],
      X2,dimX2,
      tmp_realS, opts$implicit_noisevar,
      hp_prior, hp_posterior);

  log_lambda <- matrix(out, dim(data$given_data$X1)[1]);
  
  log_lambda
}


## vdp_softmax Wrapper
# INPUT: Real Matrix of size M x N
# OUTPUT: Real Matrix of size M x N
# DESCRIPTION: for each row i of A:  (ai = row i of A)
#                bij = exp(aij - max(ai));
#                bij = bij / sum(bi);
softmax <- function(A){
  B <- .Call("vdp_softmax", A);
  B
}

## vdp_sumlogsumexp Wrapper
# INPUT: Real Matrix of size M x N
# OUTPUT: Real Matrix of size 1 x 1 (Real number).
# DESCRIPTION: for each row i of A:  (ai = row i of A)
#                bij = exp(aij - max(ai));
#                out = out + max(ai) + log(sum(bi));
sumlogsumexp <- function(log_lambda){
  out <- .Call("vdp_sumlogsumexp", log_lambda);
  out
}
