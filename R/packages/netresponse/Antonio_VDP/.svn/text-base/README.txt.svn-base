# By: António Gusmão, 84926T @ TKK
# antoniogusmao at gmail dot com

######TO COMPILE (if needed...)#######
on the command line (outside R) write: 
R CMD SHLIB vdp_mk_hp_posterior.c
R CMD SHLIB vdp_mk_log_lambda.c
R CMD SHLIB vdp_softmax.c
R CMD SHLIB vdp_sumlogsumexp.c
#######################################

############### TO TEST ###############
source('vdp_mixt.R');
source('demo.R');
#######################################

# Description
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
