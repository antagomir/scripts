/*
  This file is a part of the Agglomerative Independent Variable Group
  Analysis package
 
  Copyright (C) 2001-2007 Esa Alhoniemi, Antti Honkela, Krista Lagus,
  Jeremias Seppa, Harri Valpola, and Paul Wagner
 
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License (included in file License.txt in the
  program package) for more details.
*/

#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>

#define POW2(x) ((x) * (x))



void compute_nc(int ncentroids, long datalen, double *true_Nc,
		double *q_of_z, double *Nc)
{
  register int i, j;

  for (i=0; i<ncentroids; i++) {
    true_Nc[i] = 0.0;
    for (j=0; j<datalen; j++) {
      true_Nc[i] += q_of_z[i*datalen + j];
    }
    Nc[i] = true_Nc[i];
  }
  Nc[ncentroids-1] = 0.0;
  for (j=0; j<datalen; j++) {
    q_of_z[(ncentroids-1)*datalen+j] = 0;
  }

  return;
}


void
update_centroids(long datalen, int ncentroids, int dim1, int dim2,
		 double *data1, int **data2_int,
                 double *Nc, double *q_of_z, double *Mu_mu, double *S2_mu,
                 double *Mu_bar, double *Mu_tilde,
		 double *Ksi_alpha, double *Ksi_beta, double *Alpha_ksi,
		 double *Beta_ksi, double implicit_noisevar,
		 double *U_p, double ***U_hat_table, double *Ns) {
  register int i, k;
  register long ind, j, t;
  double term, term2, term3, s2x, s2x_new;
  
  
  if (dim1){
    for (k = 0; k < dim1; k++) {
      s2x = Beta_ksi[k] / Alpha_ksi[k];
      for (i = 0; i < ncentroids; i++) {
	term = 0.0;
	ind  = k * ncentroids + i;
	for (j = 0; j < datalen; j++)
	  term += q_of_z[i*datalen + j] * data1[k * datalen + j];
	term2         = s2x + S2_mu[k] * Nc[i];
	Mu_bar[ind]   = ((s2x * Mu_mu[k]) + (S2_mu[k] * term))/term2;
	Mu_tilde[ind] = (s2x * S2_mu[k])/term2;
	Ksi_alpha[ind] = Alpha_ksi[k] + 0.5 * Nc[i];
	term3 = 0.0;
	for (j = 0; j < datalen; j++)
	  term3 += q_of_z[i*datalen + j] *
	    (Mu_tilde[ind] + POW2(data1[k * datalen + j] - Mu_bar[ind]) +
	     implicit_noisevar);
	Ksi_beta[ind] = Beta_ksi[k] + 0.5 * term3;

	s2x_new = Ksi_beta[ind] / Ksi_alpha[ind];
	term2         = s2x_new + S2_mu[k] * Nc[i];
	Mu_bar[ind]   = ((s2x_new * Mu_mu[k]) + (S2_mu[k] * term))/term2;
	Mu_tilde[ind] = (s2x_new * S2_mu[k])/term2;
      }
    }
  }

  for(j=0;j<dim2;j++){
    for(i=0;i<ncentroids;i++){   
      for(k=0;k<(int)(Ns[j]);k++)
      	U_hat_table[j][i][k]=U_p[j];
      for(t=0;t<datalen;t++){	     /***************/
	U_hat_table[j][i][data2_int[j][t]] += q_of_z[i*datalen+t];
      }
    }
  }  
  return;
}


void update_gamma(int ncentroids, double *true_Nc, double *prior_alpha,
		  double *post_gamma)
{
  register int i;
  double ncsum, nccumsum;

  ncsum = 0.0;
  for (i=0; i<ncentroids; i++) {
    ncsum += true_Nc[i];
  }
  nccumsum = 0.0;
  for (i=0; i<ncentroids; i++) {
    nccumsum += true_Nc[i];
    post_gamma[2*i] = 1 + true_Nc[i];
    post_gamma[2*i+1] = *prior_alpha + ncsum - nccumsum;
  }

  return;
}



void
allocate_memory(long datalen,int ncentroids,int dim2,
                double ****U_hat_table,int ***data2_int,double *Ns) {
  register int i,j;
  
  if (dim2) {
    *U_hat_table=(double ***)malloc(dim2 * sizeof(double*));
    *data2_int=(int **)malloc(dim2 * sizeof(int*));
  }
  for (j=0;j<dim2;j++){
    (*data2_int)[j]  = (int *)malloc(datalen * sizeof(int));
    (*U_hat_table)[j]=(double**)malloc(ncentroids * sizeof(double*));
    for (i=0;i<ncentroids;i++) {
      (*U_hat_table)[j][i] =(double *)malloc(((int)(Ns[j]))*sizeof(double));
    }
  }
    
  return;
}
 
/************************************************************/

void
free_memory(int ncentroids, int dim2, 
	    double ****U_hat_table, int ***data2_int) {
  register int i,j;
  for (j=0;j<dim2;j++){
    for (i=0;i<ncentroids;i++) {
      free((*U_hat_table)[j][i]);
    }
    free((*data2_int)[j]);
    free((*U_hat_table)[j]);
  }

  if (dim2) {
    free(*U_hat_table);
    free(*data2_int);
  }
  return;
}


void
vdp_mk_hp_posterior(double *Mu_mu, double *S2_mu, double *Mu_bar, double *Mu_tilde, 
		    double *Alpha_ksi, double *Beta_ksi, 
		    double *Ksi_alpha, double *Ksi_beta, 
		    double *post_gamma, double *prior_alpha,
		    double *U_p, SEXP *U_hat,
		    long datalen, int dim1, int dim2, double *data1, double *data2, 
		    double *Ns, int ncentroids, 
		    double implicit_noisevar, double *q_of_z,
		    double *Nc, double *true_Nc) {
  register long i, j, t;
  register int k;
  double  *U_hat_j;
  SEXP U_hat_j_SEXP;
  double       ***U_hat_table;
  int          **data2_int;

  allocate_memory(datalen, ncentroids, dim2,
		  &U_hat_table,&data2_int,Ns );

  for (j=0;j<dim2;j++){
    for(t=0;t<datalen;t++)
      data2_int[j][t]=((int)(data2[j*datalen+t]))-1;
  }

  compute_nc(ncentroids, datalen, true_Nc, q_of_z, Nc);
  
  update_centroids(datalen, ncentroids, dim1, dim2,
		   data1, data2_int,
		   Nc, q_of_z, Mu_mu, S2_mu,
		   Mu_bar, Mu_tilde, 
		   Ksi_alpha, Ksi_beta, Alpha_ksi,
		   Beta_ksi, implicit_noisevar,
		   U_p, U_hat_table, Ns);

  update_gamma(ncentroids, true_Nc, prior_alpha, post_gamma);

  for (j=0;j<dim2;j++){
    PROTECT(U_hat_j_SEXP = NEW_NUMERIC(ncentroids*Ns[j]));
    U_hat_j=NUMERIC_POINTER(U_hat_j_SEXP);
    SET_ELEMENT(*U_hat, j, U_hat_j_SEXP); // U_hat has a list of size dim2
    for (i=0;i<ncentroids;i++)
      for (k=0;k<(int)(Ns[j]);k++)
	      U_hat_j[k*ncentroids+i]=U_hat_table[j][i][k];
  }

  free_memory(ncentroids, dim2, &U_hat_table, &data2_int);
  return;
}





/************************************************************/
/* bridge function                                          */

SEXP m_hp_post(SEXP X1, SEXP X1_Columns, SEXP X1_Rows, 
               SEXP X2, SEXP X2_Columns,
               SEXP realS, SEXP OPTSimplicit_noisevar,
               SEXP HP_PRIOR_Mu_mu,
               SEXP HP_PRIOR_S2_mu,
               SEXP HP_PRIOR_Alpha_ksi,
               SEXP HP_PRIOR_Beta_ksi,
               SEXP HP_PRIOR_U_p,
               SEXP HP_PRIOR_prior_alpha,
               SEXP Q_OF_Z,
               SEXP Q_OF_Z_Columns){
  long datalen;
  int  i, dim1, dim2, ncentroids;
  double *Mu_mu, *S2_mu, *Mu_bar, *Mu_tilde, 
    *Alpha_ksi, *Beta_ksi, *Ksi_alpha, *Ksi_beta, *U_p, *prior_alpha,
    *post_gamma;
  double *data1;
  double *data2;
  double *Ns;
  double *q_of_z_in, *q_of_z, *Nc, *true_Nc;
  double implicit_noisevar;
  const char *posterior_fields[]={"Mu_bar","Mu_tilde",
				  "Ksi_alpha","Ksi_beta",
				  "gamma","Nc","true_Nc","q_of_z","Uhat"};
  const char *prior_fields[]={"Mu_mu","S2_mu",
			      "Alpha_ksi","Beta_ksi",
			      "alpha","U_p"};
  SEXP list, list_names;
  SEXP oMu_bar, oMu_tilde,  oKsi_alpha, oKsi_beta, opost_gamma,
       oNc    , otrue_Nc ,  oq_of_z   , oU_hat;
  SEXP* U_hat;
  

  /************ CONVERTED input variables ******************/
  PROTECT(X1 = AS_NUMERIC(X1));  
  data1   = NUMERIC_POINTER(X1);
  dim1    = INTEGER_VALUE(X1_Columns);
  datalen = INTEGER_VALUE(X1_Rows);

  PROTECT(X2 = AS_NUMERIC(X2));  
  data2   = NUMERIC_POINTER(X2);
  dim2    = INTEGER_VALUE(X2_Columns);

  Ns = NUMERIC_POINTER(realS);
  implicit_noisevar = NUMERIC_VALUE(OPTSimplicit_noisevar);


  /************ CONVERTED initial values of model parameters ******************/
  Mu_mu       = NUMERIC_POINTER(HP_PRIOR_Mu_mu);
  S2_mu       = NUMERIC_POINTER(HP_PRIOR_S2_mu);
  Alpha_ksi   = NUMERIC_POINTER(HP_PRIOR_Alpha_ksi);
  Beta_ksi    = NUMERIC_POINTER(HP_PRIOR_Beta_ksi);
  U_p         = NUMERIC_POINTER(HP_PRIOR_U_p);
  prior_alpha = NUMERIC_POINTER(HP_PRIOR_prior_alpha);

  q_of_z_in   = NUMERIC_POINTER(Q_OF_Z);
  ncentroids  = INTEGER_VALUE(Q_OF_Z_Columns);


  /********* CONVERTED output variables ***********************/
  /* Necessary to allocate memory for the output variables :| */
  /*Allocation **/
  PROTECT(oMu_bar     = NEW_NUMERIC(ncentroids*dim1));
  PROTECT(oMu_tilde   = NEW_NUMERIC(ncentroids*dim1));
  PROTECT(oKsi_alpha  = NEW_NUMERIC(ncentroids*dim1));
  PROTECT(oKsi_beta   = NEW_NUMERIC(ncentroids*dim1));
  PROTECT(opost_gamma = NEW_NUMERIC(2*ncentroids));
  PROTECT(oNc         = NEW_NUMERIC(1*ncentroids));
  PROTECT(otrue_Nc    = NEW_NUMERIC(1*ncentroids));
  PROTECT(oq_of_z     = NEW_NUMERIC(datalen*ncentroids));
  PROTECT(oU_hat      = NEW_LIST(dim2)); /* CHECK This should be a Cell Matrix??? */
  
  Mu_bar     = NUMERIC_POINTER(oMu_bar);
  Mu_tilde   = NUMERIC_POINTER(oMu_tilde);
  Ksi_alpha  = NUMERIC_POINTER(oKsi_alpha);
  Ksi_beta   = NUMERIC_POINTER(oKsi_beta);
  post_gamma = NUMERIC_POINTER(opost_gamma);
  Nc         = NUMERIC_POINTER(oNc);
  true_Nc    = NUMERIC_POINTER(otrue_Nc);
  q_of_z     = NUMERIC_POINTER(oq_of_z);
  U_hat      = &oU_hat;

  for (i=0; i<datalen*ncentroids; i++) {
    q_of_z[i] = q_of_z_in[i];
  }

  vdp_mk_hp_posterior(Mu_mu, S2_mu, Mu_bar, Mu_tilde, 
		      Alpha_ksi, Beta_ksi, Ksi_alpha, Ksi_beta, 
		      post_gamma, prior_alpha,
		      U_p, U_hat,
		      datalen, dim1, dim2, data1, data2, 
		      Ns, ncentroids, implicit_noisevar, q_of_z, Nc, true_Nc);
  
  /****************** CREATE A LIST WITH THE OUTPUT *****************/
  // Creating a character string vector 
  // of the "names" attribute of the
  // objects in out list:

   PROTECT(list_names = NEW_CHARACTER(9));    

  for(i = 0; i < 9; i++)   
    SET_STRING_ELT(list_names,i,mkChar(posterior_fields[i])); 

  // Creating a list with 9 vector elements:
  PROTECT(list = NEW_LIST(9)); 
  // attaching elements to the list:
  SET_ELEMENT(list, 0, oMu_bar);
  SET_ELEMENT(list, 1, oMu_tilde);
  SET_ELEMENT(list, 2, oKsi_alpha);
  SET_ELEMENT(list, 3, oKsi_beta);
  SET_ELEMENT(list, 4, opost_gamma);
  SET_ELEMENT(list, 5, oNc);
  SET_ELEMENT(list, 6, otrue_Nc);
  SET_ELEMENT(list, 7, oq_of_z);
  SET_ELEMENT(list, 8, oU_hat);

  // and attaching the vector names:
  SET_NAMES(list, list_names);
  
  UNPROTECT(2+9+2+dim2);
  //CHECK UPDATE NUMBER OF UNPROTECTS : UNPROTECT(4);
  return list;
}
 
/************************************************************/




