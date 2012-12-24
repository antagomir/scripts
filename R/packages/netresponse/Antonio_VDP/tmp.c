/************************************************************/
/* Read Elements of a R list in C.                         */
SEXP getListElement(SEXP list, const char *str)
{
  SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  int i;
  for (i = 0; i < length(list); i++)
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  return elmt;
}

/************************************************************/
/* digamma function (by Antti Honkela)                      */

double 
digamma(double x) {
  double y = 0.0, r = 0.0, xn = x;

  if (xn <= 0) {
    return R_NaN;
  }
  
  if (xn <= DIGAMMA_S)
    y = DIGAMMA_D1 - 1.0 / xn;
  else {
    while (xn < DIGAMMA_C) {
      y  -= 1.0 / xn;
      xn += 1.0;
    }
    r = 1.0 / xn;
    y += log(xn) - .5 * r;
    r = POW2(r);
    y -= r * (DIGAMMA_S3 - r * (DIGAMMA_S4 - r*DIGAMMA_S5));
  }
 
  return y;
}                                                                              


/************************************************************/

void
compute_variance(int ncentroids, int dim1, double *Ksi_alpha, 
                 double *Ksi_beta, double **S2_x, double **Ksi_log) {
  register int i, ind, k;
  
  for (i = 0; i < ncentroids; i++) 
    for (k = 0; k < dim1; k++) {
      ind = k * ncentroids + i;
      S2_x[i][k]    = Ksi_beta[ind]/Ksi_alpha[ind];
      Ksi_log[i][k] = digamma(Ksi_alpha[ind])-log(Ksi_beta[ind]);
      
      if( S2_x[i][k] < 1e-100 ) S2_x[i][k] = 1e-100;
    }
  
  return;
}

/************************************************************/
void
compute_tempmat(long datalen, int dim1, int dim2, int ncentroids,
		double **Temp, double *data1, int **data2_int,
		double *Mu_bar, double *Mu_tilde, double **S2_x,
                double **Ksi_log, double ***U_hat_table, double *Ns,
		double implicit_noisevar, double *log_lambda) {
  register int i, k;
  long         ind, j,t;
  double term;
  
  for (i = 0; i < ncentroids; i++) {
    for (j = 0; j < datalen; j++) {
      Temp[i][j] = 0.0;
      for (k = 0; k < dim1; k++) {
	ind  = k * ncentroids + i;
	Temp[i][j] += ((Mu_tilde[ind]+POW2(data1[k*datalen + j]-Mu_bar[ind]) + implicit_noisevar)/
		       S2_x[i][k]) - Ksi_log[i][k];
      }
      Temp[i][j] /= 2.0;
    }
  }
  for(j=0;j<dim2;j++){
    for(i=0;i<ncentroids;i++){
      term=0.0;
      for(k=0;k<(int)(Ns[j]);k++){
	term += U_hat_table[j][i][k]; 
	U_hat_table[j][i][k]=digamma(U_hat_table[j][i][k]);
      }
      term=digamma(term);
      for (t=0;t<datalen;t++){
	Temp[i][t] += (term - U_hat_table[j][i][data2_int[j][t]]);
      }
    }
  }

  for (i = 0; i < ncentroids; i++) {
    for (j = 0; j < datalen; j++) {
      log_lambda[i * datalen + j] += -dim1*log(2*M_PI)/2 - Temp[i][j];
    }
  }
  return;
}


/***
function log_lambda = mk_log_lambda(data, hp_posterior, hp_prior, opts);
% log_lambda: N*K
% q(z_n=c|x_n) = lambda_n_c / sum_c lambda_n_c

[N,D] = size(data.given_data.data);
K = size(hp_posterior.Mu_bar, 1);

log_lambda = zeros(N,K);
for c=1:K
  E_log_p_of_z_given_other_z_c = ...
      psi(hp_posterior.gamma(1,c)) ...
      - psi(sum(hp_posterior.gamma(:,c),1)) ...
      + sum(psi(hp_posterior.gamma(2,[1:c-1])) - psi(sum(hp_posterior.gamma(:,[1:c-1]),1)), 2);
  log_lambda(:,c) = E_log_p_of_z_given_other_z_c;
end
***/
void log_p_of_z_given_other_z_c(int datalen, long ncentroids,
				double *post_gamma, double *log_lambda)
{
  register int c, i;
  double E_log_p;

  for (c=0; c<ncentroids; c++) {
    E_log_p = digamma(post_gamma[2*c]) - digamma(post_gamma[2*c] + post_gamma[2*c+1]);
    for (i=0; i<c; i++) {
      E_log_p += digamma(post_gamma[2*i+1]) - digamma(post_gamma[2*i] + post_gamma[2*i+1]);
    }
    for (i=0; i<datalen; i++) {
      log_lambda[c*datalen+i] = E_log_p;
    }
  }

  return;
}



void fix_lambda(int ncentroids, long datalen, double *prior_alpha, double *log_lambda)
{
  register int i;
  double correction;

  correction = log(1 - exp(digamma(*prior_alpha) - digamma(1 + *prior_alpha)));
  for (i=0; i<datalen; i++) {
    log_lambda[(ncentroids-1)*datalen + i] -= correction;
  }

  return;
}



void
allocate_memory(long datalen,int ncentroids,int dim1,int dim2,double ***S2_x,
                double ***Ksi_log, double ***Temp,
		double ****U_hat_table,int ***data2_int,double *Ns) {
  register int i,j;
  
  *Temp    = (double **)malloc(ncentroids * sizeof(double*));
  if (dim1) {
    *S2_x    = (double **)malloc(ncentroids * sizeof(double*));
    *Ksi_log = (double **)malloc(ncentroids * sizeof(double*));
  }
  if (dim2) {
    *U_hat_table=(double ***)malloc(dim2 * sizeof(double*));
    *data2_int=(int **)malloc(dim2 * sizeof(int*));
  }
  for (i = 0; i < ncentroids; i++) {
    (*Temp)[i]    = (double *)malloc(datalen * sizeof(double));
    if (dim1) {
      (*S2_x)[i]    = (double *)malloc(dim1 * sizeof(double));
      (*Ksi_log)[i] = (double *)malloc(dim1 * sizeof(double));
    }
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
free_memory(int ncentroids,int dim1, int dim2, double ***Temp, double ***W, 
            double ***S2_x, double ***Ksi_log,
		double ****U_hat_table, int ***data2_int) {
  register int i,j;
  for (j=0;j<dim2;j++){
    for (i=0;i<ncentroids;i++) {
      free((*U_hat_table)[j][i]);
    }
    free((*data2_int)[j]);
    free((*U_hat_table)[j]);
  }

  for (i = 0; i < ncentroids; i++) { 
    free((*Temp)[i]); 
    if (dim1){
      free((*S2_x)[i]); 
      free((*Ksi_log)[i]);
    }
  }
  
  free(*Temp);
  if (dim1) {
    free(*S2_x);
    free(*Ksi_log);
  }
  if (dim2) {
    free(*U_hat_table);
    free(*data2_int);
  }
  return;
}



/***
function log_lambda = mk_log_lambda(data, hp_posterior, hp_prior, opts);
% log_lambda: N*K
% q(z_n=c|x_n) = lambda_n_c / sum_c lambda_n_c

[N,D] = size(data.given_data.data);
K = size(hp_posterior.Mu_bar, 1);

log_lambda = zeros(N,K);
for c=1:K
  E_log_p_of_z_given_other_z_c = ...
      psi(hp_posterior.gamma(1,c)) ...
      - psi(sum(hp_posterior.gamma(:,c),1)) ...
      + sum(psi(hp_posterior.gamma(2,[1:c-1])) - psi(sum(hp_posterior.gamma(:,[1:c-1]),1)), 2);
  log_lambda(:,c) = E_log_p_of_z_given_other_z_c;
end

%%% IVGA-specific
types    = data.given_data.types;
X1       = data.given_data.data(:, (types == 1)); % real-valued
X2       = data.given_data.data(:, (types == 2)); % nominal
S        = data.given_data.S(types == 2);

[N,M1]=size(X1);
M2=size(X2,2);

Ksi_log   = (psi(hp_posterior.Ksi_alpha)-log(hp_posterior.Ksi_beta));
S2_x      = hp_posterior.Ksi_beta ./ hp_posterior.Ksi_alpha;

temp=zeros(K,N);
for j=1:M2,
  temp=temp + psi(hp_posterior.Uhat{j}(:, X2(:, j))) ...
       - repmat(psi(sum(hp_posterior.Uhat{j},2)),1,N);
end
for j=1:M1
  temp = temp - 0.5 * ...
	 gminus(...
	     grdivide(...
		 gplus(hp_posterior.Mu_tilde(:,j), ...
		       gminus(X1(:,j)', hp_posterior.Mu_bar(:,j)).^2), ...
		 S2_x(:,j)), ...
	     Ksi_log(:,j));
end

log_lambda = log_lambda - M1*log(2*pi)/2 + temp';
  
if isequal(opts.algorithm, 'vdp')
  log_lambda(:,end) = log_lambda(:,end) - log(1- exp(psi(hp_prior.alpha) - psi(1+hp_prior.alpha)));
end
***/


void
vdp_mk_log_lambda(double *Mu_mu, double *S2_mu, double *Mu_bar, double *Mu_tilde, 
		  double *Alpha_ksi, double *Beta_ksi, 
		  double *Ksi_alpha, double *Ksi_beta, 
		  double *post_gamma, double *log_lambda, double *prior_alpha,
		  double *U_p, SEXP *U_hat,
		  long datalen, int dim1, int dim2, double *data1, double *data2, 
		  double *Ns, int ncentroids, 
		  double implicit_noisevar) {
  register long i, j, t;
  register int k;
  double  *U_hat_j;
  SEXP U_hat_j_SEXP;
  double       **W, **Temp, **S2_x,**Ksi_log,***U_hat_table;
  int          **data2_int;

  allocate_memory(datalen, ncentroids, dim1,dim2, &S2_x, &Ksi_log, &Temp,
		  &U_hat_table,&data2_int,Ns );

  for (j=0;j<dim2;j++){
    for(t=0;t<datalen;t++)
      data2_int[j][t]=((int)(data2[j*datalen+t]))-1;

    U_hat_j_SEXP = VECTOR_ELT(*U_hat, (int)j);
    U_hat_j=NUMERIC_POINTER(U_hat_j_SEXP);

    for(i=0;i<ncentroids;i++)
      for(k=0;k<Ns[j];k++)
        U_hat_table[j][i][k]=U_hat_j[k*ncentroids+i];
  }

  
  if (dim1) 
    compute_variance(ncentroids, dim1, Ksi_alpha, Ksi_beta, S2_x, Ksi_log);

  log_p_of_z_given_other_z_c(datalen, ncentroids, post_gamma, log_lambda);

  compute_tempmat(datalen,dim1,dim2,ncentroids,Temp,data1,data2_int,
		  Mu_bar,Mu_tilde,S2_x,Ksi_log,U_hat_table,Ns,
		  implicit_noisevar, log_lambda);
    
  fix_lambda(ncentroids, datalen, prior_alpha, log_lambda);
    
  free_memory(ncentroids, dim1,dim2, &Temp, &W, &S2_x, &Ksi_log, &U_hat_table, &data2_int);
  return;
}





/************************************************************/
/* bridge function                                          */

SEXP
m_log_lambda(SEXP X1, SEXP X1_Columns, SEXP X1_Rows, 
             SEXP X2, SEXP X2_Columns,
             SEXP realS, SEXP OPTSimplicit_noisevar,
             SEXP hp_prior, SEXP hp_posterior) {
  long datalen;
  int  dim1, dim2, ncentroids;
  double *Mu_mu, *S2_mu, *Mu_bar, *Mu_tilde, 
    *Alpha_ksi, *Beta_ksi, *Ksi_alpha, *Ksi_beta, *U_p, *prior_alpha,
    *post_gamma, *log_lambda;
  double *data1;
  double *data2;
  SEXP olog_lambda, oU_hat;
  SEXP* U_hat;

  double *Ns;
  double implicit_noisevar;
  
  /******************** input variables ********************/
  
  
  /************ CONVERTED input variables ******************/
  /* data */
  PROTECT(X1 = AS_NUMERIC(X1));  
  data1   = NUMERIC_POINTER(X1);
  dim1    = INTEGER_VALUE(X1_Columns);
  datalen = INTEGER_VALUE(X1_Rows);

  PROTECT(X2 = AS_NUMERIC(X2));  
  data2   = NUMERIC_POINTER(X2);
  dim2    = INTEGER_VALUE(X2_Columns);

  Ns = NUMERIC_POINTER(realS);
  implicit_noisevar = NUMERIC_VALUE(OPTSimplicit_noisevar);
  

  /* Converted Initial Values of Model Parameters */

  if(dim1) {
    Mu_mu       = NUMERIC_POINTER(getListElement(hp_prior,"Mu_mu"));
    S2_mu       = NUMERIC_POINTER(getListElement(hp_prior,"S2_mu"));
    Alpha_ksi   = NUMERIC_POINTER(getListElement(hp_prior,"Alpha_ksi"));
    Beta_ksi    = NUMERIC_POINTER(getListElement(hp_prior,"Beta_ksi"));
    Mu_bar      = NUMERIC_POINTER(getListElement(hp_posterior,"Mu_bar"));
    Mu_tilde    = NUMERIC_POINTER(getListElement(hp_posterior,"Mu_tilde"));
    Ksi_alpha   = NUMERIC_POINTER(getListElement(hp_posterior,"Ksi_alpha"));
    Ksi_beta    = NUMERIC_POINTER(getListElement(hp_posterior,"Ksi_beta"));
  }
  if(dim2) {
    U_p         = NUMERIC_POINTER(getListElement(hp_prior,"U_p"));
    oU_hat      = getListElement(hp_posterior,"Uhat");
    U_hat      = &oU_hat;
  }
  

  prior_alpha = NUMERIC_POINTER(getListElement(hp_prior,"alpha"));
  post_gamma  = NUMERIC_POINTER(getListElement(hp_posterior,"gamma"));

  ncentroids = INTEGER_POINTER( GET_DIM(getListElement(hp_posterior,"Mu_bar")) )[0];

  /*printf("\nMu_mu ");  
  for(i=0; i< dim1;i++)
    printf("%f ", Mu_mu[i]);
  printf("\nS2_mu ");
  for(i=0; i< dim1;i++)
    printf("%f ", S2_mu[i]);
  printf("\nAlpha_ksi ");
  for(i=0; i< dim1;i++)
    printf("%f ", Alpha_ksi[i]);
  printf("\nBeta_ksi ");
  for(i=0; i< dim1;i++)
    printf("%f ", Beta_ksi[i]);
  
  printf("\nMu_bar ");
  for(i=0;i<ncentroids*dim1;i++)
    printf("%f ", Mu_bar[i]);
  printf("\nMu_tilde ");
  for(i=0;i<ncentroids*dim1;i++)
    printf("%f ", Mu_tilde[i]);
  printf("\nKsi_alpha ");
  for(i=0;i<ncentroids*dim1;i++)
    printf("%f ", Ksi_alpha[i]);
  printf("\nKsi_beta ");
  for(i=0;i<ncentroids*dim1;i++)
    printf("%f ", Ksi_beta[i]);
  printf("\nprior_alpha = %f", *prior_alpha);
  printf("\npost_gamma ");
  for(i=0;i<2*ncentroids;i++)
    printf("%f ", post_gamma[i]);
  printf("ncentroids = %d\n", ncentroids);
  printf("dim2 = %d\n",dim2);*/
  /******************** output variables ********************/
  PROTECT(olog_lambda     = NEW_NUMERIC(datalen*ncentroids));
  log_lambda = NUMERIC_POINTER(olog_lambda);


  vdp_mk_log_lambda(Mu_mu, S2_mu, Mu_bar, Mu_tilde, 
		    Alpha_ksi, Beta_ksi, Ksi_alpha, Ksi_beta, 
		    post_gamma, log_lambda, prior_alpha,
		    U_p, U_hat,
		    datalen, dim1, dim2, data1, data2, 
		    Ns, ncentroids, implicit_noisevar);

  UNPROTECT(3);

  return olog_lambda;
}
 
/************************************************************/




