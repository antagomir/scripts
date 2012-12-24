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


void sumlogsumexp(int dim1, int dim2, double *in, double *out)
{
  register int i, j;
  double rowsum, rowmax;

  *out = 0.0;
  for (i=0; i<dim1; i++) {
    rowmax = DBL_MIN;
    for (j=0; j<dim2; j++) {
      if (in[j*dim1 + i] > rowmax)
	rowmax = in[j*dim1 + i];
    }
    rowsum = 0.0;
    for (j=0; j<dim2; j++) {
      rowsum += exp(in[j*dim1 + i] - rowmax);
    }
    *out += rowmax + log(rowsum);
  }
}




/************************************************************/
/* bridge function                                          */

SEXP
vdp_sumlogsumexp(SEXP matrix_M) {
  int dim1, dim2;
  double *in, *out;
  SEXP output, dims;
  
  /******************** input variables ********************/
  in      = NUMERIC_POINTER(matrix_M);
  dim1    = INTEGER_POINTER(GET_DIM(matrix_M))[0];
  dim2    = INTEGER_POINTER(GET_DIM(matrix_M))[1];
  PROTECT(dims = allocVector(INTSXP, 2));
  INTEGER(dims)[0] = 1; INTEGER(dims)[1] = 1;

  /******************** output variables ********************/
  PROTECT(output = NEW_NUMERIC(1));
  SET_DIM(output, dims);
  out = NUMERIC_POINTER(output);

  sumlogsumexp(dim1, dim2, in, out);
  
  UNPROTECT(2);
  return output;
}
 
/************************************************************/




