/* This biking.c file contains a method to compute the number of gears on
   a bicycle:
   nGears <- bike_gears(nFront, nBack)
   This method is declared as External in R.

  * Some handy type info:
  * typedef struct SEXPREC {
  *    SEXPREC_HEADER;
  *    union {
  *      struct primsxp_struct primsxp;
  *      struct symsxp_struct symsxp;
  *      struct listsxp_struct listsxp;
  *      struct envsxp_struct envsxp;
  *      struct closxp_struct closxp;
  *      struct promsxp_struct promsxp;
  *    } u;
  * } SEXPREC, *SEXP;
  * SEXP allocVector(SEXPTYPE, R_len_t);
  * SEXP VECTOR_ELT (SEXP x, int i);
  * SEXP STRING_ELT (SEXP x, int i);
  * const char * CHAR (SEXP x);
  *
  */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>
#include <math.h>
#include <assert.h>

/* gateway routine for bike_gears(nFront, nBack)
 */
SEXP bike_gears (SEXP args)
{
  const char *funcName = "bike_gears";
  int arglen, front, back, result;
  SEXP expr;

  arglen = length(args);
  if (3 != arglen) {            /* first arg is self or name ?? */
    error ("usage: %s(nFront, nBack) - incorrect arg count", funcName);
  }

  expr = CADR(args);
  if (TYPEOF(expr) != INTSXP) {
    error ("usage: %s(nFront, nBack) - nFront must be an integer", funcName);
  }
  front = INTEGER(expr)[0];
  Rprintf ("DEBUG: nFront = %d\n", front);

  expr = CADDR(args);
  if (TYPEOF(expr) != INTSXP) {
    error ("usage: %s(nFront, nBack) - nBack must be an integer", funcName);
  }
  back = INTEGER(expr)[0];
  Rprintf ("DEBUG:  nBack = %d\n", back);

  result = front * back;

  PROTECT(expr = allocVector(INTSXP, 1));
  INTEGER(expr)[0] = result;
  UNPROTECT(1);
  return expr;
} /* bike_gears */
