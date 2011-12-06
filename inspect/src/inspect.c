/* #include <R.h> */
#include <Rinternals.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <assert.h>

SEXP inspect (SEXP args);

SEXP inspect (SEXP args)
{
  int i, n;
  SEXP ap, el, v, attr;
  const char *name;

  v = R_NilValue; 
  Rprintf ("inspect called with %d args\n", length(args)); 
  for (i = 0, ap = args;  ap != R_NilValue;  i++, ap = CDR(ap)) {
    name = isNull(TAG(ap)) ? "" : CHAR(PRINTNAME(TAG(ap)));
    el = CAR(ap);
    if (0 == length(el)) {
      if (NILSXP == TYPEOF(el))
        Rprintf ("[%d] '%s' NILSXP\n", i, name);
      else
        Rprintf ("[%d] '%s' R type, length 0\n", i, name);
      continue;
    }
    switch (TYPEOF(el)) {
    case REALSXP:
      Rprintf ("%d  [%d] '%s' REALSXP %g\n", TYPEOF(el), i, name, REAL(el)[0]);
      break;
    case STRSXP:
      Rprintf ("%d  [%d] '%s' STRSXP %s\n", TYPEOF(el), i, name, CHAR(STRING_ELT(el,0)));
      break;
    case LGLSXP:
      Rprintf ("%d  [%d] '%s' LGLSXP %d\n", TYPEOF(el), i, name, LOGICAL(el)[0]);
      break;
    case VECSXP:
      Rprintf ("%d  [%d] '%s' VECSXP\n", TYPEOF(el), i, name);
      v = el;
      break;
    default:
      Rprintf ("%d  [%d] '%s' unhandled R type\n", TYPEOF(el), i, name);
    }
  }

  if (R_NilValue != v) {
    attr = getAttrib (v, R_DimSymbol);
    if (R_NilValue != attr) {
      n = length(attr);
      for (i = 0;  i < n;  i++) {
        Rprintf ("attribute dim[%d]: %d\n", i, INTEGER(attr)[i]);
      }
    }
    attr = getAttrib (v, R_ClassSymbol);
    if (R_NilValue != attr) {
      Rprintf ("attribute class: %s\n", CHAR(STRING_ELT(attr,0)));
    }
    attr = getAttrib (v, R_RowNamesSymbol);
    if (R_NilValue != attr) {
      n = length(attr);
      for (i = 0;  i < n;  i++) {
        Rprintf ("attribute row.names[%d]: %s\n", i, "not easy");
        switch (TYPEOF(attr)) {
        case REALSXP:
          Rprintf ("%d REALSXP %g\n", TYPEOF(attr), REAL(el)[0]);
          break;
        case STRSXP:
          Rprintf ("%d STRSXP %s\n", TYPEOF(attr), CHAR(STRING_ELT(attr,i)));
          break;
        case INTSXP:
          Rprintf ("%d INTSXP %d\n", TYPEOF(attr), INTEGER(attr)[i]);
          break;
        case LGLSXP:
          Rprintf ("%d LGLSXP %d\n", TYPEOF(attr), LOGICAL(attr)[i]);
          break;
        case VECSXP:
          Rprintf ("%d VECSXP\n", TYPEOF(attr));
          break;
        default:
          Rprintf ("%d unhandled R type\n", TYPEOF(attr));
        }
      }
    }
  }

  return R_NilValue;
} /* inspect */

