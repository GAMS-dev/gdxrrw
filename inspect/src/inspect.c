#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <assert.h>

void showExp (const char *s, SEXP e, int i)
{
  int typ;

  typ = TYPEOF(e);
  if (0 == length(e)) {
    if (NILSXP == typ)
      Rprintf ("%s: NILSXP\n", s);
    else
      Rprintf ("%s: R type, TYPEOF=%d, length 0\n", s, typ);
    return;
  }
  switch (typ) {
  case REALSXP:
    Rprintf ("%s: REALSXP, TYPEOF=%d: %g\n", s, typ, REAL(e)[i]);
    break;
  case STRSXP:
    Rprintf ("%s: STRSXP, TYPEOF=%d: %s\n", s, typ, CHAR(STRING_ELT(e,i)));
    break;
  case LGLSXP:
    Rprintf ("%s: LGLSXP, TYPEOF=%d: %d\n", s, typ, LOGICAL(e)[i]);
    break;
  case VECSXP:
    Rprintf ("%s: VECSXP, TYPEOF=%d\n", s, typ);
    break;
  default:
    Rprintf ("%s: R type, TYPEOF=%d\n", s, typ);
  }

  return;
} /* showExp */

SEXP inspect (SEXP args)
{
  int i, n;
  SEXP ap, el, v, attr;
  const char *name;
  char msg[256];

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
        sprintf (msg, "attribute row.names[%d]", i);
        showExp (msg, attr, i);
      } /* for i */
    }   /* RowNamesSymbol not NULL */
  }

  showExp ("test nil", R_NilValue, 0);

  return R_NilValue;
} /* inspect */

