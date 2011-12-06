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
  case LGLSXP:
    Rprintf ("%s: LGLSXP, TYPEOF=%d: %d\n", s, typ, LOGICAL(e)[i]);
    break;
  case INTSXP:
    Rprintf ("%s: INTSXP, TYPEOF=%d: %d\n", s, typ, INTEGER(e)[i]);
    break;
  case REALSXP:
    Rprintf ("%s: REALSXP, TYPEOF=%d: %g\n", s, typ, REAL(e)[i]);
    break;
  case STRSXP:
    Rprintf ("%s: STRSXP, TYPEOF=%d: %s\n", s, typ, CHAR(STRING_ELT(e,i)));
    break;
  case VECSXP:
    Rprintf ("%s: VECSXP, TYPEOF=%d\n", s, typ);
    break;
  default:
    Rprintf ("%s: R type, TYPEOF=%d\n", s, typ);
  }

  return;
} /* showExp */

typedef struct symbRec {
  SEXP e;
  char def[32];
} symbRec_t;

#if 0
symbRec_t symbRecs[] = {
  {R_Bracket2Symbol, "R_Bracket2Symbol"},
  {R_BracketSymbol, "R_BracketSymbol"},
  {R_BraceSymbol, "R_BraceSymbol"},
  {R_ClassSymbol, "R_ClassSymbol"},
  {R_DeviceSymbol, "R_DeviceSymbol"}
};
#else
symbRec_t symbRecs[] = {
  {NULL, "R_Bracket2Symbol"},
  {NULL, "R_BracketSymbol"},
  {NULL, "R_BraceSymbol"},
  {NULL, "R_ClassSymbol"},
  {NULL, "R_DeviceSymbol"},
  {NULL, "R_DimNamesSymbol"},
  {NULL, "R_DimSymbol"},
  {NULL, "R_DollarSymbol"},
  {NULL, "R_DotsSymbol"},
  {NULL, "R_DropSymbol"},
  {NULL, "R_LastvalueSymbol"},
  {NULL, "R_LevelsSymbol"},
  {NULL, "R_ModeSymbol"},
  {NULL, "R_NameSymbol"},
  {NULL, "R_NamesSymbol"},
  {NULL, "R_NaRmSymbol"},
  {NULL, " R_PackageSymbol"},
  {NULL, " R_QuoteSymbol"},
  {NULL, "R_RowNamesSymbol"},
  {NULL, "R_SeedsSymbol"},
  {NULL, "R_SourceSymbol"},
  {NULL, "R_TspSymbol"}
};
#endif
#define N_SYMBRECS (sizeof(symbRecs)/sizeof(symbRecs[0]))

void listAttribs (SEXP e)
{
  int k;
  int hit;
  SEXP attr;

  k = 0;
  symbRecs[k++].e = R_Bracket2Symbol;
  symbRecs[k++].e = R_BracketSymbol;
  symbRecs[k++].e = R_BraceSymbol;
  symbRecs[k++].e = R_ClassSymbol;
  symbRecs[k++].e = R_DeviceSymbol;
  symbRecs[k++].e = R_DimNamesSymbol;
  symbRecs[k++].e = R_DimSymbol;
  symbRecs[k++].e = R_DollarSymbol;
  symbRecs[k++].e = R_DotsSymbol;
  symbRecs[k++].e = R_DropSymbol;
  symbRecs[k++].e = R_LastvalueSymbol;
  symbRecs[k++].e = R_LevelsSymbol;
  symbRecs[k++].e = R_ModeSymbol;
  symbRecs[k++].e = R_NameSymbol;
  symbRecs[k++].e = R_NamesSymbol;
  symbRecs[k++].e = R_NaRmSymbol;
  symbRecs[k++].e = R_PackageSymbol;
  symbRecs[k++].e = R_QuoteSymbol;
  symbRecs[k++].e = R_RowNamesSymbol;
  symbRecs[k++].e = R_SeedsSymbol;
  symbRecs[k++].e = R_SourceSymbol;
  symbRecs[k++].e = R_TspSymbol;

  assert(k==N_SYMBRECS);

  for (k = 0;  k < N_SYMBRECS;  k++) {
    attr = getAttrib (e, symbRecs[k].e);
    hit = (R_NilValue != attr);
    if (hit)
      Rprintf ("getAttrib(e,%32s): %s\n", symbRecs[k].def, hit ? "    yes" : " no");
  }
  return;
} /* listAttribs */

SEXP inspect (SEXP args)
{
  int i, k, n, n2;
  SEXP ap, e, v, attr;
  SEXP r;                       /* return value */
  SEXP f;                       /* factor - part or all of return */
  SEXP fLevels;                 /* levels for factor f */
  SEXP class;                   /* used to set the class of f (or another SEXP) */
  const char *name;
  char msg[256];

  v = R_NilValue;
  Rprintf ("inspect called with %d args\n", length(args));
  for (i = 0, ap = args;  ap != R_NilValue;  i++, ap = CDR(ap)) {
    name = isNull(TAG(ap)) ? "" : CHAR(PRINTNAME(TAG(ap)));
    e = CAR(ap);
    sprintf (msg, "arg %d (%s)", i, name);
    showExp (msg, e, 0);
    if (VECSXP == TYPEOF(e))
      v = e;
  }

  if (R_NilValue != v) {
    Rprintf ("isFrame (v) = %d\n", isFrame(v));
    Rprintf ("isFactor(v) = %d\n", isFactor(v));
    Rprintf ("Checking attributes of VECSXP\n");
    listAttribs (v);
    Rprintf ("\n");

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

    attr = getAttrib (v, R_NamesSymbol);
    if (R_NilValue != attr) {
      n = length(attr);
      for (i = 0;  i < n;  i++) {
        sprintf (msg, "attribute names[%d]", i);
        showExp (msg, attr, i);
      } /* for i */
    }

    n = length(v);
    Rprintf ("VECSXP has length %d\n", n);
    Rprintf ("Checking elements of VECSXP\n");
    for (i = 0;  i < n;  i++) {
      e = VECTOR_ELT(v, i);
      Rprintf ("isFrame (v[%d]) = %d\n", i, isFrame(e));
      Rprintf ("isFactor(v[%d]) = %d\n", i, isFactor(e));
      sprintf (msg, "dataframe column %d", i);
      showExp (msg, e, 0);
#if 1
      Rprintf ("Checking attributes of v[%d]\n", i);
      listAttribs (e);
      // Rprintf ("\n");
#endif
      attr = getAttrib (e, R_ClassSymbol);
      if (R_NilValue != attr) {
        Rprintf ("attribute class: %s\n", CHAR(STRING_ELT(attr,0)));
      }
      attr = getAttrib (e, R_LevelsSymbol);
      if (R_NilValue != attr) {
        n2 = length(attr);
        for (k = 0;  k < n2;  k++) {
          sprintf (msg, "level v[%d][%d]", i, k);
          showExp (msg, attr, k);
        } /* for k */
      }
      Rprintf ("\n");
    } /* loop over elements in the dataframe list */

  } /* if v */

  /* create f to match as.factor(c("A","B","Z")) */
  PROTECT(f = allocVector(INTSXP, 3));
  for (i = 0;  i < 3;  i++)
    INTEGER(f)[i] = i+1;
  PROTECT(fLevels = allocVector(STRSXP, 3));
  SET_STRING_ELT(fLevels, 0, mkChar("A"));
  SET_STRING_ELT(fLevels, 1, mkChar("B"));
  SET_STRING_ELT(fLevels, 2, mkChar("Z"));
  setAttrib (f, R_LevelsSymbol, fLevels);
#if 0
  setAttrib (f, R_ClassSymbol, mkChar("factor"));
#else
  PROTECT(class = allocVector(STRSXP, 1));
  SET_STRING_ELT(class, 0, mkChar("factor"));
  classgets (f, class);
#endif

  r = f;
  UNPROTECT(3);
  return r;
} /* inspect */

