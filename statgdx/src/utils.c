/* utils.c
 * utility functions for gdxrrw package
 * $Id$
 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "gdxstatic.h"
#include "gclgms.h"
#include "globals.h"

/* just to shut up some warnings on Linux */
typedef int (*compareFunc_t) (const void *, const void *);

int _P3_DllInit (void);
void _P3_DllFini (void);

/* ------------------ start of globally available functions --------------- */

/* gdxLoad will be called once, when the package is loaded
 * (i.e. when the shared library is loaded)
*/
SEXP
gdxLoad (SEXP args)
{
  // Rprintf ("*** gdxLoad called ***\n");
  _P3_DllInit();
  return R_NilValue;
} /* gdxLoad */

/* gdxUnLoad will be called once, when the package is unloaded
 * (i.e. when the shared library is unloaded)
*/
SEXP
gdxUnLoad (SEXP args)
{
  // Rprintf ("*** gdxUnLoad called ***\n");
  _P3_DllFini();
  return R_NilValue;
} /* gdxUnLoad */


/* CHAR2ShortStr
 * copy the input C-style string to a shortString buffer
 * return the output buffer, nor NULL if there is no input
 */
char *
CHAR2ShortStr (const char *from, shortStringBuf_t to)
{
  size_t n;

  if (NULL == from)
    return NULL;
  n = strlen(from);
  if (n >= sizeof(shortStringBuf_t)) {
    n = sizeof(shortStringBuf_t);
    strncpy(to, from, n);
    to[n-1] = '\0';
  }
  else
    strcpy(to, from);
  return to;
} /* CHAR2ShortStr */

/* checkFileExtension: checks the file extension of the gdx file.
 * If fileName does not have '.gdx' then we add it here
*/
void
checkFileExtension (shortStringBuf_t fileName)
{
  char *fileExt;

  fileExt = strrchr (fileName, '.');
  if (NULL == fileExt) {
    if (strlen(fileName) < sizeof(shortStringBuf_t)-4) {
      fileName = strcat(fileName, ".gdx");
    }
    else {
      error ("Input file name '%s' is too long\n", fileName);
    }
  }
  else if (0 != strcasecmp(".gdx", fileExt)) {
    error ("Input file extension '%s' is not valid: input must be a GDX file\n",
           fileExt);
  }
  return;
} /* checkFileExtension */

/* checkStringLength
 * raise an exception if the input str is too long to be a short-string
 */
void
checkStringLength (const char *str)
{
  int i;

  i = (int) strlen (str);
#if 0
  if (0 == i) {
    error ("Cannot access empty field. Please try again");
  }
#endif
  if (i >= sizeof(shortStringBuf_t)) {
    error("The data entered is too long: len=%d exceeds limit=%d.",
          i, (int)sizeof(shortStringBuf_t)-1);
  }
} /* checkStringLength */


/* get option gdx.inventSetText */
Rboolean
getInventSetText (Rboolean defVal)
{
  SEXP o = GetOption1(install("gdx.inventSetText"));

  if (R_NilValue == o)
    return defVal;
  if (LGLSXP == TYPEOF(o))
    return LOGICAL(o)[0];
  return asLogical(o);
} /* getInventSetText */

/* interpret an expression (probably an input arg) as a logical/boolean */
Rboolean
exp2Boolean (SEXP exp)
{
  const char *s;

  switch (TYPEOF(exp)) {
  case LGLSXP:
    return LOGICAL(exp)[0];
    break;
  case INTSXP:
    return INTEGER(exp)[0];
    break;
  case REALSXP:
    if (0.0 == REAL(exp)[0])
      return FALSE;
    else
      return TRUE;
    break;
  case STRSXP:
    s = CHAR(STRING_ELT(exp, 0));
    if ('\0' == s[1])
      switch (s[0]) {
      case 'T':
      case 't':
      case 'Y':
      case 'y':
      case '1':
        return TRUE;
      case 'F':
      case 'f':
      case 'N':
      case 'n':
      case '0':
        return FALSE;
      default:
        return NA_LOGICAL;
      }
    if (0 == strcmp("TRUE",s)) return TRUE;
    if (0 == strcmp("True",s)) return TRUE;
    if (0 == strcmp("true",s)) return TRUE;
    if (0 == strcmp("YES",s)) return TRUE;
    if (0 == strcmp("Yes",s)) return TRUE;
    if (0 == strcmp("yes",s)) return TRUE;

    if (0 == strcmp("FALSE",s)) return FALSE;
    if (0 == strcmp("False",s)) return FALSE;
    if (0 == strcmp("false",s)) return FALSE;
    if (0 == strcmp("NO",s)) return FALSE;
    if (0 == strcmp("No",s)) return FALSE;
    if (0 == strcmp("no",s)) return FALSE;

    return NA_LOGICAL;
    break;
  }
  return NA_LOGICAL;
} /* exp2Boolean */

/* this method for global input "compress" */
int isCompress (void)
{
  SEXP gamso, tmp, lstName;
  Rboolean logical = NA_LOGICAL;
  char *str;
  int i, infields;
  int compress = 0;
  int found = 0;
  shortStringBuf_t fName;

  str = NULL;
  gamso = findVar( install("gamso"), R_GlobalEnv );

  if (gamso == NULL || TYPEOF(gamso) == NILSXP  ||  TYPEOF(gamso) == SYMSXP) {
    globalGams = 0;
    return 0;
  }

  /*   if (TYPEOF(gamso) != VECSXP  && globalGams)
       {
       warning("To change default behavior, please enter 'gamso' as list.\n" );
       Rprintf("You entered it as %d.\n", TYPEOF(gamso) );
       globalGams = 0;
       return 0;
       } */

  else if (TYPEOF(gamso) == VECSXP && globalGams == 1) {
    lstName = getAttrib(gamso, R_NamesSymbol);
    i=0;
    infields = length(gamso);
    /* Checking if field data is for "name" */
    for (i = 0; i < infields; i++) {
      if (strcasecmp("compress", CHAR(STRING_ELT(lstName, i))) == 0) {
        found = 1;
        break;
      }
    }

    if (found == 1 && globalGams) {
      tmp = VECTOR_ELT(gamso, i);
      if (TYPEOF(tmp) == STRSXP) {
        str = CHAR2ShortStr (CHAR(STRING_ELT(tmp, 0)), fName);
        if (NULL != str) {
          if (strcasecmp(fName,"true") == 0)
            compress = 1;
          else if (strcmp(fName,"false") == 0)
            compress = 0;
          else {
            /* else warning message */
            warning ("To change default behavior of 'compress', please enter it as 'true' or 'false'\n" );
            Rprintf ("You entered it as %s.\n", str);
          }
        }
      } /* TYPEOF=STRSXP */
      else if (TYPEOF(tmp) == LGLSXP) {
        logical = LOGICAL(tmp)[0];
        if (logical == TRUE)
          compress = 1;
        else
          compress = 0;
      }
      else {
        warning ("To change default behavior of 'compress', please enter it as either a string or a logical.\n");
        Rprintf ("You entered it with TYPEOF('compress') = %d.\n", TYPEOF(tmp));
        return 0;
      }
    }
  }
  return compress;
} /* isCompress */

/* loadGDX: load the GDX API, if not already loaded,
 * and raise an exception on failure
 */
void
loadGDX (void)
{
  return;                       /* all done already */
} /* loadGDX */

/* sparseToFull: from input data in sparse form, create output data in full form
 * spVal: input .val matrix in sparse form
 * fullVal: output .val matrix in full form
 * uelLists: .uels for symbol
 * N.B.: R stores matrices column-wise, i.e. left index moving fastest
 */
void
sparseToFull (SEXP spVal, SEXP fullVal, SEXP uelLists,
              int symType, int symSubType, dField_t dField, int nRec, int symDimX)
{
  int k, iRec;
  int fullLen;             /* length of output matrix fullVal */
  int fullCard;            /* cardinality of fully allocated matrix */
  int card[GLOBAL_MAX_INDEX_DIM];
  double defVal;           /* default value - may be nonzero */
  double *p, *pFull, *tFull;
  int index;
  int symDim = symDimX;
  int ii;
  dField_t iField;

  pFull = REAL(fullVal);
  fullLen = length(fullVal);
  p = REAL(spVal);

  switch (symType) {
  case GMS_DT_SET:
    /* step 1: initialize full matrix */
    (void) memset (pFull, 0, fullLen * sizeof(*pFull));
    /* step 2: loop over each row/nonzero of sparse matrix to populate full matrix */
    fullCard = 1;
    for (k = 0;  k < symDim;  k++) {
      card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
      fullCard *= card[k];
    }
    if (fullCard != fullLen)
      error ("sparseToFull: unexpected inputs:  fullCard=%d  fullLen=%d",
             fullCard, fullLen);

    for (iRec = 0;  iRec < nRec;  iRec++) {
      ii = iRec + nRec*(symDim-1);
      for (index = p[ii]-1, k = symDim-2;  k >= 0;  k--) {
        ii -= nRec;
        index = (index * card[k]) + p[ii] - 1;
      }
      pFull[index] = 1;
    } /* end loop over nonzeros */
    break;
  case GMS_DT_PAR:
    /* step 1: initialize full matrix */
    (void) memset (pFull, 0, fullLen * sizeof(*pFull));
    /* step 2: loop over each row/nonzero of sparse matrix to populate full matrix */
    fullCard = 1;
    for (k = 0;  k < symDim;  k++) {
      card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
      fullCard *= card[k];
    }
    if (fullCard != fullLen)
      error ("sparseToFull: unexpected inputs:  fullCard=%d  fullLen=%d",
             fullCard, fullLen);

    for (iRec = 0;  iRec < nRec;  iRec++) {
      ii = iRec + nRec*(symDim-1);
      for (index = p[ii]-1, k = symDim-2;  k >= 0;  k--) {
        ii -= nRec;
        index = (index * card[k]) + p[ii] - 1;
      }
      pFull[index] = p[iRec + nRec*symDim];
    } /* end loop over nonzeros */
    break;
  case GMS_DT_VAR:
    if (all == dField) {
      double defRec[GMS_VAL_MAX];

      symDim--;
      fullCard = 1;
      for (k = 0;  k < symDim;  k++) {
        card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
        fullCard *= card[k];
      }
      if ((fullCard * 5) != fullLen)
        error ("sparseToFull: unexpected inputs:  fullCard*5=%d  fullLen=%d",
               fullCard*5, fullLen);

      /* step 1: initialize full matrix to the defaults */
      getDefRecVar (symSubType, defRec);
      for (tFull = pFull, iField = level;  iField <= scale;  iField++) {
        if (0 == defRec[iField])
          (void) memset (tFull, 0, fullCard * sizeof(*pFull));
        else {
          for (k = 0;  k < fullCard;  k++)
            tFull[k] = defRec[iField];
        }
        tFull += fullCard;
      }
      /* step 2: loop over each record of the variable to plug in non-defaults */
      for (iRec = 0;  iRec < nRec;  iRec++) {
        ii = iRec + nRec*symDim;
        for (index = p[ii]-1, k = symDim-1;  k >= 0;  k--) {
          ii -= nRec;
          index = (index * card[k]) + p[ii] - 1;
        }
        pFull[index] = p[iRec + nRec*symDimX];
      } /* end loop over nonzeros */
    }
    else {                      /* all != dField */
      /* step 1: initialize full matrix */
      defVal = getDefValVar (symSubType, dField);
      if (0 == defVal) {
        (void) memset (pFull, 0, fullLen * sizeof(*pFull));
      }
      else {
        for (k = 0;  k < fullLen;  k++)
          pFull[k] = defVal;
      }
      /* step 2: loop over each row/nonzero of sparse matrix to populate full matrix */
      fullCard = 1;
      for (k = 0;  k < symDim;  k++) {
        card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
        fullCard *= card[k];
      }
      if (fullCard != fullLen)
        error ("sparseToFull: unexpected inputs:  fullCard=%d  fullLen=%d",
               fullCard, fullLen);

      for (iRec = 0;  iRec < nRec;  iRec++) {
        ii = iRec + nRec*(symDim-1);
        for (index = p[ii]-1, k = symDim-2;  k >= 0;  k--) {
          ii -= nRec;
          index = (index * card[k]) + p[ii] - 1;
        }
        pFull[index] = p[iRec + nRec*symDim];
      } /* end loop over nonzeros */
    } /* if all == dField .. else .. */
    break;
  case GMS_DT_EQU:
    if (all == dField) {
      double defRec[GMS_VAL_MAX];

      symDim--;
      fullCard = 1;
      for (k = 0;  k < symDim;  k++) {
        card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
        fullCard *= card[k];
      }
      if ((fullCard * 5) != fullLen)
        error ("sparseToFull: unexpected inputs:  fullCard*5=%d  fullLen=%d",
               fullCard*5, fullLen);

      /* step 1: initialize full matrix to the defaults */
      getDefRecEqu (symSubType, defRec);
      for (tFull = pFull, iField = level;  iField <= scale;  iField++) {
        if (0 == defRec[iField])
          (void) memset (tFull, 0, fullCard * sizeof(*pFull));
        else {
          for (k = 0;  k < fullCard;  k++)
            tFull[k] = defRec[iField];
        }
        tFull += fullCard;
      }
      /* step 2: loop over each record of the equation to plug in non-defaults */
      for (iRec = 0;  iRec < nRec;  iRec++) {
        ii = iRec + nRec*symDim;
        for (index = p[ii]-1, k = symDim-1;  k >= 0;  k--) {
          ii -= nRec;
          index = (index * card[k]) + p[ii] - 1;
        }
        pFull[index] = p[iRec + nRec*symDimX];
      } /* end loop over nonzeros */
      /* error  ("not yet implemented YY"); */
    }
    else {                      /* all != dField */
      /* step 1: initialize full matrix */
      defVal = getDefValEqu (symSubType, dField);
      if (0 == defVal) {
        (void) memset (pFull, 0, fullLen * sizeof(*pFull));
      }
      else {
        for (k = 0;  k < fullLen;  k++)
          pFull[k] = defVal;
      }
      /* step 2: loop over each row/nonzero of sparse matrix to populate full matrix */
      fullCard = 1;
      for (k = 0;  k < symDim;  k++) {
        card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
        fullCard *= card[k];
      }
      if (fullCard != fullLen)
        error ("sparseToFull: unexpected inputs:  fullCard=%d  fullLen=%d",
               fullCard, fullLen);

      for (iRec = 0;  iRec < nRec;  iRec++) {
        ii = iRec + nRec*(symDim-1);
        for (index = p[ii]-1, k = symDim-2;  k >= 0;  k--) {
          ii -= nRec;
          index = (index * card[k]) + p[ii] - 1;
        }
        pFull[index] = p[iRec + nRec*symDim];
      } /* end loop over nonzeros */
    } /* if all == dField .. else .. */
    break;
  default:
    error("Unrecognized type of symbol found.");
  } /* end switch */

  return;
} /* sparseToFull */

/* getDefRecEqu: return the default record for an equation of type subType
 * Treat unrecognized subType like GMS_EQUTYPE_N, that seems to be the default
 */
void
getDefRecEqu (int subType, double defRec[])
{
  (void) memset (defRec, 0, GMS_VAL_MAX * sizeof(double));
  defRec[scale] = 1;
  switch (subType) {
  case GMS_EQUTYPE_E:
  case GMS_EQUTYPE_X:
    break;
  case GMS_EQUTYPE_G:
    defRec[upper] = R_PosInf;
    break;
  case GMS_EQUTYPE_L:
    defRec[lower] = R_NegInf;
    break;
  case GMS_EQUTYPE_C:
    defRec[upper] = R_PosInf;
    break;
  case GMS_EQUTYPE_N:
  default:
    defRec[upper] = R_PosInf;
    defRec[lower] = R_NegInf;
  } /* switch subType */
  return;
} /* getDefRecEqu */

/* getDefRecVar: return the default record for a variable of type subType */
void
getDefRecVar (int subType, double defRec[])
{
  (void) memset (defRec, 0, GMS_VAL_MAX * sizeof(double));
  defRec[scale] = 1;
  switch (subType) {
  case GMS_VARTYPE_BINARY:
    defRec[upper] = 1;
    break;
  case GMS_VARTYPE_INTEGER:
    defRec[upper] = 100;
    break;
  case GMS_VARTYPE_POSITIVE:
  case GMS_VARTYPE_SOS1:
  case GMS_VARTYPE_SOS2:
    defRec[upper] = R_PosInf;
    break;
  case GMS_VARTYPE_NEGATIVE:
    defRec[lower] = R_NegInf;
    break;
  case GMS_VARTYPE_FREE:
    defRec[lower] = R_NegInf;
    defRec[upper] = R_PosInf;
    break;
  case GMS_VARTYPE_SEMICONT:
    defRec[lower] = 1;
    defRec[upper] = R_PosInf;
    break;
  case GMS_VARTYPE_SEMIINT:
    defRec[lower] = 1;
    defRec[upper] = 100;
    break;
  } /* switch subType */
  return;
} /* getDefRecVar */

/* getDefValEqu: return the default value for field dField of an equation
 * of type subType */
double
getDefValEqu (int subType, dField_t dField)
{
  if (all == dField)
    error ("dField = all passed to getDefValEqu: internal error");

  if (scale == dField)
    return 1;

  switch (subType) {
  case GMS_EQUTYPE_E:
  case GMS_EQUTYPE_X:
    /* all 0 */
    break;
  case GMS_EQUTYPE_G:
  case GMS_EQUTYPE_C:
    if (upper == dField)
      return R_PosInf;
    break;
  case GMS_EQUTYPE_L:
    if (lower == dField)
      return R_NegInf;
    break;
  case GMS_EQUTYPE_N:
    if (upper == dField)
      return R_PosInf;
    if (lower == dField)
      return R_NegInf;
    break;
  } /* switch subType */
  return 0;
} /* getDefValEqu */

/* getDefValVar: return the default value for field dField of a variable
 * of type subType */
double
getDefValVar (int subType, dField_t dField)
{
  if (all == dField)
    error ("dField = all passed to getDefValVar: internal error");

  if (scale == dField)
    return 1;

  switch (subType) {
  case GMS_VARTYPE_BINARY:
    if (upper == dField)
      return 1;
    break;
  case GMS_VARTYPE_INTEGER:
    if (upper == dField)
      return 100;
    break;
  case GMS_VARTYPE_POSITIVE:
  case GMS_VARTYPE_SOS1:
  case GMS_VARTYPE_SOS2:
    if (upper == dField)
      return R_PosInf;
    break;
  case GMS_VARTYPE_NEGATIVE:
    if (lower == dField)
      return R_NegInf;
    break;
  case GMS_VARTYPE_FREE:
    if (lower == dField)
      return R_NegInf;
    if (upper == dField)
      return R_PosInf;
    break;
  case GMS_VARTYPE_SEMICONT:
    if (lower == dField)
      return 1;
    if (upper == dField)
      return R_PosInf;
    break;
  case GMS_VARTYPE_SEMIINT:
    if (lower == dField)
      return 1;
    if (upper == dField)
      return 100;
    break;
  } /* switch subType */
  return 0;
} /* getDefValVar */

/* getDefVal: return the default value consistent with the given
 *   symType (e.g. GMS_DT_VAR),
 *   subType (e.g. GMS_VARTYPE_BINARY), and
 *   dField  (e.g. lower)
 */
double
getDefVal (int symType, int subType, dField_t dField)
{
  double defVal = 0;

  switch (symType) {
  case GMS_DT_SET:    /* just choose something not to match the val */
    defVal = -1;
    break;
  case GMS_DT_VAR:
    defVal = getDefValVar (subType, dField);
    break;
  case GMS_DT_EQU:
    defVal = getDefValEqu (subType, dField);
    break;
  } /* end switch */
  return defVal;
} /* getDefVal */

