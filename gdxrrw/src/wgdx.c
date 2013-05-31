/* wgdx.c
 * code for gdxrrw::wgdx
 * $Id$
 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "gdxcc.h"
#include "gclgms.h"
#include "globals.h"

static shortStringBuf_t lastErrMsg;

/* N.B. this is the union of valid names, not all combinations are allowed */
static const char *validSymListNames[] = {
  "name"
  ,"type"
  ,"val"
  ,"uels"
  ,"form"
  ,"dim"
  ,"ts"
  ,"domains"
  ,"field"
  ,"varTypeText"
  ,"typeCode"                   /* should this be subType or subTypeCode instead? */
};
#define N_VALIDSYMLISTNAMES (sizeof(validSymListNames)/sizeof(*validSymListNames))
static char validFieldMsg[256] = "";

/* assumes gdxHandle is valid */
static void
getGDXMsg (void)
{
  int lastErr;

  lastErr = gdxGetLastError (gdxHandle);
  (void) gdxErrorStr (NULL, lastErr, lastErrMsg);
  return;
}

static void
msgInit (void) {
  int i, k, n;

  k = sprintf (validFieldMsg, "valid symbol list fields are ");
  n = k;
  for (i = 0;  i < N_VALIDSYMLISTNAMES-1;  i++) {
    k = sprintf (validFieldMsg + n, "'%s', ", validSymListNames[i]);
    n += k;
  }
  k = sprintf (validFieldMsg + n, "'%s'", validSymListNames[i]);
} /* msgInit */

static char *
typeofTxt (SEXP e, char buf[])
{
  int d;

  d = TYPEOF(e);
  switch (d) {
  case NILSXP:
    strcpy (buf, "NULL");
    break;
  case SYMSXP:
    strcpy (buf, "symbol");
    break;
  case LGLSXP:
    strcpy (buf, "logical");
    break;
  case INTSXP:
    strcpy (buf, "integer");
    break;
  case REALSXP:
    strcpy (buf, "real");
    break;
  case CPLXSXP:
    strcpy (buf, "complex");
    break;
  case STRSXP:
    strcpy (buf, "string");
    break;
  case VECSXP:
    strcpy (buf, "generic vector");
    break;
  default:
    sprintf (buf, "unknown (TYPEOF=%d)", d);
  }
  return buf;
} /* typeofTxt */

/* checkSymType2: make sure dType is one we've implemented 
 * the calling code block for */
static void
checkSymType2 (dType_t dType)
{
  switch (dType) {
  case set:
  case parameter:
    break;
  case variable:
    error ("wgdx: not yet implemented for symbol type 'variable'");
    break;
  default:
    error ("wgdx: not yet implemented for symbol type <unknown>");
  } /* end switch */
} /* checkSymType2 */

/* checkSymType3: make sure dType is one we've implemented 
 * the calling code block for */
static void
checkSymType3 (dType_t dType)
{
  switch (dType) {
  case set:
  case parameter:
  case variable:
    break;
  default:
    error ("wgdx: not yet implemented for symbol type <unknown>");
  } /* end switch */
} /* checkSymType3 */

/* checkForValidData: check the validity of input data with input uels and dims */
static void
checkForValidData(SEXP val, SEXP uelOut, dType_t dType, dForm_t dForm)
{
  SEXP dims;
  int i, j, k;
  double *pd;
  int *pi;
  double dt;
  int nDimsData, nDimsUels;
  int ncols, nrows;
  int mx;

  nDimsUels = length(uelOut);

  pd = NULL;
  pi = NULL;
  if (TYPEOF(val) == REALSXP) {
    pd = REAL(val);
  }
  else if (TYPEOF(val) == INTSXP) {
    pi = INTEGER(val);
  }
  else {
    error (".val must be numeric.");
  }
  dims = getAttrib(val, R_DimSymbol);

  if (dForm == sparse) {
    nrows = INTEGER(dims)[0];
    ncols = INTEGER(dims)[1];

    switch (dType) {
    case set:
      break;
    case parameter:
      ncols--;    /* skip last column containing parameter values */
      break;
    case variable:
      ncols--;    /* skip last column containing parameter values */
      break;
    default:
      error ("vals input not expected/implemented this symbol type.");
    } /* switch symbol type */

    if (nDimsUels != ncols) {
      error ("Number of columns in sparse data does not match with UEL dimension.");
    }
    /* the matrix of vals is stored column-major */
    for (j = 0;  j < ncols;  j++) {
      for (mx = 0, i = 0;  i < nrows;  i++) {
        if (pd) {
          dt = pd[i + j*nrows];
          if (dt < 1) {
            error ("Non-positive coordinates are not allowed in index columns of sparse data");
          }
          if (dt > INT_MAX) {
            error ("Coordinates > INT_MAX are not allowed in index columns of sparse data");
          }
          k = (int) dt;
          if (dt != k) {
            error ("Non-integer coordinates are not allowed in index columns of sparse data");
          }
        }
        else {
          k = pi[i + j*nrows];
          if (k < 1) {
            error ("Non-positive coordinates are not allowed in index columns of sparse data");
          }
        }
        /* if (!mxIsFinite(P[j + i*nrows])) {
           error("Only finite numbers are allowed in index columns of sparse data");
           } */
        if (k > mx) {
          mx = k;
        }
      }
      if (mx > (int)length(VECTOR_ELT(uelOut, j))) {
        error ("Row index in sparse matrix exceeds number of elements in UEL for that column.");
      }
    } /* end loop over cols */
  }   /* end sparse */
  else {
    /* get dimension of full matrix == number of columns in uels */
    nDimsData = length(dims);
    if (nDimsUels != nDimsData) {
      error ("Number of dimension in full data does not match number of dimensions in uels.");
    }
    /* number of elements in each dimension == number of elements in UEL
       for (i = 0;  i < nDimsUels;  i++) {
       if ( !( INTEGER(dims)[i] == 1 &&  (int)mxGetN(mxGetCell(uelOut, i )) == 0)
       && INTEGER(dims)[i] > mxGetN(mxGetCell(uelOut, i ))) {
       error("Number of element in full format data exceeds corresponding elements in UEL.");
       }
     } */
  }
} /* checkForValidData */

static void
createUelOut(SEXP val, SEXP uelOut, dType_t dType, dForm_t dForm)
{
  SEXP dims, bufferUel;
  int i, j, k;
  double *P;
  int *intVal;
  char buffer [256];
  int ncols, nrows, ndims;
  int mx;

  dims = getAttrib(val, R_DimSymbol);
  if (dForm == sparse) {
    nrows = INTEGER(dims)[0];
    ncols = INTEGER(dims)[1];
    checkSymType2 (dType);
    if (dType == parameter) {
      ncols--;
    }

    if (TYPEOF(val) == REALSXP) {
      P = REAL(val);
      for (i = 0; i < ncols; i++) {
        mx = 0;
        for (j = 0; j < nrows; j++) {
          if (P[j + i*nrows] > mx) {
            mx = (int) P[j + i*nrows];
          }
        }
        PROTECT(bufferUel = allocVector(STRSXP, mx));
        for (k = 1; k <= mx; k++) {
          sprintf(buffer, "%d", k);
          SET_STRING_ELT(bufferUel, k-1, mkChar(buffer));
        }
        SET_VECTOR_ELT(uelOut, i, bufferUel);
        UNPROTECT(1);
      }
    }
    else if (TYPEOF(val) == INTSXP) {
      intVal = INTEGER(val);

      for (i = 0; i < ncols; i++) {
        mx = 0;
        for (j = 0; j < nrows; j++) {
          if (intVal[j + i*nrows] > mx) {
            mx = intVal[j + i*nrows];
          }
        }
        PROTECT(bufferUel = allocVector(STRSXP, mx));
        for (k = 1; k <= mx; k++) {
          sprintf(buffer, "%d", k);
          SET_STRING_ELT(bufferUel, k-1, mkChar(buffer));
        }
        SET_VECTOR_ELT(uelOut, i, bufferUel);
        UNPROTECT(1);
      }
    }
  } /* if sparse */
  else {
    /*
     * Create default uel.
     * Here there is no need to calculate max as number
     * of elements in each dimension is all what I need.
     */
    ndims = length(uelOut);
    for (i = 0; i < ndims; i++) {
      PROTECT(bufferUel = allocVector(STRSXP, INTEGER(dims)[i]));

      for (k = 1; k <= INTEGER(dims)[i]; k++) {
        sprintf(buffer, "%d", k);
        SET_STRING_ELT(bufferUel, k-1, mkChar(buffer));
      }
      SET_VECTOR_ELT(uelOut, i, bufferUel);
      UNPROTECT(1);
    }
  } /* if sparse .. else .. */
} /* createUelOut */

/* registerInputUEL: take the strings in uelOut and register them,
 * in the process storing their GDX indices in uelIndex[kk]
 * uelOut is a vector of string vectors,
 * one string vector for each symbol dimension
 */
static void
registerInputUEL(SEXP uelOut, int kk, SEXP uelIndex, int *protCount)
{
  int i, k, rc, gi;
  const char *uelString;
  int uelDim;               /* should be same as the GDX symbol dim */
  int vecLen;               /* length of sVec */
  SEXP sVec;                /* vector of UEL labels: input */
  SEXP iVec;                /* GDX indices for sVec labels */
  SEXP iVecVec;             /* one iVec for each index position */

  uelDim = length(uelOut);
  PROTECT( iVecVec = allocVector(VECSXP, uelDim));
  ++*protCount;
  /* Rprintf ("DEBUG registerInputUEL: uelDim=%d\n", uelDim); */

  for (i = 0;  i < uelDim;  i++) {
    sVec = VECTOR_ELT(uelOut, i); /* UELs for i'th index position */
    vecLen = length(sVec);
    /* Rprintf ("DEBUG registerInputUEL: i=%d  vecLen=%d\n", i, vecLen); */

    PROTECT(iVec = allocVector(INTSXP, vecLen));

    for (k = 0; k < vecLen; k++) {
      /* get string and register to gdx */
      uelString = CHAR(STRING_ELT(sVec, k));
      /* Rprintf("str at %d is %s\n", k, uelString); */

      rc = gdxUELRegisterStr (gdxHandle, uelString, &gi);
      if (rc != 1) {
        error ("could not register: %s", uelString);
      }
      /* Rprintf("  input: %s  output from gdx: %d\n", uelString, gi); */
      INTEGER(iVec)[k] = gi;
    }

    SET_VECTOR_ELT(iVecVec, i, iVec);
    UNPROTECT(1);
  }
  /* store UEL indices for k'th symbol */
  SET_VECTOR_ELT(uelIndex, kk, iVecVec);
} /* registerInputUEL */

/* checkWrSymList: checks if a is potentially a valid symList
 * return:
 *   0 if OK
 *   1 if no names are found
 *   2 if an unamed field is found
 *   3 if an invalid field is found
 *   4 if symbol is not a list
 *   others possible but not yet used
 */
static int
checkWrSymList (SEXP a, shortStringBuf_t msg)
{
  int i, k, n;
  int found;
  SEXP names;
  const char *fieldName;
  char buf[512];

  if (TYPEOF(a) != VECSXP) {
    (void) CHAR2ShortStr ("symbol is not a list", msg);
    return 4;
  }
  *msg = '\0';
  n = length(a);
  names = getAttrib(a, R_NamesSymbol);
  if (R_NilValue == names) {
    (void) CHAR2ShortStr ("symbol has no names", msg);
    return 1;
  }
  for (i = 0;  i < n;  i++) {
    fieldName = CHAR(STRING_ELT(names, i));
    if ('\0' == *fieldName) {
      (void) CHAR2ShortStr ("found field with empty name", msg);
      return 2;
    }
    found = 0;
    for (k = 0;  k < N_VALIDSYMLISTNAMES;  k++) {
      /* Rprintf ("Checking against possible field name %s\n", validSymListNames[k]); */
      if (0 == strcmp(validSymListNames[k], fieldName)) {
        found = 1;
        break;
      }
    }
    if (found)
      continue;
    sprintf (buf, "invalid field name '%.20s' encountered, %s", fieldName, validFieldMsg);
    (void) CHAR2ShortStr (buf, msg);
    /* Rprintf ("Error: found symList entry with name=%s\n", fieldName); */
    return 3;
  }
  return 0;
} /* checkWrSymList */

static void
processWrListList (SEXP a, int argNum, SEXP *symList, int symListSiz, int *symListLen)
{
  int n, k;
  int rc;
  SEXP aaa;
  shortStringBuf_t msg;

  n = length(a);
  for (k = 0;  k < n;  k++) {
    /* Rprintf ("processWrListList: arg %d element %d\n", argNum, k+1); */
    aaa = VECTOR_ELT(a, k);
    rc = checkWrSymList (aaa, msg);
    if (0 == rc) {
      /* Rprintf ("processWrListList: argument %d element %d is a symList\n", argNum, k+1); */
      if (*symListLen >= symListSiz)
        error ("processWrListList: internal error processing symbol list");
      symList[*symListLen] = aaa;
      ++*symListLen;
    }
    else {
      error ("processWrListList: argument %d element %d is not a valid symbol List: %s", argNum, k+1, msg);
    }
  }
  return;
} /* processWrListList */


static void
processWrArg (SEXP a, int argNum, SEXP *symList, int symListSiz, int *symListLen)
{
  SEXP lstName;
  int rc;
  shortStringBuf_t msg;

  if (VECSXP != TYPEOF(a)) {
    error ("error in argument %d: valid symbol list expected, non-list found", argNum);
  }
  lstName = getAttrib (a, R_NamesSymbol);
  if (R_NilValue == lstName) {
    /* Rprintf ("processWrArg: found potential list of lists\n"); */
    processWrListList (a, argNum, symList, symListSiz, symListLen);
  }
  else {
    /* Rprintf ("processWrArg: found potential symbol list\n"); */
    rc = checkWrSymList (a, msg);
    if (0 == rc) {
      /* Rprintf ("processWrArg: argument %d is a symList\n", argNum); */
      if (*symListLen >= symListSiz)
        error ("processWrArg: internal error processing symbol list");
      symList[*symListLen] = a;
      ++*symListLen;
    }
    else {
      error ("processWrArg: argument %d has names but is not a valid symbol list: %s", argNum, msg);
    }
  }
  return;
} /* processWrArg */

char *
validWriteListMsg (char buf[], int bufSiz)
{
  int i, n, nFree;
  char *s;

  s = buf;
  nFree = bufSiz;
  n = snprintf (s, nFree, "Valid input list elements: '%s'", validSymListNames[0]);
  s += n;  nFree -= n;  if (nFree <=0) return buf;
  for (i = 1;  i < N_VALIDSYMLISTNAMES;  i++) {
    n = snprintf (s, nFree, ", '%s'", validSymListNames[i]);
    s += n;  nFree -= n;  if (nFree <=0) return buf;
  }
  (void) snprintf (s, nFree, ".");
  return buf;
} /* validWriteListMsg */

/* readWgdxList: read lst (aka the input request list or write specifier),
 * validate it, and store the result in *wSpecPtr.
 * Also sets certain global variables, and constructs the universe of UELs
 */
static void
readWgdxList (SEXP lst, int iSym, SEXP uelIndex,
              wSpec_t **wSpecPtr, int *protCount)
{
  SEXP lstNames, tmpUel;
  SEXP dimension;
  SEXP nameExp = NULL;
  SEXP typeExp = NULL;
  SEXP valExp = NULL;
  SEXP uelsExp = NULL;
  SEXP formExp = NULL;
  SEXP dimExp = NULL;
  SEXP tsExp = NULL;
  SEXP domExp = NULL;
  SEXP fieldExp = NULL;
  int i, j;
  int nElements;                /* number of elements in lst */
  int dimUels;
  int symDim = 0; /* consistent with GDX or GAMS idea of symbol dim */
  int sz, withDim;
  char buf[512];
  const char *tmpName;
  const char *eltName;        /* list element name */
  SEXP uelOut, bufferUel;     /* allocating temporary storage place */
  wSpec_t *wSpec;

  withDim = 0;
  uelOut = R_NilValue;

  wSpec = (wSpec_t *) malloc(sizeof(*wSpec));
  *wSpecPtr = wSpec;
  memset (wSpec, 0, sizeof(*wSpec));
  wSpec->dForm = sparse;
  wSpec->dType = set;

  nElements = length(lst);
  /* check maximum number of elements */
  if (nElements < 1 || nElements > 10) {
    error("Incorrect number of elements in input list argument.");
  }

  lstNames = getAttrib(lst, R_NamesSymbol);
  if (lstNames == R_NilValue) {
    error ("Input symbol list has no element names, %s", validFieldMsg);
  }

  /* first, check that all names are recognized, reject o/w
   * in the process, store the symbol for direct access later
   */
  for (i = 0;  i < nElements;  i++) {
    eltName = CHAR(STRING_ELT(lstNames, i));
    if (0 == strcmp("name", eltName)) {
      nameExp = VECTOR_ELT(lst, i);
    }
    else if (0 == strcmp("type", eltName)) {
      typeExp = VECTOR_ELT(lst, i);
    }
    else if (0 == strcmp("val", eltName)) {
      valExp = VECTOR_ELT(lst, i);
    }
    else if (0 == strcmp("uels", eltName)) {
      uelsExp = VECTOR_ELT(lst, i);
    }
    else if (0 == strcmp("form", eltName)) {
      formExp = VECTOR_ELT(lst, i);
    }
    else if (0 == strcmp("dim", eltName)) {
      dimExp = VECTOR_ELT(lst, i);
    }
    else if (0 == strcmp("ts", eltName)) {
      tsExp = VECTOR_ELT(lst, i);
    }
    else if (0 == strcmp("domains", eltName)) {
      domExp = VECTOR_ELT(lst, i);
    }
    else if (0 == strcmp("field", eltName)) {
      fieldExp = VECTOR_ELT(lst, i);
    }
    else {
      error ("Invalid input list element '%s' specified.  %s",
             eltName, validWriteListMsg(buf,sizeof(buf)));
    }
  }

  /* now process the fields provided */
  if (NULL == nameExp)
    error ("Required list element 'name' is missing. Please try again.");
  if (STRSXP != TYPEOF(nameExp)) {
    error ("Input list element 'name' must be a string - found %s instead.",
           typeofTxt(nameExp, buf));
  }
  tmpName = CHAR(STRING_ELT(nameExp, 0));
  checkStringLength (tmpName);
  strcpy (wSpec->name, tmpName);

  if (tsExp) {
    if (STRSXP != TYPEOF(tsExp)) {
      error ("Input list element 'ts' must be a string - found %s instead.",
             typeofTxt(tsExp, buf));
    }
    checkStringLength (CHAR(STRING_ELT(tsExp, 0)));
    wSpec->withTs = 1;
  }

  if (formExp) {
    if (STRSXP != TYPEOF(formExp)) {
      error ("Input list element 'form' must be a string - found %s instead.",
             typeofTxt(formExp, buf));
    }
    tmpName = CHAR(STRING_ELT(formExp, 0));
    if (strcasecmp("full", tmpName) == 0) {
      wSpec->dForm = full;
    }
    else if (strcasecmp("sparse", tmpName) == 0) {
      wSpec->dForm = sparse;
    }
    else {
      error("Input list element 'form' must be either 'full' or 'sparse'.");
    }
  } /* formExp */

  if (typeExp) {                /* optional */
    if (STRSXP != TYPEOF(typeExp)) {
      error ("Input list element 'type' must be a string - found %s instead.",
             typeofTxt(typeExp, buf));
    }
    tmpName = CHAR(STRING_ELT(typeExp, 0));
    if (0 == strcasecmp("set", tmpName) ) {
      wSpec->dType = set;
    }
    else if (0 == strcasecmp("parameter", tmpName) ) {
      wSpec->dType = parameter;
    }
    else if (0 == strcasecmp("variable", tmpName) ) {
      wSpec->dType = variable;
    }
    else {
      error ("Input list element 'type' must be either 'set', 'parameter' or 'variable'.");
    }
  }

  if (dimExp) {                 /* optional */
    if (INTSXP == TYPEOF(dimExp)) {
      if (length(dimExp) != 1) {
        error ("Optional input list element 'dim' must have only one element.");
      }
      if (INTEGER(dimExp)[0] < 0) {
        error("Negative value is not allowed as valid input for 'dim'.");
      }
      withDim = 1;
      wSpec->dim = INTEGER(dimExp)[0];
    }
    else if (REALSXP == TYPEOF(dimExp)) {
      if (length(dimExp) != 1) {
        error ("Optional input list element 'dim' must have only one element.");
      }
      if (REAL(dimExp)[0] < 0) {
        error("Negative value is not allowed as valid input for 'dim'.");
      }
      withDim = 1;
      wSpec->dim = (int) REAL(dimExp)[0];
      if (REAL(dimExp)[0] != wSpec->dim) {
        error("Non-integer value is not allowed as valid input for 'dim'.");
      }
    }
    else {
      error ("Optional input list element 'dim' must be numeric - found %s instead.",
             typeofTxt(dimExp, buf));
    }
  } /* dimExp */

  dimUels = -1;
  if (uelsExp) {                /* optional */
    if (VECSXP != TYPEOF(uelsExp)) {
      error ("Input list element 'uels' must be an unnamed list - found %s instead.",
             typeofTxt(uelsExp, buf));
    }
    dimUels = length(uelsExp);
    if (withDim) {
      switch (wSpec->dType) {
      case set:
      case parameter:
        if (wSpec->dim != dimUels)
          error ("Inconsistent dimension found: 'dim'=%d  doesn't match 'uels' dimension=%d.",
                 wSpec->dim, dimUels);
        break;
      case variable:
        if (wSpec->dim != (dimUels-1))
          error ("Inconsistent dimension found: 'dim'=%d  doesn't match implied 'uels' dimension=%d.",
                 wSpec->dim, dimUels-1);
        break;
      default:
        error ("uels input not expected/implemented for type=%s.", CHAR(STRING_ELT(typeExp, 0)));
      } /* switch symbol type */
    }
#if 0
    else if (0 == dimUels) {
      error ("Empty input list element 'uels' is not allowed without 'dim'=0.");
    }
#endif
    PROTECT(uelOut = allocVector(VECSXP, dimUels));
    ++*protCount;
    for (j = 0;  j < dimUels;  j++) {
      tmpUel = VECTOR_ELT(uelsExp, j);
      if (tmpUel == R_NilValue) {
        error ("Empty input field in list element 'uels' not allowed");
      }
      if (TYPEOF(tmpUel) == STRSXP) {
        /*  checkStringLength( CHAR(STRING_ELT(tmp, 0)) ); */
        SET_VECTOR_ELT (uelOut, j, tmpUel);
      }
      else if (TYPEOF(tmpUel) == REALSXP || TYPEOF(tmpUel) == INTSXP) {
        /* Convert to output */
        bufferUel = allocVector(STRSXP, length(tmpUel));
        makeStrVec (bufferUel, tmpUel);
        SET_VECTOR_ELT (uelOut, j, bufferUel);
      }
      else {
        error ("Input uels must be either string vectors or numeric vectors.");
      }
    }
    wSpec->withUel = 1;
  } /* uelsExp */

  if (NULL == valExp) {         /* .val field missing */
    if (parameter == wSpec->dType) {
      error ("Missing 'val' is a required list element for parameters.");
    }
    if (set == wSpec->dType && 0 == wSpec->withUel) {
      error ("Missing 'val' is a required list element for sets with no UELs.");
    }
  }
  else {
    dimension = getAttrib(valExp, R_DimSymbol);
    if (TYPEOF(valExp) == REALSXP || TYPEOF(valExp) == INTSXP ) {
      if (wSpec->dForm == sparse) {
        if (length(dimension) != 2) {
          error ("Only 2-dimensional 'val' is allowed as valid input in sparse format: found %d-dim val.",
                 length(dimension));
        }
        /* getting data matrix */
        sz = INTEGER(dimension)[0];
        if (sz > INT_MAX) {
          error ("Input list element 'val' exceeds row limit of %d",
                 INT_MAX);
        }
        sz = INTEGER(dimension)[1];
        if (sz > INT_MAX) {
          error ("Input list element 'val' exceeds column limit of %d",
                 INT_MAX);
        }
        symDim = sz;
        switch (wSpec->dType) {
        case set:
          break;                /* no adjustment */
        case parameter:
          symDim--;
          break;
        case variable:
          symDim -= 2;
          if (symDim < 0)
            error ("val input must have at least 2 cols when writing variables in sparse form.");
          break;
        default:
          error ("val input not expected/implemented for type=%s.", CHAR(STRING_ELT(typeExp, 0)));
        } /* switch symbol type */
        if (symDim > GMS_MAX_INDEX_DIM) {
          error ("Input list element 'val' implies symbol dimension exceeding GDX dimension limit of %d.",
                 GMS_MAX_INDEX_DIM);
        }
        if (withDim) {
          if (wSpec->dim != symDim) {
            error ("Inconsistent dimensions found: 'dim'=%d doesn't match"
                   " dimension=%d implied by 'val'", wSpec->dim, symDim);
          }
        }
        else if (dimUels > 0) {
          int ddd = dimUels;

          switch (wSpec->dType) {
          case set:
          case parameter:
            break;
          case variable:
            ddd--;                /* account for _field labels */
            break;
          default:
            error ("val input not expected/implemented for type=%s.", CHAR(STRING_ELT(typeExp, 0)));
          } /* switch symbol type */
          if (ddd != symDim) {
            error ("Inconsistent dimensions implied by 'uels' (%d) and"
                   " 'val' (%d)", ddd, symDim);
          }
        }
        wSpec->withVal = 1;
      } /* if sparse */
      else {
        /* This is for Full/Dense data */
        symDim = length(dimension);
        switch (wSpec->dType) {
        case set:
        case parameter:
          break;                /* no adjustment */
        case variable:
          symDim--;             /* account for _field dimension */
          break;
        default:
          error ("val input not expected/implemented for type=%s.", CHAR(STRING_ELT(typeExp, 0)));
        } /* switch symbol type */
        if (symDim > GMS_MAX_INDEX_DIM) {
          error ("Input list element 'val' implies symbol dimension exceeding GDX dimension limit of %d.",
                 GMS_MAX_INDEX_DIM);
        }
        if (withDim) {
          if (wSpec->dim != symDim) {
            error ("Inconsistent dimensions found: 'dim'=%d doesn't match"
                   " dimension=%d implied by 'val'", wSpec->dim, symDim);
          }
        }
        else if (dimUels > 0) {
          int ddd = dimUels;
          checkSymType3 (wSpec->dType);
          switch (wSpec->dType) {
          case set:
          case parameter:
            break;
          case variable:
            ddd--;                /* account for _field labels */
            break;
          default:
            error ("val input not expected/implemented for type=%s.", CHAR(STRING_ELT(typeExp, 0)));
          } /* switch symbol type */
          if (ddd != symDim) {
            error ("Inconsistent dimensions implied by 'uels' (%d) and"
                   " 'val' (%d)", ddd, symDim);
          }
        }
        wSpec->withVal = 1;
      }
    }
    else {
      error ("Input list element 'val' must be a numeric matrix - found %s instead.",
             typeofTxt(valExp, buf));
    }
  } /* valExp not NULL */

  if (domExp) {
    if (STRSXP != TYPEOF(domExp)) {
      error ("Input list element 'domains' must be a string vector - found %s instead.",
             typeofTxt(domExp, buf));
    }
  }


  if (wSpec->withUel == 0 && wSpec->withVal == 1) {
    switch (wSpec->dType) {
    case set:
    case parameter:
      break;                    /* no problem */
    default:
      error ("Empty UEL list not implemented for type=%s.", CHAR(STRING_ELT(typeExp, 0)));
    } /* switch symbol type */
    PROTECT(uelOut = allocVector(VECSXP, symDim));
    ++*protCount;
    createUelOut (valExp, uelOut, wSpec->dType, wSpec->dForm);
  }

  if (wSpec->withVal == 1) {
    checkForValidData (valExp, uelOut, wSpec->dType, wSpec->dForm);
  }
  registerInputUEL (uelOut, iSym, uelIndex, protCount);
} /* readWgdxList */


static void
unpackWgdxArgs (SEXP *args, int argLen, SEXP **symList,
                int *symListSiz, int *symListLen, char *zeroSqueeze)
{
  int i, stopper;
  const char *argName, *s;
  SEXP t;
  SEXP a;

#if 0
  error ("Hey what happens with Rprintf and error: arglen = %d\n"
         "And does it work to have a multi-line error message?\n"
         "That would be nice if it did.\n", argLen);
#endif
  *zeroSqueeze = 'y';           /* default is yes */
  *symListLen = *symListSiz = 0;
  for (a = *args, i = 2, stopper = argLen ;  i < argLen;  i++) {
    a = CDR(a);
    t = CAR(a);
#if 0
    Rprintf ("DEBUG: args = %p   len: %d\n", t, length(t));
#endif
    if (isNull(TAG(a))) {
      /* no name for this argument, assume it is a list, checked later */
      *symListSiz += length(t);
    }
    else {
      argName = CHAR(PRINTNAME(TAG(a)));
      if (0 != strcmp("squeeze",argName)) {
        error ("usage: wgdx: unrecognized argument name '%s'", argName);
      }
      if (i != argLen-1) {
        error ("usage: wgdx: argument '%s' must follow symbol lists", argName);
      }
      switch (TYPEOF(t)) {
      case LGLSXP:
        *zeroSqueeze = LOGICAL(t)[0] ? 'y' : 'n';
        break;
      case INTSXP:
        *zeroSqueeze = INTEGER(t)[0] ? 'y' : 'n';
        break;
      case REALSXP:
        if (0.0 == REAL(t)[0])
          *zeroSqueeze = 'n';
        break;
      case STRSXP:
        s = CHAR(STRING_ELT(t, 0));
        if ('\0' == s[0])
          error ("usage: wgdx: argument '%s=<empty_string>' is invalid", argName);
        if ('\0' == s[1]) {
          /* single character */
          switch (s[0]) {
          case 'T':
          case 't':
          case 'Y':
          case 'y':
          case '1':
            *zeroSqueeze = 'y';
            break;
          case 'F':
          case 'f':
          case 'N':
          case 'n':
          case '0':
            *zeroSqueeze = 'n';
            break;
          case 'E':
          case 'e':
            *zeroSqueeze = 'e';
            break;
          default:
            error ("usage: wgdx: argument '%s=%s' is invalid", argName, s);
          }
        }
        else {
          /* handle multi-char string */
          if ((0 == strcmp("TRUE",s)) ||
              (0 == strcmp("True",s)) ||
              (0 == strcmp("true",s)) ||
              (0 == strcmp("YES",s)) ||
              (0 == strcmp("Yes",s)) ||
              (0 == strcmp("yes",s)) )
            *zeroSqueeze = 'y';
          else if ((0 == strcmp("FALSE",s)) ||
                   (0 == strcmp("False",s)) ||
                   (0 == strcmp("false",s)) ||
                   (0 == strcmp("NO",s)) ||
                   (0 == strcmp("No",s)) ||
                   (0 == strcmp("no",s)) )
            *zeroSqueeze = 'n';
          else if ((0 == strcmp("EPS",s)) ||
                   (0 == strcmp("Eps",s)) ||
                   (0 == strcmp("eps",s)) )
            *zeroSqueeze = 'e';
          else
            error ("usage: wgdx: argument '%s=%s' is invalid", argName, s);
        }
        break;
      default:
        error ("usage: wgdx: argument '%s' is invalid", argName);
      } /* end switch(TYPEOF(t)) */
      stopper = argLen - 1;
    }
  } /* end loop over arg list */
#if 0
  Rprintf ("Last arg processed\n");
  a = CDR(a);
  Rprintf ("DEBUG: args = %p\n", CAR(a));
  Rprintf ("DEBUG: R_NilValue = %p\n", R_NilValue);
#endif

  *symList = malloc (*symListSiz * sizeof(**symList));
  memset (*symList, 0, *symListSiz * sizeof(**symList));

  for (a = *args, i = 2;  i < stopper;  i++) {
    a = CDR(a);
    t = CAR(a);
    processWrArg (t, i, *symList, *symListSiz, symListLen);
  }

  return;
} /* unpackWgdxArgs */


/* writeGdx
 * gdxFileName: name of GDX file to be written
 * symList: vector of symList's entered by user
 * zeroSqueeze: indicate how to write zero values
 */
static void
writeGdx (char *gdxFileName, int symListLen, SEXP *symList,
	  char zeroSqueeze)
{
  SEXP iVec;               /* UEL indices for one dim of one symbol */
  SEXP iVecVec;            /* UEL indices for all dims of one symbol */
  SEXP uelIndex;           /* UEL indices for all symbols */
  SEXP lstNames, valData;
  wSpec_t **wSpecPtr;           /* was data */
  gdxUelIndex_t uelIndices;
  gdxValues_t vals;
  gdxSVals_t sVals;
  d64_t d64;
  shortStringBuf_t msgBuf;
  shortStringBuf_t expText;
  int rc, errNum;
  int i, j, k;
  int iSym;
  int idx;
  SEXP dimVect;
  int iDim;
  int totalElement, total, nColumns, nRows, index, total_num_of_elements;
  int d, subindex, inner;
  double *dimVal, *pd, dt, posInf, negInf;
  int *pi;
  int *subscript;
  int wgdxAlloc = 0;

  /* shut up compiler warnings */
  valData = NULL;

  total = 1;

  loadGDX();
  rc = gdxCreate (&gdxHandle, msgBuf, sizeof(msgBuf));
  if (0 == rc)
    error ("Error creating GDX object: %s", msgBuf);

  rc = gdxOpenWrite (gdxHandle, gdxFileName, "GDXRRW:wgdx", &errNum);
  if (errNum || 0 == rc) {
    error("Could not open gdx file with gdxOpenWrite: %s",
          lastErrMsg);
  }

  gdxGetSpecialValues (gdxHandle, sVals);
  d64.u64 = 0x7fffffffffffffff; /* positive QNaN, mantissa all on */
  sVals[GMS_SVIDX_UNDEF] = d64.x;
  dt = 0.0;
  posInf =  1 / dt;
  negInf = -1 / dt;
  sVals[GMS_SVIDX_PINF] = posInf;
  sVals[GMS_SVIDX_MINF] = negInf;
  gdxSetSpecialValues (gdxHandle, sVals);

  rc = gdxUELRegisterStrStart (gdxHandle);
  if (! rc) {
    error ("could not gdxUELRegisterStrStart");
  }

  PROTECT(uelIndex = allocVector(VECSXP, symListLen));
  wgdxAlloc++;

  wSpecPtr = (wSpec_t **) malloc (symListLen * sizeof(wSpecPtr[0]));

  /* check input list(s) for data validation and to create UEL list */
  for (iSym = 0;  iSym < symListLen;  iSym++) {
    if (TYPEOF(symList[iSym]) != VECSXP) {
      error("Incorrect type of input encountered. List expected");
    }
    else {
      readWgdxList (symList[iSym], iSym, uelIndex, wSpecPtr+iSym, &wgdxAlloc);
    }
  }

  rc = gdxUELRegisterDone(gdxHandle);
  if (! rc)
    error ("could not gdxUELRegisterDone: rc = %d", rc);

  /* start writing data to GDX file */
  memset (uelIndices, 0, sizeof(gdxUelIndex_t));
  memset (vals, 0, sizeof(gdxValues_t));

  i=0;
  /* write data in GDX file */
  for (i = 0;  i < symListLen;  i++) {
    lstNames = getAttrib(symList[i], R_NamesSymbol);
    valData = NULL;
    for (j = 0; j < length(symList[i]); j++) {
      if (strcmp("val", CHAR(STRING_ELT(lstNames, j))) == 0) {
        valData = VECTOR_ELT(symList[i], j);
        break;
      }
    }
    iVecVec = VECTOR_ELT(uelIndex, i);
    if (wSpecPtr[i]->dType == set) {
      if (wSpecPtr[i]->withVal == 0 && wSpecPtr[i]->withUel == 1) {
        /* creating value for set that does not have val */
	nColumns = length(iVecVec);
	PROTECT(dimVect = allocVector(REALSXP, nColumns));
	wgdxAlloc++;
	totalElement = 1;
	dimVal = REAL(dimVect);
	for (iDim = 0;  iDim < nColumns;  iDim++) {
	  dimVal[iDim] = length(VECTOR_ELT(iVecVec, iDim));
	  totalElement *= dimVal[iDim];
	}
	PROTECT(valData = allocVector(REALSXP, totalElement));
	wgdxAlloc++;
	pd = REAL(valData);
	for (index = 0; index < totalElement; index++) {
	  pd[index] = 1;
	}
	setAttrib(valData, R_DimSymbol, dimVect);
	index = 0;
	wSpecPtr[i]->dForm = full;
      }
    }
    (void) CHAR2ShortStr ("R data from GDXRRW", expText);
    if (wSpecPtr[i]->withTs == 1) {
      /* Looking for 'ts' */
      for (j = 0;  j < length(symList[i]);  j++) {
	if (strcmp("ts", CHAR(STRING_ELT(lstNames, j))) == 0) {
          (void) CHAR2ShortStr (CHAR(STRING_ELT( VECTOR_ELT(symList[i], j), 0)), expText);
	  break;
	}
      }
    }

    if (wSpecPtr[i]->dForm == sparse) {
      dimVect = getAttrib(valData, R_DimSymbol);
      nColumns = INTEGER(dimVect)[1];
      nRows = INTEGER(dimVect)[0];

      if (wSpecPtr[i]->dType == parameter) {
	nColumns--;
	rc = gdxDataWriteMapStart (gdxHandle, wSpecPtr[i]->name, expText,
				   nColumns, GMS_DT_PAR, 0);
      }
      else {
	rc = gdxDataWriteMapStart (gdxHandle, wSpecPtr[i]->name, expText,
				   nColumns, GMS_DT_SET, 0);
	vals[0] = 0;
      }
      if (!rc) {
	error("Could not write data with gdxDataWriteMapStart");
      }

      pd = NULL;
      pi = NULL;
      if (TYPEOF(valData) == REALSXP) {
	pd = REAL(valData);
      }
      else if (TYPEOF(valData) == INTSXP) {
	pi = INTEGER(valData);
      }
      for (j = 0; j < nRows; j++) {
	for (k = 0; k < nColumns; k++) {
	  iVec = VECTOR_ELT(iVecVec, k);
	  if (pd) {
	    idx = (int) pd[k*nRows + j];
	  }
	  else {
	    idx = pi[k*nRows + j];
	  }
          uelIndices[k] = INTEGER(iVec)[idx-1];
	}
	if (wSpecPtr[i]->dType == parameter) {
	  if (pd) {
	    vals[0] = pd[nColumns*nRows + j];
	  }
	  else {
	    vals[0] = pi[nColumns*nRows + j];
	  }
#if 1
	  if (ISNA(vals[0])) {
	    vals[0] = sVals[GMS_SVIDX_NA];
	  }
#endif
	}
	if ((parameter == wSpecPtr[i]->dType) &&
	    (0 == vals[0]) && ('e' == zeroSqueeze))
	  vals[0] = sVals[GMS_SVIDX_EPS];
	if ((set == wSpecPtr[i]->dType) ||
	    ('n' == zeroSqueeze) ||
	    (0 != vals[0])) {
	  /* write the value to GDX */
	  rc = gdxDataWriteMap (gdxHandle, uelIndices, vals);
	  if (!rc) {
	    error("Could not write parameter MAP with gdxDataWriteMap");
	  }
	}
      }

      if (!gdxDataWriteDone(gdxHandle)) {
	error ("Could not end writing parameter with gdxDataWriteMapStart");
      }
    } /* if sparse */
    else {                    /* form = full */
      total_num_of_elements = length(valData);
      dimVect = getAttrib(valData, R_DimSymbol);
      nColumns = length(iVecVec);
      subscript = malloc(nColumns*sizeof(*subscript));
      if (wSpecPtr[i]->dType == parameter) {
	rc = gdxDataWriteMapStart (gdxHandle, wSpecPtr[i]->name, expText,
				   nColumns, GMS_DT_PAR, 0);
      }
      else {
	rc = gdxDataWriteMapStart (gdxHandle, wSpecPtr[i]->name, expText,
				   nColumns, GMS_DT_SET, 0);
	vals[0] = 0;
      }
      if (!rc) {
	error("Could not write data with gdxDataWriteMapStart");
      }
      pd = NULL;
      pi = NULL;
      if (TYPEOF(valData) == REALSXP) {
	pd = REAL(valData);
      }
      else if (TYPEOF(valData) == INTSXP) {
	pi = INTEGER(valData);
      }
      else {
	error ("internal error: unrecognized valData type");
      }
      for (index = 0; index < total_num_of_elements; index++) {
	subindex = index;
	if (nColumns > 0) {
	  for (d = nColumns-1; ; d--) {
	    iVec = VECTOR_ELT(iVecVec, d);
	    for (total=1, inner=0; inner<d; inner++) {
	      total *= INTEGER(dimVect)[inner];
	    }
	    subscript[d] = subindex / total;
            uelIndices[d] = INTEGER(iVec)[subscript[d]];

	    subindex = subindex % total;
	    if (d == 0) {
	      break;
	    }
	  } /* for loop over "d" */
	}

	if (pd) {
	  dt = pd[index];
	}
	else {
	  dt = pi[index];
	}
	if (wSpecPtr[i]->dType == parameter) {
	  vals[0] = dt;
	  if (ISNA(vals[0])) {
	    vals[0] = sVals[GMS_SVIDX_NA];
	  }
	}
	else if (set == wSpecPtr[i]->dType) {
	  /* could do the check in checkForValidData but
	   * that uses an additional pass through the full matrix */
	  if (0 != dt && 1 != dt) {
	    error ("Only zero-one values are allowed when specifying sets with form=full");
	  }
	}
	if ((parameter == wSpecPtr[i]->dType) &&
	    (0 == vals[0]) && ('e' == zeroSqueeze))
	  vals[0] = sVals[GMS_SVIDX_EPS];
	if (((set == wSpecPtr[i]->dType) && (0 != dt))  ||
	    ((parameter == wSpecPtr[i]->dType) &&
	     (('n' == zeroSqueeze) ||
	      (0 != vals[0]))) ) {
	  /* write the value to GDX */
	  rc = gdxDataWriteMap(gdxHandle, uelIndices, vals);
	  if (!rc) {
	    error("Could not write parameter MAP with gdxDataWriteMap");
	  }
	}
      } /* for loop over "index" */
      if (!gdxDataWriteDone(gdxHandle)) {
	error("Could not end writing data with gdxDataWriteMapStart");
      }
    } /* end of writing full data */
  } /* for (i) loop over symbols */

  /* Close GDX file */
  errNum = gdxClose (gdxHandle);
  if (errNum != 0) {
    getGDXMsg ();
    error("GDXRRW:wgdx:GDXError",
          "Could not gdxClose: %s",
          lastErrMsg);
  }
  (void) gdxFree (&gdxHandle);

  /* free memory */
  free(wSpecPtr);
  UNPROTECT(wgdxAlloc);
} /* writeGdx */


/* wgdx: gateway function for writing gdx, called from R via .External
 * first arg: GDX file name
 * remaining arg: <- lists containg symbol data to be written to GDX
 * return: R_NilValue
 */
SEXP
wgdx (SEXP args)
{
  SEXP fileName, *symList = NULL;
  int symListSiz = 0, symListLen = 0;
  shortStringBuf_t gdxFileName;
  int arglen;
  char zeroSqueeze;
  char strippedID[GMS_SSSIZE];


  arglen = length(args);
  if (arglen == 1) {
    error("No input is entered. Please enter valid input");
  }

  args = CDR(args); fileName = CAR(args);

  /* Checking that first argument is of type string
   * and second argument is of type list
   */
  if (TYPEOF(fileName) != STRSXP ) {
    error ("The GDX filename (first argument) must be of type string.");
  }

  (void) CHAR2ShortStr (CHAR(STRING_ELT(fileName, 0)), gdxFileName);

  msgInit ();
  if (0 == strcmp("?", gdxFileName)) {
    int n = (int)strlen (ID);
    memcpy (strippedID, ID+1, n-2);
    strippedID[n-2] = '\0';
    Rprintf ("R-file source info: %s\n", strippedID);
    return R_NilValue;
  } /* if audit run */

  checkFileExtension (gdxFileName);

  unpackWgdxArgs (&args, arglen, &symList, &symListSiz, &symListLen, &zeroSqueeze);

  /* check and write data to gdxfile */
  writeGdx (gdxFileName, symListLen, symList, zeroSqueeze);
  free (symList);
  return R_NilValue;
} /* wgdx */

