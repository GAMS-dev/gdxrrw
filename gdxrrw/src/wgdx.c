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
checkSymType2 (dType_t dType, int lineNum)
{
  switch (dType) {
  case set:
  case parameter:
    break;
  case variable:
    error ("wgdx checkSymType2 line %d: not yet implemented for symbol type 'variable'", lineNum);
    break;
  default:
    error ("wgdx checkSymType2 line %d: not yet implemented for symbol type <unknown>", lineNum);
  } /* end switch */
} /* checkSymType2 */

/* checkSymType3: make sure dType is one we've implemented
 * the calling code block for */
static void
checkSymType3 (dType_t dType, int lineNum)
{
  switch (dType) {
  case set:
  case parameter:
  case variable:
    break;
  default:
    error ("wgdx checkSymType3 line %d: not yet implemented for symbol type <unknown>", lineNum);
  } /* end switch */
} /* checkSymType3 */

/* idxCmp: compare indices from sparse-form 'val'
 * return:
 *    0 if the same (including if dim=0)
 *    < 0 if u1 < u2
 *    > 0 if u1 > u2
 */
static int idxCmp (int dim,  valIndex_t u1, valIndex_t u2)
{
  int i;
  int res;

#if 0
  Rprintf (" DEBUG idxCmp: dim = %d\n", dim);
  Rprintf ("   u1 =");
  for (i = 0;  i < dim;  i++)
    Rprintf (" %d", u1[i]);
  Rprintf ("\n");
  Rprintf ("   u2 =");
  for (i = 0;  i < dim;  i++)
    Rprintf (" %d", u2[i]);
  Rprintf ("\n");
#endif
  for (i = 0;  i < dim;  i++) {
    res = u1[i] - u2[i];
    if (res)
      return res;
  }
  return 0;
} /* idxCmp */

static void
getFieldMapping (SEXP val, SEXP labels, SEXP *fVec, wSpec_t *wSpec, int *protCount)
{
  SEXP dims;
  double *pd;
  int *pi;
  int *fPtr;
  const char *fieldName;
  double dt;
  int i, k;
  int nCols, nRows, nLabs;

  /* labels must be a string vector */
  if (R_NilValue == labels) {
    error ("Empty list of field labels in list element 'uels' not allowed");
  }
  if (TYPEOF(labels) != STRSXP) {
    error ("List of field labels in list element 'uels' must be strings");
  }
  nLabs = length(labels);
  Rprintf ("DEBUG getFieldMapping: nLabs=%d\n", nLabs);
  PROTECT((*fVec) = allocVector(INTSXP, nLabs));
  ++*protCount;
  fPtr = INTEGER(*fVec);
  for (i = 0;  i < nLabs;  i++)
    fPtr[i] = -1;

  pd = NULL;
  pi = NULL;
  if (TYPEOF(val) == REALSXP) {
    pd = REAL(val);
  }
  else if (TYPEOF(val) == INTSXP) {
    pi = INTEGER(val);
  }
  else {
    error ("'val' must be numeric.");
  }
  dims = getAttrib(val, R_DimSymbol);
  if (sparse == wSpec->dForm) {
    nRows = INTEGER(dims)[0];
    nCols = INTEGER(dims)[1];
    checkSymType3 (wSpec->dType, __LINE__);
    if (variable != wSpec->dType)
      error ("Only implemented for symbol type variable");
    nCols -= 2;   /* last two columns are field index and level/marginal/etc values */
    if (nCols != wSpec->symDim) {
      error ("Number of columns in sparse data not consistent with symbol dimension.");
    }
    for (i = 0;  i < nRows;  i++) {
      if (pd) {
        dt = pd[i + nCols*nRows];
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
        k = pi[i + nCols*nRows];
        if (k < 1) {
          error ("Non-positive coordinates are not allowed in index columns of sparse data");
        }
      }
      if (k > nLabs) {
        error ("field index in column %d of sparse 'val' matrix exceeds number of UEL strings for that dimension.",
               nCols+1);
      }
      fieldName = CHAR(STRING_ELT(labels, k-1));
      Rprintf ("Field indices: row i=%d  idx=%d  fieldName=%s\n", i, k, fieldName);
      if      (strcasecmp("l", fieldName) == 0) {
        fPtr[k-1] = GMS_VAL_LEVEL;
      }
      else if (strcasecmp("m", fieldName) == 0) {
        fPtr[k-1] = GMS_VAL_MARGINAL;
      }
      else if (strcasecmp("lo", fieldName) == 0) {
        fPtr[k-1] = GMS_VAL_LOWER;
      }
      else if (strcasecmp("up", fieldName) == 0) {
        fPtr[k-1] = GMS_VAL_UPPER;
      }
      else if (strcasecmp("s", fieldName) == 0) {
        fPtr[k-1] = GMS_VAL_SCALE;
      }
      else {
        error ("variable field name '%s' not valid");
      }
    }
  }
  else {
    checkSymType2 (wSpec->dType, __LINE__);
  }
} /* getFieldMapping */

/* checkVals: check validity of set index columns in input 'val'
 * no return: calls error() if not valid
 * checks include:
 *   vals must be numeric
 *   index vals must be integral, positive, not larger than uel list for that dimension
 * isSorted is input/output: if true on input,
 *  a check is made that index vals are sorted, result returned in isSorted
 */
static void
checkVals (SEXP val, SEXP uels, wSpec_t *wSpec, int *isSorted)
{
  SEXP dims;
  double *pd;
  int *pi;
  double dt;
  valIndex_t prevInd, currInd;
  int nRows, nCols;
  int i, j, k;
  int mx[GMS_MAX_INDEX_DIM];

  memset (mx, 0, sizeof(mx));
  pd = NULL;
  pi = NULL;
  if (TYPEOF(val) == REALSXP) {
    pd = REAL(val);
  }
  else if (TYPEOF(val) == INTSXP) {
    pi = INTEGER(val);
  }
  else {
    error ("Input list element 'val' must be numeric.");
  }
  dims = getAttrib(val, R_DimSymbol);

  if (wSpec->dForm == sparse) {
    nRows = INTEGER(dims)[0];
    nCols = INTEGER(dims)[1];
    checkSymType3 (wSpec->dType, __LINE__);
    switch (wSpec->dType) {
    case set:
      break;
    case parameter:
      nCols--;    /* skip last column containing parameter values */
      break;
    case variable:
      nCols -= 2;   /* skip last two columns: field index and level/marginal/etc values */
      break;
    default:
      error ("vals input not expected/implemented for this symbol type.");
    } /* switch symbol type */
    if (wSpec->symDim != nCols) {
      error ("Number of columns in sparse 'val' data not consistent with symbol dimension.");
    }

    /* the matrix of vals is stored column-major */
    memset (prevInd, 0, sizeof(valIndex_t));
    memset (currInd, 0, sizeof(valIndex_t));
    for (i = 0;  i < nRows;  i++) {
      for (j = 0;  j < nCols;  j++) {
        if (pd) {
          dt = pd[i + j*nRows];
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
          k = pi[i + j*nRows];
          if (k < 1) {
            error ("Non-positive coordinates are not allowed in index columns of sparse data");
          }
        }
        currInd[j] = k;
        if (k > mx[j])
          mx[j] = k;
      } /* loop over cols */
      if (*isSorted) {
        int r = idxCmp(nCols, prevInd, currInd);
        if (r > 0) {
          *isSorted = 0;
        }
        if (r < 0) {
          memcpy (prevInd, currInd, nCols * sizeof(prevInd[0]));
        }
      } /* if *isSorted */
    } /* loop over rows */
    for (j = 0;  j < nCols;  j++) {
      if (mx[j] > length(VECTOR_ELT(uels, j))) {
        error ("UEL index in column %d of sparse 'val' matrix exceeds number of UEL strings for that dimension.",
               j+1);
      }
    }
  } /* sparse form */
  else {
    /* compare dimension of full matrix and number of columns in uels */
    checkSymType2 (wSpec->dType, __LINE__);
    if (wSpec->symDim != length(dims)) {
      error ("Dimension of full 'val' data does not match dimensions of uels or symbol.");
    }
  }
  return;
} /* checkVals */

static void
createUelOut(SEXP val, SEXP uelOut, dType_t dType, dForm_t dForm)
{
  SEXP dims, bufferUel;
  int i, j, k;
  double *P;
  int *intVal;
  char buffer [256];
  int nCols, nRows, ndims;
  int mx;

  dims = getAttrib(val, R_DimSymbol);
  if (dForm == sparse) {
    nRows = INTEGER(dims)[0];
    nCols = INTEGER(dims)[1];
    checkSymType2 (dType, __LINE__);
    if (dType == parameter) {
      nCols--;
    }

    if (TYPEOF(val) == REALSXP) {
      P = REAL(val);
      for (i = 0; i < nCols; i++) {
        mx = 0;
        for (j = 0; j < nRows; j++) {
          if (P[j + i*nRows] > mx) {
            mx = (int) P[j + i*nRows];
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

      for (i = 0; i < nCols; i++) {
        mx = 0;
        for (j = 0; j < nRows; j++) {
          if (intVal[j + i*nRows] > mx) {
            mx = intVal[j + i*nRows];
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

/* dumpUELs: dump the strings in sVecVec
 * sVecVec is the UEL info for one symbol: a vector of string vectors,
 * one string vector for each symbol dimension
 */
static void
dumpUELs (SEXP sVecVec, wSpec_t *wSpec)
{
  int i, k;
  int dim;                  /* should be same as the GDX symbol dim */
  int vecLen;               /* length of sVec */
  const char *uelString;
  SEXP sVec;                /* vector of UEL labels: input */

  dim = length(sVecVec);
  Rprintf ("\n");
  Rprintf ("--------------------------------------------\n");
  Rprintf ("dumpUELs for symbol '%s', dim=%d\n", wSpec->name, dim);

  for (i = 0;  i < dim;  i++) {
    sVec = VECTOR_ELT(sVecVec, i); /* UELs for i'th index position */
    vecLen = length(sVec);
    Rprintf (" dim %d lenth=%d\n", i, vecLen);
    for (k = 0;  k < vecLen;  k++) {
      uelString = CHAR(STRING_ELT(sVec, k));
      Rprintf("    %s\n", uelString);
    }
  }
} /* dumpUELs */

/* registerInputUEL: take the UEL strings in sVecVec and register them,
 * in the process storing their GDX indices in uelIndex[kk]
 * sVecVec is the UEL info for one symbol: a vector of string vectors,
 * one string vector for each symbol dimension
 */
static void
registerInputUEL(SEXP sVecVec, int kk, SEXP uelIndex, int *protCount)
{
  int i, k, rc, gi;
  const char *uelString;
  int dim;                  /* should be same as the GDX symbol dim */
  int vecLen;               /* length of sVec */
  SEXP sVec;                /* vector of UEL labels: input */
  SEXP iVec;                /* GDX indices for sVec labels */
  SEXP iVecVec;             /* one iVec for each index position */

  dim = length(sVecVec);
  PROTECT( iVecVec = allocVector(VECSXP, dim));
  ++*protCount;
  /* Rprintf ("DEBUG registerInputUEL: dim=%d\n", dim); */

  for (i = 0;  i < dim;  i++) {
    sVec = VECTOR_ELT(sVecVec, i); /* UELs for i'th index position */
    vecLen = length(sVec);
    /* Rprintf ("DEBUG registerInputUEL: i=%d  vecLen=%d\n", i, vecLen); */

    PROTECT(iVec = allocVector(INTSXP, vecLen));

    for (k = 0;  k < vecLen;  k++) {
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
 * for variables and equations, also create mapping from field index to
 */
static void
readWgdxList (SEXP lst, int iSym, SEXP uelIndex, SEXP fieldIndex, SEXP rowPerms,
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
  SEXP typeCodeExp = NULL;
  int i, j;
  int nElements;                /* number of elements in lst */
  int dimUels;
  int symDimTmp = 0; /* consistent with GDX or GAMS idea of symbol dim */
  int sz;
  char buf[512];
  const char *tmpName;
  const char *eltName;        /* list element name */
  SEXP symUels;               /* UEL strings, either from user or created here from indices */
  SEXP bufferUel;             /* allocating temporary storage place */
  SEXP fVec;                  /* integer vector mapping field indices in 'val' to GMS_VAL_XXX */
  wSpec_t *wSpec;

  symUels = R_NilValue;
  fVec = R_NilValue;

  wSpec = (wSpec_t *) malloc(sizeof(*wSpec));
  *wSpecPtr = wSpec;
  memset (wSpec, 0, sizeof(*wSpec));
  wSpec->dForm = sparse;
  wSpec->dType = set;
  wSpec->symDim = -1;           /* not yet known */

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
    else if (0 == strcmp("typeCode", eltName)) {
      typeCodeExp = VECTOR_ELT(lst, i);
    }
    else if (0 == strcmp("varTypeText", eltName)) {
      /* we just ignore this: allowed for rgdx() compatibility */
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
      wSpec->withDim = 1;
      wSpec->symDim = wSpec->dim = INTEGER(dimExp)[0];
    }
    else if (REALSXP == TYPEOF(dimExp)) {
      if (length(dimExp) != 1) {
        error ("Optional input list element 'dim' must have only one element.");
      }
      if (REAL(dimExp)[0] < 0) {
        error("Negative value is not allowed as valid input for 'dim'.");
      }
      wSpec->withDim = 1;
      wSpec->symDim = wSpec->dim = (int) REAL(dimExp)[0];
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
    checkSymType3 (wSpec->dType, __LINE__);
    if (wSpec->withDim) {
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
    else {
      /* no 'dim' element, so uels imply symDim */
      switch (wSpec->dType) {
      case set:
      case parameter:
        wSpec->symDim = dimUels;
        break;
      case variable:
        wSpec->symDim = dimUels - 1;
        break;
      default:
        error ("uels input not expected/implemented for type=%s.", CHAR(STRING_ELT(typeExp, 0)));
      } /* switch symbol type */
      if (wSpec->symDim > GMS_MAX_INDEX_DIM) {
        error ("Input list element 'uels' implies symbol dimension of %d, exceeding GDX dimension limit of %d.",
               wSpec->symDim, GMS_MAX_INDEX_DIM);
      }
    }
#if 0
    else if (0 == dimUels) {
      error ("Empty input list element 'uels' is not allowed without 'dim'=0.");
    }
#endif
    PROTECT(symUels = allocVector(VECSXP, wSpec->symDim));
    ++*protCount;
    for (j = 0;  j < wSpec->symDim;  j++) {
      tmpUel = VECTOR_ELT(uelsExp, j);
      if (tmpUel == R_NilValue) {
        error ("Empty input field in list element 'uels' not allowed");
      }
      if (TYPEOF(tmpUel) == STRSXP) {
        /*  checkStringLength( CHAR(STRING_ELT(tmp, 0)) ); */
        SET_VECTOR_ELT (symUels, j, tmpUel);
      }
      else if (TYPEOF(tmpUel) == REALSXP || TYPEOF(tmpUel) == INTSXP) {
        /* Convert to output */
        bufferUel = allocVector(STRSXP, length(tmpUel));
        makeStrVec (bufferUel, tmpUel);
        SET_VECTOR_ELT (symUels, j, bufferUel);
      }
      else {
        error ("Input uels must be either string vectors or numeric vectors.");
      }
    }
    wSpec->withUel = 1;
  } /* uelsExp */

  if (NULL == valExp) {         /* .val field missing */
    if (set == wSpec->dType) {
      if (0 == wSpec->withUel) {
        error ("Missing 'val' is a required list element for sets with no UELs.");
      }
    }
    else {
      error ("Missing 'val' is a required list element for type=%s.", CHAR(STRING_ELT(typeExp, 0)));
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
        symDimTmp = sz;
        checkSymType3 (wSpec->dType, __LINE__);
        switch (wSpec->dType) {
        case set:
          break;                /* no adjustment */
        case parameter:
          symDimTmp--;
          break;
        case variable:
          symDimTmp -= 2;
          if (symDimTmp < 0)
            error ("val input must have at least 2 cols when writing variables in sparse form.");
          break;
        default:
          error ("val input not expected/implemented for type=%s.", CHAR(STRING_ELT(typeExp, 0)));
        } /* switch symbol type */
        if (symDimTmp > GMS_MAX_INDEX_DIM) {
          error ("Input list element 'val' implies symbol dimension of %d, exceeding GDX dimension limit of %d.",
                 symDimTmp, GMS_MAX_INDEX_DIM);
        }
        /* previous checks set symDim by 'dim' or 'uels' */
        if (wSpec->withDim) {
          if (wSpec->dim != symDimTmp) {
            error ("Inconsistent dimensions found: 'dim'=%d doesn't match"
                   " dimension=%d implied by 'val'", wSpec->dim, symDimTmp);
          }
        }
        else if (dimUels >= 0) {
          /* symDim implies already by 'uels' */
          if (wSpec->symDim < 0)
            error ("Internal error: symDim should already be set positive");
          if (wSpec->symDim != symDimTmp) {
            error ("Inconsistent dimensions implied by 'uels' (%d) and"
                   " 'val' (%d)", wSpec->symDim, symDimTmp);
          }
        }
        else {
          wSpec->symDim = symDimTmp;
        }
        wSpec->withVal = 1;
      } /* if sparse */
      else {
        /* This is for Full/Dense data */
        symDimTmp = length(dimension);
        switch (wSpec->dType) {
        case set:
        case parameter:
          break;                /* no adjustment */
        case variable:
          symDimTmp--;          /* account for _field dimension */
          break;
        default:
          error ("val input not expected/implemented for type=%s.", CHAR(STRING_ELT(typeExp, 0)));
        } /* switch symbol type */
        if (symDimTmp > GMS_MAX_INDEX_DIM) {
          error ("Input list element 'val' implies symbol dimension of %d, exceeding GDX dimension limit of %d.",
                 symDimTmp, GMS_MAX_INDEX_DIM);
        }
        /* previous checks set symDim by 'dim' or 'uels' */
        if (wSpec->withDim) {
          if (wSpec->dim != symDimTmp) {
            error ("Inconsistent dimensions found: 'dim'=%d doesn't match"
                   " dimension=%d implied by 'val'", wSpec->dim, symDimTmp);
          }
        }
        else if (dimUels >= 0) {
          /* symDim implies already by 'uels' */
          if (wSpec->symDim < 0)
            error ("Internal error: symDim should already be set positive");
          if (wSpec->symDim != symDimTmp) {
            error ("Inconsistent dimensions implied by 'uels' (%d) and"
                   " 'val' (%d)", wSpec->symDim, symDimTmp);
          }
        }
        else {
          wSpec->symDim = symDimTmp;
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

  if (typeCodeExp) {
    checkSymType3 (wSpec->dType, __LINE__);
    /* ignore if not needed for this parameter type */
    if (variable == wSpec->dType) {
      int typeCode;

      if (INTSXP == TYPEOF(typeCodeExp)) {
        if (length(typeCodeExp) != 1)
          error ("Input list element 'typeCode' must have only one element.");
        typeCode = INTEGER(typeCodeExp)[0];
      }
      else if (REALSXP == TYPEOF(typeCodeExp)) {
        if (length(typeCodeExp) != 1)
          error ("Input list element 'typeCode' must have only one element.");
        typeCode = (int) REAL(typeCodeExp)[0];
        if (REAL(typeCodeExp)[0] != typeCode) {
          error("Non-integer value is not allowed as valid input for 'typeCode'.");
        }
      }
      else {
        error ("Input list element 'typeCode' must be numeric - found %s instead.",
               typeofTxt(dimExp, buf));
      }
      if ((typeCode > 0) && (typeCode < GMS_VARTYPE_MAX))
        wSpec->typeCode = typeCode;
      else
        error ("Invalid typeCode %d found for symbol '%s'", typeCode, wSpec->name);
    }
  }



  checkSymType3 (wSpec->dType, __LINE__);
  if (variable == wSpec->dType) {
    /* require a typeCode */
    if (wSpec->typeCode <= 0) {
      error ("Missing typeCode for variable symbol '%s'", wSpec->name);
    }
  }

  if (wSpec->withUel == 0 && wSpec->withVal == 1) {
    switch (wSpec->dType) {
    case set:
    case parameter:
      break;                    /* no problem */
    default:
      error ("Empty UEL list not yet implemented for type=%s.", CHAR(STRING_ELT(typeExp, 0)));
    } /* switch symbol type */
    PROTECT(symUels = allocVector(VECSXP, wSpec->symDim)); /* or should this be a different dimension?? */
    ++*protCount;
    createUelOut (valExp, symUels, wSpec->dType, wSpec->dForm);
  }

  SET_VECTOR_ELT(rowPerms, iSym, R_NilValue);
  if (wSpec->withVal == 1) {
    int isSorted = 0;

    switch (wSpec->dType) {
    case set:
    case parameter:
      checkVals (valExp, symUels, wSpec, &isSorted);
      break;                    /* no problem */
    case variable:
      isSorted = 1;             /* check sort order */
      checkVals (valExp, symUels, wSpec, &isSorted);
      if (! isSorted) {
        error ("readWgdxList: go implement a sorter and save in rowPerms");
      }
      /* check out field column */
      getFieldMapping (valExp, VECTOR_ELT(uelsExp, wSpec->symDim), &fVec, wSpec, protCount);
      break;
    default:
      error ("checkVals not implemented type=%s.", CHAR(STRING_ELT(typeExp, 0)));
    } /* switch symbol type */
  }

  /* debugging function */
  /* dumpUELs (symUels, wSpec); */

  registerInputUEL (symUels, iSym, uelIndex, protCount);
  SET_VECTOR_ELT(fieldIndex, iSym, fVec);
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
  SEXP fVec;               /* field indices for one symbol */
  SEXP fieldIndex;         /* field indices for all symbols */
  SEXP rowPerm;            /* row permutation for sparse 'val' rows of one symbol */
  SEXP rowPerms;           /* vector of rowPerm's, one per symbol */
  SEXP lstNames, valData;
  wSpec_t **wSpecPtr;           /* was data */
  gdxUelIndex_t uelIndices;
  gdxValues_t vals;
  gdxSVals_t sVals;
  valIndex_t prevInd, currInd;
  d64_t d64;
  shortStringBuf_t msgBuf;
  shortStringBuf_t expText;
  int rc, errNum, empty;
  int i, j, k;
  int iSym;
  int idx;
  SEXP dimVect;
  int iDim;
  int totalElement, total, nColumns, nRows, index, total_num_of_elements;
  int d, subindex, inner;
  int fieldIdx;
  double v;
  double *dimVal, *pd, dt, posInf, negInf;
  int *pi;
  int *subscript;
  int *fPtr;
  int wgdxAlloc = 0;
  char buf[512];

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
  PROTECT(fieldIndex = allocVector(VECSXP, symListLen));
  wgdxAlloc++;
  PROTECT(rowPerms = allocVector(VECSXP, symListLen));
  wgdxAlloc++;

  wSpecPtr = (wSpec_t **) malloc (symListLen * sizeof(wSpecPtr[0]));

  /* check input list(s) for data validation and to create UEL list */
  for (iSym = 0;  iSym < symListLen;  iSym++) {
    if (TYPEOF(symList[iSym]) != VECSXP) {
      error("Incorrect type of input encountered. List expected");
    }
    else {
      SET_VECTOR_ELT(rowPerms, iSym, R_NilValue); /* readWgdxList may install a row permuation */
      readWgdxList (symList[iSym], iSym, uelIndex, fieldIndex, rowPerms, wSpecPtr+iSym, &wgdxAlloc);
    }
  }

  rc = gdxUELRegisterDone(gdxHandle);
  if (! rc)
    error ("could not gdxUELRegisterDone: rc = %d", rc);

  /* start writing data to GDX file */
  memset (uelIndices, 0, sizeof(gdxUelIndex_t));
  memset (prevInd, 0, sizeof(valIndex_t));
  memset (currInd, 0, sizeof(valIndex_t));
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
    rowPerm = VECTOR_ELT(rowPerms, i);
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

      if ((parameter == wSpecPtr[i]->dType) || (set == wSpecPtr[i]->dType)) {
        if (parameter == wSpecPtr[i]->dType) {
          nColumns--;
          rc = gdxDataWriteMapStart (gdxHandle, wSpecPtr[i]->name, expText,
                                     nColumns, GMS_DT_PAR, 0);
        }
        else {
          rc = gdxDataWriteMapStart (gdxHandle, wSpecPtr[i]->name, expText,
                                     nColumns, GMS_DT_SET, 0);
          vals[0] = 0;
        }
        if (!rc)
          error("Could not write data with gdxDataWriteMapStart");

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
            if (ISNA(vals[0])) {
              vals[0] = sVals[GMS_SVIDX_NA];
            }
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
        } /* end loop over rows */

        if (!gdxDataWriteDone(gdxHandle)) {
          error ("Could not end writing parameter with gdxDataWriteMapStart");
        }
      }    /* if set or parameter */
      else {
        fVec = VECTOR_ELT(fieldIndex, i);
        Rprintf ("typeof(valData) = %s\n", typeofTxt(valData, buf));
        Rprintf ("\n");
        fPtr = INTEGER(fVec);
        pd = NULL;
        pi = NULL;
        if (TYPEOF(valData) == REALSXP) {
          pd = REAL(valData);
        }

        nColumns -= 2;
        rc = gdxDataWriteMapStart (gdxHandle, wSpecPtr[i]->name, expText,
                                   nColumns, GMS_DT_VAR, wSpecPtr[i]->typeCode);
        if (!rc)
          error("Error calling gdxDataWriteMapStart for symbol '%s'", wSpecPtr[i]->name);

        idx = -1;
        empty = 1;
        for (j = 0; j < nRows; j++) {
          int fieldVal;

          for (k = 0;  k < nColumns;  k++) {
            if (pd) {
              idx = (int) pd[k*nRows + j];
            }
            else {
              idx = pi[k*nRows + j];
            }
            currInd[k] = idx;
          }
          if (pd) {
            fieldIdx = (int) pd[k*nRows + j];
            k++;
            v = pd[k*nRows + j];
          }
          else {
            fieldIdx = pi[k*nRows + j];
            k++;
            v = pi[k*nRows + j];
          }
          fieldVal = fPtr[fieldIdx-1];
          if (empty) {
            Rprintf ("  fieldIdx = %d   GMS_VAL_XX = %d   v = %g  into empty\n", fieldIdx, fieldVal, v);
            empty = 0;
            memcpy (prevInd, currInd, nColumns * sizeof(prevInd[0]));
            /* Rprintf ("  prevInd = %d  %d\n", prevInd[0], prevInd[1]); */
          }
          else {
            int r = idxCmp(nColumns, prevInd, currInd);
            if (r > 0) {
              if (R_NilValue == rowPerm)
                Rprintf ("DEBUG: rowPerm is NULL\n");
              else
                Rprintf ("DEBUG: rowPerm is non-NULL\n");
              error ("Unsorted input 'val' not yet implemented");
            }
            /* flush and clear */
            Rprintf ("  fieldIdx = %d   GMS_VAL_XX = %d   v = %g\n",
                     fieldIdx, fieldVal, v);
            if (r) {
              for (k = 0;  k < nColumns;  k++) {
                iVec = VECTOR_ELT(iVecVec, k);
                idx = prevInd[k];
                uelIndices[k] = INTEGER(iVec)[idx-1];
              }
              Rprintf ("  flushing\n");
              rc = gdxDataWriteMap (gdxHandle, uelIndices, vals);
              if (!rc)
                error("Error calling gdxDataWriteMap for symbol '%s'", wSpecPtr[i]->name);
              memset (vals, 0, sizeof(gdxValues_t));
              memcpy (prevInd, currInd, nColumns * sizeof(prevInd[0]));
              memset (currInd, 0, sizeof(valIndex_t)); /* not really needed */
            }
          }
          vals[fieldVal] = v;
#if 0
          Rprintf ("  fieldIdx = %d   GMS_VAL_XX = %d   v = %g\n", fieldIdx, fPtr[fieldIdx-1], v);
#endif
        } /* end loop over rows */

        if (! empty) {
          for (k = 0;  k < nColumns;  k++) {
            iVec = VECTOR_ELT(iVecVec, k);
            idx = currInd[k];
            uelIndices[k] = INTEGER(iVec)[idx-1];
          }
          Rprintf ("  flushing at end\n");
          rc = gdxDataWriteMap (gdxHandle, uelIndices, vals);
          if (!rc)
            error("Error calling gdxDataWriteMap for symbol '%s'", wSpecPtr[i]->name);
        }
        if (!gdxDataWriteDone(gdxHandle))
          error ("Error calling gdxDataWriteDone for symbol '%s'", wSpecPtr[i]->name);
        /* checkSymType2 (wSpecPtr[i]->dType, __LINE__); */
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

