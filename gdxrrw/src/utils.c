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

#include "gdxcc.h"
#include "gclgms.h"
#include "globals.h"

/* just to shut up some warnings on Linux */
typedef int (*compareFunc_t) (const void *, const void *);

/* checkForDuplicates
 * checks the input array of strings for duplicates,
 * throwing an error is there are any
 */
static void
checkForDuplicates (SEXP strExp)
{
  int k;
  shortStringBuf_t *elements;
  int nRec;

  /* store all the strings in temporary storage */
  nRec = length(strExp);

  elements = malloc(nRec * sizeof(*elements));
  for (k = 0; k < nRec; k++) {
    strcpy(elements[k], CHAR(STRING_ELT(strExp, k)));
  }

  /* sort the strings */
  qsort (elements, nRec, sizeof(*elements), (compareFunc_t)strcmp);

  /* check for duplicates */
  for (k = 0;  k < nRec - 1;  k++) {
    if (0 == strcmp(elements[k], elements[k+1])) {
      Rprintf ("Input UEL filter has duplicate entry of '%s'\n",
               elements[k]);
      error ("UEL filters must not contain duplicates.");
    }
  }
  free (elements);
} /* checkForDuplicates */

/* ------------------ start of globally available functions --------------- */

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
  if (0 == i) {
    error ("Cannot access empty field. Please try again");
  }
  else if (i >= sizeof(shortStringBuf_t)) {
    error("The data entered is too long: len=%d exceeds limit=%d.",
          i, (int)sizeof(shortStringBuf_t)-1);
  }
} /* checkStringLength */

/* compressData
 * compresses the raw data (both value and uel)
 * and removes redundant zeros from
 * value matrix and re-index output matrix.
 * And it also remove non present uel elements from UEL */
void
compressData (SEXP data, SEXP globalUEL, SEXP uelOut,
              int numberOfUel, int symbolDim, int nRec)
{
  int *mask, i, j, k, l, total, elements;
  double *col;
  SEXP bufferUel;

  mask = malloc(numberOfUel*sizeof(*mask));
  col =  REAL(data);
  for (i = 0; i < symbolDim; i++) {
    /* step 1: set all mask value to zero. */
    for (j = 0; j < numberOfUel; j++) {
      mask[j] = 0;
    }
    total = 0;
    /* step 2: loop through data martix column and set mask = 1 for corresponding positions */
    for (k = 0; k < nRec; k++) {
      if (mask[(int)col[k + nRec*i] - 1] == 0) {
        mask[(int)col[k + nRec*i] - 1] = 1;
        total = total + 1;
      }
    }
    /* step 3: create cellArray with size = total, fill in UEL if mask[] = 1 */
    /* step 4: step through 1's at mask and create sum */
    PROTECT(bufferUel = allocVector(STRSXP, total));
    elements = 0;
    for (l = 0; l < numberOfUel; l++) {
      if (mask[l] == 1) {
        elements = elements + 1;
        mask[l] = elements;
        SET_STRING_ELT(bufferUel, elements -1, duplicate(STRING_ELT(globalUEL, l)));
      }
    }
    /* step 5: step through column and update index value = mask[] */
    l = 0;
    for (l = 0; l < nRec; l++) {
      col[l + nRec*i] = mask[(int)col[l + nRec*i] - 1];
    }
    SET_VECTOR_ELT(uelOut, i, bufferUel);
    UNPROTECT(1);
  }
  free(mask);
  return;
} /* compressData */

/* createElementMatrix: what does this do?
 * create text element Matrix from sparse data and element vector
 */
void
createElementMatrix (SEXP compVal, SEXP textElement, SEXP compTe,
                     SEXP compUels, int symDim, int nRec)
{
  int i, j, iRec, index, totNumber;
  double *p;

  /* Step 1: loop over full matrix and set every value as empty string */
  for (j = 0; j < length(compTe); j++) {
    SET_STRING_ELT(compTe, j, mkChar(""));
  }

  /* Step 2: loop over each row of sparse matrix and populate full matrix */
  p = REAL(compVal);

  for (iRec = 0; iRec < nRec; iRec++) {
    index = 0;
    totNumber = 1;
    for (i = 0;  i < symDim;  i++) {
      index = index + ((int)p[iRec + nRec*i] - 1)*totNumber;
      totNumber = (totNumber)*length(VECTOR_ELT(compUels, i));
    }
    SET_STRING_ELT(compTe, index, duplicate(STRING_ELT(textElement, iRec)) );
  }

  return;
} /* createElementMatrix */

/* findInFilter: find the position of uelName in filterList[k]
 * returns:
 *   0   if uelName was not found
 *   i+1 otherwise, where i is the position of uelName in filterList[k]
 */
int findInFilter (int k, SEXP filterList, const char *uelName)
{
  SEXP filter;
  int i, n;
  const char *uelString;

  filter = VECTOR_ELT(filterList, k);
  n = length(filter);
  for (i = 0;  i < n;  i++) {
    uelString = CHAR(STRING_ELT(filter, i));
    if (0 == strcmp(uelString, uelName)) {
      return i+1;
    }
  }
  return 0;
} /* findInFilter */

/* mkIntFilter: construct an integer filter from the user-supplied string uels
 * ufilter: user-supplied filter - $uels[k]
 * hpf: high-performance filter for internal use
 */
void
mkIntFilter (SEXP uFilter, hpFilter_t *hpf)
{
  int k, n;
  int *idx;
  const char *uelString;
  int isOrdered = 1;
  int dummy, uelInt, lastUelInt;
  int allFound = 1;    /* all strings from the filter found in GDX? */
  int found;

  hpf->fType = integer;
  hpf->n = n = length(uFilter);
  /* Rprintf ("  mkIntFilter: n = %d\n", n); */
  hpf->prevPos = 0;
  hpf->idx = idx =  malloc(n * sizeof(*idx));
  if (NULL == idx)
    error ("memory exhaustion error: could not allocate index for hpFilter");
  for (lastUelInt = 0, k = 0;  k < n;  k++) {
    uelString = CHAR(STRING_ELT(uFilter, k));
    found = gdxUMFindUEL (gdxHandle, uelString, &uelInt, &dummy);
    /* Rprintf ("       k = %2d:  %s  %d  %d\n", k, uelString, uelInt, dummy); */
    if (! found) {                /* not found */
      allFound = 0;
      isOrdered = 0;            /* for now insist all are found to be ordered */
    }
    else if (isOrdered) {
      if (uelInt > lastUelInt)
        lastUelInt = uelInt;
      else
        isOrdered = 0;
    }
    idx[k] = uelInt;
  }
  hpf->isOrdered = isOrdered;
  if (! isOrdered) {
    checkForDuplicates (uFilter);
  }
  /* Rprintf ("  mkIntFilter: isOrdered = %d  allFound = %d\n", isOrdered, allFound); */
  return;
} /* mkIntFilter */

/* prepHPFilter: prep/check a high-performance filter prior to use
 * This is not initializing data, just initializing prevPos
 * and perhaps some debugging-type checks on consistency
 */
void
prepHPFilter (int symDim, hpFilter_t filterList[])
{
  int iDim;
  hpFilter_t *hpf;

  if (NULL == filterList)
    error ("internal error: NULL hpFilter");
  for (iDim = 0;  iDim < symDim;  iDim++) {
    hpf = filterList + iDim;
    switch (hpf->fType) {
    case unset:
      error ("internal error: hpFilter type unset");
      break;
    case identity:
      error ("internal error: just sanity checking");
      break;
    case integer:
      if (hpf->n <= 0)
        error ("internal error: integer hpFilter must be nonempty"); /* really? */
      hpf->prevPos = 0;
      break;
    default:
      error ("internal error: unknown hpFilter type");
    }
  } /* loop over symbol dimensions */
} /* prepHPFilter */

/* findInHPFilter: search for inUels in filterList,
 * storing the index where found in outIdx
 * as a side effect, updates previous search info in filterList
 * return:
 *   1     if found,
 *   0     otherwise
 */
int
findInHPFilter (int symDim, const int inUels[], hpFilter_t filterList[],
                int outIdx[])
{
  int iDim, k, targetUel, found;
  const int *idx;
  hpFilter_t *hpf;

  if (NULL == filterList)
    error ("internal error: NULL hpFilter");
  for (iDim = 0;  iDim < symDim;  iDim++) {
    hpf = filterList + iDim;
    switch (hpf->fType) {
    case unset:
      error ("internal error: hpFilter type unset");
      break;
    case identity:
      outIdx[iDim] = inUels[iDim];
      break;
    case integer:
      idx = hpf->idx;
      targetUel = inUels[iDim];
      for (found = 0, k = 0;  k < hpf->n; k++) {
        if (idx[k] == targetUel) {
          outIdx[iDim] = hpf->prevPos = k;
          found = 1;
          break;
        }
      }
      if (! found)
        return 0;
      break;
    default:
      error ("internal error: unknown hpFilter type");
    }
  } /* loop over symbol dimensions */
  return 1;                     /* found */
} /* findInHPFilter */

/* This method will read variable "gamso" from R workspace */
char *getGlobalString (const char *globName, shortStringBuf_t result)
{
  SEXP gamso, lstNames, tmp;
  char *res;
  int k, infields, found;

  *result = '\0';
  res = NULL;
  if (gamsoIsUnset)
    return res;

  gamso = findVar (install("gamso"), R_GlobalEnv);

  if (gamso == NULL || TYPEOF(gamso) != VECSXP) {
    gamsoIsUnset = 1;
    globalGams = 0;
    return res;
  }

  if (globalGams == 1) {
    lstNames = getAttrib (gamso, R_NamesSymbol);
    infields = length(gamso);
    /* Checking if field data is for "globName" */
    for (found = 0, k = 0;  k < infields;  k++) {
      if (strcmp(globName, CHAR(STRING_ELT(lstNames, k))) == 0) {
        found = 1;
        break;
      }
    }

    if (found) {
      tmp = VECTOR_ELT(gamso, k);
      if (TYPEOF(tmp) == STRSXP) {
        checkStringLength (CHAR(STRING_ELT(tmp, 0)));
        res = CHAR2ShortStr (CHAR(STRING_ELT(tmp, 0)), result);
      }
      else {
        warning("To change default behavior of %s, please enter it as string.\n", globName);
        Rprintf("You entered it as %d.\n", TYPEOF(tmp));
        return NULL;
      }
    }
  }
  return res;
} /* getGlobalString */

/* getNonZeroElements
 * return nonzero count for the specified field of a variable or equation
 */
int
getNonZeroElements (gdxHandle_t h, int symIdx, dField_t dField)
{
  int nRecs, changeIdx, i, cnt;
  gdxUelIndex_t uels;
  gdxValues_t values;

  gdxDataReadRawStart (h, symIdx, &nRecs);
  for (cnt = 0, i = 0;  i < nRecs;  i++) {
    gdxDataReadRaw (h, uels, values, &changeIdx);
    if (values[dField] != 0) {
      cnt++;
    }
  }
  return cnt;
} /* getNonZeroElements */

/* interpret the squeeze arg for rgdx as a logical/boolean */
Rboolean
getSqueezeArgRead (SEXP squeeze)
{
  const char *s;

  switch (TYPEOF(squeeze)) {
  case LGLSXP:
    return LOGICAL(squeeze)[0];
    break;
  case INTSXP:
    return INTEGER(squeeze)[0];
    break;
  case REALSXP:
    if (0.0 == REAL(squeeze)[0])
      return FALSE;
    else
      return TRUE;
    break;
  case STRSXP:
    s = CHAR(STRING_ELT(squeeze, 0));
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
} /* getSqueezeArgRead */

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
  shortStringBuf_t msg;
  int rc;

  if (gdxLibraryLoaded())
    return;                     /* all done already */
  rc = gdxGetReady (msg, sizeof(msg));
  if (0 == rc) {
    Rprintf ("Error loading the GDX API\n");
    Rprintf ("%s\n", msg);
    Rprintf ("Hint: try calling igdx() with the name of the GAMS system directory\n");
    Rprintf ("      before calling this routine.\n");
#if defined(_WIN32)
    Rprintf ("      You can also add the GAMS system directory to the PATH.\n");
#elif defined(__APPLE__)
    Rprintf ("      You can also add the GAMS system directory to the PATH\n");
    Rprintf ("      and to DYLD_LIBRARY_PATH.\n");
#else
    Rprintf ("      You can also add the GAMS system directory to the PATH\n");
    Rprintf ("      and to LD_LIBRARY_PATH.\n");
#endif
    error ("Error loading the GDX API: %s", msg);
  }
  return;
} /* loadGDX */

/* makeStrVec
 * converts the input vector of ints or reals into strings
 * both inExp and outExp are assumed to be allocated on input
 */
void
makeStrVec (SEXP outExp, SEXP inExp)
{
  int  len;
  char buf[256];
  double *doubleData;
  int *intData;
  int k;

  len = length(inExp);
  if (TYPEOF(inExp) == REALSXP) {
    doubleData = REAL(inExp);
    for (k = 0; k < len; k++) {
      sprintf(buf, "%g", doubleData[k]);
      SET_STRING_ELT(outExp, k, mkChar(buf));
    }
  }
  else if (TYPEOF(inExp) == INTSXP) {
    intData = INTEGER(inExp);
    for (k = 0; k < len; k++) {
      sprintf(buf, "%i", intData[k]);
      SET_STRING_ELT(outExp, k, mkChar(buf));
    }
  }
  else if (TYPEOF(inExp) == STRSXP) {
    for (k = 0; k < len; k++) {
      SET_STRING_ELT(outExp, k, duplicate(STRING_ELT(inExp, k)));
    }
  }

  checkForDuplicates (outExp);

  return;
} /* makeStrVec */

/* sparseToFull: from input data in sparse for, create output data in full form
 * spVal: input .val matrix in sparse form
 * uelLists: .uels for symbol
 * fullVal: output .val matrix in full form
 */
void
sparseToFull (SEXP spVal, SEXP fullVal, SEXP uelLists,
              int symType, int nRec, int symDim)
{
  int k, iRec;
  int card[GLOBAL_MAX_INDEX_DIM];
  double *p, *pFull;
  int index;
#if 1
  int ii;
#else
  int stride;
#endif

  /* step 1: loop over full matrix and set every value as 0 */
  pFull = REAL(fullVal);
#if 0
  for (k = 0;  k < length(fullVal);  k++) {
    pFull[k] = 0;
  }
#else
  (void) memset (pFull, 0, length(fullVal) * sizeof(*pFull));
#endif

  /* N.B.: R stores matrices column-wise, i.e. left index moving fastest */
  /* step 2: loop over each row/nonzero of sparse matrix to populate full matrix */
  p = REAL(spVal);
  for (k = 0;  k < symDim;  k++) {
    card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
  }

  for (iRec = 0;  iRec < nRec;  iRec++) {
#if 0
    index = 0;
    stride = 1;              /* something like Horner's method here */
    for (k = 0; k < symDim; k++) {
      index = index + (p[iRec + nRec*k] - 1)*stride;
      stride *= card[k];
    }
#else
    ii = iRec + nRec*(symDim-1);
    for (index = p[ii]-1, k = symDim-2;  k >= 0;  k--) {
      ii -= nRec;
      index = (index * card[k]) + p[ii] - 1;
    }
#endif
    if (symType != dt_set) {
      pFull[index] = p[iRec + nRec*symDim];
    }
    else {
      pFull[index] = 1;
    }
  }
  return;
} /* sparseToFull */

