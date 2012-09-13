/* rgdx.c
 * code for gdxrrw::rgdx
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



/* checkRgdxList: checks the input requestList for valid data
 * and updates the read specifier
 */
static void
checkRgdxList (const SEXP lst, rSpec_t *rSpec, int *protectCnt)
{
  SEXP lstNames, tmp, tmpUel;
  SEXP bufferUel;
  int i, j, found;
  int nElements;                /* number of elements in lst */
  const char *tmpName;
  const char *elmtName;         /* list element name */
  Rboolean compress = NA_LOGICAL;

  nElements = length(lst);
  /* check maximum number of elements */
  if (nElements < 1 || nElements > 7) {
    error("Incorrect number of elements in input list argument.");
  }

  lstNames = getAttrib(lst, R_NamesSymbol);

  if (lstNames == R_NilValue) {
    Rprintf("Input list must be named\n");
    Rprintf("Valid names are: 'name', 'dim', 'uels', 'form', 'compress', 'field', 'te', 'ts'.\n");
    error("Please try again with named input list.\n");
  }
  for (i = 0;  i < nElements;  i++) {
    elmtName = CHAR(STRING_ELT(lstNames, i));
    /* Checking for valid list element names */
    if ( !((0 == strcmp("name", elmtName ))
           || (0 == strcmp("dim", elmtName ))
           || (0 == strcmp("uels", elmtName ))
           || (0 == strcmp("form", elmtName ))
           || (0 == strcmp("compress", elmtName ))
           || (0 == strcmp("field", elmtName ))
           || (0 == strcmp("te", elmtName ))
           || (0 == strcmp("ts", elmtName ))
           ) ) {
      Rprintf ("Input list elements must be according to this specification:\n");
      Rprintf ("'name', 'dim', 'uels', 'form', 'compress', 'field', 'te', 'ts'.\n");
      error("Incorrect type of input list element '%s' specified.",
            elmtName);
    }
  }

  /* Checking list element "name" */
  for (found = 0, i = 0;  i < nElements;  i++) {
    if (strcmp("name", CHAR(STRING_ELT(lstNames, i))) == 0) {
      found = 1;
      break;
    }
  }
  if (found) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) == STRSXP) {
      checkStringLength( CHAR(STRING_ELT(tmp, 0)) );
      strcpy (rSpec->name, CHAR(STRING_ELT(tmp, 0)) );
    }
    else {
      Rprintf ("List element 'name' must be a string - found %d instead\n",
               TYPEOF(tmp) );
      error("Input list element 'name' must be string.\n");
    }
  }
  else {
    error("Required list element 'name' is missing. Please try again.\n" );
  }

  /* Checking for list element 'form'. Default to 'sparse' if not found */
  for (found = 0, i = 0;  i < nElements;  i++) {
    if (strcmp("form", CHAR(STRING_ELT(lstNames, i))) == 0) {
      found = 1;
      break;
    }
  }
  if (found) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) != STRSXP ) {
      Rprintf ("List element 'form' must be a string - found %d instead\n",
               TYPEOF(tmp) );
      error("Input list element 'form' must be string");
    }
    tmpName = CHAR(STRING_ELT(tmp, 0));
    if (strlen(tmpName) == 0) {
      error("Input list element 'form' must be either 'full' or 'sparse'.");
    }
    if (0 == strcasecmp("full", tmpName)) {
      rSpec->dForm = full;
    }
    else if (0 == strcasecmp("sparse", tmpName)) {
      rSpec->dForm = sparse;
    }
    else {
      error("Input list element 'form' must be either 'full' or 'sparse'.");
    }
  }

  /* Checking for list element 'compress'. Default to 'false' if not found */
  for (found = 0, i = 0;  i < nElements;  i++) {
    if (strcmp("compress", CHAR(STRING_ELT(lstNames, i))) == 0) {
      found = 1;
      break;
    }
  }
  if (found) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) == STRSXP) {
      tmpName = CHAR(STRING_ELT(tmp, 0));
      if (strlen(tmpName) == 0) {
        error("Input list element 'compress' must be either 'true' or 'false'.");
      }
      if (0 == strcasecmp("true", tmpName)) {
        rSpec->compress = 1;
      }
      else if (0 == strcasecmp("false", tmpName)) {
        rSpec->compress = 0;
      }
      else {
        error("Input list element 'compress' must be either 'true' or 'false'.");
      }
    }
    else if (TYPEOF(tmp) == LGLSXP) {
      compress = LOGICAL(tmp)[0];
      if (compress == TRUE) {
        rSpec->compress = 1;
      }
      else {
        rSpec->compress = 0;
      }
    }
    else {
      Rprintf ("List element 'compress' must be either string or logical - found %d instead\n",
               TYPEOF(tmp) );
      error("Input list element 'compress' must be either string or logical");
    }
  }

  /* Checking for list element 'field'.  Default to 'level' if not found */
  for (found = 0, i = 0;  i < nElements;  i++) {
    if (strcmp("field", CHAR(STRING_ELT(lstNames, i))) == 0) {
      found = 1;
      break;
    }
  }
  if (found) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) != STRSXP ) {
      Rprintf ("List element 'field' must be a string - found %d instead\n",
               TYPEOF(tmp) );
      error("Input list element 'field' must be string");
    }
    tmpName = CHAR(STRING_ELT(tmp, 0));
    if (strlen(tmpName) == 0) {
      error("Input list element 'field' must be from 'l', 'm', 'lo', 'up' or 's'.");
    }
    rSpec->withField = 1;
    if (0 == strcasecmp("l", tmpName)) {
      rSpec->dField = level;
    }
    else if (0 == strcasecmp("m", tmpName)) {
      rSpec->dField = marginal;
    }
    else if (0 == strcasecmp("lo", tmpName)) {
      rSpec->dField = lower;
    }
    else if (0 == strcasecmp("up", tmpName)) {
      rSpec->dField = upper;
    }
    else if (0 == strcasecmp("s", tmpName)) {
      rSpec->dField = scale;
    }
    else {
      error("Input list element 'field' must be from 'l', 'm', 'lo', 'up' or 's'.");
    }
  }

  /* Checking for list element 'ts'.  Default to 'false' if not found */
  for (found = 0, i = 0;  i < nElements;  i++) {
    if (strcmp("ts", CHAR(STRING_ELT(lstNames, i))) == 0) {
      found = 1;
      break;
    }
  }
  if (found) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) == STRSXP ) {
      tmpName = CHAR(STRING_ELT(tmp, 0));
      if (strlen(tmpName) == 0) {
        error("Input list element 'ts' must be either 'true' or 'false'.");
      }
      if (0 == strcasecmp("true", tmpName)) {
        rSpec->ts = 1;
      }
      else if (0 == strcasecmp("false", tmpName)) {
        rSpec->ts = 0;
      }
      else {
        error("Input list element 'ts' must be either 'true' or 'false'.");
      }
    }
    else if (TYPEOF(tmp) == LGLSXP) {
      if (LOGICAL(tmp)[0] == TRUE) {
        rSpec->ts = 1;
      }
    }
    else {
      Rprintf ("List element 'ts' must be either string or logical - found %d instead\n",
               TYPEOF(tmp) );
      error("Input list element 'ts' must be either string or logical");
    }
  }

  /* Checking for list element 'te'. Default to 'false' if not found */
  for (found = 0, i = 0;  i < nElements;  i++) {
    if (strcmp("te", CHAR(STRING_ELT(lstNames, i))) == 0) {
      found = 1;
      break;
    }
  }
  if (found) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) == STRSXP ) {
      tmpName = CHAR(STRING_ELT(tmp, 0));
      if (strlen(tmpName) == 0) {
        error("Input list element 'te' must be either 'true' or 'false'.");
      }
      if (0 == strcasecmp("true", tmpName)) {
        rSpec->te = 1;
      }
      else if (0 == strcasecmp("false", tmpName)) {
        rSpec->te = 0;
      }
      else {
        error("Input list element 'te' must be either 'true' or 'false'.");
      }
    }
    else if (TYPEOF(tmp) == LGLSXP) {
      if (LOGICAL(tmp)[0] == TRUE) {
        rSpec->te = 1;
      }
      else {
        rSpec->te = 0;
      }
    }
    else {
      error("Input list element 'te' must be either string or logical"
            " - found %d instead\n", TYPEOF(tmp));
    }
  }

  /* Checking for list element 'uels'.  Used in filtered read */
  for (found = 0, i = 0;  i < nElements;  i++) {
    if (strcmp("uels", CHAR(STRING_ELT(lstNames, i))) == 0) {
      found = 1;
      break;
    }
  }
  if (found) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) != VECSXP) {
      error("List element 'uels' must be a list.");
    }
    else {
      PROTECT(rSpec->filterUel = allocVector(VECSXP, length(tmp)));
      ++*protectCnt;
      for (j = 0; j < length(tmp); j++) {
        tmpUel = VECTOR_ELT(tmp, j);
        if (tmpUel == R_NilValue) {
          error("Empty Uel is not allowed");
        }
        else {
          PROTECT(bufferUel = allocVector(STRSXP, length(tmpUel)));
          /* Convert to output */
          makeStrVec (bufferUel, tmpUel);
          SET_VECTOR_ELT(rSpec->filterUel, j, bufferUel);
          UNPROTECT(1);         /* bufferUel */
        }
      }
      rSpec->withUel = 1;
    }
  }
} /* checkRgdxList */



/* rgdx: gateway function for reading gdx, called from R via .External
 * first argument <- gdx file name
 * second argument <- requestList containing several elements
 * that make up a read specifier, e.g. symbol name, dim, form, etc
 * third argument <- squeeze specifier
 * ------------------------------------------------------------------ */
SEXP rgdx (SEXP args)
{
  const char *funcName = "rgdx";
  SEXP fileName, requestList, squeeze, UEList;
  SEXP outName = R_NilValue,
    outType = R_NilValue,
    outDim = R_NilValue,
    compVal = R_NilValue,
    compFullVal = R_NilValue,
    compForm = R_NilValue,
    compUels = R_NilValue,
    compField = R_NilValue,
    compTs = R_NilValue,
    compTe = R_NilValue;
  SEXP outListNames, outList, dimVect, textElement, elVect;
  FILE    *fin;
  rSpec_t *rSpec;
  gdxUelIndex_t uels;
  gdxValues_t values;
  gdxSVals_t sVals;
  d64_t d64;
  double dt, posInf, negInf;
  shortStringBuf_t msgBuf;
  shortStringBuf_t uelName;
  const char *uelElementName;
  shortStringBuf_t gdxFileName;
  int symIdx, symDim, symType;
  int rc, errNum, ACount, mrows, ncols, nUEL, iUEL;
  int  k, kk, iRec, nRecs, index, changeIdx, kRec;
  int rgdxAlloc;                /* PROTECT count: undo this many on exit */
  int UELUserMapping, highestMappedUEL;
  int arglen,  maxPossibleElements, z, b, matched, sparesIndex;
  double *p, *dimVal, *dimElVect;
  char buf[3*sizeof(shortStringBuf_t)];
  char strippedID[GMS_SSSIZE];
  char symName[GMS_SSSIZE];
  char sText[GMS_SSSIZE], msg[GMS_SSSIZE], stringEle[GMS_SSSIZE];
  char *types[] = {"set", "parameter", "variable", "equation"};
  char *forms[] = {"full", "sparse"};
  char *fields[] = {"l", "m", "up", "lo", "s"};
  int nField, defaultIndex, elementIndex, IDum, ndimension, totalElement;
  int *returnedIndex;
  int withList = 0;
  int outElements;
  int mwNElements =0;
  int uelPos;
  Rboolean zeroSqueeze = NA_LOGICAL;

  /* setting intial values */
  kRec = 0;
  rgdxAlloc = 0;
  maxPossibleElements = 0; /* this just to shut up compiler warnings */

  /* first arg is function name - ignore it */
  arglen = length(args);

  /* ----------------- Check proper number of inputs and outputs ------------
   * Function should follow specification of
   * rgdx ('gdxFileName', requestList = NULL, squeeze = TRUE)
   * ------------------------------------------------------------------------ */
  if (4 != arglen) {
    error ("usage: %s(gdxName, requestList = NULL, squeeze = TRUE) - incorrect arg count", funcName);
  }
  fileName = CADR(args);
  requestList = CADDR(args);
  squeeze = CADDDR(args);
  if (TYPEOF(fileName) != STRSXP) {
    error ("usage: %s(gdxName, requestList = NULL) - gdxName must be a string", funcName);
  }
  if (TYPEOF(requestList) == NILSXP)
    withList = 0;
  else {
    withList = 1;
    if (TYPEOF(requestList) != VECSXP) {
      error ("usage: %s(gdxName, requestList, squeeze) - requestList must be a list", funcName);
    }
  }

  (void) CHAR2ShortStr (CHAR(STRING_ELT(fileName, 0)), gdxFileName);

  if (! withList) {
    if (0 == strcmp("?", gdxFileName)) {
      int n = (int)strlen (ID);
      memcpy (strippedID, ID+1, n-2);
      strippedID[n-2] = '\0';
      Rprintf ("R-file source info: %s\n", strippedID);
      return R_NilValue;
    } /* if audit run */
  } /* if one arg, of character type */

  zeroSqueeze = getSqueezeArgRead (squeeze);
  if (NA_LOGICAL == zeroSqueeze) {
    error ("usage: %s(gdxName, requestList, squeeze = TRUE)\n    squeeze argument could not be interpreted as logical", funcName);
  }

  /* ------------------- check if the GDX file exists --------------- */
  checkFileExtension (gdxFileName);
  fin = fopen (gdxFileName, "r");
  if (fin==NULL) {
    error ("GDX file '%s' not found\n", gdxFileName);
  }
  fclose(fin);
  /*-------------------- Checking data for input list ------------*/
  /* Setting default values */
  rSpec = malloc(sizeof(*rSpec));
  memset (rSpec, 0, sizeof(*rSpec));
  rSpec->dForm = sparse;
  rSpec->dField = level;

  if (withList) {
    checkRgdxList (requestList, rSpec, &rgdxAlloc);
    if (rSpec->compress == 1 && rSpec->withUel == 1) {
      error("Compression is not allowed with input UEL\n");
    }
  }

  loadGDX();
  rc = gdxCreate (&gdxHandle, msgBuf, sizeof(msgBuf));
  if (0 == rc)
    error ("Error creating GDX object: %s", msgBuf);
  rc = gdxOpenRead (gdxHandle, gdxFileName, &errNum);
  if (errNum || 0 == rc) {
    error("Could not open gdx file with gdxOpenRead");
  }

  gdxGetSpecialValues (gdxHandle, sVals);
  d64.u64 = 0x7fffffffffffffff; /* positive QNaN, mantissa all on */
  sVals[GMS_SVIDX_UNDEF] = d64.x;
  sVals[GMS_SVIDX_NA] = NA_REAL;
  dt = 0.0;
  posInf =  1 / dt;
  negInf = -1 / dt;
  sVals[GMS_SVIDX_EPS] = 0;
  sVals[GMS_SVIDX_PINF] = posInf;
  sVals[GMS_SVIDX_MINF] = negInf;
  gdxSetSpecialValues (gdxHandle, sVals);

  /* read symbol name only if input list is present */
  if (withList) {
    /* start searching for symbol */
    rc = gdxFindSymbol (gdxHandle, rSpec->name, &symIdx);
    if (! rc) {
      sprintf (buf, "GDX file %s contains no symbol named '%s'\n",
               gdxFileName,
               rSpec->name );
      error (buf);
    }
    gdxSymbolInfo (gdxHandle, symIdx, symName, &symDim, &symType);
    if (rSpec->ts == 1) {
      gdxSymbolInfoX(gdxHandle, symIdx, &ACount, &rc, sText);
    }

    /* checking that symbol is of type parameter/set/equaltion/variable */
    if (!(symType == dt_par || symType == dt_set || symType == dt_var || symType == dt_equ)) {
      sprintf(buf, "GDX symbol %s (index=%d, symDim=%d, symType=%d)"
              " is not recognized as set, parameter, variable, or equation\n",
              rSpec->name, symIdx, symDim, symType);
      error(buf);
    }
    else if ((symType == dt_par || symType == dt_set) && rSpec->withField == 1) {
      error("Symbol '%s' is either set or parameter that can't have field.",
            rSpec->name);
    }
    if (rSpec->te == 1 && symType != dt_set) {
      error("Text elements only exist for set and symbol '%s' is not a set.",
            rSpec->name);
    }
  } /* if (withList) */

  /* Get global UEL from GDX file */
  (void) gdxUMUelInfo (gdxHandle, &nUEL, &highestMappedUEL);
  PROTECT(UEList = allocVector(STRSXP, nUEL));
  rgdxAlloc++;
  for (iUEL = 1;  iUEL <= nUEL;  iUEL++) {
    if (!gdxUMUelGet (gdxHandle, iUEL, uelName, &UELUserMapping)) {
      error("Could not gdxUMUelGet");
    }
    SET_STRING_ELT(UEList, iUEL-1, mkChar(uelName));
  }

  outElements = 6;   /* outList has at least 6 elements, maybe more */
  if (withList) { /* aa */
    /* Checking dimension of input uel and parameter in GDX file.
     * If they are not equal then error. */

    if (rSpec->withUel == 1 && length(rSpec->filterUel) != symDim) {
      error("Dimension of UEL entered does not match with symbol in GDX");
    }
    /* Creating default uel if none entered */
    if (! rSpec->withUel) {
      PROTECT(compUels = allocVector(VECSXP, symDim));
      rgdxAlloc++;
      for (defaultIndex = 0; defaultIndex < symDim; defaultIndex++) {
        SET_VECTOR_ELT(compUels, defaultIndex, UEList);
      }
    }

    /* Start reading data */
    gdxDataReadRawStart (gdxHandle, symIdx, &nRecs);
    /* if it is a parameter, add 1 to the dimension */
    mrows = nRecs;
    if (symType != dt_set) {
      ncols = symDim+1;
    }
    else {
      ncols = symDim;
    }

    /* TODO: filter UEL */
    /* this is to check total number of elements that matches
     * in Input UEL. Then create a 2D double matrix for sparse format.
     * compute total number of elements matched in Input UEL.
     */
    mwNElements = 0;

    if (rSpec->withUel == 1) {
      maxPossibleElements = 1;
      for (z = 0; z < symDim; z++) {
        mwNElements = length(VECTOR_ELT(rSpec->filterUel, z));
        maxPossibleElements = maxPossibleElements*mwNElements;
      }
      mwNElements = 0;

      for (iRec = 0;  iRec < nRecs;  iRec++) {
        gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
        b = 0;
        for (k = 0; k < symDim; k++) {
          uelElementName = CHAR(STRING_ELT(UEList, uels[k]-1));
          uelPos = findInFilter (k, rSpec->filterUel, uelElementName);
          /* uel element exists */
          if (uelPos > 0) {
            b++;
          }
          else {
            break;
          }
        }
        if (b == symDim) {
          mwNElements++;
          if (mwNElements == maxPossibleElements) {
            break;
          }
        }
      }
      iRec = 0;
      k = 0;
    }
    textElement = R_NilValue;
    /* Allocating memory for 2D sparse matrix */
    if (rSpec->withUel == 1) {
      PROTECT(compVal = allocMatrix(REALSXP, mwNElements, ncols));
      rgdxAlloc++;
      if (rSpec->te && symType == dt_set) {
        PROTECT(textElement = allocVector(STRSXP, mwNElements));
        rgdxAlloc++;
      }
    }
    if (rSpec->withUel == 0) {
      /*  check for non zero elements for variable and equation */
      if ((symType == dt_var || symType == dt_equ) && zeroSqueeze) {
        mrows = getNonZeroElements(gdxHandle, symIdx, rSpec->dField);
      }
      /* Creat 2D sparse R array */
      PROTECT(compVal = allocMatrix(REALSXP, mrows, ncols));
      rgdxAlloc++;
      if (rSpec->te && symType == dt_set) {
        PROTECT(textElement = allocVector(STRSXP, mrows));
        rgdxAlloc++;
      }
    }

    p = REAL(compVal);
    /* TODO/TEST: filtered read */
    if (rSpec->withUel == 1) {
      matched = 0;
      gdxDataReadRawStart (gdxHandle, symIdx, &nRecs);
      /* TODO/TEST: text elements with UEL */
      if (rSpec->te == 1) {
        returnedIndex = malloc(symDim*sizeof(*returnedIndex));
        for (iRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          index = 0;
          b = 0;
          for (k = 0;  k < symDim;  k++) {
            returnedIndex[k] = 0;
            uelElementName = CHAR(STRING_ELT(UEList, uels[k]-1));
            uelPos = findInFilter (k, rSpec->filterUel, uelElementName);
            if (uelPos > 0) {
              returnedIndex[k] = uelPos;
              b++;
            }
            else {
              break;
            }
          }
          if (b == symDim) {
            for (sparesIndex = 0; sparesIndex < symDim; sparesIndex++ ) {
              p[matched + sparesIndex*mwNElements] = returnedIndex[sparesIndex];
            }

            index = matched + symDim*(int)mwNElements;

            if (values[GMS_VAL_LEVEL]) {
              elementIndex = (int) values[GMS_VAL_LEVEL];
              gdxGetElemText(gdxHandle, elementIndex, msg, &IDum);
              SET_STRING_ELT(textElement, matched, mkChar(msg));
            }
            else {
              strcpy(stringEle, "");
              for (kk = 0;  kk < symDim;  kk++) {
                strcat(stringEle, CHAR(STRING_ELT(UEList, uels[kk]-1))  );
                if (kk != symDim-1) {
                  strcat(stringEle, ".");
                }
              }
              SET_STRING_ELT(textElement, matched, mkChar(stringEle));
              kk = 0;
            }
            matched = matched +1;
          }
          if (matched == maxPossibleElements) {
            break;
          }
        }
        free(returnedIndex);
      } /* End of rSpec->te == 1 */
      else {
        returnedIndex = malloc(symDim*sizeof(*returnedIndex));
        for (iRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          index = 0;
          b = 0;

          for (k = 0;  k < symDim;  k++) {
            returnedIndex[k] = 0;
            uelElementName = CHAR(STRING_ELT(UEList, uels[k]-1));
            uelPos = findInFilter (k, rSpec->filterUel, uelElementName);
            if (uelPos > 0) {
              returnedIndex[k] = uelPos;
              b++;
            }
            else {
              break;
            }
          }
          if (b == symDim) {
            for (sparesIndex = 0; sparesIndex < symDim; sparesIndex++ ) {
              p[matched + sparesIndex*mwNElements] = returnedIndex[sparesIndex];
            }
            index = matched + symDim*(int)mwNElements;
            matched = matched +1;

            if (symType != dt_set)
              p[index] = values[rSpec->dField];
          }
          if (matched == maxPossibleElements) {
            break;
          }
        }
        free(returnedIndex);
      } /* End of else of if (te) */
    } /* End of with uels */
    else {
      if (symType == dt_var || symType == dt_equ ) {
        gdxDataReadRawStart (gdxHandle, symIdx, &nRecs);
      }
      /* text elements */
      if (rSpec->te == 1) {
        for (iRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          if (values[GMS_VAL_LEVEL]) {
            elementIndex = (int) values[GMS_VAL_LEVEL];
            gdxGetElemText(gdxHandle, elementIndex, msg, &IDum);
            SET_STRING_ELT(textElement, iRec, mkChar(msg));
          }
          else {
            strcpy(stringEle, "");
            for (kk = 0;  kk < symDim;  kk++) {
              strcat(stringEle, CHAR(STRING_ELT(UEList, uels[kk]-1))  );
              if (kk != symDim-1) {
                strcat(stringEle, ".");
              }
            }
            SET_STRING_ELT(textElement, iRec, mkChar(stringEle));
            kk = 0;
          }
          for (kk = 0;  kk < symDim;  kk++) {
            p[iRec+kk*mrows] = uels[kk];
          }
        }  /* loop over GDX records */
      }    /* inputdata->te = 1: must be a set */
      else {
        for (iRec = 0, kRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          if ((dt_set == symType) ||
              (! zeroSqueeze) ||
              (0 != values[rSpec->dField])) {
            /* store the value */
            for (kk = 0;  kk < symDim;  kk++) {
              p[kRec + kk*mrows] = uels[kk];
            }
            index = kRec + symDim*mrows;
            kRec++;
            if (symType != dt_set)
              p[index] = values[rSpec->dField];
          } /* end of if (set || val != 0) */
        } /* loop over GDX records */
        if (kRec < mrows) {
          SEXP newCV, tmp;
          double *newp;
          double *from, *to;

          PROTECT(newCV = allocMatrix(REALSXP, kRec, ncols));
          newp = REAL(newCV);
          for (kk = 0;  kk <= symDim;  kk++) {
            from = p    + kk*mrows;
            to   = newp + kk*kRec;
            MEMCPY (to, from, sizeof(*p)*kRec);
          }
          tmp = compVal;
          compVal = newCV;
          UNPROTECT_PTR(tmp);
          mrows = kRec;
        }
      }
    }

    /* Converting data into its compressed form. */
    if (rSpec->compress == 1) {
      PROTECT(compUels = allocVector(VECSXP, symDim));
      rgdxAlloc++;
      compressData (compVal, UEList, compUels, nUEL, symDim, mrows);
    }
    /* TODO/TEST: create full dimensional string matrix */
    if (rSpec->te == 1) {
      if (symDim == 1) {
        PROTECT(elVect = allocVector(REALSXP, 2));
        rgdxAlloc++;

        dimElVect = REAL(elVect);
        dimElVect[0] = length(VECTOR_ELT(compUels, 0));
        dimElVect[1] = 1;

        PROTECT(compTe = allocVector(STRSXP, length(VECTOR_ELT(compUels, 0)) ));
        rgdxAlloc++;

        compTe = createElementMatrix(compVal, textElement, compTe, compUels, symDim, mrows);
        setAttrib(compTe, R_DimSymbol, elVect);
      }
      else {
        ndimension = 0;
        PROTECT(elVect = allocVector(REALSXP, symDim));
        rgdxAlloc++;
        totalElement = 1;
        dimElVect = REAL(elVect);
        if (rSpec->withUel) {
          for (ndimension = 0; ndimension < symDim; ndimension++) {
            dimElVect[ndimension] = length(VECTOR_ELT(rSpec->filterUel, ndimension));
            totalElement = (totalElement * length(VECTOR_ELT(rSpec->filterUel, ndimension)));
          }
          PROTECT(compTe = allocVector(STRSXP, totalElement));
          rgdxAlloc++;
          compTe = createElementMatrix(compVal, textElement, compTe, rSpec->filterUel, symDim, mwNElements);
          setAttrib(compTe, R_DimSymbol, elVect);
        }
        else {
          for (ndimension = 0; ndimension < symDim; ndimension++) {
            dimElVect[ndimension] = length(VECTOR_ELT(compUels, ndimension));
            totalElement = (totalElement * length(VECTOR_ELT(compUels, ndimension)));
          }
          PROTECT(compTe = allocVector(STRSXP, totalElement));
          rgdxAlloc++;
          compTe = createElementMatrix(compVal, textElement, compTe, compUels, symDim, mrows);
          setAttrib(compTe, R_DimSymbol, elVect);
        }
      }
    }

    /* Converting sparse data into full matrix */
    if (rSpec->dForm == full) {
      switch (symDim) {
      case 0: {
        PROTECT(compFullVal = allocVector(REALSXP, 1));
        rgdxAlloc++;
        if (compVal != R_NilValue && REAL(compVal) != NULL) {
          REAL(compFullVal)[0] = REAL(compVal)[0];
        }
        else {
          REAL(compFullVal)[0] = 0;
        }
        break;
      }
      case 1: {
        PROTECT(dimVect = allocVector(REALSXP, 2));
        rgdxAlloc++;
        dimVal = REAL(dimVect);

        if (rSpec->withUel == 1) {
          dimVal[0] = length(VECTOR_ELT(rSpec->filterUel, 0));
          PROTECT(compFullVal = allocVector(REALSXP, length(VECTOR_ELT(rSpec->filterUel, 0))));
          rgdxAlloc++;
          compFullVal = sparseToFull(compVal, compFullVal, rSpec->filterUel, symType, mwNElements, symDim);
        }
        else {
          dimVal[0] = length(VECTOR_ELT(compUels, 0));
          PROTECT(compFullVal = allocVector(REALSXP, length(VECTOR_ELT(compUels, 0))));
          rgdxAlloc++;
          compFullVal = sparseToFull(compVal, compFullVal, compUels, symType, mrows, symDim);
        }
        dimVal[1] = 1;
        setAttrib(compFullVal, R_DimSymbol, dimVect);
        break;
      }
      default: {
        PROTECT(dimVect = allocVector(REALSXP, symDim));
        rgdxAlloc++;
        totalElement = 1;
        dimVal = REAL(dimVect);
        ndimension = 0;
        if (rSpec->withUel == 1) {
          for (ndimension = 0; ndimension < symDim; ndimension++) {
            dimVal[ndimension] = length(VECTOR_ELT(rSpec->filterUel, ndimension));
            totalElement = (totalElement * length(VECTOR_ELT(rSpec->filterUel, ndimension)));
          }
        }
        else {
          for (ndimension = 0; ndimension < symDim; ndimension++) {
            dimVal[ndimension] = length(VECTOR_ELT(compUels, ndimension));
            totalElement = (totalElement * length(VECTOR_ELT(compUels, ndimension)));
          }
        }
        PROTECT(compFullVal = allocVector(REALSXP, totalElement));
        rgdxAlloc++;
        if (rSpec->withUel ==1) {
          compFullVal = sparseToFull(compVal, compFullVal, rSpec->filterUel, symType, mwNElements, symDim);
        }
        else {
          compFullVal = sparseToFull(compVal, compFullVal, compUels, symType, mrows, symDim);
        }

        setAttrib(compFullVal, R_DimSymbol, dimVect);
        break;
      }
      } /* switch(symDim) */
    }
  } /* if (withList) aa */

  if (withList) { /* bb */
    /* Creating output string for symbol name */
    PROTECT(outName = allocVector(STRSXP, 1) );
    SET_STRING_ELT(outName, 0, mkChar(symName));
    rgdxAlloc++;
    /* Creating output string for symbol type */
    PROTECT(outType = allocVector(STRSXP, 1) );
    rgdxAlloc++;
    switch (symType) {
    case dt_set:
      SET_STRING_ELT(outType, 0, mkChar(types[0]) );
      break;
    case dt_par:
      SET_STRING_ELT(outType, 0, mkChar(types[1]) );
      break;
    case dt_var:
      SET_STRING_ELT(outType, 0, mkChar(types[2]) );
      break;
    case dt_equ:
      SET_STRING_ELT(outType, 0, mkChar(types[3]) );
      break;
    default:
      error("Unrecognized type of symbol found.");
    }

    /* Creating int vector for symbol dim */
    PROTECT(outDim = allocVector(INTSXP, 1) );
    INTEGER(outDim)[0] = symDim;
    rgdxAlloc++;
    /* Creating string vector for val data form */
    PROTECT(compForm = allocVector(STRSXP, 1) );
    rgdxAlloc++;
    if (rSpec->dForm == full) {
      SET_STRING_ELT(compForm, 0, mkChar(forms[0]));
    }
    else {
      SET_STRING_ELT(compForm, 0, mkChar(forms[1]));
    }


    /* Create a string vector for symbol field */
    if (symType == dt_var || symType == dt_equ) {
      outElements++;
      PROTECT(compField = allocVector(STRSXP, 1));
      rgdxAlloc++;
      switch(rSpec->dField) {
      case level:
        SET_STRING_ELT(compField, 0, mkChar( fields[0] ));
        break;
      case marginal:
        SET_STRING_ELT(compField, 0, mkChar( fields[1] ));
        break;
      case upper:
        SET_STRING_ELT(compField, 0, mkChar( fields[2] ));
        break;
      case lower:
        SET_STRING_ELT(compField, 0, mkChar( fields[3] ));
        break;
      case scale:
        SET_STRING_ELT(compField, 0, mkChar( fields[4] ));
        break;
      default:
        error("Unrecognized type of symbol found.");
      }
    }
    if (rSpec->ts) {
      outElements++;
      PROTECT(compTs = allocVector(STRSXP, 1));
      rgdxAlloc++;
      SET_STRING_ELT(compTs, 0, mkChar(sText));
    }
    if (rSpec->te) {
      outElements++;
    }
  } /* if (withList) bb */
  else {
    /* no requestList was input, so returning universe */
    /* Creating output string symbol name */
    PROTECT(outName = allocVector(STRSXP, 1));
    SET_STRING_ELT(outName, 0, mkChar("*"));
    rgdxAlloc++;
    /* Creating output string for symbol type */
    PROTECT(outType = allocVector(STRSXP, 1));
    rgdxAlloc++;
    SET_STRING_ELT(outType, 0, mkChar(types[0]));
    /* Creating int vector for symbol dim */
    PROTECT(outDim = allocVector(INTSXP, 1));
    INTEGER(outDim)[0] = 1;
    rgdxAlloc++;
  }

  PROTECT(outListNames = allocVector(STRSXP, outElements));
  rgdxAlloc++;
  /* populating list element names */
  SET_STRING_ELT(outListNames, 0, mkChar("name"));
  SET_STRING_ELT(outListNames, 1, mkChar("type"));
  SET_STRING_ELT(outListNames, 2, mkChar("dim"));
  SET_STRING_ELT(outListNames, 3, mkChar("val"));
  SET_STRING_ELT(outListNames, 4, mkChar("form"));
  SET_STRING_ELT(outListNames, 5, mkChar("uels"));

  nField = 5;
  if (withList) {
    if (symType == dt_var || symType == dt_equ) {
      nField++;
      SET_STRING_ELT(outListNames, nField, mkChar("field"));
    }
    if (rSpec->ts) {
      nField++;
      SET_STRING_ELT(outListNames, nField, mkChar("ts"));
    }
    if (rSpec->te) {
      nField++;
      SET_STRING_ELT(outListNames, nField, mkChar("te"));
    }
  }

  PROTECT(outList = allocVector(VECSXP, outElements));
  rgdxAlloc++;

  /* populating list component vector */
  SET_VECTOR_ELT(outList, 0, outName);
  SET_VECTOR_ELT(outList, 1, outType);
  SET_VECTOR_ELT(outList, 2, outDim);
  if (withList) {
    if (rSpec->dForm == full) {
      SET_VECTOR_ELT(outList, 3, compFullVal);
    }
    else {
      SET_VECTOR_ELT(outList, 3, compVal);
    }
    SET_VECTOR_ELT(outList, 4, compForm);
    if (rSpec->withUel) {
      SET_VECTOR_ELT(outList, 5, rSpec->filterUel);
    }
    else {
      SET_VECTOR_ELT(outList, 5, compUels);
    }

    nField = 5;
    if (symType == dt_var || symType == dt_equ) {
      nField++;
      SET_VECTOR_ELT(outList, nField, compField);
    }
    if (rSpec->ts) {
      nField++;
      SET_VECTOR_ELT(outList, nField, compTs);
    }
    if (rSpec->te) {
      nField++;
      SET_VECTOR_ELT(outList, nField, compTe);
    }
  }
  else {
    /* no read specifier so return the universe */
    /* entering null values if nothing else makes sense */
    SET_VECTOR_ELT(outList, 3, R_NilValue);
    SET_VECTOR_ELT(outList, 4, R_NilValue);
    SET_VECTOR_ELT(outList, 5, UEList);
  }

  /* Setting attribute name */
  setAttrib(outList, R_NamesSymbol, outListNames);
  /* Releasing allocated memory */
  free(rSpec);
  if (!gdxDataReadDone (gdxHandle)) {
    error ("Could not gdxDataReadDone");
  }
  errNum = gdxClose (gdxHandle);
  if (errNum != 0) {
    error("Errors detected when closing gdx file");
  }
  (void) gdxFree (&gdxHandle);
  UNPROTECT(rgdxAlloc);
  return outList;
} /* End of rgdx */
