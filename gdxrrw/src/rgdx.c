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



/* checkRgdxList: checks the input request list for valid data
 * and updates the read specifier
 */
static void
checkRgdxList (const SEXP lst, rSpec_t *rSpec, int *protectCnt)
{
  SEXP lstNames, tmpUel;
  SEXP bufferUel;
  SEXP compressExp = NULL;      /* from input requestList */
  SEXP dimExp = NULL;           /* from input requestList */
  SEXP fieldExp = NULL;         /* from input requestList */
  SEXP formExp = NULL;          /* from input requestList */
  SEXP nameExp = NULL;          /* from input requestList */
  SEXP teExp = NULL;            /* from input requestList */
  SEXP tsExp = NULL;            /* from input requestList */
  SEXP uelsExp = NULL;          /* from input requestList */
  int i, j;
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

  /* first, check that all names are recognized, reject o/w
   * in the process, store the symbol for direct access later
   */
  for (i = 0;  i < nElements;  i++) {
    elmtName = CHAR(STRING_ELT(lstNames, i));
    if      (strcmp("compress", elmtName) == 0) {
      compressExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("dim", elmtName) == 0) {
      dimExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("field", elmtName) == 0) {
      fieldExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("form", elmtName) == 0) {
      formExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("name", elmtName) == 0) {
      nameExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("te", elmtName) == 0) {
      teExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("ts", elmtName) == 0) {
      tsExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("uels", elmtName) == 0) {
      uelsExp = VECTOR_ELT(lst, i);
    }
    else {
      Rprintf ("Input list elements for rgdx must be according to this specification:\n");
      Rprintf ("'name', 'dim', 'uels', 'form', 'compress', 'field', 'te', 'ts'.\n");
      error("Incorrect type of rgdx input list element '%s' specified.",
            elmtName);
    }
  }

  /* now process the fields provided */

  if (compressExp) {
    if (TYPEOF(compressExp) == STRSXP) {
      tmpName = CHAR(STRING_ELT(compressExp, 0));
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
    else if (TYPEOF(compressExp) == LGLSXP) {
      compress = LOGICAL(compressExp)[0];
      if (compress == TRUE) {
        rSpec->compress = 1;
      }
      else {
        rSpec->compress = 0;
      }
    }
    else {
      Rprintf ("List element 'compress' must be either string or logical - found %d instead\n",
               TYPEOF(compressExp));
      error("Input list element 'compress' must be either string or logical");
    }
  } /* if compressExp */

  if (dimExp) {
    if (INTSXP == TYPEOF(dimExp)) {
      if (length(dimExp) != 1) {
        error ("Optional input list element 'dim' must have only one element.\n");
      }
      if (INTEGER(dimExp)[0] < 0) {
        error("Negative value is not allowed as valid input for 'dim'.\n");
      }
      rSpec->dim = INTEGER(dimExp)[0];
    }
    else if (REALSXP == TYPEOF(dimExp)) {
      if (length(dimExp) != 1) {
        error ("Optional input list element 'dim' must have only one element.\n");
      }
      if (REAL(dimExp)[0] < 0) {
        error("Negative value is not allowed as valid input for 'dim'.\n");
      }
      rSpec->dim = (int) REAL(dimExp)[0];
      if (REAL(dimExp)[0] != rSpec->dim) {
        error("Non-integer value is not allowed as valid input for 'dim'.\n");
      }
    }
    else {
      Rprintf ("List element 'dim' must be numeric - found %d instead\n",
               TYPEOF(dimExp));
      error ("Optional input list element 'dim' must be numeric.\n");
    }
  } /* dimExp */

  if (fieldExp) {
    if (TYPEOF(fieldExp) != STRSXP ) {
      Rprintf ("List element 'field' must be a string - found %d instead\n",
               TYPEOF(fieldExp));
      error("Input list element 'field' must be string");
    }
    tmpName = CHAR(STRING_ELT(fieldExp, 0));
    if (strlen(tmpName) == 0) {
      error("Input list element 'field' must be in ['l','m','lo','up','s'].");
    }
    rSpec->withField = 1;
    if      (0 == strcasecmp("l", tmpName)) {
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
  } /* if fieldExp */

  if (formExp) {
    if (STRSXP != TYPEOF(formExp)) {
      Rprintf ("List element 'form' must be a string - found %d instead\n",
               TYPEOF(formExp));
      error ("Input list element 'form' must be string");
    }
    tmpName = CHAR(STRING_ELT(formExp, 0));
    if (strcasecmp("full", tmpName) == 0) {
      rSpec->dForm = full;
    }
    else if (strcasecmp("sparse", tmpName) == 0) {
      rSpec->dForm = sparse;
    }
    else {
      error("Input list element 'form' must be either 'full' or 'sparse'.");
    }
  } /* formExp */

  if (NULL == nameExp)
    error ("Required list element 'name' is missing. Please try again.\n" );
  if (TYPEOF(nameExp) != STRSXP) {
    Rprintf ("List element 'name' must be a string - found %d instead\n",
             TYPEOF(nameExp));
    error("Input list element 'name' must be string.\n");
  }
  tmpName = CHAR(STRING_ELT(nameExp, 0));
  checkStringLength (tmpName);
  strcpy (rSpec->name, tmpName);

  if (teExp) {
    if (TYPEOF(teExp) == STRSXP ) {
      tmpName = CHAR(STRING_ELT(teExp, 0));
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
    else if (TYPEOF(teExp) == LGLSXP) {
      if (LOGICAL(teExp)[0] == TRUE) {
        rSpec->te = 1;
      }
    }
    else {
      error("Input list element 'te' must be either string or logical"
            " - found %d instead\n", TYPEOF(teExp));
    }
  } /* teExp */

  if (tsExp) {
    if (TYPEOF(tsExp) == STRSXP ) {
      tmpName = CHAR(STRING_ELT(tsExp, 0));
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
    else if (TYPEOF(tsExp) == LGLSXP) {
      if (LOGICAL(tsExp)[0] == TRUE) {
        rSpec->ts = 1;
      }
    }
    else {
      Rprintf ("List element 'ts' must be either string or logical"
               " - found %d instead\n", TYPEOF(tsExp));
      error("Input list element 'ts' must be either string or logical");
    }
  } /* tsExp */

  if (uelsExp) {
    if (TYPEOF(uelsExp) != VECSXP) {
      error("List element 'uels' must be a list.");
    }
    else {
      PROTECT(rSpec->filterUel = allocVector(VECSXP, length(uelsExp)));
      ++*protectCnt;
      for (j = 0; j < length(uelsExp); j++) {
        tmpUel = VECTOR_ELT(uelsExp, j);
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
  } /* uelsExp */
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
    outVal = R_NilValue,
    outFullVal = R_NilValue,
    outForm = R_NilValue,
    outUels = R_NilValue,
    outField = R_NilValue,
    outTs = R_NilValue,
    outTe = R_NilValue;
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
      PROTECT(outUels = allocVector(VECSXP, symDim));
      rgdxAlloc++;
      for (defaultIndex = 0; defaultIndex < symDim; defaultIndex++) {
        SET_VECTOR_ELT(outUels, defaultIndex, UEList);
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
      PROTECT(outVal = allocMatrix(REALSXP, mwNElements, ncols));
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
      PROTECT(outVal = allocMatrix(REALSXP, mrows, ncols));
      rgdxAlloc++;
      if (rSpec->te && symType == dt_set) {
        PROTECT(textElement = allocVector(STRSXP, mrows));
        rgdxAlloc++;
      }
    }

    p = REAL(outVal);
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
      }    /* rSpec->te = 1: must be a set */
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
          tmp = outVal;
          outVal = newCV;
          UNPROTECT_PTR(tmp);
          mrows = kRec;
        }
      }
    }

    /* Converting data into its compressed form. */
    if (rSpec->compress == 1) {
      PROTECT(outUels = allocVector(VECSXP, symDim));
      rgdxAlloc++;
      compressData (outVal, UEList, outUels, nUEL, symDim, mrows);
    }
    /* TODO/TEST: create full dimensional string matrix */
    if (rSpec->te == 1) {
      if (symDim == 1) {
        PROTECT(elVect = allocVector(REALSXP, 2));
        rgdxAlloc++;

        dimElVect = REAL(elVect);
        dimElVect[0] = length(VECTOR_ELT(outUels, 0));
        dimElVect[1] = 1;

        PROTECT(outTe = allocVector(STRSXP, length(VECTOR_ELT(outUels, 0)) ));
        rgdxAlloc++;

        createElementMatrix (outVal, textElement, outTe, outUels, symDim, mrows);
        setAttrib(outTe, R_DimSymbol, elVect);
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
          PROTECT(outTe = allocVector(STRSXP, totalElement));
          rgdxAlloc++;
          createElementMatrix (outVal, textElement, outTe, rSpec->filterUel, symDim, mwNElements);
          setAttrib(outTe, R_DimSymbol, elVect);
        }
        else {
          for (ndimension = 0; ndimension < symDim; ndimension++) {
            dimElVect[ndimension] = length(VECTOR_ELT(outUels, ndimension));
            totalElement = (totalElement * length(VECTOR_ELT(outUels, ndimension)));
          }
          PROTECT(outTe = allocVector(STRSXP, totalElement));
          rgdxAlloc++;
          createElementMatrix (outVal, textElement, outTe, outUels, symDim, mrows);
          setAttrib(outTe, R_DimSymbol, elVect);
        }
      }
    }

    /* Converting sparse data into full matrix */
    if (rSpec->dForm == full) {
      switch (symDim) {
      case 0: {
        PROTECT(outFullVal = allocVector(REALSXP, 1));
        rgdxAlloc++;
        if (outVal != R_NilValue && REAL(outVal) != NULL) {
          REAL(outFullVal)[0] = REAL(outVal)[0];
        }
        else {
          REAL(outFullVal)[0] = 0;
        }
        break;
      }
      case 1: {
        PROTECT(dimVect = allocVector(REALSXP, 2));
        rgdxAlloc++;
        dimVal = REAL(dimVect);

        if (rSpec->withUel == 1) {
          dimVal[0] = length(VECTOR_ELT(rSpec->filterUel, 0));
          PROTECT(outFullVal = allocVector(REALSXP, length(VECTOR_ELT(rSpec->filterUel, 0))));
          rgdxAlloc++;
          sparseToFull (outVal, outFullVal, rSpec->filterUel, symType, mwNElements, symDim);
        }
        else {
          dimVal[0] = length(VECTOR_ELT(outUels, 0));
          PROTECT(outFullVal = allocVector(REALSXP, length(VECTOR_ELT(outUels, 0))));
          rgdxAlloc++;
          sparseToFull (outVal, outFullVal, outUels, symType, mrows, symDim);
        }
        dimVal[1] = 1;
        setAttrib(outFullVal, R_DimSymbol, dimVect);
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
            dimVal[ndimension] = length(VECTOR_ELT(outUels, ndimension));
            totalElement = (totalElement * length(VECTOR_ELT(outUels, ndimension)));
          }
        }
        PROTECT(outFullVal = allocVector(REALSXP, totalElement));
        rgdxAlloc++;
        if (rSpec->withUel ==1) {
          sparseToFull (outVal, outFullVal, rSpec->filterUel, symType, mwNElements, symDim);
        }
        else {
          sparseToFull (outVal, outFullVal, outUels, symType, mrows, symDim);
        }

        setAttrib(outFullVal, R_DimSymbol, dimVect);
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
    PROTECT(outForm = allocVector(STRSXP, 1) );
    rgdxAlloc++;
    if (rSpec->dForm == full) {
      SET_STRING_ELT(outForm, 0, mkChar(forms[0]));
    }
    else {
      SET_STRING_ELT(outForm, 0, mkChar(forms[1]));
    }


    /* Create a string vector for symbol field */
    if (symType == dt_var || symType == dt_equ) {
      outElements++;
      PROTECT(outField = allocVector(STRSXP, 1));
      rgdxAlloc++;
      switch(rSpec->dField) {
      case level:
        SET_STRING_ELT(outField, 0, mkChar( fields[0] ));
        break;
      case marginal:
        SET_STRING_ELT(outField, 0, mkChar( fields[1] ));
        break;
      case upper:
        SET_STRING_ELT(outField, 0, mkChar( fields[2] ));
        break;
      case lower:
        SET_STRING_ELT(outField, 0, mkChar( fields[3] ));
        break;
      case scale:
        SET_STRING_ELT(outField, 0, mkChar( fields[4] ));
        break;
      default:
        error("Unrecognized type of symbol found.");
      }
    }
    if (rSpec->ts) {
      outElements++;
      PROTECT(outTs = allocVector(STRSXP, 1));
      rgdxAlloc++;
      SET_STRING_ELT(outTs, 0, mkChar(sText));
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
      SET_VECTOR_ELT(outList, 3, outFullVal);
    }
    else {
      SET_VECTOR_ELT(outList, 3, outVal);
    }
    SET_VECTOR_ELT(outList, 4, outForm);
    if (rSpec->withUel) {
      SET_VECTOR_ELT(outList, 5, rSpec->filterUel);
    }
    else {
      SET_VECTOR_ELT(outList, 5, outUels);
    }

    nField = 5;
    if (symType == dt_var || symType == dt_equ) {
      nField++;
      SET_VECTOR_ELT(outList, nField, outField);
    }
    if (rSpec->ts) {
      nField++;
      SET_VECTOR_ELT(outList, nField, outTs);
    }
    if (rSpec->te) {
      nField++;
      SET_VECTOR_ELT(outList, nField, outTe);
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
