/* getGamsSoln.c
 * Code for deprecated data inputs/output in gams() call
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

/* Delete single character from string */
static char *
deleteChar (char *src, char c, int len)
{
  char *dst;
  int i;

  /* Do not remove NULL characters. */
  if (c == 0)
    return NULL;

  /* Small attempt to control a buffer overflow if the
   * the string is not null-terminated and a proper length
   * is not specified. */
  if (len <= 0)
    len = MAX_STRING;

  dst = src;

  for (i = 0;  i < len && *src != 0;  i++, src++) {
    if (*src != c)
      *dst++ = *src;
  }

  /*Ensure the string is null-terminated.*/
  *dst = 0;
  return src;
} /* deleteChar */

static SEXP
getGlobalUEL (SEXP globalUEL, int withCompress)
{
  SEXP gamso, tmp, lstName, tmpUel, bufferUel;
  int i, j, infields, found;
  found = 0;

  gamso = findVar( install("gamso"), R_GlobalEnv );

  if (gamso == NULL || TYPEOF(gamso) == NILSXP  ||  TYPEOF(gamso) == SYMSXP) {
    globalGams = 0;
    wUEL = 0;
    return R_NilValue;
  }

  /*  else if (TYPEOF(gamso) != VECSXP  && globalGams)
    {
      warning("To change default behavior, please enter 'gamso' as list.\n" );
      Rprintf("You entered it as %d.\n", TYPEOF(gamso) );
      globalGams = 0;
      return R_NilValue;
      } */

  else if (TYPEOF(gamso) == VECSXP && globalGams == 1) {
    lstName = getAttrib(gamso, R_NamesSymbol);
    i=0;
    infields = length(gamso);
    if (infields == 0) {
      wUEL = 0;
    }
    /* Checking if field data is for "name" */
    for (i = 0; i < infields; i++) {
      if (strcmp("uels", CHAR(STRING_ELT(lstName, i))) == 0) {
        found = 1;
        break;
      }
    }

    if (found == 1 && globalGams == 1) {
      tmp = VECTOR_ELT(gamso, i);
      if (tmp != NULL) {
        if (withCompress == 1) {
          error("Compression is not allowed with Input UEL.");
        }
        if (TYPEOF(tmp) != VECSXP) {
          Rprintf ("List element 'uels'  must be a list - found %d instead.\n",TYPEOF(tmp));
          error("Input list element 'uels' must be a list.");
        }
        else {
          PROTECT(globalUEL = allocVector(VECSXP, length(tmp)));
          gamsAlloc++;
          for (j = 0; j < length(tmp); j++) {
            tmpUel = VECTOR_ELT(tmp, j);
            if (tmpUel == R_NilValue) {
              error("Empty Uel is not allowed ");
            }
            else {
              bufferUel = allocVector(STRSXP, length(tmpUel));
              /* Convert to output */
              makeStrVec (bufferUel, tmpUel);
              SET_VECTOR_ELT(globalUEL, j, bufferUel);
              wUEL = 1;
            }
          }
        }
      }
    }
  }
  return globalUEL;
}  /* getGlobalUEL */



SEXP
getGamsSoln(const char *gmsFileName)
{
  SEXP UEList;
  SEXP OPListComp, OPList, dimVect;
  SEXP textElement = R_NilValue;
  SEXP compName = R_NilValue,
    compType = R_NilValue,
    compDim = R_NilValue,
    compVal = R_NilValue,
    compFullVal = R_NilValue,
    compForm = R_NilValue,
    compUels = R_NilValue,
    compField = R_NilValue,
    compTs = R_NilValue,
    compTe = R_NilValue;
  rSpec_t *rSpec;
  FILE *fp, *fin;
  char line[LINELEN], astring[LINELEN], *s, *array[50], *gdxFile;
  int loop, maxPossibleElements, z;
  const char *uelElementName;
  int rc, errNum, symDim, symType, mrows, ncols, nRecs, iRec, changeIdx, index, k, kk, kRec;
  shortStringBuf_t msgBuf, uelName;
  char buf[3*sizeof(shortStringBuf_t)];
  int nUEL, iUEL, defaultIndex,  UELUserMapping, highestMappedUEL, ndimension;
  double *p, *dimVal;
  double dt, posInf, negInf;
  gdxUelIndex_t uels;
  gdxValues_t values;
  gdxSVals_t sVals;
  d64_t d64;
  shortStringBuf_t gsBuf;
  char *gForm, *field;
  int nField;
  char *types[] = {"set", "parameter", "variable", "equation"};
  char *forms[] = {"full", "sparse"};
  char *fields[] = {"l", "m", "up", "lo", "s"};
  int b, matched, sparesIndex, totalElement;
  int *returnedIndex;
  int mwNElements =0;
  int uelPos = 0;
  int outFields = 6;

  /* Setting default values */
  rSpec = malloc(sizeof(*rSpec));
  memset (rSpec, 0, sizeof(*rSpec));
  rSpec->dForm = sparse;
  rSpec->compress = isCompress();
  rSpec->dField = level;

  if ((fp = fopen(gmsFileName,"r")) == NULL) {
    error("Cannot find/open %s file.\n", gmsFileName);
  }
  astring[0] = '\0';
  /* read the file till $set matout */
  while (fgets(line,LINELEN,fp) != NULL) {
    /* throw away leading spaces */
    s = line;
    while (isspace(*s)) s++;
    if (!*s) continue;

    if (sscanf(s, "$set matout %s", astring) == 1) {
      break;
    }
  }
  fclose(fp);

  /* this bunch just to shut up compiler warnings */
  loop = 0;
  maxPossibleElements = 0;
  OPList = R_NilValue;

  if (astring != NULL && strcmp(astring, "") != 0) {
    array[0]=strtok(line," ");
    if (array[0]==NULL) {
      Rprintf("No test to search.\n");
    }
    for (loop=1; loop<50; loop++) {
      array[loop]=strtok(NULL," ");

      if (array[loop]==NULL) {
        break;
      }
      else {
        deleteChar(array[loop], ',', 0);
        deleteChar(array[loop], '\'', 0);
        deleteChar(array[loop], '"', 0);
      }
    }
  }

  /* At least one element is there in GDX file */
  if (loop - 3 > 0) {
    /* This is for global UEL */
    wUEL = 0;
    rSpec->filterUel = getGlobalUEL(rSpec->filterUel, rSpec->compress);
    if (wUEL == 0) {
      rSpec->withUel = 0;
    }
    else {
      rSpec->withUel = 1;
    }
    gForm = getGlobalString("form", gsBuf);
    if (gForm != NULL) {
      if (strcmp(gForm,"full") == 0) {
        rSpec->dForm = full;
      }
      else if (strcmp(gForm,"sparse") == 0) {
        rSpec->dForm = sparse;
      }
      else {
        /* else warning message */
        warning("To change default behavior of 'form', please enter it as 'full' or 'sparse'\n" );
        Rprintf("You entered it as %s.\n", gForm);
      }
    }

    field = getGlobalString("field", gsBuf);
    if (field != NULL) {
      if (strcmp(field, "l") == 0) {
        rSpec->dField = level;
      }
      else if (strcmp(field, "m") == 0) {
        rSpec->dField = marginal;
      }
      else if (strcmp(field, "lo") == 0) {
        rSpec->dField = lower;
      }
      else if (strcmp(field, "up") == 0) {
        rSpec->dField = upper;
      }
      else if (strcmp(field, "s") == 0) {
        rSpec->dField = scale;
      }
      else {
        /* else warning message */
        warning(" To change default behavior of 'field', please enter it as 'l/m/lo/up/s'.\n" );
        Rprintf("You entered it as %s.\n", field);
      }
    }

    /* -------------Start reading data from gdx file------------------- */
    gdxFile = array[2];
    fin = fopen (gdxFile,"r");
    if (fin==NULL) {
      sprintf (buf, "File '%s' not found!\n", gdxFile);
      error(buf);
    }
    fclose(fin);

    loadGDX();
    rc = gdxCreate (&gdxHandle, msgBuf, sizeof(msgBuf));
    if (0 == rc)
      error ("Error creating GDX object: %s", msgBuf);
    rc = gdxOpenRead (gdxHandle, gdxFile, &errNum);
    if (errNum || 0 == rc) {
      error ("Could not open gdx file with gdxOpenRead");
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

    gdxSymbolInfo (gdxHandle, 1, rSpec->name, &symDim, &symType);
    /* checking that symbol is of type parameter/set/equation/variable */
    if (!(symType == dt_par || symType == dt_set || symType == dt_var || symType == dt_equ)) {
      Rprintf("GDX symbol %s (index=1, symDim=%d, symType=%d)"
              " is not recognized as set, parameter, variable, or equation\n",
              rSpec->name, symDim, symType);
      error("Invalid symbol. Please check listing file.\n");
    }

    /* Get global UEL from GDX file */
    (void) gdxUMUelInfo (gdxHandle, &nUEL, &highestMappedUEL);
    PROTECT(UEList = allocVector(STRSXP, nUEL));
    gamsAlloc++;
    for (iUEL = 1;  iUEL <= nUEL;  iUEL++) {
      if (!gdxUMUelGet (gdxHandle, iUEL, uelName, &UELUserMapping)) {
        error("Could not gdxUMUelGet");
      }
      SET_STRING_ELT(UEList, iUEL-1, mkChar(uelName));
    }

    /* Checking dimension of input uel and parameter in GDX file.
     * If they are not equal then error. */

    if (rSpec->withUel == 1 && length(rSpec->filterUel) != symDim) {
      error("Dimension of UEL entered does not match with symbol in GDX");
    }
    /* Creating default uel if none entered */
    if (rSpec->withUel == 0) {
      PROTECT(compUels = allocVector(VECSXP, symDim));
      gamsAlloc++;
      for (defaultIndex = 0; defaultIndex < symDim; defaultIndex++) {
        SET_VECTOR_ELT(compUels, defaultIndex, UEList);
      }
    }

    /* Start reading data */
    gdxDataReadRawStart (gdxHandle, 1, &nRecs);
    /* if it is a parameter, add 1 to the dimension */
    mrows = nRecs;
    if (symType != dt_set) {
      ncols = symDim+1;
    }
    else {
      ncols = symDim;
    }

    /* TODO: filter UEL
     *  this is to check total number of elements that matches
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
    /* Allocating memory for 2D sparse matrix */
    if (rSpec->withUel == 1) {
      PROTECT(compVal = allocMatrix(REALSXP, mwNElements, ncols));
      gamsAlloc++;
      if (rSpec->te && symType == dt_set) {
        PROTECT(textElement = allocVector(STRSXP, mwNElements));
        gamsAlloc++;
      }
    }
    if (rSpec->withUel == 0) {
      /* check for non zero elements for variable and equation */
      if (symType == dt_var || symType == dt_equ) {
        mrows = getNonZeroElements (gdxHandle, 1, rSpec->dField);
      }
      /* Creat 2D sparse R array */
      PROTECT(compVal = allocMatrix(REALSXP, mrows, ncols));
      gamsAlloc++;
      if (rSpec->te && symType == dt_set) {
        PROTECT(textElement = allocVector(STRSXP, mrows));
        gamsAlloc++;
      }
    }

    p = REAL(compVal);
    /* TODO/TEST: filtered read */
    if (rSpec->withUel == 1) {
      matched = 0;
      gdxDataReadRawStart (gdxHandle, 1, &nRecs);

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
          for (sparesIndex = 0; sparesIndex < symDim; sparesIndex++) {
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
    }/* End of with uels */
    else {
      if (symType == dt_var || symType == dt_equ ) {
        gdxDataReadRawStart (gdxHandle, 1, &nRecs);
      }

      for (iRec = 0, kRec = 0;  iRec < nRecs;  iRec++) {
        gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
        if ((dt_set == symType) ||
            (0 != values[rSpec->dField]) ) {
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
        Rprintf("DEBUG getGamsSoln: shrinking matrix from %d to %d rows.\n",
                mrows, kRec);
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

    /* Converting data into its compressed form. */
    if (rSpec->compress == 1) {
      PROTECT(compUels = allocVector(VECSXP, symDim));
      gamsAlloc++;
      compressData (compVal, UEList, compUels, nUEL, symDim, mrows);
    }

    /* Converting sparse data into full matrix */
    if (rSpec->dForm == full) {
      switch (symDim) {
      case 0:
        PROTECT(compFullVal = allocVector(REALSXP, 1));
        gamsAlloc++;
        if (compVal != R_NilValue && REAL(compVal) != NULL) {
          REAL(compFullVal)[0] = REAL(compVal)[0];
        }
        else {
          REAL(compFullVal)[0] = 0;
        }
        break;
      case 1:
        PROTECT(dimVect = allocVector(REALSXP, 2));
        gamsAlloc++;
        dimVal = REAL(dimVect);

        if (rSpec->withUel == 1) {
          dimVal[0] = length(VECTOR_ELT(rSpec->filterUel, 0));
          PROTECT(compFullVal = allocVector(REALSXP, length(VECTOR_ELT(rSpec->filterUel, 0))));
          gamsAlloc++;
          compFullVal = sparseToFull(compVal, compFullVal, rSpec->filterUel, symType, mwNElements, symDim);
        }
        else {
          dimVal[0] = length(VECTOR_ELT(compUels, 0));
          PROTECT(compFullVal = allocVector(REALSXP, length(VECTOR_ELT(compUels, 0))));
          gamsAlloc++;
          compFullVal = sparseToFull(compVal, compFullVal, compUels, symType, mrows, symDim);
        }
        dimVal[1] = 1;
        setAttrib(compFullVal, R_DimSymbol, dimVect);
        break;
      default:
        PROTECT(dimVect = allocVector(REALSXP, symDim));
        gamsAlloc++;
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
        gamsAlloc++;
        if (rSpec->withUel ==1) {
          compFullVal = sparseToFull(compVal, compFullVal, rSpec->filterUel, symType, mwNElements, symDim);
        }
        else {
          compFullVal = sparseToFull(compVal, compFullVal, compUels, symType, mrows, symDim);
        }

        setAttrib(compFullVal, R_DimSymbol, dimVect);
        break;
      } /* switch(symDim) */
    }

    /* Create a 1-by-1 array of structs. */
    /* List form */
    /* Creating string vector for symbol Name */
    PROTECT(compName = allocVector(STRSXP, 1) );
    SET_STRING_ELT(compName, 0, mkChar(rSpec->name));
    gamsAlloc++;
    /* Creating string vector for symbol type */
    PROTECT(compType = allocVector(STRSXP, 1) );
    gamsAlloc++;
    switch (symType) {
    case dt_set:
      SET_STRING_ELT( compType, 0, mkChar(types[0]) );
      break;
    case dt_par:
      SET_STRING_ELT( compType, 0, mkChar(types[1]) );
      break;
    case dt_var:
      SET_STRING_ELT( compType, 0, mkChar(types[2]) );
      break;
    case dt_equ:
      SET_STRING_ELT( compType, 0, mkChar(types[3]) );
      break;
    default:
      error("Unrecognized type of symbol found.");
    }

    /* Creating int vector for symbol Dim */
    PROTECT(compDim = allocVector(INTSXP, 1) );
    INTEGER(compDim)[0] = symDim;
    gamsAlloc++;
    /* Creating string vector for val data form */
    PROTECT(compForm = allocVector(STRSXP, 1) );
    gamsAlloc++;
    if (rSpec->dForm == full) {
      SET_STRING_ELT(compForm, 0, mkChar(forms[0]));
    }
    else {
      SET_STRING_ELT(compForm, 0, mkChar(forms[1]));
    }

    /* Create a string vector for symbol  field */
    if (symType == dt_var || symType == dt_equ) {
      outFields++;
      PROTECT(compField = allocVector(STRSXP, 1));
      gamsAlloc++;
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

    PROTECT(OPListComp = allocVector(STRSXP, outFields));
    gamsAlloc++;
    /* populating list component names */
    SET_STRING_ELT(OPListComp, 0, mkChar("name"));
    SET_STRING_ELT(OPListComp, 1, mkChar("type"));
    SET_STRING_ELT(OPListComp, 2, mkChar("dim"));
    SET_STRING_ELT(OPListComp, 3, mkChar("val"));
    SET_STRING_ELT(OPListComp, 4, mkChar("form"));
    SET_STRING_ELT(OPListComp, 5, mkChar("uels"));

    nField = 5;

    if (symType == dt_var || symType == dt_equ) {
      nField++;
      SET_STRING_ELT(OPListComp, nField, mkChar("field"));
    }
    if (rSpec->ts) {
      nField++;
      SET_STRING_ELT(OPListComp, nField, mkChar("ts"));
    }
    if (rSpec->te) {
      nField++;
      SET_STRING_ELT(OPListComp, nField, mkChar("te"));
    }

    PROTECT(OPList = allocVector(VECSXP, outFields));
    gamsAlloc++;

    /* populating list component vector */
    SET_VECTOR_ELT(OPList, 0, compName);
    SET_VECTOR_ELT(OPList, 1, compType);
    SET_VECTOR_ELT(OPList, 2, compDim);
    if (rSpec->dForm == full) {
      SET_VECTOR_ELT(OPList, 3, compFullVal);
    }
    else {
      SET_VECTOR_ELT(OPList, 3, compVal);
    }
    SET_VECTOR_ELT(OPList, 4, compForm);
    if (rSpec->withUel) {
      SET_VECTOR_ELT(OPList, 5, rSpec->filterUel);
    }
    else {
      SET_VECTOR_ELT(OPList, 5, compUels);
    }

    nField = 5;
    if (symType == dt_var || symType == dt_equ) {
      nField++;
      SET_VECTOR_ELT(OPList, nField, compField);
    }
    if (rSpec->ts) {
      nField++;
      SET_VECTOR_ELT(OPList, nField, compTs);
    }
    if (rSpec->te) {
      nField++;
      SET_VECTOR_ELT(OPList, nField, compTe);
    }

    /* Setting attribute name */
    setAttrib(OPList, R_NamesSymbol, OPListComp);

    if (!gdxDataReadDone (gdxHandle)) {
      error ("Could not gdxDataReadDone");
    }
    errNum = gdxClose (gdxHandle);
    if (errNum != 0) {
      error("Errors detected when closing gdx file");
    }
    (void) gdxFree (&gdxHandle);
  } /* end if loop - 3 > 0 */
  free(rSpec);
  return OPList;
} /* getGamsSoln */
