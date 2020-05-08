/* This gdxrrw.c file contains methods to import and export data
   between GAMS and R via GDX file. Methods that are exposed for
   the users of R are:
   1. x <- rgdx("gdxFileName", lst)
   2. wgdx("gdxFileName", lst1, lst2, ...)
   3. x <- gams("modelName", l1, l2,..., s1, s2, ...)
   4. gdxInfo("gdxFileName")
   All these methods are declared as External methods in R.
   They are defined in gdxrrw.R file that can be loaded into R env
   by source("gdxrrw.R") command.

  * Some handy type info:
  * typedef struct SEXPREC {
  *    SEXPREC_HEADER;
  *    union {
  *      struct primsxp_struct primsxp;
  *      struct symsxp_struct symsxp;
  *      struct listsxp_struct listsxp;
  *      struct envsxp_struct envsxp;
  *      struct closxp_struct closxp;
  *      struct promsxp_struct promsxp;
  *    } u;
  * } SEXPREC, *SEXP;
  * SEXP allocVector(SEXPTYPE, R_len_t);
  * SEXP VECTOR_ELT (SEXP x, int i);
  * SEXP STRING_ELT (SEXP x, int i);
  * const char * CHAR (SEXP x);
  *
  */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>
#include <math.h>
#include <assert.h>

#include "gdxcc.h"
#include "gclgms.h"
#define _GDXRRW_MAIN_
#include "globals.h"

/* gateway routine for igdx(gamsSysDir, silent, returnStr)
 * If gamsSysDir==NULL, print usage information and current state of GDX loading
 * If gamsSysDir<>NULL, attempt to get the GDX interface loaded and ready
 * result if returnStr == TRUE:
 *   path to GAMS sysdir if set,
 *   empty string o/w
 * result if returnStr <> TRUE:
 *   TRUE   if we are ready for GDX, i.e. if the GDX library is loaded,
 *   FALSE  otherwise
 */
SEXP igdx (SEXP args)
{
  const char *funcName = "igdx";
  SEXP result;
  int arglen;
  int gdxLoaded;
  char loadPath[256];
  SEXP sysDirExp, silent, returnStr;
  Rboolean isSilent = NA_LOGICAL;
  Rboolean isReturnStr = NA_LOGICAL;

  arglen = length(args);
  if (4 != arglen) {
    error ("usage: %s(gamsSysDir=NULL, silent=FALSE, returnStr=FALSE) - incorrect arg count", funcName);
  }
  sysDirExp = CADR(args);
  silent = CADDR(args);
  isSilent = exp2Boolean (silent);
  if (NA_LOGICAL == isSilent) {
    isSilent = FALSE;
  }
  returnStr = CADDDR(args);
  isReturnStr = exp2Boolean (returnStr);
  if (NA_LOGICAL == isReturnStr) {
    isReturnStr = FALSE;
  }
  gdxLoaded = gdxLibraryLoaded();

  if (TYPEOF(sysDirExp) != NILSXP) { /* we should have gamsSysDir */
    if (TYPEOF(sysDirExp) != STRSXP) {
      error ("usage: %s(gamsSysDir, ...) - gamsSysDir must be a string", funcName);
    }
    else {
      error ("usage: %s(gamsSysDir, ...) - gamsSysDir is not implemented", funcName);
    }
  } /* gamsSysDir <> NULL */

  loadPath[0] = '\0';
  gdxLoaded = 0;
  if (gdxLoaded) {
    if (! isSilent)
      Rprintf ("The GDX library has been loaded\n");
    loadPath[0] = '\0';
    if (! isSilent)
      Rprintf ("GDX library load path: %s\n",
               loadPath[0] ? loadPath : "unknown");
  }
  else {
    if (! isSilent)
      Rprintf ("The GDX library has not been loaded\n");
  }

  if (isReturnStr) {
    PROTECT(result = allocVector(STRSXP, 1));
    SET_STRING_ELT(result, 0, mkChar(loadPath));
    UNPROTECT(1);
  }
  else {
    PROTECT(result = allocVector(INTSXP, 1));
    INTEGER(result)[0] = gdxLoaded;
    UNPROTECT(1);
  }

  return result;
} /* igdx */
