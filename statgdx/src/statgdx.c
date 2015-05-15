/* This gdxrrw.c file contains methods to import and export data
   between GAMS and R via GDX file. Methods that are exposed for
   the users of R are:
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
#include <math.h>
#include <assert.h>

#include "gdxold.h"
#include "gclgms.h"
#define _GDXRRW_MAIN_
#include "globals.h"

/* The version info below changes when this file is updated */
char ID[GMS_SSSIZE] = "$Id$";


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
  int rc, gdxLoaded;
  char loadPath[GMS_SSSIZE];
  SEXP sysDirExp, silent, returnStr;
  const char *sd1, *sd2;
  shortStringBuf_t sysDir, msgBuf;
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
#if defined(DYNLOAD_GDX)
  gdxLoaded = gdxLibraryLoaded();
#else
  gdxLoaded = 1;
#endif

  if (TYPEOF(sysDirExp) != NILSXP) { /* we should have gamsSysDir */
    if (TYPEOF(sysDirExp) != STRSXP) {
      error ("usage: %s(gamsSysDir) - gamsSysDir must be a string", funcName);
    }
    sd1 = CHAR(STRING_ELT(sysDirExp, 0));
    sd2 = R_ExpandFileName(sd1); /* interpret ~ as home directory */
    (void) CHAR2ShortStr (sd2, sysDir);

#if defined(DYNLOAD_GDX)
    /* ---- load the GDX API ---- */
    if (gdxLoaded) {
      (void) gdxLibraryUnload ();
    }
    rc = gdxGetReadyD (sysDir, msgBuf, sizeof(msgBuf));
    if ((0 == rc) && ! isSilent) {
      Rprintf ("Error loading the GDX API from directory %s\n", sysDir);
      Rprintf ("%s\n", msgBuf);
    }
#else
    if (! isSilent) {
      Rprintf ("STATIC LOADING: skip loading the GDX API from directory %s\n", sysDir);
    }
#endif
  }

  loadPath[0] = '\0';
  strcpy(loadPath, "Static loading no load path");
#if defined(DYNLOAD_GDX)
  gdxLoaded = gdxLibraryLoaded();
  if (gdxLoaded) {
    if (! isSilent)
      Rprintf ("The GDX library has been loaded\n");
    gdxGetLoadPath (loadPath);
    if (! isSilent)
      Rprintf ("GDX library load path: %s\n",
               loadPath[0] ? loadPath : "unknown");
  }
  else {
    if (! isSilent)
      Rprintf ("The GDX library has not been loaded\n");
  }
#else
  gdxLoaded = 1;
#endif

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
