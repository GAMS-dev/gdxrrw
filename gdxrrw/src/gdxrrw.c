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
#include <math.h>
#include <assert.h>

#include "gdxcc.h"
#include "gclgms.h"
#define _GDXRRW_MAIN_
#include "globals.h"

/* The version info below changes when this file is updated  */
char ID[GMS_SSSIZE] = "$Id$";

/* -------------------- Method declaration -----------------------*/

void
getGamsPath (char *dir);

int
callGams(const char *gamsCmd);

static int
GSExec(char *command,
       int *progrc,
       int showWindow);
void
cat2ShortStr (shortStringBuf_t dest, const char *src);

void
checkFileExtension (shortStringBuf_t fileName);

void downCase (char *string);

int
getNonZeroElements (gdxHandle_t h, int symIdx, dField_t dField);



/* -------------------- Methods definition-----------------------*/
#if defined(_WIN32)
static char lastErrorMsgBuf[128];
static const char *formatMessage(int errNum);

static const char *
formatMessage(int errNum)
{
  int nChars;

  nChars =
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL,        /* ignored based on flags above */
    errNum,
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
    (LPTSTR) lastErrorMsgBuf,
    sizeof(lastErrorMsgBuf)-1,
    NULL);
  while (nChars > 0 &&
    (10 == lastErrorMsgBuf[nChars-1]
  || 13 == lastErrorMsgBuf[nChars-1]))
    nChars--;
  lastErrorMsgBuf[nChars] = '\0';
  return lastErrorMsgBuf;
}
#endif /* #if defined(_WIN32) */

void
getGamsPath (char *dir)
{
#if defined(_WIN32)
  char buf[512] = "unknown";
  void *p;
  int rc = 0;
  HMODULE h = NULL;
  WIN32_FIND_DATA FindFileData;
  HANDLE hFind;

  *dir = '\0';            /* signals failure */
  if (8 == sizeof(p))
    h = GetModuleHandle ("gams.mexw64");
  else
    h = GetModuleHandle ("gams.mexw32");
  if (h)
    rc = GetModuleFileName (h, buf, sizeof(buf));
  if (rc) {
    strncpy(dir, buf, strlen(buf) - 7);
    strcat(dir, ".exe");
    hFind = FindFirstFile(dir, &FindFileData);
    if (hFind == INVALID_HANDLE_VALUE) {
      *dir = '\0';            /* signals failure */
      return;
    }
    else {
      FindClose(hFind);
    }
  }
#else
  *dir = '\0';            /* signals failure */
#endif

  return;
} /* getGamsPath */


/* Execute GAMS command */
int
callGams (const char *gamsCmd)
{
  char *gamsExeName = NULL;
  char *cmdLine;
  int err, rc, showWindow = 0;
#if defined(_WIN32)
  char gamsExeBaseName[] = "gams.exe";
#else
  char gamsExeBaseName[] = "gams";
#endif
  /* Following is to remove screen output due to compiler bug */
  char *word;
  shortStringBuf_t jobString;
  int loThere;
  char absGamsPath[512];

#if defined(_WIN32)
  {
    char *consoleType;
    shortStringBuf_t tbuf;

    showWindow = SW_SHOWMINNOACTIVE;
    consoleType = getGlobalString("show", tbuf);
    if (consoleType != NULL) {
      if (strcmp(consoleType,"invisible") == 0) {
        showWindow = SW_HIDE;
      }
      else if (strcmp(consoleType,"normal") == 0) {
        showWindow = SW_SHOWDEFAULT;
      }
      /* else warning message */
      else {
        warning("To change default behavior of 'show', please enter it as 'invisible' or 'normal'\n" );
        Rprintf("You entered it as %s.\n", consoleType);
      }
    }
  }
#endif /* windows */

  getGamsPath (absGamsPath);
  Rprintf ("getGamsPath returns %s\n", absGamsPath);
  if ('\0' == absGamsPath[0]) {
    gamsExeName = gamsExeBaseName;
  }
  else {
    gamsExeName = absGamsPath;
  }

  if (gamsCmd == NULL) {
    error("Internal error getting GAMS command");
  }
  (void) CHAR2ShortStr (gamsCmd, jobString);

  cmdLine = malloc(strlen(gamsExeName) + 1 + strlen(jobString)
                   + 1 + 6);
  strcpy (cmdLine, gamsExeName);
  strcat (cmdLine, " ");
  strcat (cmdLine, jobString);

  /* Check for "logoption" */
  loThere = 0;
  word = strtok(jobString," -");
  while (word != NULL) {
    if (1 == strspn(word,"lL")) {
      if (1 == strspn(&word[1],"oO")) {
        if (strlen(word) == 2 || 1 == strspn(&word[2],"=") ||
            7 == strspn(&word[2],"goptinGOPTIN")) {
          loThere = 1;
        }
      }
    }
    word = strtok(NULL," -");
  }
  if (loThere == 0) {
    strcat (cmdLine, " lo=0");
  }
  err = GSExec (cmdLine, &rc, showWindow);
  /* Rprintf("GSExec returned %d, progrc=%d\n", err, rc);
   * Rprintf("GSExec returned %d, progrc=%d\n", err, rc);
   * Rprintf ("  cmdLine was %s\n", cmdLine);
   */
  if (err) {
#if defined(_WIN32)
    const char *errMsg;

    errMsg = formatMessage (err);
    if (2 == err) {
      Rprintf("error: cannot find gams - please set path appropriately\n");
      error( "Could not run %s: %s", gamsExeBaseName, errMsg);
    }
    else {
      Rprintf("error running gams: subshell could not run '%s'\n", cmdLine);
      error("Could not run %s: %s",
             gamsExeBaseName, errMsg);
    }
#else
    /* non-Windows */
    if (127 == err) {
      Rprintf("error: cannot find gams - please set PATH appropriately\n");
      error("Could not run %s", gamsExeBaseName);
    }
    else {
      Rprintf("error running gams: subshell could not run '%s'\n", cmdLine);
      error("Could not run %s", gamsExeBaseName);
    }
#endif
    return 1;
  }

  free(cmdLine);
  return rc;
} /* callGams */


/*
  execute command by spawning process;
  return: error code for starting the process
  rc: return code from the program
*/
static int GSExec(char *command,
                  int *progrc,
                  int showWindow)
{

#if defined(_WIN32)
  PROCESS_INFORMATION ProcessInformation;
  STARTUPINFO StartupInfo;
  DWORD exitcode;

  memset(&StartupInfo, 0, sizeof(StartupInfo));
  StartupInfo.cb = sizeof(StartupInfo);
  GetStartupInfo(&StartupInfo);

  StartupInfo.dwFlags = (STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES);
  StartupInfo.wShowWindow = showWindow;
  if (FALSE == CreateProcess(NULL,command,NULL,NULL,FALSE,
                             NORMAL_PRIORITY_CLASS,NULL,NULL,
                             &StartupInfo,&ProcessInformation)) {
    *progrc = 0;
    return GetLastError();
  }
  else {
    WaitForSingleObject(ProcessInformation.hProcess,INFINITE);
    GetExitCodeProcess(ProcessInformation.hProcess,&exitcode);
    CloseHandle(ProcessInformation.hThread);
    CloseHandle(ProcessInformation.hProcess);
    *progrc = exitcode;
    return 0;
  }
#else
  /* non-Windows implementation */
  int rcode;

  rcode = system ((char *) command);
  if (WIFEXITED(rcode)) {       /* shell completed successfully */
    *progrc = WEXITSTATUS(rcode);
    if (127 == *progrc) {       /* but cmd wasn't run (e.g shell not found) */
      *progrc = 0;
      return 127;
    }
    return 0;
  }
  else if (WIFSIGNALED(rcode)) {  /* child stopped via a signal */
    *progrc = WTERMSIG(rcode);
    return 1;
  }
  else {                        /* shell not completed successfully */
    *progrc = 0;
    return 2;
  }
  return 2;            /* should never get here, but . . . */
#endif /* if defined(_WIN32) */
}


void
cat2ShortStr (shortStringBuf_t dest, const char *src)
{
  size_t n, nd, ns;

  if ((NULL == src) || (NULL == dest))
    return;
  nd = strlen(dest);
  ns = strlen(src);
  n = nd + ns;
  if (n >= sizeof(shortStringBuf_t)) {
    n = sizeof(shortStringBuf_t);
    strncpy(dest, src, n);
    dest[n-1] = '\0';
  }
  else
    strcpy(dest, src);
  return;
} /* cat2ShortStr */

/* GAMS method */
SEXP gams (SEXP args)
{
  SEXP firstArg;
  SEXP result = R_NilValue;
  const char *argStr;
  int rc, arglen;
  char strippedID[GMS_SSSIZE];

  globalGams = 1;
  arglen = length(args);

  if (2 != arglen) {
    Rprintf("usage: gams('args').\n");
    error("usage: gams('args')");
  }

  args = CDR(args); firstArg = CAR(args);

  /* checking that first argument is of type string */
  if (TYPEOF(firstArg) != STRSXP ) {
    Rprintf ("The argument must be of type string.\n");
    error ("Wrong argument type");
  }

  argStr = CHAR(STRING_ELT(firstArg, 0));

  if (0 == strcmp("?", argStr)) {
    int n = (int)strlen (ID);
    memcpy (strippedID, ID+1, n-2);
    strippedID[n-2] = '\0';
    Rprintf ("R-file source info: %s\n", strippedID);
    return R_NilValue;
  } /* if audit run */

  checkStringLength(argStr);

  rc = callGams(argStr);
  PROTECT(result = allocVector(INTSXP, 1));
  INTEGER(result)[0] = rc;
  UNPROTECT(1);
  return result;
} /* gams */


/* gateway routine for igdx
 * With no args, print usage information and current state of GDX loading
 * With one arg (the GAMS sysdir name) attempt to get the GDX
 * interface loaded and ready.
 * result:
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
  SEXP sysDirExp, silent;
  const char *sd1, *sd2;
  shortStringBuf_t sysDir, msgBuf;
  Rboolean isSilent = NA_LOGICAL;

  arglen = length(args);
  if (3 != arglen) {
    error ("usage: %s(gamsSysDir=NULL, silent=FALSE) - incorrect arg count", funcName);
  }
  sysDirExp = CADR(args);
  silent = CADDR(args);
  isSilent = exp2Boolean (silent);
  if (NA_LOGICAL == isSilent) {
    isSilent = FALSE;
  }
  gdxLoaded = gdxLibraryLoaded();

  if (TYPEOF(sysDirExp) != NILSXP) { /* we should have gamsSysDir */
    if (TYPEOF(sysDirExp) != STRSXP) {
      error ("usage: %s(gamsSysDir) - gamsSysDir must be a string", funcName);
    }
    sd1 = CHAR(STRING_ELT(sysDirExp, 0));
    sd2 = R_ExpandFileName(sd1); /* interpret ~ as home directory */
    (void) CHAR2ShortStr (sd2, sysDir);

    /* ---- load the GDX API ---- */
    if (gdxLoaded) {
      (void) gdxLibraryUnload ();
    }
    rc = gdxGetReadyD (sysDir, msgBuf, sizeof(msgBuf));
    if ((0 == rc) && ! isSilent) {
      Rprintf ("Error loading the GDX API from directory %s\n", sysDir);
      Rprintf ("%s\n", msgBuf);
    }
  }

  gdxLoaded = gdxLibraryLoaded();
  PROTECT(result = allocVector(INTSXP, 1));
  INTEGER(result)[0] = gdxLoaded;
  UNPROTECT(1);
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

  return result;
} /* igdx */
