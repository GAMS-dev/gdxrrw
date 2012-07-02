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
  *
  * TO DO: output=std global should go - no need to have old-style stuff
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

typedef void (GDX_CALLCONV *gdxGetLoadPath_t) (char *s);
GDX_FUNCPTR(gdxGetLoadPath);

#if defined(_WIN32)
# include <windows.h>
static const char *formatMessage(int errNum);
#else
# include <sys/wait.h>
#endif

#define LINELEN 1024
#define MAX_STRING 128
#if defined(__linux__) && defined(__x86_64)
/* long story: GLIBC hacked up memcpy on my Fedora 15 machine
 * so GLIBC 2.14 is required to use the gdxrrw.so.  That is not acceptable.
 */
# define MEMCPY memmove
#else
# define MEMCPY memcpy
#endif

gdxHandle_t gdxHandle = (gdxHandle_t) 0;

typedef char shortStringBuf_t[GMS_SSSIZE];

/* just to shut up some warnings on Linux */
typedef int (*compareFunc_t) (const void *, const void *);

/* Structure and Enum definition */
typedef enum dType
{
  set = GMS_DT_SET,
  parameter = GMS_DT_PAR
} dType_t;

typedef enum dForm
{
  unKnown=0,
  full,
  sparse
} dForm_t;

typedef enum dField
{
  level = GMS_VAL_LEVEL,
  marginal = GMS_VAL_MARGINAL,
  lower = GMS_VAL_LOWER,
  upper = GMS_VAL_UPPER,
  scale = GMS_VAL_SCALE,
  max = GMS_VAL_MAX
} dField_t;

struct rgdxStruct
{
  char name[1024];
  dForm_t dForm;
  dField_t dField;
  int withField;
  int compress;
  int ts;
  int te;
  int withUel;
  SEXP filterUel;
};

struct wgdxStruct
{
  char name[1024];
  dForm_t dForm;
  dType_t dType;
  int withVal;
  int withTs;
  int withUel;
  int dim;
};
typedef struct wgdxStruct wgdxStruct_t;

typedef unsigned long long int uint64_t;

union d64_t {
  double x;
  uint64_t u64;
};

static int alloc;

static int wAlloc;

static int gamsAlloc;

static char specialCommand[LINELEN];

static int globalGams;

static int wUEL;

static shortStringBuf_t lastErrMsg;

static char mexPath[512];

static const char *validSymListNames[] = {
  "name"
  ,"type"
  ,"val"
  ,"uels"
  ,"form"
  ,"dim"
  ,"ts"
};
#define N_VALIDSYMLISTNAMES (sizeof(validSymListNames)/sizeof(*validSymListNames))
static char validFieldMsg[256] = "";

/* The version info below changes when this file is updated */
static char ID[256] = "$Id$";
static char strippedID[256];

/* -------------------- Method declaration -----------------------*/
static void
checkRgdxList (const SEXP lst, struct rgdxStruct *data);

static void
unpackWgdxArgs (SEXP *args, int argLen, SEXP **symList,
                int *symListSiz, int *symListLen, char *zeroSqueeze);

static void
readWgdxList(const SEXP lst,
             int iSym,
             SEXP uelIndex,
             wgdxStruct_t **wgdxRecPtr,
             int fromGAMS);

void
registerInputUEL(SEXP uelOut,
                 int k,
                 SEXP uelIndex);

static void
getGDXMsg(void);

void
getGamsPath (char *dir);

int
callGams(const char *gamsFile);

static int
GSExec(char *command,
       int *progrc,
       int showWindow);
SEXP
getGlobalUEL(SEXP globalUEL,
             int withCompress);

SEXP
getGamsSoln(char *gmsFileName);

char *
CHAR2ShortStr (const char *from, shortStringBuf_t to);

void
cat2ShortStr (shortStringBuf_t dest, const char *src);

int isCompress(void);

char *getGlobalString (const char *globName, shortStringBuf_t result);

char *
delete_char(char *src,
            char c,
            int len);

void
createUelOut(SEXP val,
             SEXP uelOut,
             dType_t dType,
             dForm_t dForm);

void
checkForValidData(SEXP val,
                  SEXP uelOut,
                  dType_t dType,
                  dForm_t dForm);


char *
val2str (gdxHandle_t h, double val, char *s);

void
checkFileExtension (shortStringBuf_t fileName);

void
checkStringLength(const char *str);

void downCase (char *string);

int
getNonZeroElements (gdxHandle_t h, int symIdx, dField_t dField);

void
compressData (SEXP data,
              SEXP globalUEL,
              SEXP uelOut,
              int numberOfUel,
              int symbolDim,
              int nRec);

SEXP
sparseToFull(SEXP compVal,
             SEXP compFullVal,
             SEXP compUels,
             int type,
             int nRec,
             int symbolDim);

SEXP
createElementMatrix(SEXP compVal,
                    SEXP textElement,
                    SEXP compTe,
                    SEXP compUels,
                    int symbolDim,
                    int nRec);

void
checkForRepetition(SEXP bufferUel);

SEXP
convertToOutput(SEXP bufferUel,
                SEXP tmpUel);

int
checkIfExist (int k, SEXP filterUel, const char *uelName);

static void
writeGdx(char *fileName,
         int symListLen,
         SEXP *symList,
         int fromGAMS,
         char zeroSqueeze);

static void
loadGDX (void);

/*-------------------- Method exposed to R -----------------------*/

SEXP rgdx (SEXP args);

SEXP wgdx (SEXP args);

SEXP igdx (SEXP args);

SEXP gdxInfo (SEXP args);

SEXP gams (SEXP args);

SEXP getGlobal(SEXP args);


/* -------------------- Methods definition-----------------------*/
/* assumes gdxHandle is valid */
static void
getGDXMsg (void)
{
  int lastErr;

  lastErr = gdxGetLastError (gdxHandle);
  (void) gdxErrorStr (NULL, lastErr, lastErrMsg);
  return;
}

#if defined(_WIN32)
static char lastErrorMsgBuf[128];

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
callGams (const char *gamsFile)
{
  char *gamsExeName;
  char *cmdLine, *gamsPath;
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

  // gamsPath = getGlobalString("path");
  gamsPath = NULL;
  if (NULL == gamsPath) {
    getGamsPath (absGamsPath);
    if ('\0' == absGamsPath[0]) {
      gamsExeName = gamsExeBaseName;
    }
    else {
      gamsExeName = absGamsPath;
    }
  }
  else {
    gamsExeName = gamsPath;
  }

  if (gamsFile == NULL) {
    error("Error getting GAMS input file name");
    return 1;
  }
  (void) CHAR2ShortStr (gamsFile, jobString);

  cmdLine = malloc(strlen(gamsExeName) + 1 + strlen(jobString)
                   + 1 + strlen(specialCommand) + 6);
  strcpy (cmdLine, gamsExeName);
  strcat (cmdLine, " ");
  strcat (cmdLine, jobString);
  strcat (cmdLine, specialCommand); /* specialCmmand always starts with a blank */

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
  if (err) {
#if defined(_WIN32)
    const char *errMsg;

    errMsg = formatMessage (err);
    if (NULL == gamsPath) {
      if (2 == err) {
        Rprintf("error: cannot find gams - please set path appropriately\n");
        error( "Could not run %s: %s", gamsExeBaseName, errMsg);
      }
    }
    else {
      error("Could not run %s: %s: check gams.path",
             gamsPath, errMsg);
    }
#else
    /* non-Windows */
    error("Could not run %s: check gams.path", gamsPath);
#endif
    return 1;
  }

  if (0 != rc) {
    error("Abnormal GAMS termination running '%s': Check listing file.\n", cmdLine);
    return 1;
  }
  free(cmdLine);
  return 0;
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

SEXP
getGlobalUEL(SEXP globalUEL,
             int withCompress)
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
          Rprintf ("List component 'uels'  must be a list - found %d instead.\n",TYPEOF(tmp));
          error("Input list component 'uels' must be a list.");
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
              bufferUel =  convertToOutput(bufferUel, tmpUel);
              SET_VECTOR_ELT(globalUEL, j, bufferUel);
              wUEL = 1;
            }
          }
        }
      }
    }
  }
  return globalUEL;
}  /* End of getGlobalUels */


SEXP
getGamsSoln(char *gmsFileName)
{
  SEXP  UEList;
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
  struct rgdxStruct *inputData;
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
  union d64_t d64;
  shortStringBuf_t gsBuf;
  char *gForm, *field;
  int nField;
  char *types[] = {"set", "parameter", "variable", "equation"};
  char *forms[] = {"full", "sparse"};
  char *fields[] = {"l", "m", "up", "lo", "s"};
  int b, matched, sparesIndex, totalElement;
  int *returnedIndex;
  int mwNElements =0;
  int uelProperty = 0;
  int outFields = 6;

  /* Setting default values */
  inputData = malloc(sizeof(*inputData));

  inputData->dForm = sparse;
  inputData->compress = isCompress();
  inputData->dField = level;
  inputData->withUel = 0;
  inputData->ts = 0;
  inputData->te = 0;
  inputData->withField = 0;

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
        delete_char(array[loop], ',', 0);
        delete_char(array[loop], '\'', 0);
        delete_char(array[loop], '"', 0);
      }
    }
  }

  /* At least one element is there in GDX file */
  if (loop - 3 > 0) {
    /* This is for global UEL */
    wUEL = 0;
    inputData->filterUel = getGlobalUEL(inputData->filterUel, inputData->compress);
    if (wUEL == 0) {
      inputData->withUel = 0;
    }
    else {
      inputData->withUel = 1;
    }
    gForm = getGlobalString("form", gsBuf);
    if (gForm != NULL) {
      if (strcmp(gForm,"full") == 0) {
        inputData->dForm = full;
      }
      else if (strcmp(gForm,"sparse") == 0) {
        inputData->dForm = sparse;
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
        inputData->dField = level;
      }
      else if (strcmp(field, "m") == 0) {
        inputData->dField = marginal;
      }
      else if (strcmp(field, "lo") == 0) {
        inputData->dField = lower;
      }
      else if (strcmp(field, "up") == 0) {
        inputData->dField = upper;
      }
      else if (strcmp(field, "s") == 0) {
        inputData->dField = scale;
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

    gdxSymbolInfo (gdxHandle, 1, inputData->name, &symDim, &symType);
    /* checking that symbol is of type parameter/set/equaltion/variable */
    if (!(symType == dt_par || symType == dt_set || symType == dt_var || symType == dt_equ)) {
      Rprintf("GDX symbol %s (index=1, symDim=%d, symType=%d)"
              " is not recognized as set, parameter, variable, or equation\n",
              inputData->name, symDim, symType);
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

    /* Checking dimension of input uel and paramter in GDX file.
     * If they are not equal then error. */

    if (inputData->withUel == 1 && length(inputData->filterUel) != symDim) {
      error("Dimension of UEL entered does not match with symbol in GDX");
    }
    /* Creating default uel if none entered */
    if (inputData->withUel == 0) {
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

    if (inputData->withUel == 1) {
      maxPossibleElements = 1;
      for (z = 0; z < symDim; z++) {
        mwNElements = length(VECTOR_ELT(inputData->filterUel, z));
        maxPossibleElements = maxPossibleElements*mwNElements;
      }
      mwNElements = 0;

      for (iRec = 0;  iRec < nRecs;  iRec++) {
        gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
        b = 0;
        for (k = 0; k < symDim; k++) {
          uelProperty = 0;
          uelElementName = CHAR(STRING_ELT(UEList, uels[k]-1));
          uelProperty = checkIfExist (k, inputData->filterUel, uelElementName);
          /* uel element exists */
          if (uelProperty > 0) {
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
    if (inputData->withUel == 1) {
      PROTECT(compVal = allocMatrix(REALSXP, mwNElements, ncols));
      gamsAlloc++;
      if (inputData->te && symType == dt_set) {
        PROTECT(textElement = allocVector(STRSXP, mwNElements));
        gamsAlloc++;
      }
    }
    if (inputData->withUel == 0) {
      /* check for non zero elements for variable and equation */
      if (symType == dt_var || symType == dt_equ) {
        mrows = getNonZeroElements (gdxHandle, 1, inputData->dField);
      }
      /* Creat 2D sparse R array */
      PROTECT(compVal = allocMatrix(REALSXP, mrows, ncols));
      gamsAlloc++;
      if (inputData->te && symType == dt_set) {
        PROTECT(textElement = allocVector(STRSXP, mrows));
        gamsAlloc++;
      }
    }

    p = REAL(compVal);
    /* TODO/TEST: filtered read */
    if (inputData->withUel == 1) {
      matched = 0;
      gdxDataReadRawStart (gdxHandle, 1, &nRecs);

      returnedIndex = malloc(symDim*sizeof(*returnedIndex));
      for (iRec = 0;  iRec < nRecs;  iRec++) {
        gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
        index = 0;
        b = 0;

        for (k = 0;  k < symDim;  k++) {
          uelProperty = 0;
          returnedIndex[k] = 0;
          uelElementName = CHAR(STRING_ELT(UEList, uels[k]-1));
          uelProperty = checkIfExist (k, inputData->filterUel, uelElementName);
          if (uelProperty > 0) {
            returnedIndex[k] = uelProperty;
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
            p[index] = values[inputData->dField];
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
            (0 != values[inputData->dField]) ) {
          /* store the value */
          for (kk = 0;  kk < symDim;  kk++) {
            p[kRec + kk*mrows] = uels[kk];
          }
          index = kRec + symDim*mrows;
          kRec++;
          if (symType != dt_set)
            p[index] = values[inputData->dField];
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
    if (inputData->compress == 1) {
      PROTECT(compUels = allocVector(VECSXP, symDim));
      compressData (compVal, UEList, compUels, nUEL, symDim, mrows);
    }

    /* Converting sparse data into full matrix */
    if (inputData->dForm == full) {
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

        if (inputData->withUel == 1) {
          dimVal[0] = length(VECTOR_ELT(inputData->filterUel, 0));
          PROTECT(compFullVal = allocVector(REALSXP, length(VECTOR_ELT(inputData->filterUel, 0))));
          gamsAlloc++;
          compFullVal = sparseToFull(compVal, compFullVal, inputData->filterUel, symType, mwNElements, symDim);
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
        if (inputData->withUel == 1) {
          for (ndimension = 0; ndimension < symDim; ndimension++) {
            dimVal[ndimension] = length(VECTOR_ELT(inputData->filterUel, ndimension));
            totalElement = (totalElement * length(VECTOR_ELT(inputData->filterUel, ndimension)));
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
        if (inputData->withUel ==1) {
          compFullVal = sparseToFull(compVal, compFullVal, inputData->filterUel, symType, mwNElements, symDim);
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
    SET_STRING_ELT(compName, 0, mkChar(inputData->name));
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
    if (inputData->dForm == full) {
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
      switch(inputData->dField) {
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
    if (inputData->ts) {
      nField++;
      SET_STRING_ELT(OPListComp, nField, mkChar("ts"));
    }
    if (inputData->te) {
      nField++;
      SET_STRING_ELT(OPListComp, nField, mkChar("te"));
    }

    PROTECT(OPList = allocVector(VECSXP, outFields));
    gamsAlloc++;

    /* populating list component vector */
    SET_VECTOR_ELT(OPList, 0, compName);
    SET_VECTOR_ELT(OPList, 1, compType);
    SET_VECTOR_ELT(OPList, 2, compDim);
    if (inputData->dForm == full) {
      SET_VECTOR_ELT(OPList, 3, compFullVal);
    }
    else {
      SET_VECTOR_ELT(OPList, 3, compVal);
    }
    SET_VECTOR_ELT(OPList, 4, compForm);
    if (inputData->withUel) {
      SET_VECTOR_ELT(OPList, 5, inputData->filterUel);
    }
    else {
      SET_VECTOR_ELT(OPList, 5, compUels);
    }

    nField = 5;
    if (symType == dt_var || symType == dt_equ) {
      nField++;
      SET_VECTOR_ELT(OPList, nField, compField);
    }
    if (inputData->ts) {
      nField++;
      SET_VECTOR_ELT(OPList, nField, compTs);
    }
    if (inputData->te) {
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
  free(inputData);
  return OPList;
} /* getGamsSoln */

/* this method for global input "compress" */
int isCompress(void)
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


/* This method will read variable "gamso" from R workspace */
char *getGlobalString (const char *globName, shortStringBuf_t result)
{
  SEXP gamso, lstName, tmp;
  char *str;
  int i, infields, found;

  str = NULL;
  gamso = findVar (install("gamso"), R_GlobalEnv);

  if (gamso == NULL || TYPEOF(gamso) == NILSXP  ||  TYPEOF(gamso) == SYMSXP) {
    globalGams = 0;
    *result = '\0';
    return NULL;
  }
  /*  else if (TYPEOF(gamso) != VECSXP  && globalGams == 1)
    {
      warning("To change default behavior, please enter 'gamso' as list.\n");
      Rprintf("You entered it as %d.\n", TYPEOF(gamso) );
      globalGams = 0;
      return NULL;
      }*/
  else if (TYPEOF(gamso) == VECSXP && globalGams == 1) {
    lstName = getAttrib (gamso, R_NamesSymbol);
    i=0;
    infields = length(gamso);
    /* Checking if field data is for "name" */
    for (found = 0, i = 0;  i < infields;  i++) {
      if (strcmp(globName, CHAR(STRING_ELT(lstName, i))) == 0) {
        found = 1;
        break;
      }
    }

    if (found == 1 && globalGams == 1) {
      tmp = VECTOR_ELT(gamso, i);
      if (TYPEOF(tmp) == STRSXP) {
        checkStringLength (CHAR(STRING_ELT(tmp, 0)));
        str = CHAR2ShortStr (CHAR(STRING_ELT(tmp, 0)), result);
      }
      else {
        warning("To change default behavior of %s, please enter it as string.\n", globName );
        Rprintf("You entered it as %d.\n", TYPEOF(tmp));
        return NULL;
      }
    }
  }
  return str;
} /* getGlobalString */



/* Delete single character from string */
char*
delete_char(char *src,
            char c,
            int len)
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

  for (i = 0 ; i < len && *src != 0 ; i++, src++) {
    if ( *src != c )
      *dst++ = *src;
  }

  /*Ensure the string is null-terminated.*/
  *dst = 0;
  return src;
} /* End of delete_char */

void
createUelOut(SEXP val,
             SEXP uelOut,
             dType_t dType,
             dForm_t dForm)
{
  SEXP  dims, bufferUel;
  int i, j, k;
  double *P;
  int *intVal;
  char buffer [256];
  int ncols, nrows, ndims;
  int max = 0;

  dims = getAttrib(val, R_DimSymbol);
  if (dForm == sparse) {
    nrows = INTEGER(dims)[0];
    ncols = INTEGER(dims)[1];
    if (dType == parameter) {
      ncols--;
    }

    if (TYPEOF(val) == REALSXP) {
      P = REAL(val);
      for (i = 0; i < ncols; i++) {
        for (j = 0; j < nrows; j++) {
          if (P[j + i*nrows] > max) {
            max = (int) P[j + i*nrows];
          }
        }
        bufferUel = allocVector(STRSXP, max);
        for (k = 1; k <= max; k++) {
          sprintf(buffer, "%d", k);
          SET_STRING_ELT(bufferUel, k-1, mkChar(buffer));
        }
        SET_VECTOR_ELT(uelOut, i, duplicate(bufferUel));
        max = 0;
      }
    }
    else if (TYPEOF(val) == INTSXP) {
      intVal = INTEGER(val);

      for (i = 0; i < ncols; i++) {
        for (j = 0; j < nrows; j++) {
          if (intVal[j + i*nrows] > max) {
            max =  intVal[j + i*nrows];
          }
        }
        bufferUel = allocVector(STRSXP, max);
        for (k = 1; k <= max; k++) {
          sprintf(buffer, "%d", k);
          SET_STRING_ELT(bufferUel, k-1, mkChar(buffer));
        }
        SET_VECTOR_ELT(uelOut, i, duplicate(bufferUel));
        max = 0;
      }
    }
  }
  else {
    /*
     * Create default uel.
     * Here there is no need to calculate max as number
     * of elements in each dimension is all what I need.
     */
    ndims = length(uelOut);
    for (i = 0; i < ndims; i++) {
      bufferUel = allocVector(STRSXP, INTEGER(dims)[i]);

      for (k = 1; k <= INTEGER(dims)[i]; k++) {
        sprintf(buffer, "%d", k);
        SET_STRING_ELT(bufferUel, k-1, mkChar(buffer));
      }
      SET_VECTOR_ELT(uelOut, i, bufferUel);
    }
  }
} /* createUelOut */


/* This method check the validity of input data with input uels and dims */
void
checkForValidData(SEXP val,
                  SEXP uelOut,
                  dType_t dType,
                  dForm_t dForm)
{
  SEXP dims;
  int i, j, k;
  double *pd;
  int *pi;
  double dt;
  int nDimsData, nDimsUels;
  int ncols, nrows;
  int max;

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
    error (".val must be numeric.\n");
  }
  dims = getAttrib(val, R_DimSymbol);

  if (dForm == sparse) {
    nrows = INTEGER(dims)[0];
    ncols = INTEGER(dims)[1];

    if (dType == parameter) {
      ncols--;
    }
    if (nDimsUels != ncols) {
      error ("Number of columns in sparse data does not match with UEL dimension.");
    }
    /* the matrix of vals is stored column-major */
    for (j = 0;  j < ncols;  j++) {
      for (max = 0, i = 0;  i < nrows;  i++) {
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
        if (k > max) {
          max = k;
        }
      }
      if (max > (int)length(VECTOR_ELT(uelOut, j))) {
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


/* Only called from gdxInfo */
char *
val2str (gdxHandle_t h, double val, char *s)
{
  int sv;

  if (gdxAcronymName(h, val, s)) {
    return s;
  }
  else {
    gdxMapValue(h, val, &sv);
    if (sv_normal != sv)
      sprintf(s,"%s", gmsSVText[sv]);
    else
      sprintf(s,"%g", val);
    return s;
  }
} /* val2str */


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

/* This method checks the file extension of the gdx file.
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

/* Every string input has to be in certain limit to be writen to GDX file */
void
checkStringLength(const char *str)
{
  int i;

  i = (int) strlen (str);
  if (0 == i) {
    error("Cannot access empty field. Please try again" );
  }
  else if (i >= sizeof(shortStringBuf_t)) {
    error("The data entered is too long: len=%d exceeds limit=%d.",
          i, (int)sizeof(shortStringBuf_t)-1);
  }
} /* checkStringLength */


void downCase (char *s)
{
  if (NULL == s)
    return;
  for ( ;  *s;  s++)
    *s = tolower(*s);
  return;
} /* downCase */


/* Return number of non - Zero elements of variable/equation */
int
getNonZeroElements (gdxHandle_t h, int symIdx, dField_t dField)
{
  int nRecs, changeIdx, i, nonZero;
  gdxUelIndex_t uels;
  gdxValues_t values;

  nonZero = 0;
  gdxDataReadRawStart (h, symIdx, &nRecs);
  for (i = 0; i < nRecs; i++) {
    gdxDataReadRaw (h, uels, values, &changeIdx);
    if (values[dField] != 0) {
      nonZero++;
    }
  }
  return nonZero;
}

/* This method compresses the raw data (both value and uel)
 * and removes redundant zeros from
 * value matrix and re-index output matrix.
 * And it also remove non present uel elements from UEL */
void
compressData (SEXP data,
              SEXP globalUEL,
              SEXP uelOut,
              int numberOfUel,
              int symbolDim,
              int nRec)
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
    bufferUel = allocVector(STRSXP, total);
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
  }
  free(mask);
  alloc++;
  return;
} /* compressData */


/* This method converts sparse data into full data */
SEXP
sparseToFull(SEXP compVal,
             SEXP compFullVal,
             SEXP compUels,
             int type,
             int nRec,
             int symbolDim)
{
  int i,j, iRec,  nCols;
  double *p, *pVal;
  int index = 0;
  int totNumber = 1;

  /* Step 1: loop over full matrix and set every value as 0 */
  pVal = REAL(compFullVal);
  for (j = 0; j < length(compFullVal); j++) {
    pVal[j] = 0;
  }
  i = 0;
  /* Step 2: loop over each row of sparse matrix and populate full matrix */
  p = REAL(compVal);
  nCols = symbolDim;

  for (iRec = 0; iRec < nRec; iRec++) {
    index = 0;
    totNumber = 1;
    for (i = 0; i < nCols; i++) {
      index = index + (p[iRec + nRec*i] - 1)*totNumber;
      totNumber = (totNumber)*length(VECTOR_ELT(compUels, i));
    }
    if (type != dt_set) {
      pVal[index] = p[iRec + nRec*symbolDim];
    }
    else {
      pVal[index] = 1;
    }
  }
  return compFullVal;
} /* End of sparseToFull */


/* create text element Matrix from sparse data and element vector */
SEXP
createElementMatrix(SEXP compVal,
                    SEXP textElement,
                    SEXP compTe,
                    SEXP compUels,
                    int symbolDim,
                    int nRec)
{
  int i, j, iRec, index, totNumber;
  double *p;
  int  nCols;

  /* Step 1: loop over full matrix and set every value as empty string */

  for (j = 0; j < length(compTe); j++) {
    SET_STRING_ELT(compTe, j, mkChar(""));
  }

  /* Step 2: loop over each row of sparse matrix and populate full matrix */
  nCols = symbolDim;
  p = REAL(compVal);

  for (iRec = 0; iRec < nRec; iRec++) {
    index = 0;
    totNumber = 1;
    for (i = 0; i < nCols; i++) {
      index = index + ((int)p[iRec + nRec*i] - 1)*totNumber;
      totNumber = (totNumber)*length(VECTOR_ELT(compUels, i));
    }
    SET_STRING_ELT(compTe, index, duplicate(STRING_ELT(textElement, iRec)) );
  }

  return compTe;
} /* End of createElementMatrix */


/* This method is to check repetition in Input UEL.
 * It has to be a list of unique elements, without repetition */
void
checkForRepetition(SEXP bufferUel)
{
  int i;
  shortStringBuf_t *elements;
  int nRec;

  /* Step 1: get all the string element and store in array*/
  nRec = length(bufferUel);

  elements =  malloc(nRec * sizeof(*elements));
  for (i = 0; i < nRec; i++) {
    strcpy(elements[i], CHAR(STRING_ELT(bufferUel, i)));
  }
  /* Step 2: Sort the array */
  qsort(elements, nRec, sizeof(*elements), (compareFunc_t)strcmp);
  /* Step 3: loop over the sorted array and check for repetition */
  i = 0;
  for (i = 0; i < nRec - 1; i++) {
    if (0 == strcmp(elements[i], elements[i+1])) {
      Rprintf("Input UEL have repeated entry of '%s'\n",
              elements[i]);
      error("Repetition in input UEL is not allowed.");
    }
  }
  free(*elements);
} /* checkForRepetition */


/* This method convert input UEL cell into output cell array of strings */
SEXP
convertToOutput(SEXP bufferUel,
                SEXP tmpUel)
{
  int  len;
  char buffer [256];
  double *doubleData;
  int *intData;
  int w = 0;

  len = length(tmpUel);
  if (TYPEOF(tmpUel) == REALSXP) {
    doubleData = REAL(tmpUel);
    for (w = 0; w < len; w++) {
      sprintf(buffer, "%g", doubleData[w]);
      SET_STRING_ELT(bufferUel, w, mkChar(buffer));
    }
  }
  else if (TYPEOF(tmpUel) == INTSXP) {
    intData = INTEGER(tmpUel);
    for (w = 0; w < len; w++) {
      sprintf(buffer, "%i", intData[w]);
      SET_STRING_ELT(bufferUel, w, mkChar(buffer));
    }
  }
  else if (TYPEOF(tmpUel) == STRSXP) {
    for (w = 0; w < len; w++) {
      SET_STRING_ELT(bufferUel, w, duplicate(STRING_ELT(tmpUel, w)));
    }
  }

  checkForRepetition(bufferUel);

  return bufferUel;
} /* convertToOutput*/


int checkIfExist (int k, SEXP filterUel, const char *uelName)
{
  SEXP tmpUel;
  int i, n;
  const char *uelString;
  int element = 0;

  /* TODO: uelString has to be const char *,
   * that's why there is a warrning message at compilation */
  tmpUel = VECTOR_ELT(filterUel, k);
  n = length(tmpUel);
  for (i=0; i < n; i++) {
    uelString = CHAR(STRING_ELT(tmpUel, i));
    if (0 == strcmp(uelString, uelName)) {
      element = i+1;
      break;
    }
  }
  return element;
} /* checkIfExist */


/* checkSymList: checks if a is potentially a valid symList
 * return:
 *   0 if OK
 *   1 if no names are found
 *   2 if an unamed field is found
 *   3 if an invalid field is found
 *   4 if symbol is not a list
 *   others possible but not yet used
 */
static int
checkSymList (SEXP a, shortStringBuf_t msg)
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
} /* checkSymList */


void
processListList (SEXP a, int argNum, SEXP *symList, int symListSiz, int *symListLen)
{
  int n, k;
  int rc;
  SEXP aaa;
  shortStringBuf_t msg;

  n = length(a);
  for (k = 0;  k < n;  k++) {
    /* Rprintf ("processListList: arg %d element %d\n", argNum, k+1); */
    aaa = VECTOR_ELT(a, k);
    rc = checkSymList (aaa, msg);
    if (0 == rc) {
      /* Rprintf ("processListList: argument %d element %d is a symList\n", argNum, k+1); */
      if (*symListLen >= symListSiz)
        error ("processListList: internal error processing symbol list\n");
      symList[*symListLen] = aaa;
      ++*symListLen;
    }
    else {
      error ("processListList: argument %d element %d is not a valid symbol List: %s", argNum, k+1, msg);
    }
  }
  return;
} /* processListList */


static void
processArg (SEXP a, int argNum, SEXP *symList, int symListSiz, int *symListLen)
{
  SEXP lstName;
  int rc;
  shortStringBuf_t msg;

  lstName = getAttrib (a, R_NamesSymbol);
  if (R_NilValue == lstName) {
    /* Rprintf ("processArg: found potential list of lists\n"); */
    processListList (a, argNum, symList, symListSiz, symListLen);
  }
  else {
    /* Rprintf ("processArg: found potential symbol list\n"); */
    rc = checkSymList (a, msg);
    if (0 == rc) {
      /* Rprintf ("processArg: argument %d is a symList\n", argNum); */
      if (*symListLen >= symListSiz)
        error ("processArg: internal error processing symbol list\n");
      symList[*symListLen] = a;
      ++*symListLen;
    }
    else {
      error ("processArg: argument %d has names but is not a valid symbol list: %s", argNum, msg);
    }
  }
  return;
} /* processArg */


void
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
        error ("usage: wgdx: unrecognized argument name '%s'\n", argName);
      }
      if (i != argLen-1) {
        error ("usage: wgdx: argument '%s' must follow symbol lists\n", argName);
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
          error ("usage: wgdx: argument '%s=<empty_string>' is invalid\n", argName);
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
            error ("usage: wgdx: argument '%s=%s' is invalid\n", argName, s);
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
            error ("usage: wgdx: argument '%s=%s' is invalid\n", argName, s);
        }
        break;
      default:
        error ("usage: wgdx: argument '%s' is invalid\n", argName);
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
    processArg (t, i, *symList, *symListSiz, symListLen);
  }

  return;
} /* unpackWgdxArgs */


void
registerInputUEL(SEXP uelOut,
                 int k,
                 SEXP uelIndex)
{
  int i, j, rc, gi;
  char bufChar[256];
  const  char *uelString;
  int nCols, nSubElements;
  SEXP mainBuffer, subBuffer, dummy;

  nCols = length(uelOut);
  PROTECT( mainBuffer = allocVector(VECSXP, nCols));
  wAlloc++;

  for (i = 0; i < nCols; i++) {
    dummy = VECTOR_ELT(uelOut, i);

    nSubElements = length(dummy);

    subBuffer = allocVector(STRSXP, nSubElements);

    for (j = 0; j < nSubElements; j++) {
      /* get string and register to gdx */
      uelString = CHAR(STRING_ELT(dummy, j));
      /* Rprintf("str at %d is %s\n", j, uelString); */

      rc = gdxUELRegisterStr (gdxHandle, uelString, &gi);
      if (rc != 1) {
        error ("could not register: %s\n", uelString);
      }
      sprintf(bufChar, "%d", gi);
      SET_STRING_ELT(subBuffer, j, mkChar(bufChar));
    }

    SET_VECTOR_ELT(mainBuffer, i, subBuffer);
  }
  SET_VECTOR_ELT(uelIndex, k, mainBuffer);
} /* registerInputUEL */


/* This function validates the structure of the input lists,
 * sets certain global variables, and constructs the universe of UELs */
void
readWgdxList(SEXP structure,
             int iSym,
             SEXP uelIndex,
             wgdxStruct_t **wgdxRecPtr,
             int fromGAMS)
{
  SEXP lstName, tmpUel;
  SEXP dimension;
  SEXP nameExp = NULL;
  SEXP typeExp = NULL;
  SEXP valExp = NULL;
  SEXP uelsExp = NULL;
  SEXP formExp = NULL;
  SEXP dimExp = NULL;
  SEXP tsExp = NULL;
  int i, j;
  int listLen;
  int dimUels;
  int nCoords = 0, sz, withDim;
  const char *tmpName;
  const char *compName;              /* pointers to field names */
  SEXP uelOut, bufferUel;            /* allocating temporary storage place */
  wgdxStruct_t *inData;

  withDim = 0;
  uelOut = R_NilValue;

  inData = (wgdxStruct_t *) malloc(sizeof(*inData));
  *wgdxRecPtr = inData;
  inData->dForm = sparse;
  inData->dType = set;
  inData->withTs = 0;
  inData->withVal = 0;
  inData->withUel = 0;
  inData->dim = 0;

  /* check maximum number of fields */
  if (7 < length(structure) || length(structure) < 1) {
    error("Incorrect number of components in input list argument.\n");
  }

  listLen = length(structure);
  lstName = getAttrib(structure, R_NamesSymbol);
  if (lstName == R_NilValue) {
    error ("Input symbol list has no field names, %s", validFieldMsg);
  }

  /* first, check that all names are recognized, reject o/w
   * in the process, store the symbol for direct access later
   */
  for (i = 0; i < listLen; i++) {
    compName = CHAR(STRING_ELT(lstName, i));
    if (0 == strcmp("name", compName)) {
      nameExp = VECTOR_ELT(structure, i);
    }
    else if (0 == strcmp("type", compName)) {
      typeExp = VECTOR_ELT(structure, i);
    }
    else if (0 == strcmp("val", compName)) {
      valExp = VECTOR_ELT(structure, i);
    }
    else if (0 == strcmp("uels", compName)) {
      uelsExp = VECTOR_ELT(structure, i);
    }
    else if (0 == strcmp("form", compName)) {
      formExp = VECTOR_ELT(structure, i);
    }
    else if (0 == strcmp("dim", compName)) {
      dimExp = VECTOR_ELT(structure, i);
    }
    else if (0 == strcmp("ts", compName)) {
      tsExp = VECTOR_ELT(structure, i);
    }
    else {
      Rprintf ("Input list components must be according to this specification:\n");
      Rprintf ("'name', 'type', 'val', 'uels', 'form', 'dim', 'ts'.\n");
      error ("Incorrect type of input list component '%s' specified.",
             compName);
    }
  }

  /* now process the fields provided */
  if (NULL == nameExp)
    error ("Required list component 'name' is missing. Please try again.\n");
  if (STRSXP != TYPEOF(nameExp)) {
    Rprintf ("List component 'name' must be a string - found %d instead\n",
             TYPEOF(nameExp));
    error ("Input list component 'name' must be string.\n");
  }
  tmpName = CHAR(STRING_ELT(nameExp, 0));
  checkStringLength (tmpName);
  strcpy (inData->name, tmpName);

  if (tsExp) {
    if (STRSXP != TYPEOF(tsExp)) {
      Rprintf ("List component 'ts' must be a string - found %d instead\n",
               TYPEOF(tsExp));
      error ("Input list component 'ts' must be string.\n");
    }
    checkStringLength (CHAR(STRING_ELT(tsExp, 0)));
    inData->withTs = 1;
  }

  if (formExp) {
    if (STRSXP != TYPEOF(formExp)) {
      Rprintf ("List component 'form' must be a string - found %d instead\n",
               TYPEOF(formExp));
      error ("Input list component 'form' must be string");
    }
    tmpName = CHAR(STRING_ELT(formExp, 0));
    if (strcasecmp("full", tmpName) == 0) {
      inData->dForm = full;
    }
    else if (strcasecmp("sparse", tmpName) == 0) {
      inData->dForm = sparse;
    }
    else {
      error("Input list component 'form' must be either 'full' or 'sparse'.");
    }
  } /* formExp */

  if (typeExp) {                /* optional */
    if (STRSXP != TYPEOF(typeExp)) {
      Rprintf ("List component 'type' must be a string - found %d instead\n",
               TYPEOF(typeExp));
      error ("Input list component 'type' must be string.\n");
    }
    tmpName = CHAR(STRING_ELT(typeExp, 0));
    if (0 == strcasecmp("set", tmpName) ) {
      inData->dType = set;
    }
    else if (0 == strcasecmp("parameter", tmpName) ) {
      inData->dType = parameter;
    }
    else {
      Rprintf ("type found = %s\n", tmpName);
      error ("Input list component 'type' must be either 'set' or 'parameter'.\n");
    }
  }

  if (dimExp) {                 /* optional */
    if (INTSXP == TYPEOF(dimExp)) {
      if (length(dimExp) != 1) {
        error ("Optional input list component 'dim' must have only one element.\n");
      }
      if (INTEGER(dimExp)[0] < 0) {
        error("Negative value is not allowed as valid input for 'dim'.\n");
      }
      withDim = 1;
      inData->dim = INTEGER(dimExp)[0];
    }
    else if (REALSXP == TYPEOF(dimExp)) {
      if (length(dimExp) != 1) {
        error ("Optional input list component 'dim' must have only one element.\n");
      }
      if (REAL(dimExp)[0] < 0) {
        error("Negative value is not allowed as valid input for 'dim'.\n");
      }
      withDim = 1;
      inData->dim = (int) REAL(dimExp)[0];
      if (REAL(dimExp)[0] != inData->dim) {
        error("Non-integer value is not allowed as valid input for 'dim'.\n");
      }
    }
    else {
      Rprintf ("List component 'dim' must be numeric - found %d instead\n",
               TYPEOF(dimExp));
      error ("Optional input list component 'dim' must be numeric.\n");
    }
  } /* dimExp */

  dimUels = -1;
  if (uelsExp) {                /* optional */
    if (VECSXP != TYPEOF(uelsExp)) {
      Rprintf ("List component 'uels' must be an un-named list - found %d instead\n",
               TYPEOF(uelsExp));
      error ("Input list component 'uels' must be unnamed list.\n");
    }
    dimUels = length(uelsExp);
    if (0 == dimUels) {
      error ("Empty input list component 'uels' is not allowed.\n");
    }
    if (withDim && inData->dim != dimUels) {
      error ("Inconsistent dimension found: 'dim'=%d  doesn't match '.uels' dimension=%d.\n",
             inData->dim, dimUels);
    }
    PROTECT(uelOut = allocVector(VECSXP, dimUels));
    wAlloc++;
    for (j = 0;  j < dimUels;  j++) {
      tmpUel = VECTOR_ELT(uelsExp, j);
      if (tmpUel == R_NilValue) {
        error ("Empty input field in .uels not allowed\n");
      }
      if (TYPEOF(tmpUel) == STRSXP) {
        /*  checkStringLength( CHAR(STRING_ELT(tmp, 0)) ); */
        SET_VECTOR_ELT (uelOut, j, tmpUel);
      }
      else if (TYPEOF(tmpUel) == REALSXP || TYPEOF(tmpUel) == INTSXP) {
        /* Convert to output */
        bufferUel = allocVector(STRSXP, length(tmpUel));
        bufferUel =  convertToOutput (bufferUel, tmpUel);
        SET_VECTOR_ELT (uelOut, j, bufferUel);
      }
      else {
        error ("Input uels must be either string vectors or numeric vectors.\n");
      }
    }
    inData->withUel = 1;
  } /* uelsExp */

  if (NULL == valExp) {         /* .val field missing */
    if (parameter == inData->dType) {
      error ("Missing 'val' is a required list component for parameters.");
    }
    if (set == inData->dType && 0 == inData->withUel) {
      error ("Missing 'val' is a required list component for sets with no UELs.");
    }
  }
  else {
    if (fromGAMS == 1 && TYPEOF(valExp) == STRSXP) {
      tmpName = CHAR(STRING_ELT(valExp, 0));
      checkStringLength(tmpName);
      strcat(specialCommand, "--");
      strcat(specialCommand,  inData->name);
      strcat(specialCommand, "=");
      strcat(specialCommand, tmpName);
      strcat(specialCommand, " ");
      return;
    }
    dimension = getAttrib(valExp, R_DimSymbol);
    if (TYPEOF(valExp) == REALSXP || TYPEOF(valExp) == INTSXP ) {
      if (inData->dForm == sparse) {
        if (length(dimension) != 2) {
          Rprintf("You have entered a %d dimensional matrix.\n", length(dimension));
          error ("Only 2-dimensional '.val' is allowed as valid input in sparse format.");
        }
        /* getting data matrix */
        sz = INTEGER(dimension)[0];
        if (sz > INT_MAX) {
          error ("Input list component 'val' exceeds row limit of %d",
                 INT_MAX);
        }
        sz = INTEGER(dimension)[1];
        if (sz > INT_MAX) {
          error ("Input list component 'val' exceeds column limit of %d",
                 INT_MAX);
        }
        nCoords = sz;
        if (parameter == inData->dType) {
          nCoords--;
        }
        if (nCoords > GMS_MAX_INDEX_DIM) {
          error ("Input list compoment 'val' exceeds GDX dimension limit of %d.",
                 GMS_MAX_INDEX_DIM);
        }
        if (withDim) {
          if (inData->dim != nCoords) {
            error ("Inconsistent dimensions found: '.dim' = %d doesn't match"
                   " dimension=%d implied by '.val'\n", inData->dim, nCoords);
          }
        }
        else if (dimUels > 0) {
          if (dimUels != nCoords) {
            error ("Inconsistent dimensions implied by '.uels' (%d) and"
                   " '.val' (%d)\n", dimUels, nCoords);
          }
        }
        inData->withVal = 1;
      } /* if sparse */
      else {
        /* This is for Full/Dense data */
        nCoords = length(dimension);
        if (nCoords > GMS_MAX_INDEX_DIM) {
          error ("Input list component 'val' exceeds GDX dimension limit of %d.",
                 GMS_MAX_INDEX_DIM);
        }
        if (withDim) {
          if (inData->dim != nCoords) {
            error ("Inconsistent dimensions found: '.dim' = %d doesn't match"
                   " '.val' dimension %d.\n", inData->dim, nCoords);
          }
        }
        else if (dimUels > 0) {
          if (dimUels != nCoords) {
            error ("Inconsistent dimensions implied by '.uels' (%d) and"
                   " '.val' (%d)\n", dimUels, nCoords);
          }
        }
        inData->withVal = 1;
      }
    }
    else {
      Rprintf("List component 'val' must be a numeric matrix - found %d instead.\n",
              TYPEOF(valExp));
      error ("Input list component 'val' must be a numeric matrix");
    }
  } /* valExp not NULL */


  if (inData->withUel == 0 && inData->withVal == 1) {
    PROTECT(uelOut = allocVector(VECSXP, nCoords));
    wAlloc++;
    createUelOut (valExp, uelOut, inData->dType, inData->dForm);
  }

  if (inData->withVal == 1) {
    checkForValidData (valExp, uelOut, inData->dType, inData->dForm);
  }
  registerInputUEL (uelOut, iSym, uelIndex);
} /* readWgdxList */


/* This function check the input list for valid data */
void
checkRgdxList (const SEXP lst, struct rgdxStruct *data)
{
  SEXP lstName, tmp, tmpUel;
  SEXP bufferUel;
  int i, j,  nField, found;
  const char *tmpName;
  const char *compName;
  Rboolean compress = NA_LOGICAL;

  found = 0;
  nField = 0;

  /* check maximum number of fields */
  if (7  < length(lst) || length(lst) < 1) {
    error("Incorrect number of components in input list argument.");
  }
  else {
    nField = length(lst);
  }

  lstName = getAttrib(lst, R_NamesSymbol);

  if (lstName == R_NilValue) {
    Rprintf("Input list must be named\n");
    Rprintf("Valid names are: 'name', 'dim', 'uels', 'form', 'compress', 'field', 'te', 'ts'.\n");
    error("Please try again with named input list.\n");
  }
  for (i=0; i < nField; i++) {
    compName = CHAR(STRING_ELT(lstName, i));
    /* Checking for valid field name */
    if ( !((0 == strcmp("name", compName ))
           || (0 == strcmp("dim", compName ))
           || (0 == strcmp("uels", compName ))
           || (0 == strcmp("form", compName ))
           || (0 == strcmp("compress", compName ))
           || (0 == strcmp("field", compName ))
           || (0 == strcmp("te", compName ))
           || (0 == strcmp("ts", compName ))
           ) ) {
      Rprintf ("Input list components must be according to this specification:\n");
      Rprintf ("'name', 'dim', 'uels', 'form', 'compress', 'field', 'te', 'ts'.\n");
      error("Incorrect type of input list component '%s' specified.",
            compName);
    }
  }

  i=0;
  /* Checking if field data is for "name" */
  for (i = 0; i < nField; i++) {
    if (strcmp("name", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }

  if (found == 1) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) == STRSXP) {
      checkStringLength( CHAR(STRING_ELT(tmp, 0)) );
      strcpy( data->name, CHAR(STRING_ELT(tmp, 0)) );
    }
    else {
      Rprintf ("List component 'name' must be a string - found %d instead\n",
               TYPEOF(tmp) );
      error("Input list component 'name' must be string.\n");
    }
  }
  else {
    error("Required list component 'name' is missing. Please try again.\n" );
  }

  /* Checking for 'form'. Default it to 'sparse'*/
  i = 0;
  found = 0;
  for (i = 0; i < nField; i++) {
    if (strcmp("form", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }
  if (found == 1) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) != STRSXP ) {
      Rprintf ("List component 'form' must be a string - found %d instead\n",
               TYPEOF(tmp) );
      error("Input list component 'form' must be string");
    }
    tmpName = CHAR(STRING_ELT(tmp, 0));
    if (strlen(tmpName) == 0) {
      error("Input list component 'form' must be either 'full' or 'sparse'.");
    }
    if (0 == strcasecmp("full", tmpName)) {
      data->dForm = full;
    }
    else if (0 == strcasecmp("sparse", tmpName)) {
      data->dForm = sparse;
    }
    else {
      error("Input list component 'form' must be either 'full' or 'sparse'.");
    }
  }

  /* Checking for 'compress'. Default it to 'false' */
  i=0;
  found = 0;
  for (i = 0; i < nField; i++) {
    if (strcmp("compress", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }

  if (found == 1) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) == STRSXP) {
      tmpName = CHAR(STRING_ELT(tmp, 0));
      if (strlen(tmpName) == 0) {
        error("Input list component 'compress' must be either 'true' or 'false'.");
      }
      if (0 == strcasecmp("true", tmpName)) {
        data->compress = 1;
      }
      else if (0 == strcasecmp("false", tmpName)) {
        data->compress = 0;
      }
      else {
        error("Input list component 'compress' must be either 'true' or 'false'.");
      }
    }
    else if (TYPEOF(tmp) == LGLSXP) {
      compress = LOGICAL(tmp)[0];
      if (compress == TRUE) {
        data->compress = 1;
      }
      else {
        data->compress = 0;
      }
    }
    else {
      Rprintf ("List component 'compress' must be either string or logical - found %d instead\n",
               TYPEOF(tmp) );
      error("Input list component 'compress' must be either string or logical");
    }
  }

  /* Checking for field.  Default it to 'level' */
  i=0;
  found = 0;
  for (i = 0; i < nField; i++) {
    if (strcmp("field", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }

  if (found == 1) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) != STRSXP ) {
      Rprintf ("List component 'field' must be a string - found %d instead\n",
               TYPEOF(tmp) );
      error("Input list component 'field' must be string");
    }
    tmpName = CHAR(STRING_ELT(tmp, 0));
    if (strlen(tmpName) == 0) {
      error("Input list component 'field' must be from 'l', 'm', 'lo', 'up' or 's'.");
    }
    data->withField = 1;
    if (0 == strcasecmp("l", tmpName)) {
      data->dField = level;
    }
    else if (0 == strcasecmp("m", tmpName)) {
      data->dField = marginal;
    }
    else if (0 == strcasecmp("lo", tmpName)) {
      data->dField = lower;
    }
    else if (0 == strcasecmp("up", tmpName)) {
      data->dField = upper;
    }
    else if (0 == strcasecmp("s", tmpName)) {
      data->dField = scale;
    }
    else {
      error("Input list component 'field' must be from 'l', 'm', 'lo', 'up' or 's'.");
    }
  }

  /* Checking for ts. Default it to 'false' */
  i=0;
  found = 0;
  for (i = 0; i < nField; i++) {
    if (strcmp("ts", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }

  if (found == 1) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) == STRSXP ) {
      tmpName = CHAR(STRING_ELT(tmp, 0));
      if (strlen(tmpName) == 0) {
        error("Input list component 'ts' must be either 'true' or 'false'.");
      }
      if (0 == strcasecmp("true", tmpName)) {
        data->ts = 1;
      }
      else if (0 == strcasecmp("false", tmpName)) {
        data->ts = 0;
      }
      else {
        error("Input list component 'ts' must be either 'true' or 'false'.");
      }
    }
    else if (TYPEOF(tmp) == LGLSXP) {
      if (LOGICAL(tmp)[0] == TRUE) {
        data->ts = 1;
      }
    }
    else {
      Rprintf ("List component 'ts' must be either string or logical - found %d instead\n",
               TYPEOF(tmp) );
      error("Input list component 'ts' must be either string or logical");
    }
  }

  /* Checking for 'te'. Default it to 'false' */
  i=0;
  found = 0;
  for (i = 0; i < nField; i++) {
    if (strcmp("te", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }

  if (found == 1) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) == STRSXP ) {
      tmpName = CHAR(STRING_ELT(tmp, 0));
      if (strlen(tmpName) == 0) {
        error("Input list component 'te' must be either 'true' or 'false'.");
      }
      if (0 == strcasecmp("true", tmpName)) {
        data->te = 1;
      }
      else if (0 == strcasecmp("false", tmpName)) {
        data->te = 0;
      }
      else {
        error("Input list component 'te' must be either 'true' or 'false'.");
      }
    }
    else if (TYPEOF(tmp) == LGLSXP) {
      if (LOGICAL(tmp)[0] == TRUE) {
        data->te = 1;
      }
      else {
        data->te = 0;
      }
    }
    else {
      error("Input list component 'te' must be either string or logical"
            " - found %d instead\n", TYPEOF(tmp));
    }
  }

  /* Checking for uels. Used in filtered read */
  i=0;
  found = 0;
  for (i = 0; i < nField; i++) {
    if (strcmp("uels", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }

  if (found == 1) {
    tmp = VECTOR_ELT(lst, i);
    if (TYPEOF(tmp) != VECSXP) {
      error("List component 'uels' must be a list.");
    }
    else {
      PROTECT(data->filterUel = allocVector(VECSXP, length(tmp)));
      alloc++;
      for (j = 0; j < length(tmp); j++) {
        tmpUel = VECTOR_ELT(tmp, j);
        if (tmpUel == R_NilValue) {
          error("Empty Uel is not allowed ");
        }
        else {
          bufferUel = allocVector(STRSXP, length(tmpUel));
          /* Convert to output */
          bufferUel =  convertToOutput(bufferUel, tmpUel);
          SET_VECTOR_ELT(data->filterUel, j, bufferUel);
        }
      }
      data->withUel = 1;
    }
  }
} /* checkRgdxList */


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


/* This method is intended to be used by both wgdx and gams call
   fileName = name of GDX file to be writen
   symList = vector of symList's entered by user
   fromGAMS = 1 if this method is called from GAMS otherwise 0
*/
static void
writeGdx(char *gdxFileName,
         int symListLen,
         SEXP *symList,
         int fromGAMS,
         char zeroSqueeze)
{
  FILE *matdata = NULL;
  SEXP uelIndex, compName, valData;
  SEXP mainBuffer, subBuffer;
  wgdxStruct_t **data;
  gdxUelIndex_t uelIndices;
  gdxValues_t   vals;
  gdxSVals_t sVals;
  union d64_t d64;
  shortStringBuf_t msgBuf;
  shortStringBuf_t expText;
  shortStringBuf_t gsBuf;
  const char *stringUelIndex;
  int rc, errNum;
  int i, j, k, found;
  int iSym;
  int idx;
  SEXP dimVect;
  int totalElement, total,  nColumns, nRows, ndimension, index, total_num_of_elements;
  int d, subindex, inner;
  double *dimVal, *pd, dt, posInf, negInf;
  int *pi;
  int *subscript;
  const char *inputTime;

  /* shut up compiler warnings */
  valData = NULL;

  total = 1;
  wAlloc = 0;
  inputTime = "compile";
  if (fromGAMS == 1) {
    /* Open files for interface data */
    inputTime = getGlobalString("input", gsBuf);
    if (NULL == inputTime)
      inputTime = "compile";

    if ((matdata = fopen("matdata.gms","w")) == NULL)
      error("Cannot write 'matdata.gms' file.");
    if (strcmp(inputTime,"exec") != 0) {
      fprintf(matdata,"$gdxin matdata.gdx\n");
    }
  }

  loadGDX();
  rc = gdxCreate (&gdxHandle, msgBuf, sizeof(msgBuf));
  if (0 == rc)
    error ("Error creating GDX object: %s", msgBuf);

  if (fromGAMS == 1) {
    gdxFileName = "matdata.gdx";
  }

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
    error ("could not gdxUELRegisterStrStart\n");
  }

  PROTECT(uelIndex = allocVector(VECSXP, symListLen));
  wAlloc++;

  data = (wgdxStruct_t **) malloc (symListLen * sizeof(data[0]));

  /* check input list(s) for data validation and to create UEL list */
  for (iSym = 0;  iSym < symListLen;  iSym++) {
    if (TYPEOF(symList[iSym]) != VECSXP) {
      error("Incorrect type of input encountered. List expected\n");
    }
    else {
      readWgdxList (symList[iSym], iSym, uelIndex, data+iSym, fromGAMS);
    }
  }

  rc = gdxUELRegisterDone(gdxHandle);
  if (! rc)
    error ("could not gdxUELRegisterDone: rc = %d\n", rc);

  /* start writing data to GDX file */
  memset (uelIndices, 0, sizeof(gdxUelIndex_t));
  memset (vals, 0, sizeof(gdxValues_t));

  i=0;
  /* write data in GDX file */
  for (i = 0;  i < symListLen;  i++) {
    /*
     *  Looking for 'val'
     *  This is the glitch that i am worried about
     */
    compName = getAttrib(symList[i], R_NamesSymbol);
    for (found = 0, j = 0; j < length(symList[i]); j++) {
      if (strcmp("val", CHAR(STRING_ELT(compName, j))) == 0) {
        found = 1;
        break;
      }
    }

    if (1 == found) {
      valData = VECTOR_ELT(symList[i], j);
    }
    /* This is special check */
    if (fromGAMS == 0
        || (fromGAMS == 1
             && (found == 0 || TYPEOF(valData) != STRSXP))) {
      mainBuffer = VECTOR_ELT(uelIndex, i);
      if (fromGAMS) {
        if (strcmp(inputTime,"exec") == 0) {
          fprintf(matdata,"execute_load 'matdata.gdx' %s;\n", data[i]->name);
        }
        else {
          fprintf(matdata,"$kill %s\n$load %s\n",  data[i]->name, data[i]->name);
        }
      }
      /* creating value for set that does not have .val */
      if (data[i]->dType == set) {
        if (data[i]->withVal == 0 &&  data[i]->withUel == 1) {
          nColumns = length(mainBuffer);
          PROTECT(dimVect = allocVector(REALSXP, nColumns));
          wAlloc++;
          totalElement = 1;
          dimVal = REAL(dimVect);
          ndimension = 0;

          for (ndimension = 0; ndimension < (int)nColumns; ndimension++) {
            dimVal[ndimension] = length(VECTOR_ELT(mainBuffer, ndimension));
            totalElement = (totalElement * length(VECTOR_ELT(mainBuffer, ndimension)));
          }
          PROTECT(valData = allocVector(REALSXP, totalElement));
          wAlloc++;
          pd = REAL(valData);
          for (index = 0; index < totalElement; index++) {
            pd[index] = 1;
          }
          setAttrib(valData, R_DimSymbol, dimVect);
          index = 0;
          data[i]->dForm = full;
        }
      }
      (void) CHAR2ShortStr ("R data from GDXRRW", expText);
      if (data[i]->withTs == 1) {
        /* Looking for 'ts' */
        j = 0;
        found = 0;
        for (j = 0; j < length(symList[i]); j++) {
          if (strcmp("ts", CHAR(STRING_ELT(compName, j))) == 0) {
            found = 1;
            break;
          }
        }

        if (found == 1) {
          (void) CHAR2ShortStr (CHAR(STRING_ELT( VECTOR_ELT(symList[i], j), 0)), expText);
        }
      }

      if (data[i]->dForm == sparse) {
        dimVect = getAttrib(valData, R_DimSymbol);
        nColumns = INTEGER(dimVect)[1];
        nRows = INTEGER(dimVect)[0];

        if (data[i]->dType == parameter) {
          nColumns--;
          rc = gdxDataWriteMapStart (gdxHandle, data[i]->name, expText,
                                     nColumns, GMS_DT_PAR, 0);
        }
        else {
          rc = gdxDataWriteMapStart (gdxHandle, data[i]->name, expText,
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
            subBuffer = VECTOR_ELT(mainBuffer, k);
            if (pd) {
              idx = (int) pd[k*nRows + j];
            }
            else {
              idx = pi[k*nRows + j];
            }
            stringUelIndex = CHAR(STRING_ELT(subBuffer, idx-1));
            uelIndices[k] = atoi(stringUelIndex);
          }
          if (data[i]->dType == parameter) {
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
          if ((parameter == data[i]->dType) && 
              (0 == vals[0]) && ('e' == zeroSqueeze))
            vals[0] = sVals[GMS_SVIDX_EPS];
          if ((set == data[i]->dType) ||
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
        nColumns = length(mainBuffer);
        subscript = malloc(nColumns*sizeof(*subscript));
        if (data[i]->dType == parameter) {
          rc = gdxDataWriteMapStart (gdxHandle, data[i]->name, expText,
                                     nColumns, GMS_DT_PAR, 0);
        }
        else {
          rc = gdxDataWriteMapStart (gdxHandle, data[i]->name, expText,
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
              subBuffer = VECTOR_ELT(mainBuffer, d);
              for (total=1, inner=0; inner<d; inner++) {
                total *= INTEGER(dimVect)[inner];
              }
              subscript[d] = subindex / total;

              stringUelIndex = CHAR(STRING_ELT(subBuffer, subscript[d]));

              uelIndices[d] = atoi(stringUelIndex);

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
          if (data[i]->dType == parameter) {
            vals[0] = dt;
            if (ISNA(vals[0])) {
              vals[0] = sVals[GMS_SVIDX_NA];
            }
          }
          else if (set == data[i]->dType) {
            /* could do the check in checkForValidData but
             * that uses an additional pass through the full matrix */
            if (0 != dt && 1 != dt) {
              error ("Only zero-one values are allowed when specifying sets with form=full\n");
            }
          }
          if ((parameter == data[i]->dType) && 
              (0 == vals[0]) && ('e' == zeroSqueeze))
            vals[0] = sVals[GMS_SVIDX_EPS];
          if (((set == data[i]->dType) && (0 != dt))  ||
              ((parameter == data[i]->dType) && 
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
    }
  }    /* End of for(i) loop */

  if (fromGAMS) {
    if (strcmp(inputTime,"exec") != 0) {
      fprintf(matdata,"$gdxin\n");
    }
    fclose(matdata);
  }
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
  free(data);
  UNPROTECT(wAlloc);
} /* writeGdx */

static void
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

/* interpret the squeeze arg for rgdx as a logical/boolean */
static Rboolean getSqueezeArgRead (SEXP squeeze)
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

/* ---------------------------- rgdx -----------------------------------
 * This is the main gateway function
 * to be called from R console
 * First argument <- gdx file name
 * Second argument <- list containing several components
 * that give information about symbol in gdx file
 * ------------------------------------------------------------------ */
SEXP rgdx (SEXP args)
{
  const char *funcName = "rgdx";
  SEXP fileName, requestList, squeeze, UEList;
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
  SEXP OPListComp, OPList, dimVect, textElement, elVect;
  FILE    *fin;
  struct rgdxStruct *inputData;
  gdxUelIndex_t uels;
  gdxValues_t values;
  gdxSVals_t sVals;
  union d64_t d64;
  double dt, posInf, negInf;
  shortStringBuf_t msgBuf;
  shortStringBuf_t uelName;
  const char *uelElementName;
  shortStringBuf_t gdxFileName;
  int symIdx, symDim, symType;
  int rc, errNum, ACount, mrows, ncols, nUEL, iUEL;
  int  k, kk, iRec, nRecs, index, changeIdx, kRec;
  int UELUserMapping, highestMappedUEL;
  int arglen,  maxPossibleElements, z, b, matched, sparesIndex;
  double *p, *dimVal, *dimElVect;
  char buf[3*sizeof(shortStringBuf_t)];
  char symName[GMS_SSSIZE];
  char sText[GMS_SSSIZE], msg[GMS_SSSIZE], stringEle[GMS_SSSIZE];
  char *types[] = {"set", "parameter", "variable", "equation"};
  char *forms[] = {"full", "sparse"};
  char *fields[] = {"l", "m", "up", "lo", "s"};
  int nField, defaultIndex, elementIndex, IDum, ndimension, totalElement;
  int *returnedIndex;
  int withList = 0;
  int outFields = 6;
  int mwNElements =0;
  int uelProperty = 0;
  Rboolean zeroSqueeze = NA_LOGICAL;

  /* setting intial values */
  kRec = 0;
  alloc = 0;
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
  inputData = malloc(sizeof(*inputData));

  inputData->dForm = sparse;
  inputData->compress = 0;
  inputData->dField = level;
  inputData->ts = 0;
  inputData->te = 0;
  inputData->withUel = 0;
  inputData->withField = 0;

  if (withList) {
    checkRgdxList (requestList, inputData);
    if (inputData->compress == 1 && inputData->withUel == 1) {
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
    rc = gdxFindSymbol (gdxHandle, inputData->name, &symIdx);
    if (! rc) {
      sprintf (buf, "GDX file %s contains no symbol named '%s'\n",
               gdxFileName,
               inputData->name );
      error (buf);
    }
    gdxSymbolInfo (gdxHandle, symIdx, symName, &symDim, &symType);
    if (inputData->ts == 1) {
      gdxSymbolInfoX(gdxHandle, symIdx, &ACount, &rc, sText);
    }

    /* checking that symbol is of type parameter/set/equaltion/variable */
    if (!(symType == dt_par || symType == dt_set || symType == dt_var || symType == dt_equ)) {
      sprintf(buf, "GDX symbol %s (index=%d, symDim=%d, symType=%d)"
              " is not recognized as set, parameter, variable, or equation\n",
              inputData->name, symIdx, symDim, symType);
      error(buf);
    }
    else if ((symType == dt_par || symType == dt_set) && inputData->withField == 1) {
      error("Symbol '%s' is either set or parameter that can't have field.",
            inputData->name);
    }
    if (inputData->te == 1 && symType != dt_set) {
      error("Text elements only exist for set and symbol '%s' is not a set.",
            inputData->name);
    }
  }
  /* Get global UEL from GDX file */
  (void) gdxUMUelInfo (gdxHandle, &nUEL, &highestMappedUEL);
  PROTECT(UEList = allocVector(STRSXP, nUEL));
  alloc++;
  for (iUEL = 1;  iUEL <= nUEL;  iUEL++) {
    if (!gdxUMUelGet (gdxHandle, iUEL, uelName, &UELUserMapping)) {
      error("Could not gdxUMUelGet");
    }
    SET_STRING_ELT(UEList, iUEL-1, mkChar(uelName));
  }

  if (withList) {
    /* Checking dimension of input uel and paramter in GDX file.
     * If they are not equal then error. */

    if (inputData->withUel == 1 && length(inputData->filterUel) != symDim) {
      error("Dimension of UEL entered does not match with symbol in GDX");
    }
    /* Creating default uel if none entered */
    if (inputData->withUel != 1) {
      PROTECT(compUels = allocVector(VECSXP, symDim));
      alloc++;
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

    if (inputData->withUel == 1) {
      maxPossibleElements = 1;
      for (z = 0; z < symDim; z++) {
        mwNElements = length(VECTOR_ELT(inputData->filterUel, z));
        maxPossibleElements = maxPossibleElements*mwNElements;
      }
      mwNElements = 0;

      for (iRec = 0;  iRec < nRecs;  iRec++) {
        gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
        b = 0;
        for (k = 0; k < symDim; k++) {
          uelProperty = 0;
          uelElementName = CHAR(STRING_ELT(UEList, uels[k]-1));
          uelProperty = checkIfExist (k, inputData->filterUel, uelElementName);
          /* uel element exists */
          if (uelProperty > 0) {
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
    if (inputData->withUel == 1) {
      PROTECT(compVal = allocMatrix(REALSXP, mwNElements, ncols));
      alloc++;
      if (inputData->te && symType == dt_set) {
        PROTECT(textElement = allocVector(STRSXP, mwNElements));
        alloc++;
      }
    }
    if (inputData->withUel == 0) {
      /*  check for non zero elements for variable and equation */
      if ((symType == dt_var || symType == dt_equ) && zeroSqueeze) {
        mrows = getNonZeroElements(gdxHandle, symIdx, inputData->dField);
      }
      /* Creat 2D sparse R array */
      PROTECT(compVal = allocMatrix(REALSXP, mrows, ncols));
      alloc++;
      if (inputData->te && symType == dt_set) {
        PROTECT(textElement = allocVector(STRSXP, mrows));
        alloc++;
      }
    }

    p = REAL(compVal);
    /* TODO/TEST: filtered read */
    if (inputData->withUel == 1) {
      matched = 0;
      gdxDataReadRawStart (gdxHandle, symIdx, &nRecs);
      /* TODO/TEST: text elements with UEL */
      if (inputData->te == 1) {
        returnedIndex = malloc(symDim*sizeof(*returnedIndex));
        for (iRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          index = 0;
          b = 0;
          for (k = 0;  k < symDim;  k++) {
            uelProperty = 0;
            returnedIndex[k] = 0;
            uelElementName = CHAR(STRING_ELT(UEList, uels[k]-1));
            uelProperty = checkIfExist (k, inputData->filterUel, uelElementName);
            if (uelProperty > 0) {
              returnedIndex[k] = uelProperty;
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
      } /* End of inputData->te == 1 */
      else {
        returnedIndex = malloc(symDim*sizeof(*returnedIndex));
        for (iRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          index = 0;
          b = 0;

          for (k = 0;  k < symDim;  k++) {
            uelProperty = 0;
            returnedIndex[k] = 0;
            uelElementName = CHAR(STRING_ELT(UEList, uels[k]-1));
            uelProperty = checkIfExist (k, inputData->filterUel, uelElementName);
            if (uelProperty > 0) {
              returnedIndex[k] = uelProperty;
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
              p[index] = values[inputData->dField];
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
      if (inputData->te == 1) {
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
              (0 != values[inputData->dField])) {
            /* store the value */
            for (kk = 0;  kk < symDim;  kk++) {
              p[kRec + kk*mrows] = uels[kk];
            }
            index = kRec + symDim*mrows;
            kRec++;
            if (symType != dt_set)
              p[index] = values[inputData->dField];
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
    if (inputData->compress == 1) {
      PROTECT(compUels = allocVector(VECSXP, symDim));
      compressData (compVal, UEList, compUels, nUEL, symDim, mrows);
    }
    /* TODO/TEST: create full dimensional string matrix */
    if (inputData->te == 1) {
      if (symDim == 1) {
        PROTECT(elVect = allocVector(REALSXP, 2));
        alloc++;

        dimElVect = REAL(elVect);
        dimElVect[0] = length(VECTOR_ELT(compUels, 0));
        dimElVect[1] = 1;

        PROTECT(compTe = allocVector(STRSXP, length(VECTOR_ELT(compUels, 0)) ));
        alloc++;

        compTe = createElementMatrix(compVal, textElement, compTe, compUels, symDim, mrows);
        setAttrib(compTe, R_DimSymbol, elVect);
      }
      else {
        ndimension = 0;
        PROTECT(elVect = allocVector(REALSXP, symDim));
        alloc++;
        totalElement = 1;
        dimElVect = REAL(elVect);
        if (inputData->withUel) {
          for (ndimension = 0; ndimension < symDim; ndimension++) {
            dimElVect[ndimension] = length(VECTOR_ELT(inputData->filterUel, ndimension));
            totalElement = (totalElement * length(VECTOR_ELT(inputData->filterUel, ndimension)));
          }
          PROTECT(compTe = allocVector(STRSXP, totalElement));
          alloc++;
          compTe = createElementMatrix(compVal, textElement, compTe, inputData->filterUel, symDim, mwNElements);
          setAttrib(compTe, R_DimSymbol, elVect);
        }
        else {
          for (ndimension = 0; ndimension < symDim; ndimension++) {
            dimElVect[ndimension] = length(VECTOR_ELT(compUels, ndimension));
            totalElement = (totalElement * length(VECTOR_ELT(compUels, ndimension)));
          }
          PROTECT(compTe = allocVector(STRSXP, totalElement));
          alloc++;
          compTe = createElementMatrix(compVal, textElement, compTe, compUels, symDim, mrows);
          setAttrib(compTe, R_DimSymbol, elVect);
        }
      }
    }

    /* Converting sparse data into full matrix */
    if (inputData->dForm == full) {
      switch (symDim) {
      case 0: {
        PROTECT(compFullVal = allocVector(REALSXP, 1));
        alloc++;
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
        alloc++;
        dimVal = REAL(dimVect);

        if (inputData->withUel == 1) {
          dimVal[0] = length(VECTOR_ELT(inputData->filterUel, 0));
          PROTECT(compFullVal = allocVector(REALSXP, length(VECTOR_ELT(inputData->filterUel, 0))));
          alloc++;
          compFullVal = sparseToFull(compVal, compFullVal, inputData->filterUel, symType, mwNElements, symDim);
        }
        else {
          dimVal[0] = length(VECTOR_ELT(compUels, 0));
          PROTECT(compFullVal = allocVector(REALSXP, length(VECTOR_ELT(compUels, 0))));
          alloc++;
          compFullVal = sparseToFull(compVal, compFullVal, compUels, symType, mrows, symDim);
        }
        dimVal[1] = 1;
        setAttrib(compFullVal, R_DimSymbol, dimVect);
        break;
      }
      default: {
        PROTECT(dimVect = allocVector(REALSXP, symDim));
        alloc++;
        totalElement = 1;
        dimVal = REAL(dimVect);
        ndimension = 0;
        if (inputData->withUel == 1) {
          for (ndimension = 0; ndimension < symDim; ndimension++) {
            dimVal[ndimension] = length(VECTOR_ELT(inputData->filterUel, ndimension));
            totalElement = (totalElement * length(VECTOR_ELT(inputData->filterUel, ndimension)));
          }
        }
        else {
          for (ndimension = 0; ndimension < symDim; ndimension++) {
            dimVal[ndimension] = length(VECTOR_ELT(compUels, ndimension));
            totalElement = (totalElement * length(VECTOR_ELT(compUels, ndimension)));
          }
        }
        PROTECT(compFullVal = allocVector(REALSXP, totalElement));
        alloc++;
        if (inputData->withUel ==1) {
          compFullVal = sparseToFull(compVal, compFullVal, inputData->filterUel, symType, mwNElements, symDim);
        }
        else {
          compFullVal = sparseToFull(compVal, compFullVal, compUels, symType, mrows, symDim);
        }
        
        setAttrib(compFullVal, R_DimSymbol, dimVect);
        break;
      }
      } /* switch(symDim) */
    }
  } /* if (withList) */

  if (withList) {
    /* Creating string vector for symbol Name */
    PROTECT(compName = allocVector(STRSXP, 1) );
    SET_STRING_ELT(compName, 0, mkChar(symName));
    alloc++;
    /* Creating string vector for symbol type */
    PROTECT(compType = allocVector(STRSXP, 1) );
    alloc++;
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
    alloc++;
    /* Creating string vector for val data form */
    PROTECT(compForm = allocVector(STRSXP, 1) );
    alloc++;
    if (inputData->dForm == full) {
      SET_STRING_ELT(compForm, 0, mkChar(forms[0]));
    }
    else {
      SET_STRING_ELT(compForm, 0, mkChar(forms[1]));
    }


    /* Create a string vector for symbol  field */
    if (symType == dt_var || symType == dt_equ) {
      outFields++;
      PROTECT(compField = allocVector(STRSXP, 1));
      alloc++;
      switch(inputData->dField) {
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
    if (inputData->ts) {
      outFields++;
      PROTECT(compTs = allocVector(STRSXP, 1));
      alloc++;
      SET_STRING_ELT(compTs, 0, mkChar(sText));
    }
    if (inputData->te) {
      outFields++;
    }
  }

  PROTECT(OPListComp = allocVector(STRSXP, outFields));
  alloc++;
  /* populating list component names */
  SET_STRING_ELT(OPListComp, 0, mkChar("name"));
  SET_STRING_ELT(OPListComp, 1, mkChar("type"));
  SET_STRING_ELT(OPListComp, 2, mkChar("dim"));
  SET_STRING_ELT(OPListComp, 3, mkChar("val"));
  SET_STRING_ELT(OPListComp, 4, mkChar("form"));
  SET_STRING_ELT(OPListComp, 5, mkChar("uels"));

  nField = 5;
  if (withList) {
    if (symType == dt_var || symType == dt_equ) {
      nField++;
      SET_STRING_ELT(OPListComp, nField, mkChar("field"));
    }
    if (inputData->ts) {
      nField++;
      SET_STRING_ELT(OPListComp, nField, mkChar("ts"));
    }
    if (inputData->te) {
      nField++;
      SET_STRING_ELT(OPListComp, nField, mkChar("te"));
    }
  }


  PROTECT(OPList = allocVector(VECSXP, outFields));
  alloc++;
  if (withList) {
    /* populating list component vector */
    SET_VECTOR_ELT(OPList, 0, compName);
    SET_VECTOR_ELT(OPList, 1, compType);
    SET_VECTOR_ELT(OPList, 2, compDim);
    if (inputData->dForm == full) {
      SET_VECTOR_ELT(OPList, 3, compFullVal);
    }
    else {
      SET_VECTOR_ELT(OPList, 3, compVal);
    }
    SET_VECTOR_ELT(OPList, 4, compForm);
    if (inputData->withUel) {
      SET_VECTOR_ELT(OPList, 5, inputData->filterUel);
    }
    else {
      SET_VECTOR_ELT(OPList, 5, compUels);
    }

    nField = 5;
    if (symType == dt_var || symType == dt_equ) {
      nField++;
      SET_VECTOR_ELT(OPList, nField, compField);
    }
    if (inputData->ts) {
      nField++;
      SET_VECTOR_ELT(OPList, nField, compTs);
    }
    if (inputData->te) {
      nField++;
      SET_VECTOR_ELT(OPList, nField, compTe);
    }
  }
  else {
    /* entering null values other then UEL */
    SET_VECTOR_ELT(OPList, 0, R_NilValue);
    SET_VECTOR_ELT(OPList, 1, R_NilValue);
    SET_VECTOR_ELT(OPList, 2, R_NilValue);
    SET_VECTOR_ELT(OPList, 3, R_NilValue);
    SET_VECTOR_ELT(OPList, 4, R_NilValue);
    SET_VECTOR_ELT(OPList, 5, UEList);
  }

  /* Setting attribute name */
  setAttrib(OPList, R_NamesSymbol, OPListComp);
  /* Releasing allocated memory */
  free(inputData);
  if (!gdxDataReadDone (gdxHandle)) {
    error ("Could not gdxDataReadDone");
  }
  errNum = gdxClose (gdxHandle);
  if (errNum != 0) {
    error("Errors detected when closing gdx file");
  }
  (void) gdxFree (&gdxHandle);
  UNPROTECT(alloc);
  return OPList;
} /* End of rgdx */


/* This is R gateway function to write data to GDX file.
   First arg <- String representing GDX file name to be created
   Rest arg <- list containg symbol data to be written in GDX File.
   This method will return R-NilValue i.e. NULL.
*/

SEXP wgdx (SEXP args)
{
  SEXP fileName, *symList = NULL;
  int symListSiz = 0, symListLen = 0;
  shortStringBuf_t gdxFileName;
  int arglen;
  char zeroSqueeze;

  arglen = length(args);
  if (arglen == 1) {
    error("No input is entered. Please enter valid input\n");
  }

  args = CDR(args); fileName = CAR(args);

  /* Checking that first argument is of type string
   * and second argument is of type list
   */
  if (TYPEOF(fileName) != STRSXP ) {
    error ("The GDX filename (first argument) must be of type string.\n");
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
  writeGdx (gdxFileName, symListLen, symList, 0, zeroSqueeze);
  free (symList);
  return R_NilValue;
} /* wgdx */


/* GAMS method */
SEXP gams (SEXP args)
{
  SEXP firstArg;
  SEXP *symList, result = R_NilValue;
  int symListLen = 0;
  FILE *fp;
  const char *argStr;
  char *writeDataStr, *fileExt, *p;
  shortStringBuf_t input;
  shortStringBuf_t gmsFileName;
  shortStringBuf_t gsBuf;
  int writeData, rc, i, arglen;

  writeData = 1;
  gamsAlloc = 0;
  globalGams = 1;
  arglen = length(args);

  if (arglen == 1) {
    error("No input is entered. Please enter valid input\n");
  }

  args = CDR(args); firstArg = CAR(args);

  /* Checking that first argument is of type string
   * and second argument is of type list
   */
  if (TYPEOF(firstArg) != STRSXP ) {
    Rprintf("The GAMS filename (first argument) must be of type string.\n");
    error("Wrong Argument Type");
  }

  argStr = CHAR(STRING_ELT(firstArg, 0));

  if (2 == arglen) {
    if (0 == strcmp("?", argStr)) {
      int n = (int)strlen (ID);
      memcpy (strippedID, ID+1, n-2);
      strippedID[n-2] = '\0';
      warning("R-file source info: %s", strippedID);
      return R_NilValue;
    } /* if audit run */
  } /* if one arg, of character type */

  checkStringLength(argStr);
  (void) CHAR2ShortStr (argStr, input);
  p = strtok (input, " ");
  (void) CHAR2ShortStr (p, gmsFileName);
  fileExt = strstr (p, ".");
  if (NULL == fileExt) {
    cat2ShortStr (gmsFileName, ".gms");
  }
  fp = fopen (gmsFileName,"r");
  if (fp == NULL) {
    error("Cannot find or open GAMS source file '%s'.\n", gmsFileName);
  }
  else {
    fclose(fp);
  }

  /* specialCommand always starts with a blank */
  strcpy(specialCommand, " ");

  if (arglen > 2) {
    writeDataStr = getGlobalString("write_data", gsBuf);
    if (writeDataStr != NULL) {
      if (0 == strncmp(writeDataStr,"n",1) || 0 == strncmp(writeDataStr,"N",1)) {
        writeData = 0;
      }
      else {
        warning("To change default behavior of 'write_data', please enter it as 'n' or 'N'\n" );
        Rprintf("You entered it as %s.\n", writeDataStr);
      }
    }
  }

  msgInit ();
  if (1 == writeData) {
    symListLen = arglen - 2;
    symList = malloc (symListLen * sizeof(*symList));
    /* get the pointer of input argument and store it locally for better access */
    for (i = 0;  i < arglen-2;  i++) {
      args = CDR(args); symList[i] = CAR(args);
    }
    writeGdx (gmsFileName, symListLen, symList, 1, 'y');
    /* free memory */
    free(symList);
  }
  rc = callGams(argStr);
  if (rc) {
    error("GAMS run failed: rc = %d.\n", rc);
  }
  /* read only first element from GDX file */
  result = getGamsSoln (gmsFileName);
  UNPROTECT(gamsAlloc);
  return result;
} /* gams */


#define GDXLIBRARYVER 0
#define GDXFILEVER    1
#define GDXPRODUCER   2
#define GDXSYMCOUNT   3
#define GDXUELCOUNT   4
#define GDXSETS       5
#define GDXPARS       6
#define GDXVARS       7
#define GDXEQUS       8
#define GDXALIASES    9
#define RETLIST_LEN   10

/* the gateway routine for gdxInfo.
 * This is very similar to gdxdump: it prints everything on R command prompt.
*/
SEXP gdxInfo (SEXP args)
{
  const char *funcName = "gdxInfo";
  SEXP fileName;
  SEXP dumpExp, returnListExp, returnDFExp;
  SEXP result = R_NilValue;
  SEXP retList = R_NilValue;    /* list to return */
  SEXP elt[RETLIST_LEN];        /* list elements */
  SEXP listNames;
    
  int dump, returnList, returnDF;
  int arglen;
  shortStringBuf_t gdxFileName;
  int rc, i, j, k, nUels;
  int symDim, symCount;
  int symType, symType2;
  int symUser, symUser2;        /* user data - different meaning for different symbols types */
  int nRecs, FDim, BadUels=0;
  int iDummy;
  int allocCnt = 0;
  int iSym, nSyms;
  int iSet, nSets; 
  int iPar, nPars;
  int iVar, nVars;
  int iEqu, nEqus;
  int iAli, nAliases;
  char msg[GMS_SSSIZE];
  char FileVersion[GMS_SSSIZE], FileProducer[GMS_SSSIZE];
  char symName[GMS_SSSIZE], symName2[GMS_SSSIZE], sText[GMS_SSSIZE], UelName[GMS_SSSIZE];
  double Vals[GMS_VAL_MAX];
  double dv[GMS_VAL_MAX];
  int Keys[GMS_MAX_INDEX_DIM];
  char *dn, c;

#if 0
  SEXP ap, el;
  const char *name;

  Rprintf ("gdxInfo called with %d args\n", length(args)); 
  for (i = 0, ap = args;  ap != R_NilValue;  i++, ap = CDR(ap)) {
    name = isNull(TAG(ap)) ? "" : CHAR(PRINTNAME(TAG(ap)));
    el = CAR(ap);
    if (0 == length(el)) {
      if (NILSXP == TYPEOF(el))
        Rprintf ("[%d] '%s' NILSXP\n", i, name);
      else
        Rprintf ("[%d] '%s' R type, length 0\n", i, name);
      continue;
    }
    switch (TYPEOF(el)) {
    case REALSXP:
      Rprintf ("%d  [%d] '%s' REALSXP %g\n", TYPEOF(el), i, name, REAL(el)[0]);
      break;
    case STRSXP:
      Rprintf ("%d  [%d] '%s' STRSXP %s\n", TYPEOF(el), i, name, CHAR(STRING_ELT(el,0)));
      break;
    case LGLSXP:
      Rprintf ("%d  [%d] '%s' LGLSXP %d\n", TYPEOF(el), i, name, LOGICAL(el)[0]);
      break;
    default:
      Rprintf ("%d  [%d] '%s' unhandled R type\n", TYPEOF(el), i, name);
    }
  }
#endif

  /* first arg is function name - ignore it */
  arglen = length(args);

  /* ----------------- Check proper number of inputs ------------
   * Function should follow specification of
   * gdxInfo ('gdxName',dump=LGLSXP)
   * -----------------------------------------------------------------------*/
  if (5 != arglen) {
    error ("usage: %s(gdxName=NULL, dump=TRUE, returnList=FALSE, returnDF=FALSE) - incorrect arg count", funcName);
  }
  args = CDR(args);             /* skip first arg: it's the function name */
  fileName = CAR(args);

  loadGDX();
  if (TYPEOF(fileName) == NILSXP) {
    /* no argument: just load GDX and print the version info */
    rc = gdxCreate (&gdxHandle, msg, sizeof(msg));
    if (0 == rc)
      error ("Error creating GDX object: %s", msg);
    mexPath[0] = '\0';
    Rprintf ("* Library location: %s\n", *mexPath ? mexPath : "unknown");
    gdxGetDLLVersion (gdxHandle, msg);
    Rprintf ("*  Library version: %s\n", msg);
    (void) gdxFree (&gdxHandle);
    return R_NilValue;
  }

  /* Checking that first argument is of type string */
  if (TYPEOF(fileName) != STRSXP) {
    error ("usage: %s - argument 'gdxName' must be a string", funcName);
  }

  dumpExp = CADR(args);
  /* Checking that dump argument is of type logical */
  if (TYPEOF(dumpExp) != LGLSXP) {
    error ("usage: %s - argument 'dump' must be a logical", funcName);
  }
  dump = LOGICAL(dumpExp)[0];

  returnListExp = CADDR(args);
  /* Checking that returnList argument is of type logical */
  if (TYPEOF(returnListExp) != LGLSXP) {
    error ("usage: %s - argument 'returnList' must be a logical", funcName);
  }
  returnList = LOGICAL(returnListExp)[0];

  returnDFExp = CADDDR(args);
  /* Checking that returnDF argument is of type logical */
  if (TYPEOF(returnDFExp) != LGLSXP) {
    error ("usage: %s - argument 'returnDF' must be a logical", funcName);
  }
  returnDF = LOGICAL(returnDFExp)[0];

  if (returnList && returnDF) {
    Rprintf ("Cannot return both a list and a data frame: setting returnList FALSE\n");
    returnList = 0;
  }

  (void) CHAR2ShortStr (CHAR(STRING_ELT(fileName, 0)), gdxFileName);
  checkFileExtension (gdxFileName);

  rc = gdxCreate (&gdxHandle, msg, sizeof(msg));
  if (0 == rc)
    error ("Error creating GDX object: %s", msg);
  rc = gdxOpenRead (gdxHandle, gdxFileName, &i);
  if (0 == rc) {
    gdxErrorStr (gdxHandle, i, msg);
    error ("Could not read GDX file %s: %s (rc=%d)\n", gdxFileName, msg, rc);
  }

  rc = gdxGetLastError (gdxHandle);
  if (rc) {
    gdxErrorStr (gdxHandle, rc, msg);
    Rprintf ("Problems processing GDX file %s: %s (rc=%d)\n",
             gdxFileName, msg, rc);
  }

  if (dump) {
    gdxFileVersion (gdxHandle, FileVersion, FileProducer);
    gdxSystemInfo (gdxHandle, &nSyms, &nUels);
    Rprintf("*  File version   : %s\n", FileVersion);
    Rprintf("*  Producer       : %s\n", FileProducer);
    Rprintf("*  Symbols        : %d\n", nSyms);
    Rprintf("*  Unique Elements: %d\n", nUels);

    /* Acroynms */
    for (i = 1;  i <= gdxAcronymCount (gdxHandle);  i++) {
      gdxAcronymGetInfo (gdxHandle, i, symName, sText, &rc);
      Rprintf("Acronym %s", symName);
      if (strlen(sText))
        Rprintf(" '%s'", sText);
      Rprintf(";\n");
    }

    /* Symbolinfo */
    Rprintf ("$ontext\n");
    for (i = 1;  i <= nSyms;  i++) {
      gdxSymbolInfo (gdxHandle, i, symName, &symDim, &symType);
      gdxSymbolInfoX (gdxHandle, i, &symCount, &rc, sText);
      Rprintf ("%-15s %3d %-12s %s\n", symName, symDim, gmsGdxTypeText[symType], sText);
    }
    Rprintf ("$offtext\n");

    Rprintf ("$onempty onembedded\n");
    dn = NULL;
    for (i = 1;  i <= nSyms;  i++) {
      gdxSymbolInfo (gdxHandle, i, symName, &symDim, &symType);
      gdxSymbolInfoX (gdxHandle, i, &symCount, &symUser, sText);

      if (GMS_DT_VAR == symType || GMS_DT_EQU == symType)
        Rprintf ("$ontext\n");

      if (GMS_DT_VAR == symType) {
        if (symUser < 0 || symUser>=GMS_VARTYPE_MAX)
          symUser = GMS_VARTYPE_FREE;
        MEMCPY (dv, gmsDefRecVar[symUser], GMS_VAL_MAX*sizeof(double));
        dn = (char *) gmsVarTypeText[symUser];
      }
      else if (GMS_DT_EQU == symType) {
        if (symUser < 0 || symUser>=GMS_EQUTYPE_MAX)
          symUser = GMS_EQUTYPE_E;
        MEMCPY (dv, gmsDefRecEqu[symUser], GMS_VAL_MAX*sizeof(double));
      }
      else
        dv[GMS_VAL_LEVEL] = 0.0;

      if (0 == symDim && GMS_DT_PAR == symType) /* Scalar */
        Rprintf ("Scalar");
      else {
        if (GMS_DT_VAR == symType)
          Rprintf ("%s ", dn);
        Rprintf ("%s", gmsGdxTypeText[symType]);
      }
      if (GMS_DT_ALIAS == symType) {
        gdxSymbolInfo (gdxHandle, symUser, symName2, &j, &symType2);
        Rprintf (" (%s, %s);\n", symName, symName2);
      }
      else {
        Rprintf(" %s", symName);
        if (symDim > 0) {
          gdxSymbolGetDomain (gdxHandle, i, Keys);
          Rprintf ("(");
          for (j = 0;  j < symDim;  j++) {
            if (Keys[j]==0)
              strcpy (symName,"*");
            else
              gdxSymbolInfo (gdxHandle, Keys[j], symName2, &symUser2, &symType2);
            if (j < symDim-1)
              Rprintf ("%s,", symName);
            else
              Rprintf ("%s)", symName);
          }
        }
        if (strlen(sText))
          Rprintf(" '%s'", sText);
      }
      if (0 == symCount) {
        if (0 == symDim && GMS_DT_PAR == symType) /* Scalar */
          Rprintf (" / 0.0 /;\n");
        else if (GMS_DT_ALIAS != symType)
          Rprintf (" / /;\n");
      }
      else {
        Rprintf ("/\n");
        gdxDataReadRawStart (gdxHandle, i, &nRecs);
        while (gdxDataReadRaw (gdxHandle, Keys, Vals, &FDim)) {
          if ((GMS_DT_VAR == symType || GMS_DT_EQU == symType) && 0 == memcmp(Vals,dv,GMS_VAL_MAX*sizeof(double))) /* all default records */
            continue;
          if (GMS_DT_PAR == symType && 0.0 == Vals[GMS_VAL_LEVEL])
            continue;
          for (j = 1;  j <= symDim;  j++) {
            if (1 == gdxUMUelGet (gdxHandle, Keys[j-1], UelName, &iDummy))
              Rprintf ("'%s'", UelName);
            else {
              Rprintf ("L__", Keys[j-1]);
              BadUels++;
            }
            if (j < symDim)
              Rprintf (".");
          }
          if (GMS_DT_PAR == symType)
            Rprintf(" %s\n", val2str(gdxHandle, Vals[GMS_VAL_LEVEL], msg));
          else if (GMS_DT_SET == symType)
            if (Vals[GMS_VAL_LEVEL]) {
              j = (int) Vals[GMS_VAL_LEVEL];
              gdxGetElemText (gdxHandle, j, msg, &iDummy);
              Rprintf (" '%s'\n", msg);
            }
            else
              Rprintf ("\n");
          else if (GMS_DT_VAR == symType || GMS_DT_EQU == symType) {
            Rprintf (" .");
            c = '(';
            for (j = GMS_VAL_LEVEL; j < GMS_VAL_MAX;  j++) {
              if (Vals[j] != dv[j]) {
                if (GMS_VAL_SCALE == j && GMS_DT_VAR == symType &&
                    symUser != GMS_VARTYPE_POSITIVE && symUser != GMS_VARTYPE_NEGATIVE && symUser != GMS_VARTYPE_FREE)
                  Rprintf ("%c prior %s", c, val2str (gdxHandle, Vals[GMS_VAL_SCALE], msg));
                else
                  Rprintf ("%c %s %s", c, gmsValTypeText[j]+1, val2str(gdxHandle, Vals[j], msg));
                if ('(' == c)
                  c = ',';
              }
            }
            Rprintf (" )\n");
          }
        }
      }
      Rprintf ("/;\n");
      j = 1;
      while (gdxSymbolGetComment (gdxHandle, i, j++, msg))
        Rprintf ("* %s\n", msg);
      if (GMS_DT_VAR == symType || GMS_DT_EQU == symType)
        Rprintf ("$offtext\n");
      Rprintf("\n");
    }
    Rprintf ("$offempty offembedded\n");

    if (BadUels > 0)
      Rprintf("**** %d reference(s) to unique elements without a string representation\n", BadUels);
  }

  nSets = nPars = nVars = nEqus = nAliases = 0;
  if (returnList || returnDF) {
    /* generate the components for the list */
    gdxGetDLLVersion (gdxHandle, msg);
    PROTECT(elt[GDXLIBRARYVER] = allocVector(STRSXP, 1));
    allocCnt++;
    SET_STRING_ELT(elt[GDXLIBRARYVER], 0, mkChar(msg));

    gdxFileVersion (gdxHandle, FileVersion, FileProducer);
    PROTECT(elt[GDXFILEVER] = allocVector(STRSXP, 1));
    allocCnt++;
    SET_STRING_ELT(elt[GDXFILEVER], 0, mkChar(FileVersion));
    PROTECT(elt[GDXPRODUCER] = allocVector(STRSXP, 1));
    allocCnt++;
    SET_STRING_ELT(elt[GDXPRODUCER], 0, mkChar(FileProducer));

    gdxSystemInfo (gdxHandle, &nSyms, &nUels);
    PROTECT(elt[GDXSYMCOUNT] = allocVector(INTSXP, 1));
    allocCnt++;
    INTEGER(elt[GDXSYMCOUNT])[0] = nSyms;
    PROTECT(elt[GDXUELCOUNT] = allocVector(INTSXP, 1));
    allocCnt++;
    INTEGER(elt[GDXUELCOUNT])[0] = nUels;

    for (iSym = 1;  iSym <= nSyms;  iSym++) {
      gdxSymbolInfo (gdxHandle, iSym, symName, &symDim, &symType);
      switch (symType) {
      case GMS_DT_SET:
        nSets++;
        break;
      case GMS_DT_PAR:
        nPars++;
        break;
      case GMS_DT_VAR:
        nVars++;
        break;
      case GMS_DT_EQU:
        nEqus++;
        break;
      case GMS_DT_ALIAS:
        nAliases++;
        break;
      }
    }

    /* generate the names for the list */
    PROTECT(listNames = allocVector(STRSXP, RETLIST_LEN));
    allocCnt++;
    SET_STRING_ELT(listNames, GDXLIBRARYVER, mkChar("gdxLibraryVer"));
    SET_STRING_ELT(listNames, GDXFILEVER   , mkChar("gdxFileVer"));
    SET_STRING_ELT(listNames, GDXPRODUCER  , mkChar("producer"));
    SET_STRING_ELT(listNames, GDXSYMCOUNT  , mkChar("symCount"));
    SET_STRING_ELT(listNames, GDXUELCOUNT  , mkChar("uelCount"));
    SET_STRING_ELT(listNames, GDXSETS      , mkChar("sets"));
    SET_STRING_ELT(listNames, GDXPARS      , mkChar("parameters"));
    SET_STRING_ELT(listNames, GDXVARS      , mkChar("variables"));
    SET_STRING_ELT(listNames, GDXEQUS      , mkChar("equations"));
    SET_STRING_ELT(listNames, GDXALIASES   , mkChar("aliases"));

    PROTECT(retList = allocVector(VECSXP, RETLIST_LEN));
    allocCnt++;

    /* populating retList with its common components */
    for (i = 0;  i <= GDXUELCOUNT;  i++) {
      SET_VECTOR_ELT (retList, i, elt[i]);
    }
    /* put in the names for the components */
    setAttrib (retList, R_NamesSymbol, listNames);
    result = retList;
  } /* if (returnList or returnDF */

  if (returnList) {
    PROTECT(elt[GDXSETS] = allocVector(STRSXP, nSets));
    allocCnt++;
    PROTECT(elt[GDXPARS] = allocVector(STRSXP, nPars));
    allocCnt++;
    PROTECT(elt[GDXVARS] = allocVector(STRSXP, nVars));
    allocCnt++;
    PROTECT(elt[GDXEQUS] = allocVector(STRSXP, nEqus));
    allocCnt++;
    PROTECT(elt[GDXALIASES] = allocVector(STRSXP, nAliases));
    allocCnt++;

    iSet = iPar = iVar = iEqu = iAli = 0;
    for (iSym = 1;  iSym <= nSyms;  iSym++) {
      gdxSymbolInfo (gdxHandle, iSym, symName, &symDim, &symType);
      switch (symType) {
      case GMS_DT_SET:
        SET_STRING_ELT(elt[GDXSETS], iSet, mkChar(symName));
        iSet++;
        break;
      case GMS_DT_PAR:
        SET_STRING_ELT(elt[GDXPARS], iPar, mkChar(symName));
        iPar++;
        break;
      case GMS_DT_VAR:
        SET_STRING_ELT(elt[GDXVARS], iVar, mkChar(symName));
        iVar++;
        break;
      case GMS_DT_EQU:
        SET_STRING_ELT(elt[GDXEQUS], iEqu, mkChar(symName));
        iEqu++;
        break;
      case GMS_DT_ALIAS:
        SET_STRING_ELT(elt[GDXALIASES], iAli, mkChar(symName));
        iAli++;
        break;
      }
    }

    /* populating retList with its returnList-specific components */
    for (i = GDXSETS;  i < RETLIST_LEN;  i++) {
      SET_VECTOR_ELT (retList, i, elt[i]);
    }
  } /* if (returnList) */
  else if (returnDF) {
    SEXP dfClass, asIsClass;
    SEXP domTmp;

    /* columns for sets DF: name, index, dim, card, text, doms */
    SEXP setName, setIndex, setDim, setCard, setText, setDoms;
    SEXP setColNames, setRowNames;

    /* columns for parameters DF: name, index, dim, card, text, doms */
    SEXP parName, parIndex, parDim, parCard, parText, parDoms;
    SEXP parColNames, parRowNames;

    /* columns for variables DF: name, index, dim, card, doms */
    SEXP varName, varIndex, varDim, varCard, varDoms;
    SEXP varColNames, varRowNames;

    /* columns for equations DF: name, index, dim, card, doms */
    SEXP equName, equIndex, equDim, equCard, equDoms;
    SEXP equColNames, equRowNames;

    /* columns for aliases DF: name, index, base */
    SEXP aliName, aliIndex, aliBase;
    SEXP aliColNames, aliRowNames;

    PROTECT(dfClass = allocVector(STRSXP, 1));
    allocCnt++;
    SET_STRING_ELT(dfClass, 0, mkChar("data.frame"));
    PROTECT(asIsClass = allocVector(STRSXP, 1));
    allocCnt++;
    SET_STRING_ELT(asIsClass, 0, mkChar("AsIs"));

    PROTECT(setName = allocVector(STRSXP, nSets));
    allocCnt++;
    PROTECT(setIndex = allocVector(INTSXP, nSets));
    allocCnt++;
    PROTECT(setDim = allocVector(INTSXP, nSets));
    allocCnt++;
    PROTECT(setCard = allocVector(INTSXP, nSets));
    allocCnt++;
    PROTECT(setText = allocVector(STRSXP, nSets));
    allocCnt++;
    PROTECT(setDoms = allocVector(VECSXP, nSets));
    allocCnt++;
    classgets (setDoms, asIsClass);
    
    PROTECT(parName = allocVector(STRSXP, nPars));
    allocCnt++;
    PROTECT(parIndex = allocVector(INTSXP, nPars));
    allocCnt++;
    PROTECT(parDim = allocVector(INTSXP, nPars));
    allocCnt++;
    PROTECT(parCard = allocVector(INTSXP, nPars));
    allocCnt++;
    PROTECT(parText = allocVector(STRSXP, nPars));
    allocCnt++;
    PROTECT(parDoms = allocVector(VECSXP, nPars));
    allocCnt++;
    classgets (parDoms, asIsClass);
    
    PROTECT(varName = allocVector(STRSXP, nVars));
    allocCnt++;
    PROTECT(varIndex = allocVector(INTSXP, nVars));
    allocCnt++;
    PROTECT(varDim = allocVector(INTSXP, nVars));
    allocCnt++;
    PROTECT(varCard = allocVector(INTSXP, nVars));
    allocCnt++;
    PROTECT(varDoms = allocVector(VECSXP, nVars));
    allocCnt++;
    classgets (varDoms, asIsClass);
    
    PROTECT(equName = allocVector(STRSXP, nEqus));
    allocCnt++;
    PROTECT(equIndex = allocVector(INTSXP, nEqus));
    allocCnt++;
    PROTECT(equDim = allocVector(INTSXP, nEqus));
    allocCnt++;
    PROTECT(equCard = allocVector(INTSXP, nEqus));
    allocCnt++;
    PROTECT(equDoms = allocVector(VECSXP, nEqus));
    allocCnt++;
    classgets (equDoms, asIsClass);
    
    PROTECT(aliName = allocVector(STRSXP, nAliases));
    allocCnt++;
    PROTECT(aliIndex = allocVector(INTSXP, nAliases));
    allocCnt++;
    PROTECT(aliBase = allocVector(INTSXP, nAliases));
    allocCnt++;

    iSet = iPar = iVar = iEqu = iAli = 0;
    for (iSym = 1;  iSym <= nSyms;  iSym++) {
      gdxSymbolInfo (gdxHandle, iSym, symName, &symDim, &symType);
      gdxSymbolInfoX (gdxHandle, iSym, &symCount, &symUser, sText);
      switch (symType) {
      case GMS_DT_SET:
        SET_STRING_ELT(setName, iSet, mkChar(symName));
        INTEGER(setIndex)[iSet] = iSym;
        INTEGER(setDim)[iSet] = symDim;
        INTEGER(setCard)[iSet] = symCount;
        SET_STRING_ELT(setText, iSet, mkChar(sText));
        PROTECT(domTmp = allocVector(INTSXP, symDim));
        allocCnt++;
        gdxSymbolGetDomain (gdxHandle, iSym, Keys);
        for (k = 0;  k < symDim;  k++) {
          INTEGER(domTmp)[k] = Keys[k];
        }
        SET_VECTOR_ELT(setDoms, iSet, domTmp);
        domTmp = R_NilValue;
        iSet++;
        break;
      case GMS_DT_PAR:
        SET_STRING_ELT(parName, iPar, mkChar(symName));
        INTEGER(parIndex)[iPar] = iSym;
        INTEGER(parDim)[iPar] = symDim;
        INTEGER(parCard)[iPar] = symCount;
        SET_STRING_ELT(parText, iPar, mkChar(sText));
        PROTECT(domTmp = allocVector(INTSXP, symDim));
        allocCnt++;
        gdxSymbolGetDomain (gdxHandle, iSym, Keys);
        for (k = 0;  k < symDim;  k++) {
          INTEGER(domTmp)[k] = Keys[k];
        }
        SET_VECTOR_ELT(parDoms, iPar, domTmp);
        domTmp = R_NilValue;
        iPar++;
        break;
      case GMS_DT_VAR:
        SET_STRING_ELT(varName, iVar, mkChar(symName));
        INTEGER(varIndex)[iVar] = iSym;
        INTEGER(varDim)[iVar] = symDim;
        INTEGER(varCard)[iVar] = symCount;
        PROTECT(domTmp = allocVector(INTSXP, symDim));
        allocCnt++;
        gdxSymbolGetDomain (gdxHandle, iSym, Keys);
        for (k = 0;  k < symDim;  k++) {
          INTEGER(domTmp)[k] = Keys[k];
        }
        SET_VECTOR_ELT(varDoms, iVar, domTmp);
        domTmp = R_NilValue;
        iVar++;
        break;
      case GMS_DT_EQU:
        SET_STRING_ELT(equName, iEqu, mkChar(symName));
        INTEGER(equIndex)[iEqu] = iSym;
        INTEGER(equDim)[iEqu] = symDim;
        INTEGER(equCard)[iEqu] = symCount;
        PROTECT(domTmp = allocVector(INTSXP, symDim));
        allocCnt++;
        gdxSymbolGetDomain (gdxHandle, iSym, Keys);
        for (k = 0;  k < symDim;  k++) {
          INTEGER(domTmp)[k] = Keys[k];
        }
        SET_VECTOR_ELT(equDoms, iEqu, domTmp);
        domTmp = R_NilValue;
        iEqu++;
        break;
      case GMS_DT_ALIAS:
        SET_STRING_ELT(aliName, iAli, mkChar(symName));
        INTEGER(aliIndex)[iAli] = iSym;
        INTEGER(aliBase)[iAli] = symUser;
        iAli++;
        break;
      }
    } /* loop over symbols */

    /* -------------------------------------------------------------- */
    PROTECT(elt[GDXSETS] = allocVector(VECSXP, 6));
    allocCnt++;

    SET_VECTOR_ELT(elt[GDXSETS], 0, setName);
    SET_VECTOR_ELT(elt[GDXSETS], 1, setIndex);
    SET_VECTOR_ELT(elt[GDXSETS], 2, setDim);
    SET_VECTOR_ELT(elt[GDXSETS], 3, setCard);
    SET_VECTOR_ELT(elt[GDXSETS], 4, setText);
    SET_VECTOR_ELT(elt[GDXSETS], 5, setDoms);

    PROTECT(setColNames = allocVector(STRSXP, 6));
    allocCnt++;
    /* columns for sets DF: name, index, dim, card, text, doms */
    SET_STRING_ELT(setColNames, 0, mkChar("name"));
    SET_STRING_ELT(setColNames, 1, mkChar("index"));
    SET_STRING_ELT(setColNames, 2, mkChar("dim"));
    SET_STRING_ELT(setColNames, 3, mkChar("card"));
    SET_STRING_ELT(setColNames, 4, mkChar("text"));
    SET_STRING_ELT(setColNames, 5, mkChar("doms"));
    PROTECT(setRowNames = allocVector(INTSXP, nSets));
    allocCnt++;
    for (iSet = 0;  iSet < nSets;  iSet++)
      INTEGER(setRowNames)[iSet] = iSet+1;

    setAttrib (elt[GDXSETS], R_NamesSymbol, setColNames);
    setAttrib (elt[GDXSETS], R_RowNamesSymbol, setRowNames);
    classgets (elt[GDXSETS], dfClass);

    /* -------------------------------------------------------------- */
    PROTECT(elt[GDXPARS] = allocVector(VECSXP, 6));
    allocCnt++;

    SET_VECTOR_ELT(elt[GDXPARS], 0, parName);
    SET_VECTOR_ELT(elt[GDXPARS], 1, parIndex);
    SET_VECTOR_ELT(elt[GDXPARS], 2, parDim);
    SET_VECTOR_ELT(elt[GDXPARS], 3, parCard);
    SET_VECTOR_ELT(elt[GDXPARS], 4, parText);
    SET_VECTOR_ELT(elt[GDXPARS], 5, parDoms);

    PROTECT(parColNames = allocVector(STRSXP, 6));
    allocCnt++;
    /* columns for parameters DF: name, index, dim, card, text, doms */
    SET_STRING_ELT(parColNames, 0, mkChar("name"));
    SET_STRING_ELT(parColNames, 1, mkChar("index"));
    SET_STRING_ELT(parColNames, 2, mkChar("dim"));
    SET_STRING_ELT(parColNames, 3, mkChar("card"));
    SET_STRING_ELT(parColNames, 4, mkChar("text"));
    SET_STRING_ELT(parColNames, 5, mkChar("doms"));
    PROTECT(parRowNames = allocVector(INTSXP, nPars));
    allocCnt++;
    for (iPar = 0;  iPar < nPars;  iPar++)
      INTEGER(parRowNames)[iPar] = iPar+1;

    setAttrib (elt[GDXPARS], R_NamesSymbol, parColNames);
    setAttrib (elt[GDXPARS], R_RowNamesSymbol, parRowNames);
    classgets (elt[GDXPARS], dfClass);

    /* -------------------------------------------------------------- */
    PROTECT(elt[GDXVARS] = allocVector(VECSXP, 5));
    allocCnt++;

    SET_VECTOR_ELT(elt[GDXVARS], 0, varName);
    SET_VECTOR_ELT(elt[GDXVARS], 1, varIndex);
    SET_VECTOR_ELT(elt[GDXVARS], 2, varDim);
    SET_VECTOR_ELT(elt[GDXVARS], 3, varCard);
    SET_VECTOR_ELT(elt[GDXVARS], 4, varDoms);

    PROTECT(varColNames = allocVector(STRSXP, 5));
    allocCnt++;
    /* columns for variables DF: name, index, dim, card, doms */
    SET_STRING_ELT(varColNames, 0, mkChar("name"));
    SET_STRING_ELT(varColNames, 1, mkChar("index"));
    SET_STRING_ELT(varColNames, 2, mkChar("dim"));
    SET_STRING_ELT(varColNames, 3, mkChar("card"));
    SET_STRING_ELT(varColNames, 4, mkChar("doms"));
    PROTECT(varRowNames = allocVector(INTSXP, nVars));
    allocCnt++;
    for (iVar = 0;  iVar < nVars;  iVar++)
      INTEGER(varRowNames)[iVar] = iVar+1;

    setAttrib (elt[GDXVARS], R_NamesSymbol, varColNames);
    setAttrib (elt[GDXVARS], R_RowNamesSymbol, varRowNames);
    classgets (elt[GDXVARS], dfClass);

    /* -------------------------------------------------------------- */
    PROTECT(elt[GDXEQUS] = allocVector(VECSXP, 5));
    allocCnt++;

    SET_VECTOR_ELT(elt[GDXEQUS], 0, equName);
    SET_VECTOR_ELT(elt[GDXEQUS], 1, equIndex);
    SET_VECTOR_ELT(elt[GDXEQUS], 2, equDim);
    SET_VECTOR_ELT(elt[GDXEQUS], 3, equCard);
    SET_VECTOR_ELT(elt[GDXEQUS], 4, equDoms);

    PROTECT(equColNames = allocVector(STRSXP, 5));
    allocCnt++;
    /* columns for equations DF: name, index, dim, card, doms */
    SET_STRING_ELT(equColNames, 0, mkChar("name"));
    SET_STRING_ELT(equColNames, 1, mkChar("index"));
    SET_STRING_ELT(equColNames, 2, mkChar("dim"));
    SET_STRING_ELT(equColNames, 3, mkChar("card"));
    SET_STRING_ELT(equColNames, 4, mkChar("doms"));
    PROTECT(equRowNames = allocVector(INTSXP, nEqus));
    allocCnt++;
    for (iEqu = 0;  iEqu < nEqus;  iEqu++)
      INTEGER(equRowNames)[iEqu] = iEqu+1;

    setAttrib (elt[GDXEQUS], R_NamesSymbol, equColNames);
    setAttrib (elt[GDXEQUS], R_RowNamesSymbol, equRowNames);
    classgets (elt[GDXEQUS], dfClass);

    /* -------------------------------------------------------------- */
    PROTECT(elt[GDXALIASES] = allocVector(VECSXP, 3));
    allocCnt++;

    SET_VECTOR_ELT(elt[GDXALIASES], 0, aliName);
    SET_VECTOR_ELT(elt[GDXALIASES], 1, aliIndex);
    SET_VECTOR_ELT(elt[GDXALIASES], 2, aliBase);

    PROTECT(aliColNames = allocVector(STRSXP, 3));
    allocCnt++;
    /* columns for alias DF: name, index, base */
    SET_STRING_ELT(aliColNames, 0, mkChar("name"));
    SET_STRING_ELT(aliColNames, 1, mkChar("index"));
    SET_STRING_ELT(aliColNames, 2, mkChar("base"));
    PROTECT(aliRowNames = allocVector(INTSXP, nAliases));
    allocCnt++;
    for (iAli = 0;  iAli < nAliases;  iAli++)
      INTEGER(aliRowNames)[iAli] = iAli+1;

    setAttrib (elt[GDXALIASES], R_NamesSymbol, aliColNames);
    setAttrib (elt[GDXALIASES], R_RowNamesSymbol, aliRowNames);
    classgets (elt[GDXALIASES], dfClass);

    /* populating retList with its returnDF-specific components */
    for (i = GDXSETS;  i < RETLIST_LEN;  i++) {
      SET_VECTOR_ELT (retList, i, elt[i]);
    }
  }   /* returnDF */

  (void) gdxClose (gdxHandle);
  gdxFree (&gdxHandle);

  UNPROTECT(allocCnt);
  return result;
} /* gdxInfo */

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
  SEXP pSysDir, silent;
  shortStringBuf_t sysDir, msgBuf;
  Rboolean isSilent = NA_LOGICAL;

  arglen = length(args);
  if (3 != arglen) {
    error ("usage: %s(gamsSysDir=NULL, silent=FALSE) - incorrect arg count", funcName);
  }
  pSysDir = CADR(args);
  silent = CADDR(args);
  isSilent = getSqueezeArgRead (silent);
  if (NA_LOGICAL == isSilent) {
    isSilent = FALSE;
  }
  gdxLoaded = gdxLibraryLoaded();

  if (TYPEOF(pSysDir) != NILSXP) { /* we should have gamsSysDir */
    if (TYPEOF(pSysDir) != STRSXP) {
      error ("usage: %s(gamsSysDir) - gamsSysDir must be a string", funcName);
    }
    (void) CHAR2ShortStr (CHAR(STRING_ELT(pSysDir, 0)), sysDir);

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
      Rprintf ("The GDX library has been not been loaded\n");
  }
  
  return result;
} /* igdx */
