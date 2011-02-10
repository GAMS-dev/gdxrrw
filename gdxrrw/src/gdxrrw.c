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

static int alloc;

static int wAlloc;

static int gamsAlloc;

static char specialCommand[LINELEN];

static int globalGams;

static int wUEL;

static shortStringBuf_t lastErrMsg;

static char mexPath[512];

static char ID[256] = "$Id$";
static char strippedID[256];

/* -------------------- Method declaration -----------------------*/
static void
checkRgdxList (const SEXP lst, struct rgdxStruct *data);

static void
checkWgdxList(const SEXP lst,
              int i,
              SEXP uelIndex,
              struct wgdxStruct **data,
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
getNonZeroElements (gdxHandle_t h, int symIdx, dField_t *dField);

SEXP
compressData(SEXP data,
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

void
registerInputUEL(SEXP uelOut,
                 int k,
                 SEXP uelIndex);

static void
writeGdx(char *fileName,
         int arglen,
         SEXP *argList,
         int fromGAMS);

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
      errorr("Could not run %s: %s: check gams.path",
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
  SEXP OPListComp, OPList, dimVect, textElement;
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
  int rc, errNum, symDim, symType, mrows, ncols, nRecs, iRec, changeIdx, index, k, kk, nonZero;
  shortStringBuf_t msgBuf, uelName;
  char buf[3*sizeof(shortStringBuf_t)];
  int nUEL, iUEL, defaultIndex,  UELUserMapping, highestMappedUEL, ndimension;
  double  *p, *dimVal;
  gdxUelIndex_t uels;
  gdxValues_t values;
  shortStringBuf_t gsBuf;
  char *gForm, *field, *outputStyle;
  int outStyle, nField;
  char *types[] = {"set", "parameter", "variable", "equation"};
  char *forms[] = {"full", "sparse"};
  char *fields[] = {"l", "m", "up", "lo", "s"};
  int b, matched, sparesIndex, totNumber, totalElement;
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
  OPList = NULL;

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

    outStyle = 0;
    outputStyle = getGlobalString("output", gsBuf);
    if ((outputStyle != NULL) && ('\0' != *outputStyle)) {
      if (strcmp(outputStyle,"std") == 0) {
        outStyle = 1;
      }
      else {
        /* else warning message */
        warning("To change default behavior of 'output', please enter it as 'std'.\n" );
        Rprintf("You entered it as %s.\n", outputStyle);
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
      if (symType == dt_var || symType == dt_equ ) {
        mrows = getNonZeroElements (gdxHandle, 1, &inputData->dField);
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
        totNumber = 1;
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

          if (symType != dt_set) {
            if (gdxMapValue (gdxHandle, values[inputData->dField], &k)) { /* it's special */
              switch (k) {
              case sv_valpin:
                p[index] = NA_REAL;
                break;
              case sv_valmin:
                p[index] = NA_REAL;
                break;
              case sv_valeps:
                p[index] = 0;
                break;
              case sv_normal:
                p[index] = values[inputData->dField];
                break;
              case sv_valund:
              case sv_valna:
                p[index] = NA_REAL;
                break;
              default:
                sprintf(buf,
                        "Unrecognized map-value %d returned for %g",
                        k,
                        values[inputData->dField]);
                error(buf);
              } /* end of switch/case */
            }
            else {
              p[index] = values[inputData->dField];
            }
          }
        }
        if (matched == maxPossibleElements) {
          break;
        }
      }
      free(returnedIndex);
    }/* End of with uels */
    else {
      nonZero = 0;
      if (symType == dt_var || symType == dt_equ ) {
        gdxDataReadRawStart (gdxHandle, 1, &nRecs);
      }

      for (iRec = 0;  iRec < nRecs;  iRec++) {
        gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
        if (symType == dt_set || values[inputData->dField] != 0) {
          index = 0;
          /* For non zero */
          for (kk = 0;  kk < symDim;  kk++) {
            p[nonZero+kk*mrows] = uels[kk];
            index = nonZero+symDim*mrows;
          }
          nonZero++;
          if (symType != dt_set) {
            if (gdxMapValue (gdxHandle, values[inputData->dField], &k)) { /* it's special */
              switch (k) {
              case sv_valpin:
                p[index] = NA_REAL;
                break;
              case sv_valmin:
                p[index] = -NA_REAL;
                break;
              case sv_valeps:
                p[index] = 0;
                break;
              case sv_normal:
                p[index] = values[inputData->dField];
                break;
              case sv_valund:
              case sv_valna:
                p[index] = NA_REAL;
                break;
              default:
                sprintf(buf,
                        "Unrecognized map-value %d returned for %g",
                        k,
                        values[inputData->dField]);
                error(buf);
              }
            }
            else {
              p[index] = values[inputData->dField];
            }
          }
        } /* end of if (set || val != 0) */
      } /* loop over GDX records */
    }

    /* Converting data into its compressed form. */
    if (inputData->compress == 1) {
      PROTECT(compUels = allocVector(VECSXP, symDim));
      compVal = compressData(compVal, UEList, compUels, nUEL, symDim, mrows);
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
    if (outStyle == 0) {
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
    }
    else {
      /* Only value */
      OPList = compVal;
    }

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

/* this mehtod for global input "compress" */
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
  int i, j;
  double *P;
  int ndims;
  int ncols, nrows, nuels;
  int max = 0;

  nuels = length(uelOut);

  P = NULL;
  if (TYPEOF(val) == REALSXP) {
    P = REAL(val);
  }

  else if (TYPEOF(val) == INTSXP) {
    P = (double*)INTEGER(val);
  }
  else {
    error("val must me numeric.\n");
  }
  dims = getAttrib(val, R_DimSymbol);

  if (dForm == sparse) {
    nrows = INTEGER(dims)[0];
    ncols = INTEGER(dims)[1];

    if (dType == parameter) {
      ncols--;
    }
    if (nuels != ncols) {
      error("Number of columns in sparse data does not match with uels.");
    }
    for (i = 0; i < ncols; i++) {
      for (j = 0; j < nrows; j++) {
        if (P[j + i*nrows] - floor(P[j + i*nrows]) > 0) {
          error("Non-integer coordinates are not allowed in index columns of sparse data");
        }
        if (P[j + i*nrows] < 1) {
          error("Non-positive coordinates are not allowed in index columns of sparse data");
        }
        /* if (!mxIsFinite(P[j + i*nrows])) {
           error("Only finite numbers are allowed in index columns of sparse data");
           } */
        if (P[j + i*nrows] > max) {
          max = (int) P[j + i*nrows];
        }
      }
      if (max > (int)length(VECTOR_ELT(uelOut, i ))) {
        error("Row index in sparse matrix exceeds number of elements in UEL for that column.");
      }
      max = 0;
    }
  }
  else {
    /* get dimension of full matrix == number of columns in uels */
    ndims = length(dims);
    if ( !((nuels == 0 || nuels == 1) && ndims ==2) && nuels != ndims) {
      error("Number of dimension in full data does not match with uels.");
    }

    /* special check for vector */
    if (nuels == 1 && INTEGER(dims)[1] != 1) {
      error("Vector should have only 1 dimensional column elements.");
    }
    /* special check for scalar */
    if (nuels == 0 && ( INTEGER(dims)[0] != 1 || INTEGER(dims)[1] != 1)) {
      error("Scalar should have only one element.");
    }
    /* number of elements in each dimension == number of elements in UEL
       for (i = 0; i < (int)nuels; i++) {
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

  fileExt = rindex (fileName, '.');
  if (NULL == fileExt) {
    if (strlen(fileName) < sizeof(shortStringBuf_t)-4) {
      fileName = strcat(fileName, ".gdx");
    }
    else {
      Rprintf ("Input file name '%s' is too long\n", fileName);
      error ("Input file name is too long.");
    }
  }
  else if (0 != strcasecmp(".gdx", fileExt)) {
    Rprintf ("Input file extension '%s' is not valid\n", fileExt);
    error ("Input file must be a GDX file.");
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
getNonZeroElements (gdxHandle_t h, int symIdx, dField_t *dField)
{
  int nRecs, changeIdx, i, nonZero;
  gdxUelIndex_t uels;
  gdxValues_t values;

  nonZero = 0;
  gdxDataReadRawStart (h, symIdx, &nRecs);
  for (i = 0; i < nRecs; i++) {
    gdxDataReadRaw (h, uels, values, &changeIdx);
    if (values[dField[0]] != 0) {
      nonZero++;
    }
  }
  return nonZero;
}

/* This method compress the raw data(both value and uel) and remove redundant zeros from
 * value matrix and re-index output matrix. And it also remove non present uel elements from UEL */
SEXP
compressData(SEXP data,
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
  return data;
} /* End of compressData */


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
      assert(rc);
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


/* This function validate input structure and set certain global variables */
void
checkWgdxList(SEXP structure,
              int k,
              SEXP uelIndex,
              struct  wgdxStruct **data,
              int fromGAMS)
{
  SEXP lstName, tmp, tmpUel;
  SEXP dimension;
  int i, j, infields;
  int  nCoords, sz, withDim;
  const char *tmpName;
  const char *compName;              /* pointers to field names */
  SEXP   uelOut, bufferUel;          /* allocating temporary storage place */
  int found = 0;

  withDim = 0;
  infields = 0;

  data[k] = (struct wgdxStruct *)malloc(sizeof(struct wgdxStruct));

  data[k]->dForm = sparse;
  data[k]->dType = set;
  data[k]->withTs = 0;
  data[k]->withVal = 0;
  data[k]->withUel = 0;
  data[k]->dim = 0;

  /* check maximum number of fields */
  if (7 < length(structure) || length(structure) < 1) {
    error("Incorrect number of components in input list argument.\n");
  }
  else {
    infields = length(structure);
  }

  lstName = getAttrib(structure, R_NamesSymbol);
  if (lstName == R_NilValue) {
    Rprintf("Input list must be named\n");
    Rprintf("Valid names are: 'name', 'type', 'form', 'val', 'uels', 'dim', 'ts'.\n");
    error("Please try again with named input  list.\n");
  }
  for (i=0;  i < infields;  i++) {
    compName = CHAR(STRING_ELT(lstName, i));
    /* Checking for valid field name */
    if ( !((0 == strcmp("name", compName ))
          || (0 == strcmp("uels", compName ))
          || (0 == strcmp("dim", compName ))
          || (0 == strcmp("type", compName ))
          || (0 == strcmp("form", compName ))
          || (0 == strcmp("val", compName ))
          || (0 == strcmp("ts", compName ))) ) {
      Rprintf ("Input list components must be according to this specification:\n");
      Rprintf ("'name', 'type', 'val', 'dim', 'uels', 'form', 'ts'.\n");
      error("Incorrect type of input list component '%s' specified.",
            compName);
    }
  }

  /*
   * TODO: I couldn't find a way to directly get component by name.
   * So I am looping through list of name to find desired name and then
   * using that index to find value from main list
   */
  i=0;
  /* Checking if field data is for "name" */
  for (i = 0; i < infields; i++) {
    if (strcmp("name", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }

  if (found == 1) {
    tmp = VECTOR_ELT(structure, i);
    if (TYPEOF(tmp) == STRSXP) {
      checkStringLength( CHAR(STRING_ELT(tmp, 0)));
      strcpy( data[k]->name, CHAR(STRING_ELT(tmp, 0)) );
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

  i=0;
  found = 0;
  /* Checking if field data is for "ts" */
  for (i = 0; i < infields; i++) {
    if (strcmp("ts", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }
  if (found == 1) {
    tmp = VECTOR_ELT(structure, i);
    if (TYPEOF(tmp) == STRSXP) {
      checkStringLength( CHAR(STRING_ELT(tmp, 0)));
      data[k]->withTs = 1;
    }
    else {
      Rprintf ("List component 'ts' must be a string - found %d instead\n",
               TYPEOF(tmp) );
      error("Input list component 'ts' must be string.\n");
    }
  }

  i=0;
  found = 0;
  /* Checking if field data is for "form" */
  for (i = 0; i < infields; i++) {
    if (strcmp("form", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }

  if (found == 1) {
    tmp = VECTOR_ELT(structure, i);
    if (TYPEOF(tmp) != STRSXP) {
      Rprintf ("List component 'form' must be a string - found %d instead\n",
               TYPEOF(tmp));
      error("Input list component 'form' must be string");
    }
    tmpName = CHAR(STRING_ELT(tmp, 0));
    if (strlen(tmpName) == 0) {
      error("Input list component 'form' must be either 'full' or 'sparse'.");
    }
    if (strcasecmp("full", tmpName) == 0) {
      data[k]->dForm = full;
    }
    else if (strcasecmp("sparse", tmpName) == 0) {
      data[k]->dForm = sparse;
    }
    else {
      error("Input list component 'form' must be either 'full' or 'sparse'.");
    }
  }

  i=0;
  found = 0;
  /* Checking if field data is for "type" */
  for (i = 0; i < infields; i++) {
    if (strcmp("type", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }

  if (found == 1) {
    tmp = VECTOR_ELT(structure, i);
    if ( TYPEOF(tmp) != STRSXP) {
      Rprintf ("List component 'type' must be a string - found %d instead\n",
               TYPEOF(tmp));
      error("Input list component 'type' must be string.\n");
    }
    tmpName = CHAR(STRING_ELT(tmp, 0));
    if (strlen(tmpName) == 0) {
      Rprintf("Before changing to lower");
      error("Input list component 'type' must be either 'set' or 'parameter'.\n");
    }
    if (0 == strcasecmp("set", tmpName) ) {
      data[k]->dType = set;
    }
    else if (0 == strcasecmp("parameter", tmpName) ) {
      data[k]->dType = parameter;
    }
    else {
      Rprintf("type found = %s\n", tmpName);
      error("Input list component 'type' must be either 'set' or 'parameter'.\n");
    }
  }

  i=0;
  found = 0;
 /* Checking for dimension .dim */
  for (i = 0; i < infields; i++) {
    if (strcmp("dim", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }
  if (found == 1) {
    tmp = VECTOR_ELT(structure, i);

    if (TYPEOF(tmp) == INTSXP) {
      if (length(tmp) != 1) {
        error("Input list component 'dim' must have only one element.\n");
      }
      /* for negative value */
      if (INTEGER(tmp)[0] < 0) {
        error("Negative value is not allowed as valid input for 'dim'.\n");
      }
      withDim = 1;
      data[k]->dim = INTEGER(tmp)[0];
    }
    else if (TYPEOF(tmp) == REALSXP ) {
      if (length(tmp) != 1) {
        error("Input list component 'dim' must have only one element.\n");
      }
      /* For fractional value */
      else if (REAL(tmp)[0] - floor(REAL(tmp)[0]) > 0) {
        error("Non-integer value is not allowed as valid input for 'dim'.\n");
      }
      /* for Negeative value */
      else if (REAL(tmp)[0] < 0) {
        error("Negative value is not allowed as valid input for 'dim'.\n");
      }
      withDim = 1;
      data[k]->dim = (int) REAL(tmp)[0];
    }
    else {
      Rprintf ("List component 'dim' must be a numeric - found %d instead\n",
               TYPEOF(tmp));
      error("Input list component 'dim' must be integer.\n");
    }
  }

  i=0;
  found = 0;
  /* Checking for dimension .uels */
  for (i = 0; i < infields; i++) {
    if (strcmp("uels", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }

  if (found == 1) {
    tmp = VECTOR_ELT(structure, i);
    if (TYPEOF(tmp) != VECSXP) {
      Rprintf ("List component 'uels' must be a un-named list - found %d instead\n",
               TYPEOF(tmp));
      error("Input list component 'uels' must be unnamed list.\n");
    }
    else {
      if (length(tmp) == 0) {
        error("Empty input list component 'uels' is not allowed.\n");
      }
      PROTECT(uelOut = allocVector(VECSXP, length(tmp)));
      wAlloc++;
      for (j = 0; j < length(tmp); j++) {
        tmpUel = VECTOR_ELT(tmp, j);
        if (tmpUel == R_NilValue) {
          error("Empty Input  field (uels) not allowed");
        }
        if (TYPEOF(tmpUel) == STRSXP ) {
          /*  checkStringLength( CHAR(STRING_ELT(tmp, 0)) ); */
          SET_VECTOR_ELT(uelOut, j, tmpUel);
        }
        else if (TYPEOF(tmpUel) == REALSXP || TYPEOF(tmpUel) == INTSXP) {
          /* Convert to output */
          bufferUel = allocVector(STRSXP, length(tmpUel));
          bufferUel =  convertToOutput(bufferUel, tmpUel);
          SET_VECTOR_ELT(uelOut, j, bufferUel);
        }
        else {
          error("Input uels must be either string vector of numeric vector.\n");
        }
      }
      data[k]->withUel = 1;
    }
  }

  i=0;
  found = 0;
  /* Checking for dimension .val */
  for (i = 0; i < infields; i++) {
    if (strcmp("val", CHAR(STRING_ELT(lstName, i))) == 0) {
      found = 1;
      break;
    }
  }
  if (found == 1) {
    tmp = VECTOR_ELT(structure, i);
    if (fromGAMS == 1 && TYPEOF(tmp) == STRSXP) {
      checkStringLength(CHAR(STRING_ELT(tmp, 0)));
      strcat(specialCommand, "--");
      strcat(specialCommand,  data[k]->name);
      strcat(specialCommand, "=");
      strcat(specialCommand, CHAR(STRING_ELT(tmp, 0)));
      strcat(specialCommand, " ");
      return;
    }
    dimension = getAttrib(tmp, R_DimSymbol);
    if (TYPEOF(tmp) == REALSXP || TYPEOF(tmp) == INTSXP ) {
      if (data[k]->dForm == sparse) {
        if (length(dimension) != 2) {
          Rprintf("You have entered %d dimensional matrix.\n", length(dimension));
          error("Only 2 dimensional val is allowed as valid input in sparse format.");
        }
        /* getting data matrix */
        sz = INTEGER(dimension)[0];
        if (sz > INT_MAX) {
          error("Input list component 'val' exceeds row limit of %d",
                INT_MAX);
        }
        sz = INTEGER(dimension)[1];
        if (sz > INT_MAX) {
          error("Input list component 'val' exceeds column limit of %d",
                INT_MAX);
        }
        nCoords = sz;
        if (parameter == data[k]->dType) {
          nCoords--;
        }
        if (nCoords > GMS_MAX_INDEX_DIM) {
          error("Input list compoment 'val' exceeds GDX dimension limit of %d.",
                GMS_MAX_INDEX_DIM);
        }
        data[k]->withVal = 1;
      }
      else {
        /* This is for Full/Dense data */
        if (length(dimension) > GMS_MAX_INDEX_DIM) {
          error("Input list component 'val' exceeds GDX dimension limit of %d.",
                GMS_MAX_INDEX_DIM);
        }
        if (withDim == 1 && (data[k]->dim == 0 || data[k]->dim == 1)  && length(dimension) == 2) {
          nCoords = data[k]->dim;
        }
        else if (withDim == 1 &&  data[k]->dim !=  length(dimension)) {
          error("Inconsistent dimension found. Value entered for 'dim' doesn't matches with 'val'.\n");
        }
        else {
          nCoords =  length(dimension);
        }
        data[k]->withVal = 1;
      }
    }
    else {
      Rprintf("List component 'val' must be a double matrix - found %d instead.\n",
              TYPEOF(tmp));
      error("Input list component 'val' must be double matrix");
    }
  }
  else if (data[k]->dType == parameter  || data[k]->withUel == 0 ) {
    Rprintf("data[%d]->dType = %d\n",k,  data[k]->dType);
    Rprintf("data[%d]->withUel = %d\n",k, data[k]->withUel);
    error("'val' is required list component. Please enter it and try again.");
  }


  if (data[k]->withUel == 0 && data[k]->withVal == 1) {
    /* special check for scalar */
    if (withDim == 1 && data[k]->dim == 0 && length(tmp) != 1) {
      error("Scalar should have only one element.");
    }
    /* special check for vector */
    if (withDim == 1 && data[k]->dim == 1 && INTEGER(dimension)[1] != 1) {
      error("Vector should have only 1 dimensional column elements.");
    }
    PROTECT(uelOut = allocVector(VECSXP, nCoords));
    wAlloc++;
    createUelOut(tmp, uelOut, data[k]->dType, data[k]->dForm);
  }

  if (data[k]->withVal == 1) {
    checkForValidData(tmp, uelOut, data[k]->dType, data[k]->dForm);
  }
  registerInputUEL(uelOut, k, uelIndex);
} /* checkWgdxList */


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
    Rprintf("Valid names are: 'name', 'form', 'uels', 'compress', 'dim', 'field', 'ts', 'te'.\n");
    error("Please try again with named input list.\n");
  }
  for (i=0; i < nField; i++) {
    compName = CHAR(STRING_ELT(lstName, i));
    /* Checking for valid field name */
    if ( !((0 == strcmp("name", compName ))
           || (0 == strcmp("uels", compName ))
           || (0 == strcmp("compress", compName ))
           || (0 == strcmp("dim", compName ))
           || (0 == strcmp("form", compName ))
           || (0 == strcmp("field", compName ))
           || (0 == strcmp("te", compName ))
           || (0 == strcmp("ts", compName ))) ) {
      Rprintf ("Input list components must be according to this specification:\n");
      Rprintf ("'name', 'type', 'val', 'dim', 'uels', 'form', 'ts'.\n");
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
        error("Input list component 'form' must be either 'true' or 'false'.");
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
      Rprintf ("List component 'te' must be either string or logical - found %d instead\n",
               TYPEOF(tmp) );
      error("Input list component 'te' must be either string or logical");
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
}


/* This method is intended to be used by both wgdx and gams call
   fileName = name of GDX file to be writen
   argList = Argument list entered by user
   fromGAMS = 1 if this method is called from GAMS otherwise 0
*/
static void
writeGdx(char *gdxFileName,
         int arglen,
         SEXP *symbolList,
         int fromGAMS)
{
  FILE *matdata;
  SEXP uelIndex, compName, valData;
  SEXP mainBuffer, subBuffer;
  struct wgdxStruct **data;
  gdxUelIndex_t uelIndices;
  gdxValues_t   vals;
  shortStringBuf_t msgBuf;
  shortStringBuf_t expText;
  shortStringBuf_t gsBuf;
  const char *stringUelIndex;
  int rc, errNum;
  int i, j, k, z, found;
  SEXP dimVect;
  int totalElement, total,  nColumns, nRows, ndimension, index, total_num_of_elements;
  int d, subindex, inner;
  double *dimVal, *p;
  int *subscript;
  const char *inputTime;

  /* shut up compiler warnings */
  valData = NULL;

  total = 1;
  wAlloc = 0;
  if (fromGAMS == 1) {
    /* Open files for interface data */
    inputTime = "compile";
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

  rc = gdxUELRegisterStrStart (gdxHandle);
  assert(rc);

  PROTECT(uelIndex = allocVector(VECSXP, arglen-2));
  wAlloc++;

  /* ---- load the GDX API ---- */
  /* check input list(s) for data validation */
  i = 0;
  data = (struct  wgdxStruct **)malloc( (arglen - 2) * sizeof(struct wgdxStruct *));

  for (i = 0; i < arglen-2; i++) {
    if (TYPEOF(symbolList[i]) != VECSXP) {
      error("Incorrect type of input argument entered. List expected\n");
    }
    else {
      checkWgdxList (symbolList[i], i, uelIndex, data, fromGAMS);
    }
  }

  rc = gdxUELRegisterDone(gdxHandle);
  assert(rc);

  /* start writing data to GDX file */
  memset (uelIndices, 0, sizeof(gdxUelIndex_t));
  memset (vals, 0, sizeof(gdxValues_t));

  i=0;
  /* write data in GDX file */
  for (i = 0;  i < arglen-2;  i++) {
    /*
     *  Looking for 'val'
     *  This is the glitch that i am worried about
     */
    compName = getAttrib(symbolList[i], R_NamesSymbol);
    for (found = 0, j = 0; j < length(symbolList[i]); j++) {
      if (strcmp("val", CHAR(STRING_ELT(compName, j))) == 0) {
        found = 1;
        break;
      }
    }

    if (1 == found) {
      valData = VECTOR_ELT(symbolList[i], j);
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
          p = REAL(valData);
          for (index = 0; index < totalElement; index++) {
            p[index] = 1;
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
        for (j = 0; j < length(symbolList[i]); j++) {
          if (strcmp("ts", CHAR(STRING_ELT(compName, j))) == 0) {
            found = 1;
            break;
          }
        }

        if (found == 1) {
          (void) CHAR2ShortStr (CHAR(STRING_ELT( VECTOR_ELT(symbolList[i], j), 0)), expText);
        }
      }

      if (data[i]->dForm == sparse) {
        dimVect = getAttrib(valData, R_DimSymbol);
        nColumns = INTEGER(dimVect)[1];
        nRows =INTEGER(dimVect)[0];

        if (data[i]->dType == parameter) {
          nColumns--;
          rc = gdxDataWriteMapStart (gdxHandle, data[i]->name, expText,
                                     nColumns, GMS_DT_PAR, 0);
        }
        else {
          rc = gdxDataWriteMapStart (gdxHandle, data[i]->name, expText,
                                     nColumns, GMS_DT_SET, 0);
        }
        if (!rc) {
          error("Could not write data with gdxDataWriteMapStart");
        }

        /*
         * TODO: This is frustrating.
         * Find a better way to deal with REAL and INTEGER
         * I don't want to loop over whole data set and convert from int to double
         */
        if (TYPEOF(valData) == REALSXP) {
          p = REAL(valData);
        }
        else if (TYPEOF(valData) == INTSXP) {
          p = (double *) INTEGER(valData);
        }
        for (k = 0, j = 0; j < nRows; j++) {
          for (k = 0; k < nColumns; k++) {
            subBuffer =  VECTOR_ELT(mainBuffer, k);
            stringUelIndex = CHAR(STRING_ELT(subBuffer, p[nRows*k + j]-1));
            uelIndices[k] = atoi(stringUelIndex);
          }
          if (data[i]->dType == parameter) {
            vals[0] = p[nRows*(nColumns) + j];
            if (vals[0] != 0 && gdxMapValue (gdxHandle, vals[0], &z)) { /* it's special */
              switch (z) {
              case sv_valpin:
                vals[0] =  NA_REAL;
                break;
              case sv_valmin:
                vals[0] = NA_REAL;
                break;
              case sv_valeps:
                vals[0] = 0;
                break;
              case sv_valund:
              case sv_valna:
                vals[0] = NA_REAL;
                              break;
              default:
                error("Unrecognized map-value %d returned for %g", z, vals[0]);
              } /* end of switch/case */
            }
          }
          /* No need to write zero values */
          if ((data[i]->dType == parameter && vals[0] != 0)
             || data[i]->dType == set) {
            rc = gdxDataWriteMap(gdxHandle, uelIndices, vals);
            if (!rc) {
              error("Could not write parameter MAP with gdxDataWriteMap");
            }
          }
        }

        if (!gdxDataWriteDone(gdxHandle)) {
          error("Could not end writing parameter with gdxDataWriteMapStart");
        }
      }
      else {
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
        }
        if (!rc) {
          error("Could not write data with gdxDataWriteMapStart");
        }
        for (index = 0; index < total_num_of_elements; index++) {
          subindex = index;
          if (nColumns > 0) {
            for (d = nColumns-1; ; d--) {
              subBuffer =  VECTOR_ELT(mainBuffer, d);
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

          if (TYPEOF(valData) == REALSXP) {
            p = REAL(valData);
          }
          else if (TYPEOF(valData) == INTSXP) {
            p = (double*)INTEGER(valData);
          }
          if (data[i]->dType == parameter) {
            vals[0] = p[index];
            if (vals[0] != 0 && gdxMapValue (gdxHandle, vals[0], &z)) { /* it's special */
              switch (z) {
              case sv_valpin:
                vals[0] = NA_REAL;
                break;
              case sv_valmin:
                vals[0] = NA_REAL;
                break;
              case sv_valeps:
                vals[0] = 0;
                break;
              case sv_valund:
              case sv_valna:
                vals[0] = NA_REAL;
                break;
              default:
                error("Unrecognized map-value %d returned for %g", z, vals[0]);
              } /* end of switch/case */
            }
          }
          /* No need to write zero values */
          if ((data[i]->dType == parameter && vals[0] != 0)
             || (data[i]->dType == set && p[index] == 1)) {
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

/* ---------------------------- rgdx -----------------------------------
 * This is the main gateway function
 * to be called from R console
 * First argument <- gdx file name
 * Second argument <- list containing several components
 * that give information about symbol in gdx file
 * ------------------------------------------------------------------ */
SEXP rgdx (SEXP args)
{
  SEXP fileName, symbolList, UEList;
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
  shortStringBuf_t msgBuf;
  shortStringBuf_t uelName;
  const char *uelElementName;
  shortStringBuf_t gdxFileName;
  char *s;
  int symIdx, symDim, symType;
  int rc, errNum, ACount, mrows, ncols, nUEL, iUEL;
  int  k, kk, iRec, nRecs, index, totNumber, changeIdx, nonZero;
  int UELUserMapping, highestMappedUEL;
  int arglen,  maxPossibleElements, z, b, matched, sparesIndex;
  double  *p, *dimVal, *dimElVect;
  char buf[3*sizeof(shortStringBuf_t)];
  char sText[GMS_SSSIZE], msg[GMS_SSSIZE], stringEle[GMS_SSSIZE];
  dField_t dField;
  char *types[] = {"set", "parameter", "variable", "equation"};
  char *forms[] = {"full", "sparse"};
  char *fields[] = {"l", "m", "up", "lo", "s"};
  int nField, defaultIndex, elementIndex, IDum, ndimension, totalElement;
  int *returnedIndex;
  int withList = 0;
  int outFields = 6;
  int mwNElements =0;
  int uelProperty = 0;

  /* setting intial values */
  nonZero = 0;
  alloc = 0;

  /* Due to some reason length is always equal to actual length plus 1 */
  arglen = length(args);

  /* ----------------- Check proper number of inputs and outputs ------------
   * Function should follow specification of
   * rgdx ('gdxFileName', structure)
   * There are 2 inputs
  * -------------------------------------------------------------------------*/
  if (arglen > 3 || arglen < 2) {
    Rprintf("%d entries are entered!\n", arglen-1);
    Rprintf("Input must be according to this specification:\n");
    Rprintf("(gdx_filename, struct)\n");
    error("Incorrect input argument specified.");
  }

  args = CDR(args); fileName = CAR(args);
  args = CDR(args); symbolList = CAR(args);

  /* get the number of input arguments to decide output presentation */
  if (arglen == 3) {
    withList = 1;
  }
  else {
    withList = 0;
  }

  /* Checking that first argument is of type string
   * and second argument is of type list
   */
  if (TYPEOF(fileName) != STRSXP || (withList && TYPEOF(symbolList) != VECSXP)) {
    Rprintf("Input must be according to this specification:\n");
    Rprintf("(gdxFileName, list)\n");
    Rprintf("Argument types are (string, list)\n");
    error("Wrong Argument Type");
  }

  s = CHAR2ShortStr (CHAR(STRING_ELT(fileName, 0)), gdxFileName);

  if (2 == arglen) {
    if (0 == strcmp("?", gdxFileName)) {
      int n = (int)strlen (ID);
      memcpy (strippedID, ID+1, n-2);
      strippedID[n-2] = '\0';
      warning("R-file source info: %s", strippedID);
      return R_NilValue;
    } /* if audit run */
  } /* if one arg, of character type */

  /* ------------------- check if the GDX file exists --------------- */
  checkFileExtension (gdxFileName);
  fin = fopen (gdxFileName, "r");
  if (fin==NULL) {
    sprintf (buf, "File '%s' not found!\n", gdxFileName );
    error(buf);
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
    checkRgdxList(symbolList, inputData);
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
    error("Could not gdx file with gdxOpenRead");
  }

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
    gdxSymbolInfo (gdxHandle, symIdx, buf, &symDim, &symType);
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
      if (symType == dt_var || symType == dt_equ ) {
        mrows = getNonZeroElements(gdxHandle, symIdx, &inputData->dField);
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
          totNumber = 1;
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
          totNumber = 1;
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

            if (symType != dt_set) {
              if (gdxMapValue (gdxHandle, values[dField], &k)) /* it's special */ {
                switch (k) {
                case sv_valpin:
                  p[index] = NA_REAL;
                  break;
                case sv_valmin:
                  p[index] = NA_REAL;
                  break;
                case sv_valeps:
                  p[index] = 0;
                  break;
                case sv_normal:
                  p[index] = values[dField];
                  break;
                case sv_valund:
                case sv_valna:
                  p[index] = NA_REAL;
                  break;
                default:
                  sprintf(buf,
                          "Unrecognized map-value %d returned for %g",
                          k,
                          values[dField]);
                  error(buf);
                } /* end of switch/case */
              }
              else {
                p[index] = values[dField];
              }
            }
          }
          if (matched == maxPossibleElements) {
            break;
          }
        }
        free(returnedIndex);
      } /* End of else of if (te) */
    } /* End of with uels */
    else {
      nonZero = 0;
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
            SET_STRING_ELT(textElement, nonZero, mkChar(msg));
          }
          else {
            strcpy(stringEle, "");
            for (kk = 0;  kk < symDim;  kk++) {
              strcat(stringEle, CHAR(STRING_ELT(UEList, uels[kk]-1))  );
              if (kk != symDim-1) {
                strcat(stringEle, ".");
              }
            }
            SET_STRING_ELT(textElement, nonZero, mkChar(stringEle));
            kk = 0;
          }
          index = 0;

          for (kk = 0;  kk < symDim;  kk++) {
            p[nonZero+kk*mrows] = uels[kk];
            index = nonZero+symDim*mrows;
          }
          nonZero++;
        }  /* loop over GDX records */
      }
      else {
        for (iRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          if (symType == dt_set || values[inputData->dField] != 0) {
            index = 0;
            /* For non zero */
            for (kk = 0;  kk < symDim;  kk++) {
              p[nonZero+kk*mrows] = uels[kk];
              index = nonZero+symDim*mrows;
            }
            nonZero++;
            if (symType != dt_set) {
              if (gdxMapValue (gdxHandle, values[inputData->dField], &k)) /* it's special */ {
                switch (k) {
                case sv_valpin:
                  p[index] = NA_REAL;
                  break;
                case sv_valmin:
                  p[index] = -NA_REAL;
                  break;
                case sv_valeps:
                  p[index] = 0;
                  break;
                case sv_normal:
                  p[index] = values[dField];
                  break;
                case sv_valund:
                case sv_valna:
                  p[index] = NA_REAL;
                  break;
                default:
                  sprintf(buf,
                          "Unrecognized map-value %d returned for %g",
                          k,
                          values[inputData->dField]);
                  error(buf);
                }
              }
              else {
                p[index] = values[inputData->dField];
              }
            }
          } /* end of if (set || val != 0) */
        } /* loop over GDX records */
      }
    }

    /* Converting data into its compressed form. */
    if (inputData->compress == 1) {
      PROTECT(compUels = allocVector(VECSXP, symDim));
      compVal = compressData(compVal, UEList, compUels, nUEL, symDim, mrows);
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
    SET_STRING_ELT(compName, 0, mkChar(inputData->name));
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
  SEXP fileName, *symbolList;
  shortStringBuf_t gdxFileName;
  int arglen, i;

  arglen = length(args);
  if (arglen == 1) {
    error("No input is entered. Please enter valid input\n");
  }

  args = CDR(args); fileName = CAR(args);

  /* Checking that first argument is of type string
   * and second argument is of type list
   */
  if (TYPEOF(fileName) != STRSXP ) {
    Rprintf("The GDX filename (first argument) must be of type string.\n");
    error("Wrong Argument Type");
  }

  (void) CHAR2ShortStr (CHAR(STRING_ELT(fileName, 0)), gdxFileName);
  checkFileExtension (gdxFileName);

  if (2 == arglen) {
    if (0 == strcmp("?", gdxFileName)) {
      int n = (int)strlen (ID);
      memcpy (strippedID, ID+1, n-2);
      strippedID[n-2] = '\0';
      warning("R-file source info: %s", strippedID);
      return R_NilValue;
    } /* if audit run */
  } /* if one arg, of character type */

  symbolList = malloc((arglen-2)*sizeof(*symbolList));
  /* get the pointer of input argument and store it locally for better access */
  for (i = 1; i < arglen-1; i++) {
    args = CDR(args); symbolList[i-1] = CAR(args);
  }
  /* check and write data to gdxfile */
  writeGdx (gdxFileName, arglen, symbolList, 0);
  /* Free up memory */
  free(symbolList);
  return R_NilValue;
} /* End of wgdx */



/* GAMS method */
SEXP gams (SEXP args)
{
  SEXP firstArg,
    *argList,
    result = R_NilValue;
  FILE *fp;
  const char *inputPtr;
  char *writeDataStr, *fileExt, *p;
  shortStringBuf_t input;
  shortStringBuf_t gmsFileName;
  shortStringBuf_t gsBuf;
  int writeData, rc, i, arglen;

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

  inputPtr = CHAR(STRING_ELT(firstArg, 0));

  if (2 == arglen) {
    if (0 == strcmp("?", inputPtr)) {
      int n = (int)strlen (ID);
      memcpy (strippedID, ID+1, n-2);
      strippedID[n-2] = '\0';
      warning("R-file source info: %s", strippedID);
      return R_NilValue;
    } /* if audit run */
  } /* if one arg, of character type */

  checkStringLength(inputPtr);
  (void) CHAR2ShortStr (inputPtr, input);
  p = strtok (input, " ");
  (void) CHAR2ShortStr (p, gmsFileName);
  fileExt = strstr (p, ".");
  if (NULL == fileExt) {
    cat2ShortStr (gmsFileName, ".gms");
  }
  fp = fopen (gmsFileName,"r");
  if (fp == NULL) {
    error("Cannot find or open %s file.\n", gmsFileName);
  }
  else {
    fclose(fp);
  }

  /* specialCommand always starts with a blank */
  strcpy(specialCommand, " ");

  if (arglen > 2) {
    writeData = 1;
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

  if (1 == writeData) {
    argList = malloc((arglen-2)*sizeof(*argList));
    /* get the pointer of input argument and store it locally for better access */
    for (i = 1; i < arglen-1; i++) {
      args = CDR(args); argList[i-1] = CAR(args);
    }
    writeGdx (gmsFileName, arglen, argList, 1);
    /* free memory */
    free(argList);
  }
  rc = callGams(input);
  if (rc) {
    error("GAMS run failed.\n");
  }
  /* read only first element from GDX file */
  result = getGamsSoln (gmsFileName);
  UNPROTECT(gamsAlloc);
  return result;
} /* gams */


/* the gateway routine for gdxInfo.
 * This is very similar to gdxdump: it prints everything on R command prompt.
*/
SEXP gdxInfo (SEXP args)
{
  const char *funcName = "gdxInfo";
  SEXP fileName;
  int arglen;
  shortStringBuf_t gdxFileName;
  int rc,i,j,NrSy,NrUel,ADim,ACount,AUser,AUser2,NRec,FDim,IDum, BadUels=0;
  int ATyp, ATyp2;
  char
    msg[GMS_SSSIZE],
    FileVersion[GMS_SSSIZE], FileProducer[GMS_SSSIZE],
    sName[GMS_SSSIZE], sName2[GMS_SSSIZE], sText[GMS_SSSIZE], UelName[GMS_SSSIZE];

  char DomainIDs[GMS_MAX_INDEX_DIM][GMS_SSSIZE];
  char *DP[GMS_MAX_INDEX_DIM];
  double
    Vals[GMS_VAL_MAX],
    dv[GMS_VAL_MAX];
  int
    Keys[GMS_MAX_INDEX_DIM];
  char *dn, c;

  GDXSTRINDEXPTRS_INIT(DomainIDs,DP);

  /* first arg is function name - ignore it */
  arglen = length(args);

  /* ----------------- Check proper number of inputs ------------
   * Function should follow specification of
   * rgdx ('gdxFileName')
   * -----------------------------------------------------------------------*/
  if (arglen < 1 || arglen > 2) {
    Rprintf ("usage: %s(<gdxFileName>)\n", funcName);
    error ("usage: gdxInfo(<gdxFileName>) - incorrect arg count");
  }

  loadGDX();
  if (1 == arglen) {
    /* no arguments: just load GDX and print the version info */
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

  fileName = CADR(args);
  /* Checking that argument is of type string */
  if (TYPEOF(fileName) != STRSXP) {
    Rprintf ("usage: %s(<gdxFileName>)\n", funcName);
    Rprintf ("  gdxFileName argument must be a string\n");
    error ("usage: gdxInfo(<gdxFileName>) - gdxFileName must be a string");
  }

  (void) CHAR2ShortStr (CHAR(STRING_ELT(fileName, 0)), gdxFileName);

  checkFileExtension (gdxFileName);

  loadGDX();
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

  gdxFileVersion (gdxHandle, FileVersion, FileProducer);
  gdxSystemInfo (gdxHandle, &NrSy, &NrUel);
  Rprintf("*  File version   : %s\n", FileVersion);
  Rprintf("*  Producer       : %s\n", FileProducer);
  Rprintf("*  Symbols        : %d\n", NrSy);
  Rprintf("*  Unique Elements: %d\n", NrUel);

  /* Acroynms */
  for (i = 1;  i <= gdxAcronymCount (gdxHandle);  i++) {
    gdxAcronymGetInfo (gdxHandle, i, sName, sText, &rc);
    Rprintf("Acronym %s", sName);
    if (strlen(sText))
      Rprintf(" '%s'", sText);
    Rprintf(";\n");
  }

  /* Symbolinfo */
  Rprintf ("$ontext\n");
  for (i = 1;  i <= NrSy;  i++) {
    gdxSymbolInfo (gdxHandle, i, sName, &ADim, &ATyp);
    gdxSymbolInfoX (gdxHandle, i, &ACount, &rc, sText);
    Rprintf ("%-15s %3d %-12s %s\n", sName, ADim, gmsGdxTypeText[ATyp], sText);
  }
  Rprintf ("$offtext\n");

  Rprintf ("$onempty onembedded\n");
  dn = NULL;
  for (i = 1;  i <= NrSy;  i++) {
    gdxSymbolInfo (gdxHandle, i, sName, &ADim, &ATyp);
    gdxSymbolInfoX (gdxHandle, i, &ACount, &AUser, sText);

    if (GMS_DT_VAR == ATyp || GMS_DT_EQU == ATyp)
      Rprintf ("$ontext\n");

    if (GMS_DT_VAR == ATyp) {
      if (AUser < 0 || AUser>=GMS_VARTYPE_MAX)
        AUser = GMS_VARTYPE_FREE;
      memcpy (dv, gmsDefRecVar[AUser], GMS_VAL_MAX*sizeof(double));
      dn = (char *) gmsVarTypeText[AUser];
    }
    else if (GMS_DT_EQU == ATyp) {
      if (AUser < 0 || AUser>=GMS_EQUTYPE_MAX)
        AUser = GMS_EQUTYPE_E;
      memcpy (dv, gmsDefRecEqu[AUser], GMS_VAL_MAX*sizeof(double));
    }
    else
      dv[GMS_VAL_LEVEL] = 0.0;

    if (0 == ADim && GMS_DT_PAR == ATyp) /* Scalar */
      Rprintf ("Scalar");
    else {
      if (GMS_DT_VAR == ATyp)
        Rprintf ("%s ", dn);
      Rprintf ("%s", gmsGdxTypeText[ATyp]);
    }
    if (GMS_DT_ALIAS == ATyp) {
      gdxSymbolInfo (gdxHandle, AUser, sName2, &j, &ATyp2);
      Rprintf (" (%s, %s);\n", sName, sName2);
    }
    else {
      Rprintf(" %s", sName);
      if (ADim > 0) {
        gdxSymbolGetDomain (gdxHandle, i, Keys);
        Rprintf ("(");
        for (j = 0;  j < ADim;  j++) {
          if (Keys[j]==0)
            strcpy (sName,"*");
          else
            gdxSymbolInfo (gdxHandle, Keys[j], sName2, &AUser2, &ATyp2);
          if (j < ADim-1)
            Rprintf ("%s,", sName);
          else
            Rprintf ("%s)", sName);
        }
      }
      if (strlen(sText))
        Rprintf(" '%s'", sText);
    }
    if (0 == ACount) {
      if (0 == ADim && GMS_DT_PAR == ATyp) /* Scalar */
        Rprintf (" / 0.0 /;\n");
      else if (GMS_DT_ALIAS != ATyp)
        Rprintf (" / /;\n");
    }
    else {
      Rprintf ("/\n");
      gdxDataReadRawStart (gdxHandle, i, &NRec);
      while (gdxDataReadRaw (gdxHandle, Keys, Vals, &FDim)) {
        if ((GMS_DT_VAR == ATyp || GMS_DT_EQU == ATyp) && 0 == memcmp(Vals,dv,GMS_VAL_MAX*sizeof(double))) /* all default records */
          continue;
        if (GMS_DT_PAR == ATyp && 0.0 == Vals[GMS_VAL_LEVEL])
          continue;
        for (j = 1;  j <= ADim;  j++) {
          if (1 == gdxUMUelGet (gdxHandle, Keys[j-1], UelName, &IDum))
            Rprintf ("'%s'", UelName);
          else {
            Rprintf ("L__", Keys[j-1]);
            BadUels++;
          }
          if (j < ADim)
            Rprintf (".");
        }
        if (GMS_DT_PAR == ATyp)
          Rprintf(" %s\n", val2str(gdxHandle, Vals[GMS_VAL_LEVEL], msg));
        else if (GMS_DT_SET == ATyp)
          if (Vals[GMS_VAL_LEVEL]) {
            j = (int) Vals[GMS_VAL_LEVEL];
            gdxGetElemText (gdxHandle, j, msg, &IDum);
            Rprintf (" '%s'\n", msg);
          }
          else
            Rprintf ("\n");
        else if (GMS_DT_VAR == ATyp || GMS_DT_EQU == ATyp) {
          Rprintf (" .");
          c = '(';
          for (j = GMS_VAL_LEVEL; j < GMS_VAL_MAX;  j++) {
            if (Vals[j] != dv[j]) {
              if (GMS_VAL_SCALE == j && GMS_DT_VAR == ATyp &&
                  AUser != GMS_VARTYPE_POSITIVE && AUser != GMS_VARTYPE_NEGATIVE && AUser != GMS_VARTYPE_FREE)
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
    if (GMS_DT_VAR == ATyp || GMS_DT_EQU == ATyp)
      Rprintf ("$offtext\n");
    Rprintf("\n");
  }
  Rprintf ("$offempty offembedded\n");

  if (BadUels > 0)
    Rprintf("**** %d reference(s) to unique elements without a string representation\n", BadUels);

  gdxFree (&gdxHandle);

  return R_NilValue;
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
  SEXP result;
  int arglen;
  int rc, gdxLoaded;
  char loadPath[GMS_SSSIZE];
  SEXP pSysDir;
  shortStringBuf_t sysDir, msgBuf;

  arglen = length(args);

  if (arglen < 1 || arglen > 2) {
    Rprintf ("usage: %s(<gamsSysDir>)\n", CHAR(STRING_ELT(CAR(args),0)));
    error ("usage: igdx(<gamsSysDir>) - incorrect arg count");
  }

  gdxLoaded = gdxLibraryLoaded();

  if (arglen > 1) {             /* we have gamsSysDir */
    pSysDir = CADR(args);
    if (TYPEOF(pSysDir) != STRSXP) {
      Rprintf ("usage: %s(<gamsSysDir>)\n", CHAR(STRING_ELT(CAR(args),0)));
      Rprintf ("  gamsSysDir argument must be a string\n");
      error ("usage: igdx(<gamsSysDir>) - gamsSysDir must be a string");
    }
    (void) CHAR2ShortStr (CHAR(STRING_ELT(pSysDir, 0)), sysDir);

    /* ---- load the GDX API ---- */
    if (gdxLoaded) {
      (void) gdxLibraryUnload ();
    }
    rc = gdxGetReadyD (sysDir, msgBuf, sizeof(msgBuf));
    if (0 == rc) {
      Rprintf ("Error loading the GDX API from directory %s\n", sysDir);
      Rprintf ("%s\n", msgBuf);
    }
  }

  gdxLoaded = gdxLibraryLoaded();
  PROTECT(result = allocVector(INTSXP, 1));
  INTEGER(result)[0] = gdxLoaded;
  UNPROTECT(1);
  if (gdxLoaded) {
    Rprintf ("The GDX library has been loaded\n");
    gdxGetLoadPath (loadPath);
    Rprintf ("GDX library load path: %s\n",
             loadPath[0] ? loadPath : "unknown");
  }
  else {
    Rprintf ("The GDX library has been not been loaded\n");
  }
  
  return result;
} /* igdx */
