/*  C code generated by apiwrapper */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>

#define GDX_MAIN
#include "gdxcc.h"

#if defined(_WIN32)
# include <windows.h>
  static char winErr[] = "Windows error";
  typedef HINSTANCE soHandle_t;
#elif defined(CIA_HP7)
# include <unistd.h>
# include <dl.h>
  typedef shl_t soHandle_t;
#else
# include <unistd.h>
# include <dlfcn.h>
  typedef void *soHandle_t;
#endif

static soHandle_t h;
static int isLoaded = 0;
static int objectCount = 0;
static int ScreenIndicator = 1;
static int ExceptionIndicator = 0;
static int ExitIndicator = 1;
static gdxErrorCallback_t ErrorCallBack = NULL;
static int APIErrorCount = 0;

typedef void (GDX_CALLCONV *XCreate_t) (gdxHandle_t *pgdx);
static GDX_FUNCPTR(XCreate);
typedef void (GDX_CALLCONV *XFree_t)   (gdxHandle_t *pgdx);
static GDX_FUNCPTR(XFree);
typedef int (GDX_CALLCONV *XAPIVersion_t) (int api, char *msg, int *cl);
static GDX_FUNCPTR(XAPIVersion);
typedef int (GDX_CALLCONV *XCheck_t) (const char *ep, int nargs, int s[], char *msg);
static GDX_FUNCPTR(XCheck);


typedef void (GDX_CALLCONV *gdxSetLoadPath_t) (const char *s);
GDX_FUNCPTR(gdxSetLoadPath);
typedef void (GDX_CALLCONV *gdxGetLoadPath_t) (char *s);
GDX_FUNCPTR(gdxGetLoadPath);

#define printNoReturn(f,nargs) { \
  char d_msgBuf[256]; \
  strcpy(d_msgBuf,#f " could not be loaded: "); \
  XCheck(#f,nargs,d_s,d_msgBuf+strlen(d_msgBuf)); \
  gdxErrorHandling(d_msgBuf); \
}
#define printAndReturn(f,nargs,rtype) { \
  char d_msgBuf[256]; \
  strcpy(d_msgBuf,#f " could not be loaded: "); \
  XCheck(#f,nargs,d_s,d_msgBuf+strlen(d_msgBuf)); \
  gdxErrorHandling(d_msgBuf); \
  return (rtype) 0; \
}

int  GDX_CALLCONV d_gdxAcronymAdd (gdxHandle_t pgdx, const char *AName, const char *AText, int Indx)
{ int d_s[]={3,11,11,3}; printAndReturn(gdxAcronymAdd,3,int ) }

int  GDX_CALLCONV d_gdxAcronymCount (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxAcronymCount,0,int ) }

int  GDX_CALLCONV d_gdxAcronymGetInfo (gdxHandle_t pgdx, int N, char *AName, char *AText, int *Indx)
{ int d_s[]={3,3,12,12,4}; printAndReturn(gdxAcronymGetInfo,4,int ) }

int  GDX_CALLCONV d_gdxAcronymGetMapping (gdxHandle_t pgdx, int N, int *orgIndx, int *newIndx, int *autoIndex)
{ int d_s[]={3,3,4,4,4}; printAndReturn(gdxAcronymGetMapping,4,int ) }

int  GDX_CALLCONV d_gdxAcronymIndex (gdxHandle_t pgdx, double V)
{ int d_s[]={3,13}; printAndReturn(gdxAcronymIndex,1,int ) }

int  GDX_CALLCONV d_gdxAcronymName (gdxHandle_t pgdx, double V, char *AName)
{ int d_s[]={3,13,12}; printAndReturn(gdxAcronymName,2,int ) }

int  GDX_CALLCONV d_gdxAcronymNextNr (gdxHandle_t pgdx, int NV)
{ int d_s[]={3,3}; printAndReturn(gdxAcronymNextNr,1,int ) }

int  GDX_CALLCONV d_gdxAcronymSetInfo (gdxHandle_t pgdx, int N, const char *AName, const char *AText, int Indx)
{ int d_s[]={3,3,11,11,3}; printAndReturn(gdxAcronymSetInfo,4,int ) }

double  GDX_CALLCONV d_gdxAcronymValue (gdxHandle_t pgdx, int Indx)
{ int d_s[]={13,3}; printAndReturn(gdxAcronymValue,1,double ) }

int  GDX_CALLCONV d_gdxAddAlias (gdxHandle_t pgdx, const char *AName1, const char *AName2)
{ int d_s[]={3,11,11}; printAndReturn(gdxAddAlias,2,int ) }

int  GDX_CALLCONV d_gdxAddSetText (gdxHandle_t pgdx, const char *s, int *N)
{ int d_s[]={3,11,4}; printAndReturn(gdxAddSetText,2,int ) }

int  GDX_CALLCONV d_gdxAutoConvert (gdxHandle_t pgdx, int NV)
{ int d_s[]={3,3}; printAndReturn(gdxAutoConvert,1,int ) }

int  GDX_CALLCONV d_gdxClose (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxClose,0,int ) }

int  GDX_CALLCONV d_gdxDataErrorCount (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxDataErrorCount,0,int ) }

int  GDX_CALLCONV d_gdxDataErrorRecord (gdxHandle_t pgdx, int Rn, int *AElements, double *AVals)
{ int d_s[]={3,3,52,54}; printAndReturn(gdxDataErrorRecord,3,int ) }

int  GDX_CALLCONV d_gdxDataReadDone (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxDataReadDone,0,int ) }

int  GDX_CALLCONV d_gdxDataReadFilteredStart (gdxHandle_t pgdx, int SyNr, const int *ADomainNrs, int *NrRecs)
{ int d_s[]={3,3,51,4}; printAndReturn(gdxDataReadFilteredStart,3,int ) }

int  GDX_CALLCONV d_gdxDataReadMap (gdxHandle_t pgdx, int Rn, int *AElements, double *AVals, int *AFDim)
{ int d_s[]={3,3,52,54,4}; printAndReturn(gdxDataReadMap,4,int ) }

int  GDX_CALLCONV d_gdxDataReadMapStart (gdxHandle_t pgdx, int SyNr, int *NrRecs)
{ int d_s[]={3,3,4}; printAndReturn(gdxDataReadMapStart,2,int ) }

int  GDX_CALLCONV d_gdxDataReadRaw (gdxHandle_t pgdx, int *AElements, double *AVals, int *AFDim)
{ int d_s[]={3,52,54,4}; printAndReturn(gdxDataReadRaw,3,int ) }

int  GDX_CALLCONV d_gdxDataReadRawStart (gdxHandle_t pgdx, int SyNr, int *NrRecs)
{ int d_s[]={3,3,4}; printAndReturn(gdxDataReadRawStart,2,int ) }

int  GDX_CALLCONV d_gdxDataReadSlice (gdxHandle_t pgdx, const char **AFiltElements, int *ADim, TDataStoreProc_t DP)
{ int d_s[]={3,55,4,59}; printAndReturn(gdxDataReadSlice,3,int ) }

int  GDX_CALLCONV d_gdxDataReadSliceStart (gdxHandle_t pgdx, int ASyNr, int *ANrElems)
{ int d_s[]={3,3,52}; printAndReturn(gdxDataReadSliceStart,2,int ) }

int  GDX_CALLCONV d_gdxDataReadStr (gdxHandle_t pgdx, char **AStrElements, double *AVals, int *AFDim)
{ int d_s[]={3,56,54,4}; printAndReturn(gdxDataReadStr,3,int ) }

int  GDX_CALLCONV d_gdxDataReadStrStart (gdxHandle_t pgdx, int SyNr, int *NrRecs)
{ int d_s[]={3,3,4}; printAndReturn(gdxDataReadStrStart,2,int ) }

int  GDX_CALLCONV d_gdxDataSliceUELS (gdxHandle_t pgdx, const int *AIndx, char **AUELs)
{ int d_s[]={3,51,56}; printAndReturn(gdxDataSliceUELS,2,int ) }

int  GDX_CALLCONV d_gdxDataWriteDone (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxDataWriteDone,0,int ) }

int  GDX_CALLCONV d_gdxDataWriteMap (gdxHandle_t pgdx, const int *AElements, const double *AVals)
{ int d_s[]={3,51,53}; printAndReturn(gdxDataWriteMap,2,int ) }

int  GDX_CALLCONV d_gdxDataWriteMapStart (gdxHandle_t pgdx, const char *AName, const char *AText, int ADim, int AType, int AUserInfo)
{ int d_s[]={3,11,11,3,3,3}; printAndReturn(gdxDataWriteMapStart,5,int ) }

int  GDX_CALLCONV d_gdxDataWriteRaw (gdxHandle_t pgdx, const int *AElements, const double *AVals)
{ int d_s[]={3,51,53}; printAndReturn(gdxDataWriteRaw,2,int ) }

int  GDX_CALLCONV d_gdxDataWriteRawStart (gdxHandle_t pgdx, const char *AName, const char *AText, int ADim, int AType, int AUserInfo)
{ int d_s[]={3,11,11,3,3,3}; printAndReturn(gdxDataWriteRawStart,5,int ) }

int  GDX_CALLCONV d_gdxDataWriteStr (gdxHandle_t pgdx, const char **AStrElements, const double *AVals)
{ int d_s[]={3,55,53}; printAndReturn(gdxDataWriteStr,2,int ) }

int  GDX_CALLCONV d_gdxDataWriteStrStart (gdxHandle_t pgdx, const char *AName, const char *AText, int ADim, int AType, int AUserInfo)
{ int d_s[]={3,11,11,3,3,3}; printAndReturn(gdxDataWriteStrStart,5,int ) }

int  GDX_CALLCONV d_gdxGetDLLVersion (gdxHandle_t pgdx, char *V)
{ int d_s[]={3,12}; printAndReturn(gdxGetDLLVersion,1,int ) }

int  GDX_CALLCONV d_gdxErrorCount (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxErrorCount,0,int ) }

int  GDX_CALLCONV d_gdxErrorStr (gdxHandle_t pgdx, int N, char *s)
{ int d_s[]={3,3,12}; printAndReturn(gdxErrorStr,2,int ) }

int  GDX_CALLCONV d_gdxFileInfo (gdxHandle_t pgdx, int *FileVer, int *ComprLev)
{ int d_s[]={3,4,4}; printAndReturn(gdxFileInfo,2,int ) }

int  GDX_CALLCONV d_gdxFileVersion (gdxHandle_t pgdx, char *FileStr, char *ProduceStr)
{ int d_s[]={3,12,12}; printAndReturn(gdxFileVersion,2,int ) }

int  GDX_CALLCONV d_gdxFilterExists (gdxHandle_t pgdx, int N)
{ int d_s[]={3,3}; printAndReturn(gdxFilterExists,1,int ) }

int  GDX_CALLCONV d_gdxFilterRegister (gdxHandle_t pgdx, int V)
{ int d_s[]={3,3}; printAndReturn(gdxFilterRegister,1,int ) }

int  GDX_CALLCONV d_gdxFilterRegisterDone (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxFilterRegisterDone,0,int ) }

int  GDX_CALLCONV d_gdxFilterRegisterStart (gdxHandle_t pgdx, int Nr)
{ int d_s[]={3,3}; printAndReturn(gdxFilterRegisterStart,1,int ) }

int  GDX_CALLCONV d_gdxFindSymbol (gdxHandle_t pgdx, const char *AName, int *AIx)
{ int d_s[]={3,11,4}; printAndReturn(gdxFindSymbol,2,int ) }

int  GDX_CALLCONV d_gdxGetElemText (gdxHandle_t pgdx, int N, char *s, int *Node)
{ int d_s[]={3,3,12,4}; printAndReturn(gdxGetElemText,3,int ) }

int  GDX_CALLCONV d_gdxGetLastError (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxGetLastError,0,int ) }

int  GDX_CALLCONV d_gdxGetSpecialValues (gdxHandle_t pgdx, double *AVals)
{ int d_s[]={3,58}; printAndReturn(gdxGetSpecialValues,1,int ) }

int  GDX_CALLCONV d_gdxGetUEL (gdxHandle_t pgdx, int UelNr, char *s)
{ int d_s[]={3,3,12}; printAndReturn(gdxGetUEL,2,int ) }

int  GDX_CALLCONV d_gdxMapValue (gdxHandle_t pgdx, double D, int *sv)
{ int d_s[]={3,13,4}; printAndReturn(gdxMapValue,2,int ) }

int  GDX_CALLCONV d_gdxOpenRead (gdxHandle_t pgdx, const char *Afn, int *ErrNr)
{ int d_s[]={3,11,4}; printAndReturn(gdxOpenRead,2,int ) }

int  GDX_CALLCONV d_gdxOpenWrite (gdxHandle_t pgdx, const char *Afn, const char *AProducer, int *ErrNr)
{ int d_s[]={3,11,11,4}; printAndReturn(gdxOpenWrite,3,int ) }

int  GDX_CALLCONV d_gdxOpenWriteEx (gdxHandle_t pgdx, const char *Afn, const char *AProducer, int Compr, int *ErrNr)
{ int d_s[]={3,11,11,3,4}; printAndReturn(gdxOpenWriteEx,4,int ) }

int  GDX_CALLCONV d_gdxResetSpecialValues (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxResetSpecialValues,0,int ) }

int  GDX_CALLCONV d_gdxSetHasText (gdxHandle_t pgdx, int N)
{ int d_s[]={3,3}; printAndReturn(gdxSetHasText,1,int ) }

int  GDX_CALLCONV d_gdxSetReadSpecialValues (gdxHandle_t pgdx, const double *AVals)
{ int d_s[]={3,57}; printAndReturn(gdxSetReadSpecialValues,1,int ) }

int  GDX_CALLCONV d_gdxSetSpecialValues (gdxHandle_t pgdx, const double *AVals)
{ int d_s[]={3,57}; printAndReturn(gdxSetSpecialValues,1,int ) }

int  GDX_CALLCONV d_gdxSetTextNodeNr (gdxHandle_t pgdx, int N, int Node)
{ int d_s[]={3,3,3}; printAndReturn(gdxSetTextNodeNr,2,int ) }

int  GDX_CALLCONV d_gdxSetTraceLevel (gdxHandle_t pgdx, int N, const char *s)
{ int d_s[]={3,3,11}; printAndReturn(gdxSetTraceLevel,2,int ) }

int  GDX_CALLCONV d_gdxSymbIndxMaxLength (gdxHandle_t pgdx, int SyNr, int *DimInfo)
{ int d_s[]={3,3,52}; printAndReturn(gdxSymbIndxMaxLength,2,int ) }

int  GDX_CALLCONV d_gdxSymbMaxLength (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxSymbMaxLength,0,int ) }

int  GDX_CALLCONV d_gdxSymbolAddComment (gdxHandle_t pgdx, int SyNr, const char *s)
{ int d_s[]={3,3,11}; printAndReturn(gdxSymbolAddComment,2,int ) }

int  GDX_CALLCONV d_gdxSymbolGetComment (gdxHandle_t pgdx, int SyNr, int N, char *s)
{ int d_s[]={3,3,3,12}; printAndReturn(gdxSymbolGetComment,3,int ) }

int  GDX_CALLCONV d_gdxSymbolGetDomain (gdxHandle_t pgdx, int SyNr, int *DomainIDs)
{ int d_s[]={3,3,52}; printAndReturn(gdxSymbolGetDomain,2,int ) }

int  GDX_CALLCONV d_gdxSymbolInfo (gdxHandle_t pgdx, int SyNr, char *AName, int *ADim, int *ATyp)
{ int d_s[]={3,3,12,4,4}; printAndReturn(gdxSymbolInfo,4,int ) }

int  GDX_CALLCONV d_gdxSymbolInfoX (gdxHandle_t pgdx, int SyNr, int *ACount, int *AUserInfo, char *AExplTxt)
{ int d_s[]={3,3,4,4,12}; printAndReturn(gdxSymbolInfoX,4,int ) }

int  GDX_CALLCONV d_gdxSymbolSetDomain (gdxHandle_t pgdx, const char **DomainIDs)
{ int d_s[]={3,55}; printAndReturn(gdxSymbolSetDomain,1,int ) }

int  GDX_CALLCONV d_gdxSystemInfo (gdxHandle_t pgdx, int *NrSy, int *NrUel)
{ int d_s[]={3,4,4}; printAndReturn(gdxSystemInfo,2,int ) }

int  GDX_CALLCONV d_gdxUELMaxLength (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxUELMaxLength,0,int ) }

int  GDX_CALLCONV d_gdxUELRegisterDone (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxUELRegisterDone,0,int ) }

int  GDX_CALLCONV d_gdxUELRegisterMap (gdxHandle_t pgdx, int UelNr, const char *Elem)
{ int d_s[]={3,3,11}; printAndReturn(gdxUELRegisterMap,2,int ) }

int  GDX_CALLCONV d_gdxUELRegisterMapStart (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxUELRegisterMapStart,0,int ) }

int  GDX_CALLCONV d_gdxUELRegisterRaw (gdxHandle_t pgdx, const char *Elem)
{ int d_s[]={3,11}; printAndReturn(gdxUELRegisterRaw,1,int ) }

int  GDX_CALLCONV d_gdxUELRegisterRawStart (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxUELRegisterRawStart,0,int ) }

int  GDX_CALLCONV d_gdxUELRegisterStr (gdxHandle_t pgdx, const char *Elem, int *UelNr)
{ int d_s[]={3,11,4}; printAndReturn(gdxUELRegisterStr,2,int ) }

int  GDX_CALLCONV d_gdxUELRegisterStrStart (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxUELRegisterStrStart,0,int ) }

int  GDX_CALLCONV d_gdxUMFindUEL (gdxHandle_t pgdx, const char *s, int *EN, int *UMap)
{ int d_s[]={3,11,4,4}; printAndReturn(gdxUMFindUEL,3,int ) }

int  GDX_CALLCONV d_gdxUMUelGet (gdxHandle_t pgdx, int N, char *s, int *UMap)
{ int d_s[]={3,3,12,4}; printAndReturn(gdxUMUelGet,3,int ) }

int  GDX_CALLCONV d_gdxUMUelInfo (gdxHandle_t pgdx, int *NrElem, int *HighMap)
{ int d_s[]={3,4,4}; printAndReturn(gdxUMUelInfo,2,int ) }

int  GDX_CALLCONV d_gdxCurrentDim (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxCurrentDim,0,int ) }

/* return dirName on success, NULL on failure */
static char *
extractFileDirFileName (const char *fileName, char *dirName, char *fName)
{
  int fileNameLen, shave=0;
  const char *end, *s;
  char *t;

  if (NULL == fileName || NULL == dirName || fName == NULL) {
    return NULL;
  }
  fileNameLen = (int) strlen(fileName);

#if defined(_WIN32)
  /* get the last delimiter */
  for (end = fileName + fileNameLen - 1;
       end >= fileName && '\\' != *end && ':' != *end;  end--);
  /* shave off the trailing delimiter if:
   *  it isn't the first char,
   *  it is a backslash, and
   *  it is not preceded by a delimiter
   */
  if (end > fileName && '\\' == *end
   && (! ('\\' == *(end-1) || ':' == *(end-1)))
     ) {
    end--; shave=1;
  }
#else
  /* non-Windows: implicitly, this is the Unix version */
  /* get the last delimiter */
  for (end = fileName + fileNameLen - 1;
       end >= fileName && '/' != *end;  end--);

  if (end > fileName && '/' == *end) {
    end--; shave=1;
  }
#endif  /* if defined(_WIN32) */

  for (s = fileName, t = dirName;  s <= end;  s++, t++)
    *t = *s;
  *t = '\0';

  if (shave) s++;
  for (t = fName;  s <= fileName + fileNameLen - 1;  s++, t++)
    *t = *s;
  *t = '\0';

  return dirName;
} /* extractFileDirFileName */

static soHandle_t
loadLib (const char *libName, char **errMsg)
{
  soHandle_t h;
#if defined(CIA_HP7)
  int flag = 0;
#endif

#if defined(_WIN32)
  h = LoadLibrary (libName);
  if (NULL == h) {
    *errMsg = winErr;
  }
  else {
    *errMsg = NULL;
  }
#elif defined(CIA_HP7)
  flag = BIND_IMMEDIATE | BIND_VERBOSE | DYNAMIC_PATH;
  h = shl_load (libName, flag, 0L);
  if (NULL == h) {
    *errMsg = strerror(errno);
  }
  else {
    *errMsg = NULL;
  }
#else
  (void) dlerror();
  h = dlopen (libName, RTLD_NOW);
  if (NULL == h) {
    *errMsg = dlerror();
  }
  else {
    *errMsg = NULL;
  }
#endif

  return h;
} /* loadLib */

static int
unLoadLib (soHandle_t hh)
{
  int rc;

#if defined(_WIN32)
  rc = FreeLibrary (hh);
  return ! rc;
#elif defined(CIA_HP7)
  rc = shl_unload (hh);
#else
  rc = dlclose (hh);
#endif
  return rc;
} /* unLoadLib */

static void *
loadSym (soHandle_t h, const char *sym, char **errMsg)
{
  void *s;
  const char *from;
  char *to;
  const char *tripSym;
  char lcbuf[257];
  char ucbuf[257];
  size_t symLen;
  int trip;
#if defined(CIA_HP7)
  int rc;
#endif

  /* search in this order:
   *  1. lower
   *  2. original
   *  3. upper
   */

  symLen = 0;
  for (trip = 1;  trip <= 3;  trip++) {
    switch (trip) {
    case 1:                             /* lower */
      for (from = sym, to = lcbuf;  *from;  from++, to++) {
        *to = tolower(*from);
      }
      symLen = from - sym;
      lcbuf[symLen] = '\0';
      tripSym = lcbuf;
      break;
    case 2:                             /* original */
      tripSym = sym;
      break;
    case 3:                             /* upper */
      for (from = sym, to = ucbuf;  *from;  from++, to++) {
        *to = toupper(*from);
      }
      ucbuf[symLen] = '\0';
      tripSym = ucbuf;
      break;
    default:
      tripSym = sym;
    } /* end switch */
#if defined(_WIN32)
    s = GetProcAddress (h, tripSym);
    if (NULL != s) {
      return s;
    }
#elif defined(CIA_HP7)
    rc = shl_findsym (&h, tripSym, TYPE_UNDEFINED, &s);
    if (rc) {                     /* failure */
      *errMsg = strerror(errno);
    }
    else {                        /* success */
      *errMsg = NULL;
      return s;
    }
#else
    (void) dlerror();
    s = dlsym (h, tripSym);
    *errMsg = dlerror();
    if (NULL == *errMsg) {
      return s;
    }
#endif
  } /* end loop over symbol name variations */

  return NULL;
} /* loadSym */

/* TNAME = type name, ENAME = exported name */
#define LOADIT(TNAME,ENAME) symName = ENAME; TNAME = (TNAME##_t) loadSym (h, symName, &errMsg); if (NULL == TNAME) goto symMissing
#define LOADIT_ERR_OK(TNAME,ENAME) symName = ENAME; TNAME = (TNAME##_t) loadSym (h, symName, &errMsg)

#define BASENAME "libgdxdclib"
#if defined(CIA_DEX) || defined(CIA_LEX) || defined(CIA_WEX) || defined(CIA_SOX)
# define SUFFIX "64"
#else
# define SUFFIX "64"
#endif
#if defined(_WIN32)
# undef BASENAME
# define BASENAME "gdxdclib"
# define EXTENSION ".dll"
#elif defined(CIA_HP7)
# define EXTENSION ".sl"
#elif defined(CIA_DAR) || defined(CIA_DEX) || defined(CIA_DII)
# define EXTENSION ".dylib"
#else
# define EXTENSION ".so"
#endif

/* XLibraryLoad: return 0 on success, ~0 on failure */
static int
XLibraryLoad (const char *dllName, char *errBuf, int errBufSize)
{
  char *errMsg;
  char *symName;
  int rc, elen, cl;
  char *ebuf;

  if (isLoaded)
    return 0;
  h = loadLib (dllName, &errMsg);
  if (NULL == h) {
    if (NULL != errBuf) {
      elen = errBufSize;  ebuf = errBuf;
      rc = sprintf (ebuf, "%.*s", elen, "Could not load shared library ");
      elen -= rc;  ebuf+= rc;
      rc = sprintf (ebuf, "%.*s", elen, dllName);
      elen -= rc;  ebuf+= rc;
      rc = sprintf (ebuf, "%.*s", elen, ": ");
      elen -= rc;  ebuf+= rc;
      rc = sprintf (ebuf, "%.*s", elen, errMsg);
      elen -= rc;  ebuf+= rc;
      errBuf[errBufSize-1] = '\0';
    }
    return 1;
  }
  else {
     /* printf ("Loaded shared library %s successfully\n", dllName); */
    if (errBuf && errBufSize)
      errBuf[0] = '\0';
  }

  LOADIT(XCreate, "XCreate");
  LOADIT(XFree, "XFree");
  LOADIT(XCheck, "CXCheck");
  LOADIT(XAPIVersion, "CXAPIVersion");

  if (!XAPIVersion(7,errBuf,&cl))
    return 1;


  LOADIT_ERR_OK(gdxSetLoadPath, "CgdxSetLoadPath");
  LOADIT_ERR_OK(gdxGetLoadPath, "CgdxGetLoadPath");
#define CheckAndLoad(f,nargs,prefix) \
  if (!XCheck(#f,nargs,s,errBuf)) \
    f = &d_##f; \
  else { \
    LOADIT(f,prefix #f); \
  }
  {int s[]={3,11,11,3}; CheckAndLoad(gdxAcronymAdd,3,"C"); }
  {int s[]={3}; CheckAndLoad(gdxAcronymCount,0,""); }
  {int s[]={3,3,12,12,4}; CheckAndLoad(gdxAcronymGetInfo,4,"C"); }
  {int s[]={3,3,4,4,4}; CheckAndLoad(gdxAcronymGetMapping,4,""); }
  {int s[]={3,13}; CheckAndLoad(gdxAcronymIndex,1,""); }
  {int s[]={3,13,12}; CheckAndLoad(gdxAcronymName,2,"C"); }
  {int s[]={3,3}; CheckAndLoad(gdxAcronymNextNr,1,""); }
  {int s[]={3,3,11,11,3}; CheckAndLoad(gdxAcronymSetInfo,4,"C"); }
  {int s[]={13,3}; CheckAndLoad(gdxAcronymValue,1,""); }
  {int s[]={3,11,11}; CheckAndLoad(gdxAddAlias,2,"C"); }
  {int s[]={3,11,4}; CheckAndLoad(gdxAddSetText,2,"C"); }
  {int s[]={3,3}; CheckAndLoad(gdxAutoConvert,1,""); }
  {int s[]={3}; CheckAndLoad(gdxClose,0,""); }
  {int s[]={3}; CheckAndLoad(gdxDataErrorCount,0,""); }
  {int s[]={3,3,52,54}; CheckAndLoad(gdxDataErrorRecord,3,""); }
  {int s[]={3}; CheckAndLoad(gdxDataReadDone,0,""); }
  {int s[]={3,3,51,4}; CheckAndLoad(gdxDataReadFilteredStart,3,""); }
  {int s[]={3,3,52,54,4}; CheckAndLoad(gdxDataReadMap,4,""); }
  {int s[]={3,3,4}; CheckAndLoad(gdxDataReadMapStart,2,""); }
  {int s[]={3,52,54,4}; CheckAndLoad(gdxDataReadRaw,3,""); }
  {int s[]={3,3,4}; CheckAndLoad(gdxDataReadRawStart,2,""); }
  {int s[]={3,55,4,59}; CheckAndLoad(gdxDataReadSlice,3,"C"); }
  {int s[]={3,3,52}; CheckAndLoad(gdxDataReadSliceStart,2,""); }
  {int s[]={3,56,54,4}; CheckAndLoad(gdxDataReadStr,3,"C"); }
  {int s[]={3,3,4}; CheckAndLoad(gdxDataReadStrStart,2,""); }
  {int s[]={3,51,56}; CheckAndLoad(gdxDataSliceUELS,2,"C"); }
  {int s[]={3}; CheckAndLoad(gdxDataWriteDone,0,""); }
  {int s[]={3,51,53}; CheckAndLoad(gdxDataWriteMap,2,""); }
  {int s[]={3,11,11,3,3,3}; CheckAndLoad(gdxDataWriteMapStart,5,"C"); }
  {int s[]={3,51,53}; CheckAndLoad(gdxDataWriteRaw,2,""); }
  {int s[]={3,11,11,3,3,3}; CheckAndLoad(gdxDataWriteRawStart,5,"C"); }
  {int s[]={3,55,53}; CheckAndLoad(gdxDataWriteStr,2,"C"); }
  {int s[]={3,11,11,3,3,3}; CheckAndLoad(gdxDataWriteStrStart,5,"C"); }
  {int s[]={3,12}; CheckAndLoad(gdxGetDLLVersion,1,"C"); }
  {int s[]={3}; CheckAndLoad(gdxErrorCount,0,""); }
  {int s[]={3,3,12}; CheckAndLoad(gdxErrorStr,2,"C"); }
  {int s[]={3,4,4}; CheckAndLoad(gdxFileInfo,2,""); }
  {int s[]={3,12,12}; CheckAndLoad(gdxFileVersion,2,"C"); }
  {int s[]={3,3}; CheckAndLoad(gdxFilterExists,1,""); }
  {int s[]={3,3}; CheckAndLoad(gdxFilterRegister,1,""); }
  {int s[]={3}; CheckAndLoad(gdxFilterRegisterDone,0,""); }
  {int s[]={3,3}; CheckAndLoad(gdxFilterRegisterStart,1,""); }
  {int s[]={3,11,4}; CheckAndLoad(gdxFindSymbol,2,"C"); }
  {int s[]={3,3,12,4}; CheckAndLoad(gdxGetElemText,3,"C"); }
  {int s[]={3}; CheckAndLoad(gdxGetLastError,0,""); }
  {int s[]={3,58}; CheckAndLoad(gdxGetSpecialValues,1,""); }
  {int s[]={3,3,12}; CheckAndLoad(gdxGetUEL,2,"C"); }
  {int s[]={3,13,4}; CheckAndLoad(gdxMapValue,2,""); }
  {int s[]={3,11,4}; CheckAndLoad(gdxOpenRead,2,"C"); }
  {int s[]={3,11,11,4}; CheckAndLoad(gdxOpenWrite,3,"C"); }
  {int s[]={3,11,11,3,4}; CheckAndLoad(gdxOpenWriteEx,4,"C"); }
  {int s[]={3}; CheckAndLoad(gdxResetSpecialValues,0,""); }
  {int s[]={3,3}; CheckAndLoad(gdxSetHasText,1,""); }
  {int s[]={3,57}; CheckAndLoad(gdxSetReadSpecialValues,1,""); }
  {int s[]={3,57}; CheckAndLoad(gdxSetSpecialValues,1,""); }
  {int s[]={3,3,3}; CheckAndLoad(gdxSetTextNodeNr,2,""); }
  {int s[]={3,3,11}; CheckAndLoad(gdxSetTraceLevel,2,"C"); }
  {int s[]={3,3,52}; CheckAndLoad(gdxSymbIndxMaxLength,2,""); }
  {int s[]={3}; CheckAndLoad(gdxSymbMaxLength,0,""); }
  {int s[]={3,3,11}; CheckAndLoad(gdxSymbolAddComment,2,"C"); }
  {int s[]={3,3,3,12}; CheckAndLoad(gdxSymbolGetComment,3,"C"); }
  {int s[]={3,3,52}; CheckAndLoad(gdxSymbolGetDomain,2,""); }
  {int s[]={3,3,12,4,4}; CheckAndLoad(gdxSymbolInfo,4,"C"); }
  {int s[]={3,3,4,4,12}; CheckAndLoad(gdxSymbolInfoX,4,"C"); }
  {int s[]={3,55}; CheckAndLoad(gdxSymbolSetDomain,1,"C"); }
  {int s[]={3,4,4}; CheckAndLoad(gdxSystemInfo,2,""); }
  {int s[]={3}; CheckAndLoad(gdxUELMaxLength,0,""); }
  {int s[]={3}; CheckAndLoad(gdxUELRegisterDone,0,""); }
  {int s[]={3,3,11}; CheckAndLoad(gdxUELRegisterMap,2,"C"); }
  {int s[]={3}; CheckAndLoad(gdxUELRegisterMapStart,0,""); }
  {int s[]={3,11}; CheckAndLoad(gdxUELRegisterRaw,1,"C"); }
  {int s[]={3}; CheckAndLoad(gdxUELRegisterRawStart,0,""); }
  {int s[]={3,11,4}; CheckAndLoad(gdxUELRegisterStr,2,"C"); }
  {int s[]={3}; CheckAndLoad(gdxUELRegisterStrStart,0,""); }
  {int s[]={3,11,4,4}; CheckAndLoad(gdxUMFindUEL,3,"C"); }
  {int s[]={3,3,12,4}; CheckAndLoad(gdxUMUelGet,3,"C"); }
  {int s[]={3,4,4}; CheckAndLoad(gdxUMUelInfo,2,""); }
  {int s[]={3}; CheckAndLoad(gdxCurrentDim,0,""); }

 return 0;

 symMissing:
  if (errBuf && errBufSize>0) {
    elen = errBufSize;  ebuf = errBuf;
    rc = sprintf (ebuf, "%.*s", elen, "Could not load symbol '");
    elen -= rc;  ebuf+= rc;
    rc = sprintf (ebuf, "%.*s", elen, symName);
    elen -= rc;  ebuf+= rc;
    rc = sprintf (ebuf, "%.*s", elen, "': ");
    elen -= rc;  ebuf+= rc;
    rc = sprintf (ebuf, "%.*s", elen, errMsg);
    elen -= rc;  ebuf+= rc;
    errBuf[errBufSize-1] = '\0';
    /* printf ("%s\n", errBuf); */
    return 2;
  }

 return 0;

} /* XLibraryLoad */

static int
libloader(const char *dllPath, const char *dllName, char *msgBuf, int msgBufSize)
{

  char dllNameBuf[512];
  int myrc = 0;

  if (NULL != msgBuf) msgBuf[0] = '\0';

  if (! isLoaded) {
    if (NULL != dllPath && '\0' != *dllPath) {
      strncpy(dllNameBuf, dllPath, sizeof(dllNameBuf)-1);
      dllNameBuf[sizeof(dllNameBuf)-2] = '\0';
#if defined(_WIN32)
      if ('\\' != dllNameBuf[strlen(dllNameBuf)])
        strcat(dllNameBuf,"\\");
#else
      if ('/' != dllNameBuf[strlen(dllNameBuf)])
        strcat(dllNameBuf,"/");
#endif
    }
    else {
      dllNameBuf[0] = '\0';
    }
    if (NULL != dllName && '\0' != *dllName) {
      strncat(dllNameBuf, dllName, sizeof(dllNameBuf)-strlen(dllNameBuf));
      dllNameBuf[sizeof(dllNameBuf)-1] = '\0';
    }
    else {
      strncat(dllNameBuf, BASENAME SUFFIX EXTENSION, sizeof(dllNameBuf)-strlen(dllNameBuf));
    }
    isLoaded = ! XLibraryLoad (dllNameBuf, msgBuf, msgBufSize);
    if (isLoaded) {
       if (NULL != gdxSetLoadPath && NULL != dllPath && '\0' != *dllPath) {
         gdxSetLoadPath(dllPath);
       }
       else {                            /* no setLoadPath call found */
         myrc |= 2;
       }
    }
    else {                              /* library load failed */
      myrc |= 1;
    }
  }
  return (myrc & 1) == 0;
}


/* gdxGetReady: return false on failure to load library, true on success */
int gdxGetReady (char *msgBuf, int msgBufSize)
{
  return libloader(NULL, NULL, msgBuf, msgBufSize);
} /* gdxGetReady */

/* gdxGetReadyD: return false on failure to load library, true on success */
int gdxGetReadyD (const char *dirName, char *msgBuf, int msgBufSize)
{
  return libloader(dirName, NULL, msgBuf, msgBufSize);
} /* gdxGetReadyD */

/* gdxGetReadyL: return false on failure to load library, true on success */
int gdxGetReadyL (const char *libName, char *msgBuf, int msgBufSize)
{
  char dirName[1024],fName[1024];
  extractFileDirFileName (libName, dirName, fName);
  return libloader(dirName, fName, msgBuf, msgBufSize);
} /* gdxGetReadyL */

/* gdxCreate: return false on failure to load library, true on success */
int gdxCreate (gdxHandle_t *pgdx, char *msgBuf, int msgBufSize)
{
  int gdxIsReady;

  gdxIsReady = gdxGetReady (msgBuf, msgBufSize);
  if (! gdxIsReady) {
    return 0;
  }
  assert(XCreate);
  XCreate(pgdx);
  if(pgdx == NULL)
  { strcpy(msgBuf,"Error while creating object"); return 0; }
  objectCount++;
  return 1;                     /* return true on successful library load */
} /* gdxCreate */

/* gdxCreateD: return false on failure to load library, true on success */
int gdxCreateD (gdxHandle_t *pgdx, const char *dirName,
                char *msgBuf, int msgBufSize)
{
  int gdxIsReady;

  gdxIsReady = gdxGetReadyD (dirName, msgBuf, msgBufSize);
  if (! gdxIsReady) {
    return 0;
  }
  assert(XCreate);
  XCreate(pgdx);
  if(pgdx == NULL)
  { strcpy(msgBuf,"Error while creating object"); return 0; }
  objectCount++;
  return 1;                     /* return true on successful library load */
} /* gdxCreateD */

/* gdxCreateL: return false on failure to load library, true on success */
int gdxCreateL (gdxHandle_t *pgdx, const char *libName,
                char *msgBuf, int msgBufSize)
{
  int gdxIsReady;

  gdxIsReady = gdxGetReadyL (libName, msgBuf, msgBufSize);
  if (! gdxIsReady) {
    return 0;
  }
  assert(XCreate);
  XCreate(pgdx);
  if(pgdx == NULL)
  { strcpy(msgBuf,"Error while creating object"); return 0; }
  objectCount++;
  return 1;                     /* return true on successful library load */
} /* gdxCreateL */

int gdxFree   (gdxHandle_t *pgdx)
{
  assert(XFree);
  XFree(pgdx); pgdx = NULL;
  objectCount--;
  return 1;
} /* gdxFree */

int gdxLibraryLoaded(void)
{
  return isLoaded;
} /* gdxLibraryLoaded */

int gdxLibraryUnload(void)
{
  if (objectCount > 0)
    return 0;
  isLoaded = 0;
  (void) unLoadLib(h);
  return 1;
} /* gdxLibraryUnload */

int gdxGetScreenIndicator(void)
{
  return ScreenIndicator;
}

void gdxSetScreenIndicator(int scrind)
{
  ScreenIndicator = scrind ? 1 : 0;
}

int gdxGetExceptionIndicator(void)
{
   return ExceptionIndicator;
}

void gdxSetExceptionIndicator(int excind)
{
  ExceptionIndicator = excind ? 1 : 0;
}

int gdxGetExitIndicator(void)
{
  return ExitIndicator;
}

void gdxSetExitIndicator(int extind)
{
  ExitIndicator = extind ? 1 : 0;
}

gdxErrorCallback_t gdxGetErrorCallback(void)
{
  return ErrorCallBack;
}

void gdxSetErrorCallback(gdxErrorCallback_t func)
{
  ErrorCallBack = func;
}

int gdxGetAPIErrorCount(void)
{
  return APIErrorCount;
}

void gdxSetAPIErrorCount(int ecnt)
{
  APIErrorCount = ecnt;
}

void gdxErrorHandling(const char *msg)
{
  APIErrorCount++;
  if (ScreenIndicator) { printf("%s\n", msg); fflush(stdout); }
  if (ErrorCallBack)
     if (ErrorCallBack(APIErrorCount, msg)) exit(123);
  assert(!ExceptionIndicator);
  if (ExitIndicator) exit(123);
}

