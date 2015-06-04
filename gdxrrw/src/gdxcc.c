/*  C code generated by apiwrapper for GAMS Version 24.3.0 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>

#define GDX_MAIN
#include "gdxcc.h"

#if ! defined(_GCL_RHACK_)
# error "this header modified to work with R extensions.  Do not use outside of R"
#endif

#if defined(_WIN32)
# include <windows.h>
  static char winErr[] = "Windows error";
  typedef HINSTANCE soHandle_t;
#else
# include <unistd.h>
# include <dlfcn.h>
# include <sys/utsname.h>
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

#if defined(HAVE_MUTEX)
#include "gcmt.h"
static GC_mutex_t libMutex;
static GC_mutex_t objMutex;
static GC_mutex_t exceptMutex;

static int MutexIsInitialized = 0;

void gdxInitMutexes(void)
{
  int rc;
  if (0==MutexIsInitialized) {
    rc = GC_mutex_init (&libMutex);     assert(0==rc);
    rc = GC_mutex_init (&objMutex);     assert(0==rc);
    rc = GC_mutex_init (&exceptMutex);  assert(0==rc);
    MutexIsInitialized = 1;
  }
}

void gdxFiniMutexes(void)
{
  if (1==MutexIsInitialized) {
    GC_mutex_delete (&libMutex);
    GC_mutex_delete (&objMutex);
    GC_mutex_delete (&exceptMutex);
    MutexIsInitialized = 0;
  }
}
#  define lock(MUTEX)   if(MutexIsInitialized) GC_mutex_lock (&MUTEX);
#  define unlock(MUTEX) if(MutexIsInitialized) GC_mutex_unlock (&MUTEX);
#else
#  define lock(MUTEX)   ;
#  define unlock(MUTEX) ;
void gdxInitMutexes(void) {}
void gdxFiniMutexes(void) {}
#endif

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

#define printNoReturn(f,nargs) {}
#define printAndReturn(f,nargs,rtype) { return (rtype) d_s[0]; }

int  GDX_CALLCONV d_gdxAcronymAdd (gdxHandle_t pgdx, const char *AName, const char *Txt, int AIndx)
{ int d_s[]={3,11,11,3}; printAndReturn(gdxAcronymAdd,3,int ) }

int  GDX_CALLCONV d_gdxAcronymCount (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxAcronymCount,0,int ) }

int  GDX_CALLCONV d_gdxAcronymGetInfo (gdxHandle_t pgdx, int N, char *AName, char *Txt, int *AIndx)
{ int d_s[]={3,3,12,12,4}; printAndReturn(gdxAcronymGetInfo,4,int ) }

int  GDX_CALLCONV d_gdxAcronymGetMapping (gdxHandle_t pgdx, int N, int *orgIndx, int *newIndx, int *autoIndex)
{ int d_s[]={3,3,4,4,4}; printAndReturn(gdxAcronymGetMapping,4,int ) }

int  GDX_CALLCONV d_gdxAcronymIndex (gdxHandle_t pgdx, double V)
{ int d_s[]={3,13}; printAndReturn(gdxAcronymIndex,1,int ) }

int  GDX_CALLCONV d_gdxAcronymName (gdxHandle_t pgdx, double V, char *AName)
{ int d_s[]={3,13,12}; printAndReturn(gdxAcronymName,2,int ) }

int  GDX_CALLCONV d_gdxAcronymNextNr (gdxHandle_t pgdx, int NV)
{ int d_s[]={3,3}; printAndReturn(gdxAcronymNextNr,1,int ) }

int  GDX_CALLCONV d_gdxAcronymSetInfo (gdxHandle_t pgdx, int N, const char *AName, const char *Txt, int AIndx)
{ int d_s[]={3,3,11,11,3}; printAndReturn(gdxAcronymSetInfo,4,int ) }

double  GDX_CALLCONV d_gdxAcronymValue (gdxHandle_t pgdx, int AIndx)
{ int d_s[]={13,3}; printAndReturn(gdxAcronymValue,1,double ) }

int  GDX_CALLCONV d_gdxAddAlias (gdxHandle_t pgdx, const char *Id1, const char *Id2)
{ int d_s[]={3,11,11}; printAndReturn(gdxAddAlias,2,int ) }

int  GDX_CALLCONV d_gdxAddSetText (gdxHandle_t pgdx, const char *Txt, int *TxtNr)
{ int d_s[]={3,11,4}; printAndReturn(gdxAddSetText,2,int ) }

int  GDX_CALLCONV d_gdxAutoConvert (gdxHandle_t pgdx, int NV)
{ int d_s[]={3,3}; printAndReturn(gdxAutoConvert,1,int ) }

int  GDX_CALLCONV d_gdxClose (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxClose,0,int ) }

int  GDX_CALLCONV d_gdxDataErrorCount (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxDataErrorCount,0,int ) }

int  GDX_CALLCONV d_gdxDataErrorRecord (gdxHandle_t pgdx, int RecNr, int KeyInt[], double Values[])
{ int d_s[]={3,3,52,54}; printAndReturn(gdxDataErrorRecord,3,int ) }

int  GDX_CALLCONV d_gdxDataReadDone (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxDataReadDone,0,int ) }

int  GDX_CALLCONV d_gdxDataReadFilteredStart (gdxHandle_t pgdx, int SyNr, const int FilterAction[], int *NrRecs)
{ int d_s[]={3,3,51,4}; printAndReturn(gdxDataReadFilteredStart,3,int ) }

int  GDX_CALLCONV d_gdxDataReadMap (gdxHandle_t pgdx, int RecNr, int KeyInt[], double Values[], int *DimFrst)
{ int d_s[]={3,3,52,54,4}; printAndReturn(gdxDataReadMap,4,int ) }

int  GDX_CALLCONV d_gdxDataReadMapStart (gdxHandle_t pgdx, int SyNr, int *NrRecs)
{ int d_s[]={3,3,4}; printAndReturn(gdxDataReadMapStart,2,int ) }

int  GDX_CALLCONV d_gdxDataReadRaw (gdxHandle_t pgdx, int KeyInt[], double Values[], int *DimFrst)
{ int d_s[]={3,52,54,4}; printAndReturn(gdxDataReadRaw,3,int ) }

int  GDX_CALLCONV d_gdxDataReadRawFast (gdxHandle_t pgdx, int SyNr, TDataStoreProc_t DP, int *NrRecs)
{ int d_s[]={3,3,59,4}; printAndReturn(gdxDataReadRawFast,3,int ) }

int  GDX_CALLCONV d_gdxDataReadRawStart (gdxHandle_t pgdx, int SyNr, int *NrRecs)
{ int d_s[]={3,3,4}; printAndReturn(gdxDataReadRawStart,2,int ) }

int  GDX_CALLCONV d_gdxDataReadSlice (gdxHandle_t pgdx, const char *UelFilterStr[], int *Dimen, TDataStoreProc_t DP)
{ int d_s[]={3,55,4,59}; printAndReturn(gdxDataReadSlice,3,int ) }

int  GDX_CALLCONV d_gdxDataReadSliceStart (gdxHandle_t pgdx, int SyNr, int ElemCounts[])
{ int d_s[]={3,3,52}; printAndReturn(gdxDataReadSliceStart,2,int ) }

int  GDX_CALLCONV d_gdxDataReadStr (gdxHandle_t pgdx, char *KeyStr[], double Values[], int *DimFrst)
{ int d_s[]={3,56,54,4}; printAndReturn(gdxDataReadStr,3,int ) }

int  GDX_CALLCONV d_gdxDataReadStrStart (gdxHandle_t pgdx, int SyNr, int *NrRecs)
{ int d_s[]={3,3,4}; printAndReturn(gdxDataReadStrStart,2,int ) }

int  GDX_CALLCONV d_gdxDataSliceUELS (gdxHandle_t pgdx, const int SliceKeyInt[], char *KeyStr[])
{ int d_s[]={3,51,56}; printAndReturn(gdxDataSliceUELS,2,int ) }

int  GDX_CALLCONV d_gdxDataWriteDone (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxDataWriteDone,0,int ) }

int  GDX_CALLCONV d_gdxDataWriteMap (gdxHandle_t pgdx, const int KeyInt[], const double Values[])
{ int d_s[]={3,51,53}; printAndReturn(gdxDataWriteMap,2,int ) }

int  GDX_CALLCONV d_gdxDataWriteMapStart (gdxHandle_t pgdx, const char *SyId, const char *ExplTxt, int Dimen, int Typ, int UserInfo)
{ int d_s[]={3,11,11,3,3,3}; printAndReturn(gdxDataWriteMapStart,5,int ) }

int  GDX_CALLCONV d_gdxDataWriteRaw (gdxHandle_t pgdx, const int KeyInt[], const double Values[])
{ int d_s[]={3,51,53}; printAndReturn(gdxDataWriteRaw,2,int ) }

int  GDX_CALLCONV d_gdxDataWriteRawStart (gdxHandle_t pgdx, const char *SyId, const char *ExplTxt, int Dimen, int Typ, int UserInfo)
{ int d_s[]={3,11,11,3,3,3}; printAndReturn(gdxDataWriteRawStart,5,int ) }

int  GDX_CALLCONV d_gdxDataWriteStr (gdxHandle_t pgdx, const char *KeyStr[], const double Values[])
{ int d_s[]={3,55,53}; printAndReturn(gdxDataWriteStr,2,int ) }

int  GDX_CALLCONV d_gdxDataWriteStrStart (gdxHandle_t pgdx, const char *SyId, const char *ExplTxt, int Dimen, int Typ, int UserInfo)
{ int d_s[]={3,11,11,3,3,3}; printAndReturn(gdxDataWriteStrStart,5,int ) }

int  GDX_CALLCONV d_gdxGetDLLVersion (gdxHandle_t pgdx, char *V)
{ int d_s[]={3,12}; printAndReturn(gdxGetDLLVersion,1,int ) }

int  GDX_CALLCONV d_gdxErrorCount (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxErrorCount,0,int ) }

int  GDX_CALLCONV d_gdxErrorStr (gdxHandle_t pgdx, int ErrNr, char *ErrMsg)
{ int d_s[]={3,3,12}; printAndReturn(gdxErrorStr,2,int ) }

int  GDX_CALLCONV d_gdxFileInfo (gdxHandle_t pgdx, int *FileVer, int *ComprLev)
{ int d_s[]={3,4,4}; printAndReturn(gdxFileInfo,2,int ) }

int  GDX_CALLCONV d_gdxFileVersion (gdxHandle_t pgdx, char *FileStr, char *ProduceStr)
{ int d_s[]={3,12,12}; printAndReturn(gdxFileVersion,2,int ) }

int  GDX_CALLCONV d_gdxFilterExists (gdxHandle_t pgdx, int FilterNr)
{ int d_s[]={3,3}; printAndReturn(gdxFilterExists,1,int ) }

int  GDX_CALLCONV d_gdxFilterRegister (gdxHandle_t pgdx, int UelMap)
{ int d_s[]={3,3}; printAndReturn(gdxFilterRegister,1,int ) }

int  GDX_CALLCONV d_gdxFilterRegisterDone (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxFilterRegisterDone,0,int ) }

int  GDX_CALLCONV d_gdxFilterRegisterStart (gdxHandle_t pgdx, int FilterNr)
{ int d_s[]={3,3}; printAndReturn(gdxFilterRegisterStart,1,int ) }

int  GDX_CALLCONV d_gdxFindSymbol (gdxHandle_t pgdx, const char *SyId, int *SyNr)
{ int d_s[]={3,11,4}; printAndReturn(gdxFindSymbol,2,int ) }

int  GDX_CALLCONV d_gdxGetElemText (gdxHandle_t pgdx, int TxtNr, char *Txt, int *Node)
{ int d_s[]={3,3,12,4}; printAndReturn(gdxGetElemText,3,int ) }

int  GDX_CALLCONV d_gdxGetLastError (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxGetLastError,0,int ) }

INT64  GDX_CALLCONV d_gdxGetMemoryUsed (gdxHandle_t pgdx)
{ int d_s[]={23}; printAndReturn(gdxGetMemoryUsed,0,INT64 ) }

int  GDX_CALLCONV d_gdxGetSpecialValues (gdxHandle_t pgdx, double AVals[])
{ int d_s[]={3,58}; printAndReturn(gdxGetSpecialValues,1,int ) }

int  GDX_CALLCONV d_gdxGetUEL (gdxHandle_t pgdx, int UelNr, char *Uel)
{ int d_s[]={3,3,12}; printAndReturn(gdxGetUEL,2,int ) }

int  GDX_CALLCONV d_gdxMapValue (gdxHandle_t pgdx, double D, int *sv)
{ int d_s[]={3,13,4}; printAndReturn(gdxMapValue,2,int ) }

int  GDX_CALLCONV d_gdxOpenAppend (gdxHandle_t pgdx, const char *FileName, const char *Producer, int *ErrNr)
{ int d_s[]={3,11,11,4}; printAndReturn(gdxOpenAppend,3,int ) }

int  GDX_CALLCONV d_gdxOpenRead (gdxHandle_t pgdx, const char *FileName, int *ErrNr)
{ int d_s[]={3,11,4}; printAndReturn(gdxOpenRead,2,int ) }

int  GDX_CALLCONV d_gdxOpenWrite (gdxHandle_t pgdx, const char *FileName, const char *Producer, int *ErrNr)
{ int d_s[]={3,11,11,4}; printAndReturn(gdxOpenWrite,3,int ) }

int  GDX_CALLCONV d_gdxOpenWriteEx (gdxHandle_t pgdx, const char *FileName, const char *Producer, int Compr, int *ErrNr)
{ int d_s[]={3,11,11,3,4}; printAndReturn(gdxOpenWriteEx,4,int ) }

int  GDX_CALLCONV d_gdxResetSpecialValues (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxResetSpecialValues,0,int ) }

int  GDX_CALLCONV d_gdxSetHasText (gdxHandle_t pgdx, int SyNr)
{ int d_s[]={3,3}; printAndReturn(gdxSetHasText,1,int ) }

int  GDX_CALLCONV d_gdxSetReadSpecialValues (gdxHandle_t pgdx, const double AVals[])
{ int d_s[]={3,57}; printAndReturn(gdxSetReadSpecialValues,1,int ) }

int  GDX_CALLCONV d_gdxSetSpecialValues (gdxHandle_t pgdx, const double AVals[])
{ int d_s[]={3,57}; printAndReturn(gdxSetSpecialValues,1,int ) }

int  GDX_CALLCONV d_gdxSetTextNodeNr (gdxHandle_t pgdx, int TxtNr, int Node)
{ int d_s[]={3,3,3}; printAndReturn(gdxSetTextNodeNr,2,int ) }

int  GDX_CALLCONV d_gdxSetTraceLevel (gdxHandle_t pgdx, int N, const char *s)
{ int d_s[]={3,3,11}; printAndReturn(gdxSetTraceLevel,2,int ) }

int  GDX_CALLCONV d_gdxSymbIndxMaxLength (gdxHandle_t pgdx, int SyNr, int LengthInfo[])
{ int d_s[]={3,3,52}; printAndReturn(gdxSymbIndxMaxLength,2,int ) }

int  GDX_CALLCONV d_gdxSymbMaxLength (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxSymbMaxLength,0,int ) }

int  GDX_CALLCONV d_gdxSymbolAddComment (gdxHandle_t pgdx, int SyNr, const char *Txt)
{ int d_s[]={3,3,11}; printAndReturn(gdxSymbolAddComment,2,int ) }

int  GDX_CALLCONV d_gdxSymbolGetComment (gdxHandle_t pgdx, int SyNr, int N, char *Txt)
{ int d_s[]={3,3,3,12}; printAndReturn(gdxSymbolGetComment,3,int ) }

int  GDX_CALLCONV d_gdxSymbolGetDomain (gdxHandle_t pgdx, int SyNr, int DomainSyNrs[])
{ int d_s[]={3,3,52}; printAndReturn(gdxSymbolGetDomain,2,int ) }

int  GDX_CALLCONV d_gdxSymbolGetDomainX (gdxHandle_t pgdx, int SyNr, char *DomainIDs[])
{ int d_s[]={3,3,56}; printAndReturn(gdxSymbolGetDomainX,2,int ) }

int  GDX_CALLCONV d_gdxSymbolDim (gdxHandle_t pgdx, int SyNr)
{ int d_s[]={3,3}; printAndReturn(gdxSymbolDim,1,int ) }

int  GDX_CALLCONV d_gdxSymbolInfo (gdxHandle_t pgdx, int SyNr, char *SyId, int *Dimen, int *Typ)
{ int d_s[]={3,3,12,4,4}; printAndReturn(gdxSymbolInfo,4,int ) }

int  GDX_CALLCONV d_gdxSymbolInfoX (gdxHandle_t pgdx, int SyNr, int *RecCnt, int *UserInfo, char *ExplTxt)
{ int d_s[]={3,3,4,4,12}; printAndReturn(gdxSymbolInfoX,4,int ) }

int  GDX_CALLCONV d_gdxSymbolSetDomain (gdxHandle_t pgdx, const char *DomainIDs[])
{ int d_s[]={3,55}; printAndReturn(gdxSymbolSetDomain,1,int ) }

int  GDX_CALLCONV d_gdxSymbolSetDomainX (gdxHandle_t pgdx, int SyNr, const char *DomainIDs[])
{ int d_s[]={3,3,55}; printAndReturn(gdxSymbolSetDomainX,2,int ) }

int  GDX_CALLCONV d_gdxSystemInfo (gdxHandle_t pgdx, int *SyCnt, int *UelCnt)
{ int d_s[]={3,4,4}; printAndReturn(gdxSystemInfo,2,int ) }

int  GDX_CALLCONV d_gdxUELMaxLength (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxUELMaxLength,0,int ) }

int  GDX_CALLCONV d_gdxUELRegisterDone (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxUELRegisterDone,0,int ) }

int  GDX_CALLCONV d_gdxUELRegisterMap (gdxHandle_t pgdx, int UMap, const char *Uel)
{ int d_s[]={3,3,11}; printAndReturn(gdxUELRegisterMap,2,int ) }

int  GDX_CALLCONV d_gdxUELRegisterMapStart (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxUELRegisterMapStart,0,int ) }

int  GDX_CALLCONV d_gdxUELRegisterRaw (gdxHandle_t pgdx, const char *Uel)
{ int d_s[]={3,11}; printAndReturn(gdxUELRegisterRaw,1,int ) }

int  GDX_CALLCONV d_gdxUELRegisterRawStart (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxUELRegisterRawStart,0,int ) }

int  GDX_CALLCONV d_gdxUELRegisterStr (gdxHandle_t pgdx, const char *Uel, int *UelNr)
{ int d_s[]={3,11,4}; printAndReturn(gdxUELRegisterStr,2,int ) }

int  GDX_CALLCONV d_gdxUELRegisterStrStart (gdxHandle_t pgdx)
{ int d_s[]={3}; printAndReturn(gdxUELRegisterStrStart,0,int ) }

int  GDX_CALLCONV d_gdxUMFindUEL (gdxHandle_t pgdx, const char *Uel, int *UelNr, int *UelMap)
{ int d_s[]={3,11,4,4}; printAndReturn(gdxUMFindUEL,3,int ) }

int  GDX_CALLCONV d_gdxUMUelGet (gdxHandle_t pgdx, int UelNr, char *Uel, int *UelMap)
{ int d_s[]={3,3,12,4}; printAndReturn(gdxUMUelGet,3,int ) }

int  GDX_CALLCONV d_gdxUMUelInfo (gdxHandle_t pgdx, int *UelCnt, int *HighMap)
{ int d_s[]={3,4,4}; printAndReturn(gdxUMUelInfo,2,int ) }

int  GDX_CALLCONV d_gdxGetDomainElements (gdxHandle_t pgdx, int SyNr, int DimPos, int FilterNr, TDomainIndexProc_t DP, int *NrElem, void *Uptr)
{ int d_s[]={3,3,3,3,59,4,1}; printAndReturn(gdxGetDomainElements,6,int ) }

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

#if defined(_WIN32)
  h = LoadLibrary (libName);
  if (NULL == h) {
    *errMsg = winErr;
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
#  if defined(HAVE_INTPTR_T)
    s = (void *)(intptr_t)GetProcAddress (h, tripSym);
#  else
    s = (void *)GetProcAddress (h, tripSym);
#  endif
    if (NULL != s) {
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
#if defined(HAVE_INTPTR_T)
#  define LOADIT(TNAME,ENAME) symName = ENAME; TNAME = (TNAME##_t) (intptr_t) loadSym (h, symName, &errMsg); if (NULL == TNAME) goto symMissing
#  define LOADIT_ERR_OK(TNAME,ENAME) symName = ENAME; TNAME = (TNAME##_t) (intptr_t) loadSym (h, symName, &errMsg)
#else
#  define LOADIT(TNAME,ENAME) symName = ENAME; TNAME = (TNAME##_t) loadSym (h, symName, &errMsg); if (NULL == TNAME) goto symMissing
#  define LOADIT_ERR_OK(TNAME,ENAME) symName = ENAME; TNAME = (TNAME##_t) loadSym (h, symName, &errMsg)
#endif

#if ! defined(GMS_DLL_BASENAME)
# define GMS_DLL_BASENAME "gdxdclib"
#endif
#if defined(_WIN32)
# if ! defined(GMS_DLL_PREFIX)
#  define GMS_DLL_PREFIX ""
# endif
# if ! defined(GMS_DLL_EXTENSION)
#  define GMS_DLL_EXTENSION ".dll"
# endif
# if ! defined(GMS_DLL_SUFFIX)
#  if defined(_WIN64)
#   define GMS_DLL_SUFFIX "64"
#  else
#   define GMS_DLL_SUFFIX ""
#  endif
# endif

#else  /* start non-Windows */

# if ! defined(GMS_DLL_PREFIX)
#  define GMS_DLL_PREFIX "lib"
# endif
# if ! defined(GMS_DLL_EXTENSION)
#  if defined(__APPLE__)
#   define GMS_DLL_EXTENSION ".dylib"
#  else
#   define GMS_DLL_EXTENSION ".so"
#  endif
# endif
# if ! defined(GMS_DLL_SUFFIX)
#  if defined(__WORDSIZE)
#   if 64 == __WORDSIZE
#    define GMS_DLL_SUFFIX "64"
#   else
#    define GMS_DLL_SUFFIX ""
#   endif
#  elif defined(__SIZEOF_POINTER__)
#   if 4 == __SIZEOF_POINTER__
#    define GMS_DLL_SUFFIX ""
#   elif 8 == __SIZEOF_POINTER__
#    define GMS_DLL_SUFFIX "64"
#   endif
#  elif defined(__sparcv9)
#   define GMS_DLL_SUFFIX "64"
#  elif defined(__sparc)
/*  check __sparc after __sparcv9, both are defined for 64-bit */
#   define GMS_DLL_SUFFIX ""
#  endif
# endif /* ! defined(GMS_DLL_SUFFIX) */
#endif

/* XLibraryLoad: return 0 on success, ~0 on failure */
static int
XLibraryLoad (const char *dllName, char *errBuf, int errBufSize)
{
  char *errMsg;
  const char *symName;
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
    {f = &d_##f; goto symMissing;} \
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
  {int s[]={3,3,59,4}; CheckAndLoad(gdxDataReadRawFast,3,""); }
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
  {int s[]={23}; CheckAndLoad(gdxGetMemoryUsed,0,""); }
  {int s[]={3,58}; CheckAndLoad(gdxGetSpecialValues,1,""); }
  {int s[]={3,3,12}; CheckAndLoad(gdxGetUEL,2,"C"); }
  {int s[]={3,13,4}; CheckAndLoad(gdxMapValue,2,""); }
  {int s[]={3,11,11,4}; CheckAndLoad(gdxOpenAppend,3,"C"); }
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
  {int s[]={3,3,56}; CheckAndLoad(gdxSymbolGetDomainX,2,"C"); }
  {int s[]={3,3}; CheckAndLoad(gdxSymbolDim,1,""); }
  {int s[]={3,3,12,4,4}; CheckAndLoad(gdxSymbolInfo,4,"C"); }
  {int s[]={3,3,4,4,12}; CheckAndLoad(gdxSymbolInfoX,4,"C"); }
  {int s[]={3,55}; CheckAndLoad(gdxSymbolSetDomain,1,"C"); }
  {int s[]={3,3,55}; CheckAndLoad(gdxSymbolSetDomainX,2,"C"); }
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
  {int s[]={3,3,3,3,59,4,1}; CheckAndLoad(gdxGetDomainElements,6,""); }
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

#include "loadpathutil.h"

static int
libloader(const char *dllPath, const char *dllName, char *msgBuf, int msgBufSize)
{

  char dllNameBuf[512];
  int myrc = 0;
  char gms_dll_suffix[4];

#if ! defined(GMS_DLL_PREFIX)
# error "GMS_DLL_PREFIX expected but not defined"
#endif
#if ! defined(GMS_DLL_BASENAME)
# error "GMS_DLL_BASENAME expected but not defined"
#endif
#if ! defined(GMS_DLL_EXTENSION)
# error "GMS_DLL_EXTENSION expected but not defined"
#endif
#if ! defined(GMS_DLL_SUFFIX)
# if defined (_WIN32)
#   error "GMS_DLL_SUFFIX expected but not defined"
# else
  struct utsname uts;

  myrc = uname(&uts);
  if (myrc) {
    strcpy(msgBuf,"Error, cannot define library name suffix");
    return 0;
  }
  if (0 == strcmp(uts.sysname, "AIX")) /* assume AIX is 64-bit */
    strcpy (gms_dll_suffix, "64");
  else if (0 == strcmp(uts.sysname, "Darwin")) {
    /* keep Darwin test in here: fat binaries must check at run time */
    if (8 == (int)sizeof(void *))
      strcpy (gms_dll_suffix, "64");
    else
      strcpy (gms_dll_suffix, "");
  }
  else {
    strcpy(msgBuf,"Error, cannot define library name suffix");
    return 0;
  }
# endif
#else
  strcpy (gms_dll_suffix, GMS_DLL_SUFFIX);
#endif


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
      strncat(dllNameBuf, GMS_DLL_PREFIX GMS_DLL_BASENAME, sizeof(dllNameBuf)-strlen(dllNameBuf));
      strncat(dllNameBuf, gms_dll_suffix                 , sizeof(dllNameBuf)-strlen(dllNameBuf));
      strncat(dllNameBuf, GMS_DLL_EXTENSION              , sizeof(dllNameBuf)-strlen(dllNameBuf));
    }
    isLoaded = ! XLibraryLoad (dllNameBuf, msgBuf, msgBufSize);
    if (isLoaded) {
      if (NULL != gdxSetLoadPath) {
        if (NULL != dllPath && '\0' != *dllPath) {
          gdxSetLoadPath(dllPath);
        }
        else {
          char myLoadPath[256];
          strcpy (myLoadPath, "/home/sdirkse/hacked");
          loadPathHack (myLoadPath, gdxClose);
          if (strlen(myLoadPath) > 0)
            gdxSetLoadPath (myLoadPath);
        }
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
  int rc;
  lock(libMutex);
  rc = libloader(NULL, NULL, msgBuf, msgBufSize);
  unlock(libMutex);
  return rc;
} /* gdxGetReady */

/* gdxGetReadyD: return false on failure to load library, true on success */
int gdxGetReadyD (const char *dirName, char *msgBuf, int msgBufSize)
{
  int rc;
  lock(libMutex);
  rc = libloader(dirName, NULL, msgBuf, msgBufSize);
  unlock(libMutex);
  return rc;
} /* gdxGetReadyD */

/* gdxGetReadyL: return false on failure to load library, true on success */
int gdxGetReadyL (const char *libName, char *msgBuf, int msgBufSize)
{
  char dirName[1024],fName[1024];
  int rc;
  extractFileDirFileName (libName, dirName, fName);
  lock(libMutex);
  rc = libloader(dirName, fName, msgBuf, msgBufSize);
  unlock(libMutex);
  return rc;
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
  lock(objMutex);
  objectCount++;
  unlock(objMutex);
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
  lock(objMutex);
  objectCount++;
  unlock(objMutex);
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
  lock(objMutex);
  objectCount++;
  unlock(objMutex);
  return 1;                     /* return true on successful library load */
} /* gdxCreateL */

int gdxFree   (gdxHandle_t *pgdx)
{
  assert(XFree);
  XFree(pgdx); pgdx = NULL;
  lock(objMutex);
  objectCount--;
  unlock(objMutex);
  return 1;
} /* gdxFree */

int gdxLibraryLoaded(void)
{
  int rc;
  lock(libMutex);
  rc = isLoaded;
  unlock(libMutex);
  return rc;
} /* gdxLibraryLoaded */

int gdxLibraryUnload(void)
{
  lock(objMutex);
  if (objectCount > 0)
  {
    unlock(objMutex);
    return 0;
  }
  unlock(objMutex);
  lock(libMutex);
  isLoaded = 0;
  (void) unLoadLib(h);
  unlock(libMutex);
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
  lock(exceptMutex);
  ErrorCallBack = func;
  unlock(exceptMutex);
}

int gdxGetAPIErrorCount(void)
{
  return APIErrorCount;
}

void gdxSetAPIErrorCount(int ecnt)
{
  APIErrorCount = ecnt;
}

#if 0
void gdxErrorHandling(const char *msg)
{
  APIErrorCount++;
  if (ScreenIndicator) { printf("%s\n", msg); fflush(stdout); }
  lock(exceptMutex);
  if (ErrorCallBack)
    if (ErrorCallBack(APIErrorCount, msg)) { unlock(exceptMutex); exit(123); }
  unlock(exceptMutex);
  assert(!ExceptionIndicator);
  if (ExitIndicator) exit(123);
}
#endif
