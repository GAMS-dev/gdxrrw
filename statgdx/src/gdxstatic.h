#if ! defined(_GDXSTATIC_H_)
#     define  _GDXSTATIC_H_

#include "gclgms.h"

struct gdxRec;
typedef struct gdxRec *gdxHandle_t;

#define gdxAcronymCount gdxacronymcount
#define gdxAcronymGetInfo gdxacronymgetinfo
#define gdxAcronymName gdxacronymname
#define gdxClose gdxclose
#define gdxDataReadRaw gdxdatareadraw
#define gdxDataReadRawStart gdxdatareadrawstart
#define gdxGetDLLVersion gdxgetdllversion
#define gdxErrorStr gdxerrorstr
#define gdxFileVersion gdxfileversion
#define gdxGetElemText gdxgetelemtext
#define gdxGetLastError gdxgetlasterror
#define gdxMapValue gdxmapvalue
#define gdxOpenRead gdxopenread
#define gdxSymbIndxMaxLength gdxsymbindxmaxlength
#define gdxSymbMaxLength gdxsymbmaxlength
#define gdxSymbolGetComment gdxsymbolgetcomment
#define gdxSymbolGetDomain gdxsymbolgetdomain
#define gdxSymbolGetDomainX gdxsymbolgetdomainx
#define gdxSymbolDim gdxsymboldim
#define gdxSymbolInfo gdxsymbolinfo
#define gdxSymbolInfoX gdxsymbolinfox
#define gdxSystemInfo gdxsysteminfo
#define gdxUMUelGet gdxumuelget

#if defined(__cplusplus)
extern "C" {
#endif

/* headers for "wrapper" routines implemented in C */
int gdxGetReady  (char *msgBuf, int msgBufLen);
int gdxCreate    (gdxHandle_t *pgdx, char *msgBuf, int msgBufLen);
int gdxFree      (gdxHandle_t *pgdx);

int gdxAcronymCount (gdxHandle_t pgdx);
int gdxAcronymGetInfo (gdxHandle_t pgdx, int N, char *AName, char *Txt, int *AIndx);
int gdxAcronymName (gdxHandle_t pgdx, double V, char *AName);
int gdxClose (gdxHandle_t pgdx);
int gdxDataReadRaw (gdxHandle_t pgdx, int KeyInt[], double Values[], int *DimFrst);
int gdxDataReadRawStart (gdxHandle_t pgdx, int SyNr, int *NrRecs);
int gdxGetDLLVersion (gdxHandle_t pgdx, char *V);
int gdxErrorStr (gdxHandle_t pgdx, int ErrNr, char *ErrMsg);
int gdxFileVersion (gdxHandle_t pgdx, char *FileStr, char *ProduceStr);
int gdxGetElemText (gdxHandle_t pgdx, int TxtNr, char *Txt, int *Node);
int gdxGetLastError (gdxHandle_t pgdx);
int gdxMapValue (gdxHandle_t pgdx, double D, int *sv);
int gdxOpenRead (gdxHandle_t pgdx, const char *FileName, int *ErrNr);
int gdxSymbIndxMaxLength (gdxHandle_t pgdx, int SyNr, int LengthInfo[]);
int gdxSymbMaxLength (gdxHandle_t pgdx);
int gdxSymbolGetComment (gdxHandle_t pgdx, int SyNr, int N, char *Txt);
int gdxSymbolGetDomain (gdxHandle_t pgdx, int SyNr, int DomainSyNrs[]);
int gdxSymbolGetDomainX (gdxHandle_t pgdx, int SyNr, char *DomainIDs[]);
int gdxSymbolDim (gdxHandle_t pgdx, int SyNr);
int gdxSymbolInfo (gdxHandle_t pgdx, int SyNr, char *SyId, int *Dimen, int *Typ);
int gdxSymbolInfoX (gdxHandle_t pgdx, int SyNr, int *RecCnt, int *UserInfo, char *ExplTxt);
int gdxSystemInfo (gdxHandle_t pgdx, int *SyCnt, int *UelCnt);
int gdxUMUelGet (gdxHandle_t pgdx, int UelNr, char *Uel, int *UelMap);

#if defined(__cplusplus)
}
#endif

#endif /* #if ! defined(_GDXSTATIC_H_) */
