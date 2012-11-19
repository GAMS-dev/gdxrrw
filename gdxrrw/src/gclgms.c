/* global constants (symbol dimensions, symbol layout, etc.)
 * that might ultimately come from global
 * created: Steve Dirkse, July 2007
 */

#include "gclgms.h"

#if ! defined(_GCL_RHACK_)
# error "this file modified to work with R extensions.  Do not use outside of R"
#endif

const char *gmsGdxTypeText[GMS_DT_MAX] =
  {"Set","Parameter","Variable","Equation","Alias"};
const char *gmsVarTypeText[GMS_VARTYPE_MAX] = {"unknown","binary","integer","positive","negative","free","sos1","sos2","semicont","semiint"};
const char *gmsValTypeText[GMS_VAL_MAX] = {".l",".m",".lo",".up",".scale"};
const char *gmsSVText[GMS_SVIDX_MAX] = {"UNdef","NA","+Inf","-Inf","Eps","0","AcroN"};

const double gmsDefRecVar[GMS_VARTYPE_MAX][GMS_VAL_MAX] = {
 /* .l   .m           .lo         .ub  .scale */
  { 0.0, 0.0,         0.0,         0.0, 1.0},    /* unknown */
  { 0.0, 0.0,         0.0,         1.0, 1.0},    /* binary */
  { 0.0, 0.0,         0.0,       100.0, 1.0},    /* integer */
  { 0.0, 0.0,         0.0, GMS_SV_PINF, 1.0},    /* positive */
  { 0.0, 0.0, GMS_SV_MINF,         0.0, 1.0},    /* negative */
  { 0.0, 0.0, GMS_SV_MINF, GMS_SV_PINF, 1.0},    /* free */
  { 0.0, 0.0,         0.0, GMS_SV_PINF, 1.0},    /* sos1 */
  { 0.0, 0.0,         0.0, GMS_SV_PINF, 1.0},    /* sos2 */
  { 0.0, 0.0,         1.0, GMS_SV_PINF, 1.0},    /* semicont */
  { 0.0, 0.0,         1.0,       100.0, 1.0}     /* semiint */
};

const double gmsDefRecEqu[GMS_EQUTYPE_MAX][GMS_VAL_MAX] = {
 /* .l   .m           .lo         .ub  .scale */
  { 0.0, 0.0,         0.0,         0.0, 1.0},    /* =e= */
  { 0.0, 0.0,         0.0, GMS_SV_PINF, 1.0},    /* =g= */
  { 0.0, 0.0, GMS_SV_MINF,         0.0, 1.0},    /* =l= */
  { 0.0, 0.0, GMS_SV_MINF, GMS_SV_PINF, 1.0},    /* =n= */
  { 0.0, 0.0,         0.0,         0.0, 1.0},    /* =x= */
  { 0.0, 0.0,         0.0, GMS_SV_PINF, 1.0}     /* =c= */
};

/* extract an equation type in [GMS_EQUTYPE_E,GMS_VAL_MAX) 
 * from the userInfo value stored for an equation symbol
 */
int
gmsFixEquType (int userInfo)
{
  if ((userInfo >= GMS_EQU_USERINFO_BASE) &&
      (userInfo < GMS_EQU_USERINFO_BASE + GMS_EQUTYPE_MAX))
    /* proper value for equtype with internal base */
    return userInfo - GMS_EQU_USERINFO_BASE;
  /* assume 0-bases, coerce if not */
  if (userInfo < 0)
    userInfo = 0;
  if (userInfo >= GMS_EQUTYPE_MAX)
    userInfo = GMS_EQUTYPE_MAX - 1;
  return userInfo;
} /* gmsFixEquType */
