/* globals.h
 * Global #defines, typedefs, and (gasp!) data for gdxrrw
 * $Id$
 */

#if ! defined(_GDXRRW_GLOBALS_H_)
#define       _GDXRRW_GLOBALS_H_

#if defined(_WIN32)
# include <windows.h>
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

typedef char shortStringBuf_t[GMS_SSSIZE];
typedef void (GDX_CALLCONV *gdxGetLoadPath_t) (char *s);
typedef enum dType {
  set = GMS_DT_SET,
  parameter = GMS_DT_PAR
} dType_t;
typedef enum dForm {
  unKnown=0,
  full,
  sparse
} dForm_t;
typedef enum dField {
  level = GMS_VAL_LEVEL,
  marginal = GMS_VAL_MARGINAL,
  lower = GMS_VAL_LOWER,
  upper = GMS_VAL_UPPER,
  scale = GMS_VAL_SCALE,
  max = GMS_VAL_MAX
} dField_t;
typedef struct rgdxStruct {
  char name[1024];
  dForm_t dForm;
  dField_t dField;
  int withField;
  int compress;
  int ts;
  int te;
  int withUel;
  SEXP filterUel;
} rgdxStruct_t;
typedef struct wgdxStruct {
  char name[1024];
  dForm_t dForm;
  dType_t dType;
  int withVal;
  int withTs;
  int withUel;
  int dim;
} wgdxStruct_t;
typedef unsigned long long int uint64_t;
typedef union d64 {
  double x;
  uint64_t u64;
} d64_t;


GDX_FUNCPTR(gdxGetLoadPath);


#endif /* ! defined(_GDXRRW_GLOBALS_H_) */
