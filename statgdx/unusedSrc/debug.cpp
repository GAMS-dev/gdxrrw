#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>

#include "p3io.h"
#include "system_p3.h"
#include "p3utils.h"
#include "gmsspecs.h"
#include "gmsgen.h"
#include "gxdefs.h"
#include "gmsobj.h"
#include "gmsdata.h"
#include "clibtypes.h"
#include "xcompress.h"
#include "gmsstrm.h"
#include "strhash.h"
#include "gmsheapnew.h"
#include "datastorage.h"
#include "gxfile.h"

#include "ddd.h"

#include "debug.h"

Extern_C _P3_DllExport Procedure  STDCALL xxfree(SYSTEM_pointer *pgdx)
{
  SYSTEM_tobject_DOT_free(ValueCast(SYSTEM_tobject,*pgdx));
  *pgdx = NULL;
}  /* xxfree */

struct gdxRec;
typedef struct gdxRec *gdxHandle_t;

#define XCreate xcreate
#define XFree   x2free

extern "C" {
void XCreate (SYSTEM_pointer *pgdx);
void XFree   (SYSTEM_pointer *pgdx);
void jjfree  (SYSTEM_pointer *pgdx);
void kkfree   (SYSTEM_pointer *pgdx);
}

int doDebug (void)
{
  gdxHandle_t h;

  Rprintf ("doDebug: 000\n");
  h = NULL;
#if 0
  XCreate(&h);
#else
  {
    SYSTEM_shortstring msg;
    SYSTEM_pointer v;
    GXFILE_tgxfileobj fileobj;
    SYSTEM_tobject o;

#if 0
    v = ValueCast(GXFILE_tgxfileobj,GXFILE_tgxfileobj_DOT_create(ValueCast(
          GXFILE_tgxfileobj,_P3alloc_object(&GXFILE_tgxfileobj_CD)),msg));
#else
    fileobj = (GXFILE_tgxfileobj) _P3alloc_object(&GXFILE_tgxfileobj_CD);
    GXFILE_tgxfileobj_DOT_create (fileobj,msg);
    v = fileobj;
    o = (SYSTEM_tobject) fileobj;
    Rprintf ("fileobj = %p\n", fileobj);
#endif
    h = (gdxHandle_t) v;

    Rprintf ("doDebug: 100\n");
    Rprintf ("o = %p\n", o);
    Rprintf ("h = %p\n", h);
    // SYSTEM_tobject_DOT_free(o);
    // xxxfree((SYSTEM_pointer *) &h);
    // xxfree((SYSTEM_pointer *)&h);
    x2free((SYSTEM_pointer *)&h);
// kkfree((SYSTEM_pointer *)&h);
    // jjfree((SYSTEM_pointer *)&h);
  }
#endif
  Rprintf ("doDebug: 200\n");
// XFree(&h);
  Rprintf ("doDebug: 999\n");

  return 1;
} /* doDebug */
