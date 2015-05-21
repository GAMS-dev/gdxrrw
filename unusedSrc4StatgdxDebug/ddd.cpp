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

Extern_C _P3_DllExport Procedure  STDCALL xxxfree(SYSTEM_pointer *pgdx)
{
  SYSTEM_tobject_DOT_free(ValueCast(SYSTEM_tobject,*pgdx));
  *pgdx = NULL;
}  /* xxxfree */
