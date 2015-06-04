#if defined(__GNUC__)
# define _GNU_SOURCE
/* _GNU_SOURCE assures that dladdr() is available */
#endif

#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#if defined(__linux__) || defined(__sparc) || defined(__sun) || defined(__sun__) || defined(__APPLE__)
# include <dlfcn.h>
#endif

#include "loadpathutil.h"

#if defined(__linux__) || defined(__sparc) || defined(__sun) || defined(__sun__) || defined(__APPLE__)
void loadPathHack (char buf[256], void *addr)
{
  const char *p;
  Dl_info dlInfo;
  size_t sz;
  int rc;
  char libBuf[PATH_MAX];

  buf[0] = '\0';
  rc = dladdr (addr, &dlInfo);
  if (rc && ('\0' != *dlInfo.dli_fname)) {
    if (realpath(dlInfo.dli_fname, libBuf)) { /* success */
      sz = strlen(libBuf);
      p = libBuf + sz;
      while (('/' != *p) && (p > libBuf)) {
        p--;
      }
      if (p > libBuf)           /* must have stopped on the rightmost / */
        p--;
      sz = p - libBuf + 1;
      if (sz <= 255) {
        memcpy (buf, libBuf, sz);
        buf[sz] = '\0';
      }
    }
  }
  return;
} /* loadPathHack */

#else

/* The code to do this for AIX is UGLY!!!
 * Probably nobody notices or cares if I leave it out.
 */

void loadPathHack (char buf[256], void *addr)
{
  buf[0] = '\0';

  return;
} /* loadPathHack */

#endif
