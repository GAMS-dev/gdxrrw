#include <R.h>
#include <Rinternals.h>

#include "globals2.h"

void
exit2R (const char *msg)
{
  error(msg);
  return;
} // exit2R
