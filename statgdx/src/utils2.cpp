#include <R.h>
#include <Rinternals.h>

#include "globals2.h"

void
exit2R (const char *msg)
{
  error(msg);
  return;
} // exit2R

void
msg2R (const char *msg)
{
  Rprintf ("%s\n", msg);
  return;
}
