As an experiment, I packaged up a representative subset of GDXRRW into
a new package using static GDX, i.e. making the P3-generated C++
sources part of the src code of the R package and eliminating the
dependence on an external shared library.  The idea here is to make a
package that is easy to install in the usual R way - via CRAN.  I
didn't automate the process, but here's a list of relevant things that
need doing if we choose to make this public.

1. [ONGOING] The P3-generated source contains several things that CRAN
will not accept: references to exit(), abort(), stdout, stderr, and
perhaps a few other things need to be removed.  There are not many
such references.  This can be done by hand or by triggering a #define
if we edit the sources like p3io.[ch], etc.

1a. [ONGOING] Wrapped P3 objects have C++ src with symbols like
xcreate and xfree.  xfree is use by R already, so we have to rename
this to something else like x2free.

1b. [ONGOING] We don't use p3threads at all, so I gutted the
pthreads.[hc] files in the R src.  Can we selectively take it out of
p3compat.pas?

2. [ONGOING??] I hand-edited gdxcc.[ch] to get something that fits
with using P3-generated C++ source.  This could be another output from
apiwrap, or we can just live with hand-editing.  Each has pros and
cons.

3. [ALREADY DONE] Adjust p3 sources to use compiler-defined
preprocessor symbols instead of what we define in makefile.p3.  This
means we don't have to do all the configuration magic within the R
package build system, which is unwieldy.

4. [DO ONCE] The R package system contains hooks for library loading
and unloading.  We have to use these to call the unit initialization
and finalization sections for P3-generated units.

5. [DO ONCE] Much of the documentation and examples for GDXRRW is
about getting started, i.e. finding/loading the shared GDX library.
We'll have to clean that out if we stop using the shared GDX library.

6. [DO ONCE] What about the gams() call?  It's connected with the
location of the shared library used: this needs to be re-thought if
there is not shared library.
