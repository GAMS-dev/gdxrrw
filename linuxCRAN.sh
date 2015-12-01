#!/bin/bash

xtra=`svn st gdxrrw --no-ignore | wc -l`
if [ "0" != "$xtra" ] ; then
    echo "unversioned files found!!"
    svn st gdxrrw --no-ignore
    exit 1
fi

cmd="R CMD build gdxrrw"
eval $cmd || { rc=$? ; echo "failed cmd: $cmd" ; echo "rc=$rc" ; exit 1 ; }

src=`ls -1 gdxrrw_?.?.?.tar.gz | tail -1`

echo "Just built $src"

cmd="R CMD check $src --as-cran"
eval $cmd || { rc=$? ; echo "failed cmd: $cmd" ; echo "rc=$rc" ; exit 1 ; }
