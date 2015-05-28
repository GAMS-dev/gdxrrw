#!/bin/bash

cmd="R CMD build gdxrrw"
eval $cmd || { rc=$? ; echo "failed cmd: $cmd" ; echo "rc=$rc" ; exit ; }

src=`ls -1 gdxrrw_?.?.?.tar.gz | tail -1`

echo "Just built $src"

cmd="R CMD check $src --as-cran"
eval $cmd || { rc=$? ; echo "failed cmd: $cmd" ; echo "rc=$rc" ; exit ; }
