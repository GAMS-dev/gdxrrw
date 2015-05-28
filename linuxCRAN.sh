#!/bin/bash

cmd="R CMD build gdxrrw"
eval $cmd || { rc=$? ; echo "failed cmd: $cmd" ; echo "rc=$rc" ; exit ; }
