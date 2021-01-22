#!/usr/bin/env bash

rm -r MLB run
SML_LIB=../../../mlkit ../../../mlkit/bin/mlkit --no_gc $1
#mlkit --no_delete_target_files --no_gc --no_basislib -Pcee -Pole --comments_in_asmcode -g --disable_atbot_analysis $1
