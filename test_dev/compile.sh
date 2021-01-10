#!/usr/bin/env bash

rm -r MLB run
../../mlkit/bin/mlkit --no_delete_target_files --no_basislib --no_gc -Pcee -Pole --comments_in_asmcode $1
