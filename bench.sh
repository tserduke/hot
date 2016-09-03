#!/bin/sh

rm -rf bench/data
mkdir bench/data

stack build hot:bench:native   --verbosity warn --bench --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1.csv"
stack build hot:bench:llvm-3.7 --verbosity warn --bench --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1-llvm.csv"

cd bench/compare
stack build
stack exec compare-bench
