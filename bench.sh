#!/bin/sh

rm -rf bench/data
mkdir bench/data

STACK_YAML=bench/stack/7.10.3.yaml        stack build hot:bench:native   --verbosity warn --bench --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3.csv"
STACK_YAML=bench/stack/7.10.3-inline.yaml stack build hot:bench:native   --verbosity warn --bench --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3-inline.csv"
STACK_YAML=bench/stack/7.10.3.yaml        stack build hot:bench:llvm-3.5 --verbosity warn --bench --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3-llvm.csv"
STACK_YAML=bench/stack/7.10.3-inline.yaml stack build hot:bench:llvm-3.5 --verbosity warn --bench --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3-llvm-inline.csv"
STACK_YAML=bench/stack/8.0.1.yaml         stack build hot:bench:native   --verbosity warn --bench --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1.csv"
STACK_YAML=bench/stack/8.0.1-inline.yaml  stack build hot:bench:native   --verbosity warn --bench --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1-inline.csv"
STACK_YAML=bench/stack/8.0.1.yaml         stack build hot:bench:llvm-3.7 --verbosity warn --bench --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1-llvm.csv"
STACK_YAML=bench/stack/8.0.1-inline.yaml  stack build hot:bench:llvm-3.7 --verbosity warn --bench --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1-llvm-inline.csv"

cd bench/compare
stack build
stack exec compare-bench
