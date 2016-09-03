#!/bin/sh

rm -rf data
mkdir data

STACK_YAML=stack/7.10.3.yaml        stack build hot:bench:micro-hot --verbosity warn --bench --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3.csv"
STACK_YAML=stack/7.10.3-inline.yaml stack build hot:bench:micro-hot --verbosity warn --bench --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3-inline.csv"
STACK_YAML=stack/7.10.3.yaml        stack build hot:bench:micro-hot --verbosity warn --bench --ghc-options "-fllvm -pgmlo opt-3.5 -pgmlc llc-3.5" --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3-llvm.csv"
STACK_YAML=stack/7.10.3-inline.yaml stack build hot:bench:micro-hot --verbosity warn --bench --ghc-options "-fllvm -pgmlo opt-3.5 -pgmlc llc-3.5" --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3-llvm-inline.csv"
STACK_YAML=stack/8.0.1.yaml         stack build hot:bench:micro-hot --verbosity warn --bench --ghc-options "-no-warn-redundant-constraints" --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1.csv"
STACK_YAML=stack/8.0.1-inline.yaml  stack build hot:bench:micro-hot --verbosity warn --bench --ghc-options "-no-warn-redundant-constraints" --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1-inline.csv"
STACK_YAML=stack/8.0.1.yaml         stack build hot:bench:micro-hot --verbosity warn --bench --ghc-options "-fllvm -pgmlo opt-3.7 -pgmlc llc-3.7 -no-warn-redundant-constraints" --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1-llvm.csv"
STACK_YAML=stack/8.0.1-inline.yaml  stack build hot:bench:micro-hot --verbosity warn --bench --ghc-options "-fllvm -pgmlo opt-3.7 -pgmlc llc-3.7 -no-warn-redundant-constraints" --benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1-llvm-inline.csv"

cd compare
stack build
stack exec compare-bench
