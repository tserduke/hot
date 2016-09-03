#!/bin/sh

rm -rf data
mkdir data


STACK_YAML=stack/7.10.3.yaml stack build hot:bench:micro-hot  --verbosity warn --bench \
	--benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3.csv"

STACK_YAML=stack/7.10.3.yaml stack build hot:bench:micro-hot  --verbosity warn --bench \
	--benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3-inline.csv" --flag hot:inline

STACK_YAML=stack/7.10.3.yaml stack build hot:bench:micro-hot --verbosity warn --bench \
	--benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3-llvm.csv" \
	--ghc-options "-fllvm -pgmlo opt-3.5 -pgmlc llc-3.5"

STACK_YAML=stack/7.10.3.yaml stack build hot:bench:micro-hot  --verbosity warn --bench \
	--benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3-llvm-inline.csv" --flag hot:inline \
	--ghc-options "-fllvm -pgmlo opt-3.5 -pgmlc llc-3.5"


STACK_YAML=stack/7.10.3.yaml stack build hot:bench:micro-list --verbosity warn --bench \
	--benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3-list.csv"

STACK_YAML=stack/7.10.3.yaml stack build hot:bench:micro-list  --verbosity warn --bench \
	--benchmark-arguments "-v 0 -L 0.2 --csv bench/data/7.10.3-llvm-list.csv" \
	--ghc-options "-fllvm -pgmlo opt-3.5 -pgmlc llc-3.5"


STACK_YAML=stack/8.0.1.yaml  stack build hot:bench:micro-hot  --verbosity warn --bench \
	--benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1.csv" \
	--ghc-options "-fno-warn-redundant-constraints"

STACK_YAML=stack/8.0.1.yaml  stack build hot:bench:micro-hot  --verbosity warn --bench \
	--benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1-inline.csv" --flag hot:inline \
	--ghc-options "-fno-warn-redundant-constraints"

STACK_YAML=stack/8.0.1.yaml  stack build hot:bench:micro-hot  --verbosity warn --bench \
	--benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1-llvm.csv" \
	--ghc-options "-fllvm -pgmlo opt-3.7 -pgmlc llc-3.7 -fno-warn-redundant-constraints"

STACK_YAML=stack/8.0.1.yaml  stack build hot:bench:micro-hot  --verbosity warn --bench \
	--benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1-llvm-inline.csv" --flag hot:inline \
	--ghc-options "-fllvm -pgmlo opt-3.7 -pgmlc llc-3.7 -fno-warn-redundant-constraints"


STACK_YAML=stack/8.0.1.yaml  stack build hot:bench:micro-list --verbosity warn --bench \
	--benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1-list.csv" \
	--ghc-options "-fno-warn-redundant-constraints"

STACK_YAML=stack/8.0.1.yaml  stack build hot:bench:micro-list --verbosity warn --bench \
	--benchmark-arguments "-v 0 -L 0.2 --csv bench/data/8.0.1-llvm-list.csv" \
	--ghc-options "-fllvm -pgmlo opt-3.7 -pgmlc llc-3.7 -fno-warn-redundant-constraints"


cd compare
stack build
stack exec compare-bench
