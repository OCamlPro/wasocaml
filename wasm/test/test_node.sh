#!/bin/sh

set -eu

alias time='/usr/bin/time -f"real %e user %U sys %S"'

ULIMIT_STACK_SIZE=20000
STACK_SIZE=10000
NODE="node-canary --stack-size=${STACK_SIZE}"

ulimit -s $ULIMIT_STACK_SIZE

bench() {
  echo "*** Running ${1}"
  echo -n "Wasocaml (node):            "
  ../../ocamlopt -O3 ./${2}.ml > /dev/null
  time $NODE ./main_node.mjs > /dev/null
  wasm-opt --enable-gc --enable-reference-types --enable-multivalue --enable-tail-call a.out.wasm -o a.out.wasm -O3
  echo -n "Wasocaml + wasm-opt (node): "
  time $NODE ./main_node.mjs > /dev/null
  echo -n "OCaml native:               "
  ocamlopt -O3 ./${2}.ml > /dev/null
  time ./a.out > /dev/null
  echo -n "OCaml bytecode:             "
  ocamlc ./${2}.ml > /dev/null
  time ocamlrun ./a.out > /dev/null
  echo -n "js_of_ocaml (node):         "
  js_of_ocaml compile --target-env=nodejs --opt=3 ./a.out
  time $NODE ./a.js > /dev/null
  echo -n "wasm_of_ocaml (node):       "
  rm a.js a.wat || true 2> /dev/null
  wasm_of_ocaml compile --opt=3 ./a.out > /dev/null
  time $NODE ./a.js > /dev/null
  rm -rf a.assets*
}

bench "Knuth-Bendix" "kb"
bench "Knuth-Bendix (no exception)" "kb_no_exc"
bench "Soli" "soli"
bench "Fibonacci" "fib"
#bench "Almabench" "almabench" # global init must have correct type
bench "Binary Decision Diagram" "bdd"
#bench "Binary Trees" "binary_trees" # unreachable
#bench "Boyer" "boyer" # unreachable
#bench "Boyer no exceptions" "boyer_no_exc" # unreachable
#bench "Pfannkuchen" "fannkuch" # unreachable
#bench "Pfannkuchen 2" "fannkuch2" # unreachable
#bench "Fast Fourier Transform" "fft"
#bench "Hamming" "hamming"
bench "Loop" "loop"
#bench "Nucleic" "nucleic" # unreachable
#bench "Quicksort" "quicksort"
#bench "Ray-Trace" "raytrace"
#bench "Splay Tree" "splay"
bench "Takc" "takc"
bench "Taku" "taku"
