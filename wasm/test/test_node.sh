#!/bin/sh

set -eu

alias time='/usr/bin/time -f"real %e user %U sys %S"'

bench() {
  echo "*** Running ${1}"
  echo -n "Wasocaml (node):    "
  ../../ocamlopt -O2 ./${2}.ml > /dev/null
  time node-canary ./main_node.mjs > /dev/null
  echo -n "OCaml native:       "
  ocamlopt -O2 ./${2}.ml > /dev/null
  time ./a.out > /dev/null
  echo -n "OCaml bytecode:     "
  ocamlc ./${2}.ml > /dev/null
  time ocamlrun ./a.out > /dev/null
  echo -n "js_of_ocaml (node): "
  js_of_ocaml compile --target-env=nodejs --opt=3 ./a.out
  time node-canary ./a.js > /dev/null
}

bench "Knuth-Bendix" "kb"
#bench "Soli" "soli"
bench "Fibonacci" "fib"
#bench "Almabench" "almabench"
bench "Binary Decision Diagram" "bdd"
#bench "Binary Trees" "binary_trees"
#bench "Boyer" "boyer"
#bench "Boyer no exceptions" "boyer_no_exc"
#bench "Pfannkuchen" "fannkuch"
#bench "Fast Fourier Transform" "fft"
#bench "Hamming" "hamming"
bench "Loop" "loop"
#bench "Nucleic" "nucleic"
#bench "Quicksort" "quicksort"
#bench "Ray-Trace" "raytrace"
#bench "Splay Tree" "splay"
bench "Takc" "takc"
bench "Taku" "taku"
