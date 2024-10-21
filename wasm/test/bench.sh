#!/bin/sh

set -eu

NODE="node-canary --stack-size=10000"

ulimit -s 20000

#UNSAFE="-unsafe"
UNSAFE=""

bench() {
  echo "*** Running ${1}:"

  echo ""

  # clean-up
  rm -rf *.assets *.wasm *.wat *.bc *.js || true 2> /dev/null
  # compile native and wasocaml
  ../../ocamlopt $UNSAFE -O3 ./${2}.ml > /dev/null
  # optimize wasocaml code
  wasm-opt --enable-gc --enable-reference-types --enable-multivalue --enable-tail-call --enable-nontrapping-float-to-int --traps-never-happen --skip-pass=inlining-optimizing a.out.wasm -o a.out.wasm -O3
  # compile bytecode
  ocamlc $UNSAFE ./${2}.ml -o a.out.bc > /dev/null
  # compile wsoo
  wasm_of_ocaml compile --opt=3 ./a.out.bc -o a.wsoo.bc.js > /dev/null
  # compile jsoo
  js_of_ocaml compile --target-env=nodejs --opt=3 ./a.out.bc -o a.jsoo.bc.js

  # bench
  hyperfine --runs 3 \
    -n "OCaml native" "./a.out" \
    -n "Wasocaml" "${NODE} ./main_node.mjs" \
    -n "Wsoo" "${NODE} ./a.wsoo.bc.js" \
    -n "Jsoo" "${NODE} ./a.jsoo.bc.js" \
    -n "Bytecode" "ocamlrun ./a.out.bc" # TODO: use ../../ocamlrun when it'll be produced

  echo ""
}


#bench "Almabench" "almabench" # global init must have correct type
#bench "Binary Trees" "binary_trees" # unreachable
#bench "Boyer" "boyer" # unreachable
#bench "Boyer no exceptions" "boyer_no_exc" # unreachable
#bench "Pfannkuchen" "fannkuch" # unreachable
#bench "Pfannkuchen 2" "fannkuch2" # missing "caml_string_notequal" and "caml_lessthan"
#bench "Fast Fourier Transform" "fft" # unreachable
#bench "Hamming" "hamming" # missing value let-rec
#bench "Nucleic" "nucleic" # unreachable
#bench "Ray-Trace" "raytrace" # global init must have correct type
#bench "Splay Tree" "splay" # unreachable

bench "Knuth-Bendix" "kb"
bench "Knuth-Bendix (no exception)" "kb_no_exc"
bench "Soli" "soli"
bench "Fibonacci" "fib"
bench "Binary Decision Diagram" "bdd"
bench "Loop" "loop"
bench "Takc" "takc"
bench "Taku" "taku"
bench "Quicksort" "quicksort"
