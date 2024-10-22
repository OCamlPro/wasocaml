#!/bin/sh

set -eu

ULIMIT_STACK_SIZE=20000
STACK_SIZE=10000
NODE="node-canary --stack-size=${STACK_SIZE}"
alias time='/usr/bin/time -f"real %e user %U sys %S"'

ulimit -s $ULIMIT_STACK_SIZE

#UNSAFE="-unsafe" # TODO: this requires the prim mod primitives
UNSAFE=""

bench_native() {
  echo -n "    OCaml native:               "
  ocamlopt $UNSAFE -O3 ./${2}.ml > /dev/null
  time ./a.out > output_${2}_ocaml_native.txt
}

bench_wasocaml_opt() {
  echo -n "    Wasocaml + wasm-opt (node): "
  ../../ocamlopt $UNSAFE -O3 ./${2}.ml > /dev/null
  cat a.out.wat | grep -v "^ (export" > a.out.noexport.wat
  WASMOPT="wasm-opt --enable-gc --enable-reference-types --enable-multivalue --enable-tail-call --enable-nontrapping-float-to-int --traps-never-happen -O3 --abstract-type-refining --cfp --coalesce-locals --closed-world --type-ssa --gufa-optimizing --type-merging --strip-debug --strip-dwarf --strip-producers --strip-target-features --type-refining --unsubtyping --vacuum --tuple-optimization --ssa --simplify-locals --simplify-globals-optimizing --signature-refining --signature-pruning --roundtrip --reorder-locals --reorder-globals --reorder-functions --remove-unused-types --remove-unused-names --remove-unused-module-elements --remove-unused-brs --remove-memory --precompute --optimize-instructions --optimize-casts --once-reduction --monomorphize --minimize-rec-groups --merge-similar-functions --merge-locals --merge-blocks --local-subtyping --local-cse --licm --intrinsic-lowering --inlining-optimizing --heap-store-optimization --gto --gsi --global-refining --duplicate-import-elimination --duplicate-function-elimination --directize --dce --dae-optimizing"
  # TODO: try --flatten ; --rereloop ; --precompute-propagate ; --dfo
  $WASMOPT -o a.out.wasm a.out.noexport.wat
  $WASMOPT -S -o a.out.wat a.out.wasm
  time $NODE ./main_node.mjs > output_${2}_wasocaml_opt.txt
  diff output_${2}_ocaml_native.txt output_${2}_wasocaml_opt.txt
}

bench_wasocaml() {
  echo -n "    Wasocaml (node):            "
  ../../ocamlopt $UNSAFE -O3 ./${2}.ml > /dev/null
  time $NODE ./main_node.mjs > output_${2}_wasocaml.txt
  diff output_${2}_ocaml_native.txt output_${2}_wasocaml.txt
}

bench_wsoo() {
  echo -n "    wasm_of_ocaml (node):       "
  ocamlc $UNSAFE ./${2}.ml > /dev/null
  rm -rf a.assets* a.js a.wat || true 2> /dev/null
  wasm_of_ocaml compile --opt=3 ./a.out > /dev/null
  time $NODE ./a.js > output_${2}_wsoo.txt
  diff output_${2}_ocaml_native.txt output_${2}_wsoo.txt
}

bench_jsoo() {
  echo -n "    js_of_ocaml (node):         "
  ocamlc $UNSAFE ./${2}.ml > /dev/null
  rm -f a.js a.wat || true 2> /dev/null
  js_of_ocaml compile --target-env=nodejs --opt=3 ./a.out
  time $NODE ./a.js > output_${2}_jsoo.txt
  diff output_${2}_ocaml_native.txt output_${2}_jsoo.txt
}

bench_bytecode() {
  echo -n "    OCaml bytecode:             "
  ocamlc $UNSAFE ./${2}.ml > /dev/null
  time ocamlrun ./a.out > output_${2}_bytecode.txt
  diff output_${2}_ocaml_native.txt output_${2}_bytecode.txt
}

bench() {
  echo "*** Running ${1}:"

  echo ""

  bench_native "${1}" "${2}"
  bench_wasocaml_opt "${1}" "${2}"
  #bench_wasocaml "${1}" "${2}"
  bench_wsoo "${1}" "${2}"
  bench_jsoo "${1}" "${2}"
  bench_bytecode "${1}" "${2}"

  echo ""
}

#bench "Almabench" "almabench" # global init must have correct type
bench "Binary Trees" "binary_trees" # unreachable
#bench "Boyer" "boyer" # unreachable
#bench "Boyer no exceptions" "boyer_no_exc" # unreachable
bench "Pfannkuchen" "fannkuch" # unreachable
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
