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
  cat a.out.wat | grep -v "^ (export" > a.out.noexport.wat
  WASMOPT="wasm-opt --enable-gc --enable-reference-types --enable-multivalue --enable-tail-call --enable-nontrapping-float-to-int --traps-never-happen -O3 --abstract-type-refining --cfp --coalesce-locals --closed-world --type-ssa --gufa-optimizing --type-merging --strip-debug --strip-dwarf --strip-producers --strip-target-features --type-refining --unsubtyping --vacuum --tuple-optimization --ssa --simplify-locals --simplify-globals-optimizing --signature-refining --signature-pruning --roundtrip --reorder-locals --reorder-globals --reorder-functions --remove-unused-types --remove-unused-names --remove-unused-module-elements --remove-unused-brs --remove-memory --precompute --optimize-instructions --optimize-casts --once-reduction --monomorphize --minimize-rec-groups --merge-similar-functions --merge-locals --merge-blocks --local-subtyping --local-cse --licm --intrinsic-lowering --inlining-optimizing --heap-store-optimization --gto --gsi --global-refining --duplicate-import-elimination --duplicate-function-elimination --directize --dce --dae-optimizing -O3 -O3 --gufa -O3 -O3 --gufa -O3 -O3 --converge --abstract-type-refining --cfp --coalesce-locals --closed-world --type-ssa --gufa-optimizing --type-merging --strip-debug --strip-dwarf --strip-producers --strip-target-features --type-refining --unsubtyping --vacuum --tuple-optimization --ssa --simplify-locals --simplify-globals-optimizing --signature-refining --signature-pruning --roundtrip --reorder-locals --reorder-globals --reorder-functions --remove-unused-types --remove-unused-names --remove-unused-module-elements --remove-unused-brs --remove-memory --precompute --optimize-instructions --optimize-casts --once-reduction --monomorphize --minimize-rec-groups --merge-similar-functions --merge-locals --merge-blocks --local-subtyping --local-cse --licm --intrinsic-lowering --inlining-optimizing --heap-store-optimization --gto --gsi --global-refining --duplicate-import-elimination --duplicate-function-elimination --directize --dce --dae-optimizing -O3 -O3 --gufa -O3 -O3 -O3 -O3 -O3 -O3 -O3"
  # TODO: try --flatten ; --rereloop ; --precompute-propagate ; --dfo
  $WASMOPT -o a.out.wasm a.out.noexport.wat
  # compile bytecode
  ocamlc $UNSAFE ./${2}.ml -o a.out.bc > /dev/null
  # compile wsoo
  wasm_of_ocaml compile --opt=3 ./a.out.bc -o a.wsoo.bc.js > /dev/null
  # compile jsoo
  js_of_ocaml compile --target-env=nodejs --opt=3 ./a.out.bc -o a.jsoo.bc.js

  # bench
  # TODO: use ../../ocamlrun when it'll be produced
  hyperfine --warmup 2 --runs 5 --export-json ${2}.json \
    -n "OCaml native" "./a.out" \
    -n "Wasocaml" "${NODE} ./main_node.mjs" \
    -n "Wsoo" "${NODE} ./a.wsoo.bc.js" \
    -n "Jsoo" "${NODE} ./a.jsoo.bc.js" \
    -n "Bytecode" "ocamlrun ./a.out.bc"

  echo ""
}


#bench "Boyer" "boyer" # unreachable: caml_compare_blocks
#bench "Boyer no exceptions" "boyer_no_exc" # unreachable: caml_compare_blocks
#bench "Hamming" "hamming" # missing value let-rec
#bench "Nucleic" "nucleic" # wrong result
#bench "Ray-Trace" "raytrace" # global init must have correct type
#bench "Splay Tree" "splay" # string_of_float is not supported

bench "Almabench" "almabench"
bench "Binary Decision Diagram" "bdd"
bench "Binary Trees" "binary_trees" # seems wrong if we uncomment the check print in loop_depths ?
bench "Fibonacci" "fib"
bench "Knuth-Bendix" "kb"
bench "Knuth-Bendix (no exception)" "kb_no_exc"
bench "Loop" "loop"
bench "Pfannkuchen" "fannkuch"
bench "Pfannkuchen 2" "fannkuch2"
bench "Quicksort" "quicksort"
bench "Soli" "soli"
bench "Takc" "takc"
bench "Taku" "taku"

python script.py
# bench "Fast Fourier Transform" "fft" # requires --disable-flat-float-array illegal cast on float array, see comment in file

# bench "Pascal" "pascal" # not part of the wsoo bench suite
