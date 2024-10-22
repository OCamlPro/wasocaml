#!/bin/sh

set -eu

#UNSAFE="-unsafe"
UNSAFE=""

../../ocamlopt $UNSAFE -O3 ./fannkuch2.ml

cat a.out.wat | grep -v "^ (export" > a.out.noexport.wat
WASMOPT="wasm-opt --enable-gc --enable-reference-types --enable-multivalue --enable-tail-call --enable-nontrapping-float-to-int --traps-never-happen -O3 --abstract-type-refining --cfp --coalesce-locals --closed-world --type-ssa --gufa-optimizing --type-merging --type-refining --unsubtyping --vacuum --tuple-optimization --ssa --simplify-locals --simplify-globals-optimizing --signature-refining --signature-pruning --roundtrip --reorder-locals --reorder-globals --reorder-functions --remove-unused-types --remove-unused-names --remove-unused-module-elements --remove-unused-brs --remove-memory --precompute --optimize-instructions --optimize-casts --once-reduction --monomorphize --minimize-rec-groups --merge-similar-functions --merge-locals --merge-blocks --local-subtyping --local-cse --licm --intrinsic-lowering --inlining-optimizing --heap-store-optimization --gto --gsi --global-refining --duplicate-import-elimination --duplicate-function-elimination --directize --dce --dae-optimizing"
WASMOPT="wasm-opt -all -O1"
$WASMOPT -o a.out.wasm a.out.noexport.wat
python3 -m http.server 8000 --directory .
