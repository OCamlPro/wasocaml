#!/bin/sh

../../ocamlopt ./kb.ml
../../ocamlopt ./soli.ml
../../ocamlopt ./fib.ml
# wasm-opt --enable-gc --enable-reference-types --enable-exception-handling --enable-multivalue --enable-tail-call a.out.wasm -o a.out.wasm -O3
python3 -m http.server 8000 --directory .
