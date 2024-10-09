#!/bin/sh

set -eu

bench() {
  echo "*** Running ${1}"
  ocamlopt -O2 ./${2}.ml > /dev/null
  time -p node-canary ./main_node.mjs > /dev/null
}

bench "Knuth-Bendix" "kb"
# bench "Soli" "soli" || true
bench "Fibonacci" "fib"
