# Wasocaml

*Wasocaml* is an [OCaml] compiler to [Wasm].

It uses the Flambda IR of the compiler as a source language and targets [Wasm-GC].

## Quickstart

### Install

You need the `wasm-merge` binary in your path. You can get it from the main branch of [Binaryen].

```shell-session
$ ./configure
$ make
$ make install
```

[Binaryen]: https://github.com/WebAssembly/binaryen
[OCaml]: https://ocaml.org
[Wasm]: https://webassembly.org
[Wasm-GC]: https://github.com/WebAssembly/gc
