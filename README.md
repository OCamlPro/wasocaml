# Wasocaml

*Wasocaml* is an [OCaml] compiler to [Wasm].

It uses the Flambda IR of the compiler as a source language and targets [Wasm-GC].

## Quickstart

### Install

#### Binaryen

You need the `wasm-merge` binary in your path. You can get it from the main branch of [Binaryen].

```shell-session
$ git clone https://github.com/WebAssembly/binaryen.git
$ cd binaryen
$ git submodule init
$ git submodule update
$ cmake . && make
$ sudo make install
```

#### Wasocaml

You can now build and install Wasocaml. You can either build the compiler locally or install a switch with the Wasocaml compiler.

##### Locally

```shell-session
$ ./configure
$ make
$ sudo make install
```

##### Opam switch


```shell-session
$ opam switch create wasocaml --repos default,wasocaml=git+https://github.com/ocamlpro/wasocaml-opam.git ocaml-variants.4.14.1+wasocaml
```

### Usage

Running the compiler will produce two files: `a.out.wasm` (the Wasm binary) and `a.out.wast` (the Wast text format).

```shell-session
$ /usr/local/bin/ocamlopt file.ml
$ ls
a.out a.out.wasm a.out.wast
```

For a complete example using the compiler installed as an opam switch, see [wasocaml-demo].

[Binaryen]: https://github.com/WebAssembly/binaryen
[OCaml]: https://ocaml.org
[Wasm]: https://webassembly.org
[Wasm-GC]: https://github.com/WebAssembly/gc
[wasocaml-demo]: https://github.com/ocamlpro/wasocaml-demo
