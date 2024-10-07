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
$ ./configure --enable-flambda
$ make
$ sudo make install
```

##### Opam switch

```shell-session
$ opam switch create wasocaml --repos default,wasocaml=git+https://github.com/ocamlpro/wasocaml-opam.git ocaml-variants.4.14.1+wasocaml
```

### Usage

Running the compiler will produce two files: `a.out.wasm` (the Wasm binary) and `a.out.wat` (the Wast text format).

```shell-session
$ /usr/local/bin/ocamlopt file.ml
$ ls
a.out a.out.wasm a.out.wat
```

For a complete example using the compiler installed as an opam switch, see [wasocaml-demo].

### Publications & talks

#### Publications

- [Wasocaml: compiling OCaml to WebAssembly]

#### Talks

- [Wasm GC meeting] - January 2023 - Online
- [Foundations of WebAssembly (Dagstuhl Seminar 23101)] - March 2023 - Dagstuhl (Germany)
- [IFL'23] - August 2023 - Braga (Portugal)
- [ICFP ML track] - September 2023 - Seattle (U.S.)
- [Wasm Research Day] - October 2023 - Munich (Germany) (accepted but we retracted to let the wasm\_of\_ocaml people present their work)

[Binaryen]: https://github.com/WebAssembly/binaryen
[Foundations of WebAssembly (Dagstuhl Seminar 23101)]: https://drops.dagstuhl.de/storage/04dagstuhl-reports/volume13/issue03/DagRep.13.3/DagRep.13.3.pdf#subsection.3.2
[ICFP ML track]: https://invidious.zapashcanon.fr/watch?v=i8PQXQ6a22Q
[IFL'23]: https://ifl23.github.io/accepted_papers.html
[OCaml]: https://ocaml.org
[Wasm]: https://webassembly.org
[Wasm-GC]: https://github.com/WebAssembly/gc
[Wasm GC meeting]: https://github.com/WebAssembly/meetings/blob/main/gc/2023/GC-01-10.md
[Wasm Research Day]: https://www.cs.cmu.edu/wrc/wrc-events
[Wasocaml: compiling OCaml to WebAssembly]: https://inria.hal.science/hal-04311345
[wasocaml-demo]: https://github.com/ocamlpro/wasocaml-demo
