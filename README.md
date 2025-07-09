# watib
Watib (WebAssembly Toolchain In Bigloo) is a (WIP) WebAssembly toolchain. It
aims at being user friendly and fast. We currently support a big part of the
current draft of the version 3 of the standard. See [here](#missing) for more
details on what is currently missing.

It is used by the Bigloo scheme compiler for its wasm backend.

## Building
To build watib, a working [Bigloo](https://www-sop.inria.fr/indes/fp/Bigloo/)
distribution is required. You can, then, build it with:

```sh
make
```
## Using
Once built, watib can be called on a WebAssembly Text file to validate,
optimise and convert it to binary format. It supports several options:
+ `-o <file>` to specify an output file,
+ `-k` or `--keep-going` to continue validation after an error has been
  encoutered (won't produce an output file),
+ `-v` or `--validate-only` to stop after validation,
+ `-O0` to disable the optimisations,
+ `-j <n>` to use more than one job.
## Overview
The source code consists of the following directories:
+ The `Val` directory contains the validation logic. In particular, the
  `instruction-types.sch` file contains the descritption of the type of most
  instructions. It also handles the desuggaring of the text format.
+ The `Type` directory contains functions to manipulate and compare types.
+ The `Ast` directory contains the internal representation of wasm code
  outputted by the validation and used by the two following directories.
+ The `Asm` directory contains the translation to the binary format logic.
+ The `Opt` directory contains the optimisation logic. Each subdirectory is an
  optimisation pass and everything is glued together by the `optimise.scm` file.
+ The `Misc` directory contains various routines used by the previous
  directories.

The functions in the first four directories are more or less straightforward
transcriptions of the spec. The code is annotated with the positions in the spec
it implements (the precision of the annotations depends on the subtlety of what
is being implemented).
## Missing
Watib doesn't support the following features yet (the list may be incomplete):
+ Tables,
+ Memory instructions,
+ Vector instructions,
+ Active data declarations,
+ Elem declarations other than `(elem declare func ...)`,
+ Start declarations,
+ Omission of the module keyword,
+ Type indices in type-uses (i.e. functions have to be declared with `(param ...)` and `(result ...)`,
+ Offset and alignment in `load8_sx` and `store8`,
+ NaN of the form `nan:...` (only nan is supported).

If you want to use watib and need one of these, feel free to raise an issue or
send an email at [aghilas.boussaa@normalesup.org](mailto:aghilas.boussaa@normalesup.org).

## Acknowledgment
Thanks to Manuel Serrano who wrote the Bigloo compiler, became the first user
and provided help, feedbacks and the internship which led to watib. Thanks to
Andreas Rossberg for his reactivity and valuable knowledge concerning doubts on
the specification and Wasm more generally.
