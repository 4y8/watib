# Adding an optimisation pass

Each optimisation pass is contained in a subdirectory of `Opt/`. The main logic
of the pass should be contained in the `walk.scm` file. Once the pass has been
implemented in its subdirectory, the file `Opt/optimise.scm` should be modified
to take it into account in the optimisation pipeline and the file `watib.scm`
should be modified to add the corresponding command line flags.

Passes rely on Bigloo's object system for their implementation. This
architecture is inspired by the one of the Bigloo compiler itself. The different
classes of instruction are described in `Ast/node.scm`. Each optimisation pass,
then, is coded with *generic* functions and *methods*. See the `Opt/Unreachable`
or `Opt/Peephole` passes for readable examples.
