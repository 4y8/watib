# Adding an optimisation pass

Each optimisation pass is contained in a subdirectory of `Opt/`. The main logic
of the pass should be contained in the `walk.scm` file.

Passes rely on Bigloo's object system for their implementation. This
architecture is inspired by the one of the Bigloo compiler itself. The different
classes of instruction are described in `Ast/node.scm`. Each optimisation pass,
then, is coded with *generic* functions and *methods*. See the `Opt/Unreachable`
or `Opt/Peephole` passes for readable examples.
