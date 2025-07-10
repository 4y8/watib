# Adding an instruction
To add a plain instruction with a fixed number of parameters which do not
require any kind of type variables (such as `ref.as_non_null`), one needs to
modify the following files:
+ `Val/instruction-types.sch` to add the type of the instruction to the
  validation pass,
+ `Opt/PureDrop/side-effect-table.sch` to add the purity of the instruction,
+ `Asm/opcodes.sch` to add the opcode of the instruction, if the opcode depends
  on the argument it has to be added in an ad-hoc way to `Asm/binary.scm`.

The format in which all these additions have to be made is described in each
file individually and the already present instructions serve as examples.

For other instructions, the files `Val/validate.scm` and `Asm/binary.scm` will
have to be modified, for validation and assembly. To replace the need of type
variables, instructions can be validated by the `adhoc-instr` function of the
`Val/validate.scm`. There they can rely on the stack for their validation. For
instance, `ref.as_non_null` will be of type `(ref null ht) -> (ref ht)` where
`ht` is the heap type corresponding to the reference type on top of the stack.

The file `Ast/node.scm` should also be modified to record new kinds of
instructions such as new blocks. Each optimisation pass should be modified to
take it into account (roughly everything in `Opt/`).
