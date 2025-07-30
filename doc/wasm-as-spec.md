# Wasm-as' extensions to the specification
We try to list wasm-as' extensions to the spec, i.e. code modification made by
wasm-as silently that do not appear specification. To our knowledge, this
behaviours are not documented. The list is non-exhaustive. Feel free to contact
us if you find anything missing.

## Unreachable insertion
Wasm-as tend to replace dead code code by `unreachable` and add `unreachable`
after blocks that don't exit. While semantically preserving, this transformation
can transform invalid code in valid code. For instance, the following function
is not well-typed according to the spec. But wasm-as inserts an `unreachable` at
the end, making it well-typed.

```wasm
(func (result i32)
  (if (i32.const 0)
    (then (return (i32.const 0)))
    (else (return (i32.const 0)))))
```

In the following piece of code, the `br` is not well-typed but wasm-as accepts
it:
```wasm
(func
  (result i32)
  (block $l (result i32)
    (i32.const 0)
    (if
      (then (return (i32.const 0)))
      (else (return (i32.const 0))))
    (br $l)))
```
## Automatic function reference declaration
According to the specification, functions appearing in a `ref.func` have to
appear outside the function bodies (in a global or in an elem section). Wasm-as
puts all the undeclared function reference in an `elem declare func` section.

## Sign extension insertion for packed get
According to the specification, when using `array.get` or `struct.get` on a
packed field (`i8` or `i16`), a sign extension has to be specified. It means
that either `.get_u` or `.get_s` has to be used. Wasm-as allows `.get`
instructions on packed types, defaulting to unsigned.

## Replacement of block input values by locals
Binaryen's README mentions that block input values are represented in the IR by
`pop` subexpressions for `catch` blocks and not supported for the others. It is
not mentioned that code using block input values will be replaced by the use
local variables. For instance, the following code:
```wasm
(i32.const 0) 
(block (param i32)
  (drop))
```
is assembled to:
```wasm
(i32.const 0)
(local.set 0)
(local.get 0)
(drop)
```
