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
the end, making it well type.

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
