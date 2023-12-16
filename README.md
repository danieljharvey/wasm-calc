# wasm-calc

Like [llvm-calc](https://github.com/danieljharvey/llvm-calc), but adapting it for learning WASM.

Each directory slowly adds features to a calculator that compiles to WASM.

## wasm-calc

Simple numerical expressions

`1 + 2 + 3 * 3`

## wasm-calc2

Basic if expressions

`if 1 == 2 then 7 else 8`

## wasm-calc3

Simple functions

```
function sum(a: Integer, b: Integer) {
    a + b
}

sum(1,2)
```

## wasm-calc4

Simple compound types (tuples).

```
function swapPair(pair: (Integer, Boolean)) {
  (pair.2, pair.1)
}

if swapPair((100,True)).1 then 7 else 8
```

## wasm-calc5

Floats, use different WASM types

```
function sumPair(pair: (Float,Float)) {
  pair.1 + pair.2
}

sumPair((100.0,200.0))
```
