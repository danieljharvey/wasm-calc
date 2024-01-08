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

Floats, boxing, polymorphism, use different WASM types

```
function sumPair(pair: (Float,Float)) {
  pair.1 + pair.2
}

sumPair((100.0,200.0))
```

```
function makePair<a,b>(left: a, right: b) {
  (left, right)
}

makePair(Box(100.0),Box(200.0))
```

## wasm-calc6

- Linearity checker - boxed types must be used once

```
function usingBoxedValueTwiceWillFail(a: Box(Integer)) { (a,a) } 

usingBoxedValueTwiceWillFail(Box(1)
```

```
function notUsingValueWillFail(a: Box(Integer)) { True } 

notUsingValueWillFail(Box(1)
```

```
function usingPrimitiveTwiceIsFine(a: Integer) { a + a } 

usingPrimitiveTwiceIsFine(123)
```

- Let bindings 

```
let a = 123;
let b = a + 123;
b + 100
```

- Destructuring

```
let (a,b) = (1,2); a + b
```


