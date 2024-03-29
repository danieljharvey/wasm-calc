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

## wasm-calc7

- Use precise WASM types `Int32`, `Int64`, `Float32`, `Float64`

- Imports

```
import imports.draw as draw(
  x: Int64, y: Int64, r: Int64, g: Int64, b: Int64
) -> Void
```

- Prettyprinting

```
function min(floor: Int64, value: Int64) { 
  if value > floor then value else floor
}

function max(ceiling: Int64, value: Int64) { 
  if value < ceiling then value else ceiling
}

function clamp(
  floor: Int64, ceiling: Int64, value: Int64
) { min(floor, max(ceiling, value))}

function drawBounded(
  x: Int64, y: Int64, r: Int64, g: Int64, b: Int64
) { 
  let maxWidth = 400; 
  let maxHeight = 300; 
  draw(
    clamp(0, maxWidth, x), clamp(0, maxHeight, y), r, g, b
  )
}

export function test(index: Int64) { 
  let r = clamp(0, 255, index * 2); 
  let g = clamp(0, 255, 255 - r); 
  let b = clamp(0, 255, r * 3); 
  drawBounded(index * 2, index * 3, r, g, b); 
  drawBounded(100 - index, index * 3, b, g, r); 
  drawBounded(10 + index * 3, 50 - index * 2, g, r, b); 
  drawBounded(index * 4, 200 - index * 3, b, r, g)
}
```
