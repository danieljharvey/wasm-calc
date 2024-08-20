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

## wasm-calc8

- Read and write from linear memory:

```
memory 1000

function sum(a: Int64, b: Int64) -> Int64 { a + b }

function main -> Int64 { 
    store(0, (20: Int64)); 
    store(8, (22: Int64)); 
    sum(load(0), load(8))
} 
```

We provide a size upfront so any other allocations start after this.

- Use Wasm memory imported from Javascript:

```
import env.memory as memory 1000

import console.log as consoleLog(number: Int64) -> Void

export function test() -> Int64 { 
    let (a,b) = ((1: Int64), (2: Int64)); 
    let _ = consoleLog(a + b); 
    100 
}
```

- Read and write from globals

```
global immutable: Int64 = 1

function main() -> Int64 {
    immutable + 1
}
```

```
global mut counter: Int64 = 0

function main() -> Int64 {
    set(counter, 2);
    counter
}
```

## wasm-calc9

Upgrade from bump allocator (never free memory) to an actual malloc / free
implementation [written in the language
itself](/wasm-calc/blob/trunk/wasm-calc9/static/malloc.calc).

Ability checking - annotate functions with things they're not allowed to do:

```
function [noglobalmutate noallocate noimports] add(
  a: Int8, b: Int8
) -> Int8 { a + b}
```

Inline tests - expressions that return `True` or `False` that are automatically
run on each run of the typechecker.

```
test dropThenReallocate = 
  let a = malloc(3); 
  drop(a); 
  malloc(3) == a
```

## wasm-calc10

Pattern matching and literals in patterns

```
function patternMatch(
  tuple: (Boolean,Boolean,Int8)
) -> Int8 { 
  case tuple {
    (True,False,c) -> { c }, 
    (False,True,c) -> { 1 - c }, 
    _ -> 0 
  }
}
```

## wasm-calc11

Declare algebraic data types and pattern match on them

```
type Colour = Red | Green | Blue

function test() -> Int8 {
    case Blue { Red -> 1, Green -> 2, Blue -> 3 }
}
```

```
type These<a,b> = This(a) | That(b) | These(a,b)

function test() -> Boolean { 
  case These(True,False) { 
    This(a) -> a, 
    That(b) -> b, 
    These(a,b) -> a && b 
  }
}
```

```
type Maybe<a> = Just(a) | Nothing

function fromMaybe<a>(maybe: Maybe(a), default: a) -> a { 
  case maybe { 
    Just(a) -> a, 
    Nothing -> default
  }
}

function test() -> Int64 { 
  let matchValue: Maybe(Box(Int64)) = Just(Box(100)); 
  let default: Box(Int64) = Box(0);
  let Box(result) = fromMaybe(matchValue, default); 
  result
}
```

```
type List<a> = Cons(a, List(a)) | Nil

function sum(list:List(Int64)) -> Int64 {
  case list { 
    Cons(a, rest) -> a + sum(rest), 
    Nil -> 0 
  }
}
 
function test() -> Int64 { 
    sum(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
}
```
