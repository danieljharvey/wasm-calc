(module
  (type (;0;) (func (result i64)))
  (type (;1;) (func (param i64)))
  (import "console" "log" (func (;1;) (type 1)))
  (func (;0;) (type 0) (result i64)
    i64.const 42
    call 1
    drop
    i64.const 100
    drop
    )
  (memory (;0;) 1)
  (global (;0;) (mut i32) (i32.const 32))
  (export "main" (func 0)))
