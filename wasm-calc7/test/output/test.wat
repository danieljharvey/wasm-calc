(module
  (type (;0;) (func))
  (type (;1;) (func (param i64) (result i64)))
  (import "console" "log" (func (;0;) (type 1)))
  (func (;2;) (type 0)
    i64.const 42
    call 1
    drop)
  (memory (;0;) 1)
  (global (;0;) (mut i32) (i32.const 32))
  (export "main" (func 0)))
