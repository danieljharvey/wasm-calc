(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 1
    i32.load align=1
    local.set 1
    local.get 1
    local.set 2
    global.get 1
    i32.const 4
    i32.add
    i32.load align=1
    local.set 3
    local.get 3
    local.set 4
    local.get 4
    i32.const 0
    i32.eq
    local.get 2
    i32.const 0
    i32.eq
    i32.and
    local.get 0
    local.get 2
    i32.eq
    i32.or
    if (result i32)  ;; label = @1
      global.get 1
      local.set 5
      local.get 5
      local.set 6
      global.get 1
      local.get 0
      i32.store align=1
      global.get 1
      i32.const 4
      i32.add
      i32.const 1
      i32.store align=1
      global.get 1
      local.get 0
      i32.add
      i32.const 8
      i32.add
      global.set 1
      global.get 1
      local.set 7
      local.get 7
      local.set 8
      local.get 8
      i32.load align=1
      i32.const 0
      i32.eq
      if (result i32)  ;; label = @2
        local.get 8
        i32.const 0
        i32.store align=1
        local.get 8
        i32.const 4
        i32.add
        i32.const 0
        i32.store align=1
        i32.const 1
      else
        i32.const 1
      end
      drop
      local.get 6
      i32.const 8
      i32.add
    else
      global.get 1
      local.get 2
      i32.add
      i32.const 8
      i32.add
      global.set 1
      local.get 0
      call 1
    end)
  (func (;1;) (type 1) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32)
    local.get 0
    i32.const 8
    i32.sub
    i32.load align=1
    local.set 1
    local.get 1
    local.set 2
    local.get 0
    i32.const 4
    i32.sub
    i32.const 0
    i32.store align=1
    local.get 0
    local.get 2
    i32.add
    i32.load align=1
    local.set 3
    local.get 3
    local.set 4
    local.get 0
    local.get 2
    i32.add
    i32.const 4
    i32.add
    i32.load align=1
    local.set 5
    local.get 5
    local.set 6
    local.get 6
    i32.const 0
    i32.eq
    if (result i32)  ;; label = @1
      local.get 2
      local.get 4
      i32.add
      local.set 7
      local.get 7
      local.set 8
      local.get 0
      i32.const 8
      i32.sub
      local.get 8
      i32.store align=1
      i32.const 1
    else
      i32.const 1
    end
    drop
    local.get 0
    i32.const 8
    i32.sub
    global.set 1)
  (memory (;0;) 1)
  (global (;0;) (mut i32) (i32.const 32))
  (global (;1;) (mut i32) (i32.const 0)))
