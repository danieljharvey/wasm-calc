(module
  (memory 1)
  (global $heap_start (mut i32) (i32.const 0))
  (global $heap_end (mut i32) (i32.const 0))

  (func $initialize_memory (param $size i32)
    (global.set $heap_start (i32.const 0))
    (global.set $heap_end (local.get $size))
    (i32.store (global.get $heap_start) (local.get $size))
  )

  (func $malloc (param $size i32) (result i32)
    (local $current_ptr i32)
    (local $prev_ptr i32)
    (local $allocated_ptr i32)

    ;; If memory is not initialized, initialize it
    (if (i32.eq (global.get $heap_start) (i32.const 0))
      (then
        (call $initialize_memory (local.get $size))
      )
    )

    (local.set $prev_ptr (i32.const 0))
    (local.set $current_ptr (global.get $heap_start))

    (loop
      (if (i32.eq (i32.load (local.get $current_ptr)) (local.get $size))
        (then
          (local.set $allocated_ptr (i32.add (local.get $current_ptr) (i32.const 4)))
          (if (i32.eq (local.get $prev_ptr) (i32.const 0))
            (then
              (global.set $heap_start (i32.load (i32.add (local.get $current_ptr) (i32.const 4))))
            )
            (else
              (i32.store (local.get $prev_ptr) (i32.load (i32.add (local.get $current_ptr) (i32.const 4))))
            )
          )
          (return (local.get $allocated_ptr))
        )
      )

      (if (i32.eq (i32.load (local.get $current_ptr)) (i32.const 0))
        (then
          (local.set $allocated_ptr (global.get $heap_end))
          (global.set $heap_end (i32.add (global.get $heap_end) (i32.add (local.get $size) (i32.const 4))))
          (i32.store (local.get $current_ptr) (local.get $size))
          (i32.store (i32.add (local.get $current_ptr) (i32.const 4)) (i32.const 0))
          (return (local.get $allocated_ptr))
        )
      )

      (local.set $prev_ptr (local.get $current_ptr))
      (local.set $current_ptr (i32.add (i32.load (local.get $current_ptr)) (i32.const 8)))
    )
  )

  (func $free (param $ptr i32)
    (local $current_ptr i32)
    (local $prev_ptr i32)
    (local $block_size i32)
    (local.set $prev_ptr (i32.const 0))
    (local.set $current_ptr (global.get $heap_start))

    (loop
      (if (i32.eq (local.get $current_ptr) (local.get $ptr))
        (then
          (local.set $block_size (i32.load (local.get $current_ptr)))
          (if (i32.eq (local.get $prev_ptr) (i32.const 0))
            (then
              (i32.store (local.get $current_ptr) (i32.const 0))
              (i32.store (i32.add (local.get $current_ptr) (i32.const 4)) (global.get $heap_start))
              (global.set $heap_start (local.get $current_ptr))
            )
            (else
              (i32.store (local.get $current_ptr) (i32.const 0))
              (i32.store (i32.add (local.get $prev_ptr) (i32.const 4)) (local.get $current_ptr))
            )
          )
          (return)
        )
      )

      (local.set $prev_ptr (local.get $current_ptr))
      (local.set $current_ptr (i32.add (i32.load (local.get $current_ptr)) (i32.const 8)))
    )
  )

  (export "malloc" (func $malloc))
  (export "free" (func $free))
)

