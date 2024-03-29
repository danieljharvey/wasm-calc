(module
   ;; (memory 1)
   ;; (global $heap_start (mut i32) (i32.const 0))
   ;; (global $heap_end (mut i32) (i32.const 0))

  (func $malloc (param $size i32) (result i32)
    (local $current_ptr i32)
    (local $prev_ptr i32)
    (local $allocated_ptr i32)

    ;; If memory is not initialized, initialize it
    (if (i32.eq (global.get 0) (i32.const 0)) ;; 0 == $heap_start
      (then
        (global.set 0 (i32.const 0)) ;; 0 == $heap_start
        (global.set 1 (local.get $size)) ;; 1 == $heap_end
        (i32.store (global.get 0) (local.get $size)) ;; 0 == $heap_start
      )
    )

    ;; start searching at the beginning
    (local.set $prev_ptr (i32.const 0))
    (local.set $current_ptr (global.get 0)) ;; 0 == $heap_start

    (loop
      ;; if current_ptr is a block of size $size
      (if (i32.eq (i32.load (local.get $current_ptr)) (local.get $size))
        (then
          ;; set allocated_ptr to current_ptr + 4
          (local.set $allocated_ptr (i32.add (local.get $current_ptr) (i32.const 4)))
          ;; if this is the first block..
          (if (i32.eq (local.get $prev_ptr) (i32.const 0))

            (then
              ;; set heap_start to whatever is it at current_ptr + 4
              (global.set 0 (i32.load (i32.add (local.get $current_ptr) (i32.const 4)))) ;; 0 == $heap_start
            )
            (else
              ;; store current_ptr + 4 at prev_ptr
              (i32.store (local.get $prev_ptr) (i32.load (i32.add (local.get $current_ptr) (i32.const 4))))
            )
          )
          ;; return allocator_ptr
          (return (local.get $allocated_ptr))
        )
      )

      ;; if data at current_ptr = 0...
      (if (i32.eq (i32.load (local.get $current_ptr)) (i32.const 0))
        (then
          ;; set allocated_ptr to heap_end
          (local.set $allocated_ptr (global.get 1)) ;; 1 == $heap_end
          ;;
          (global.set 1 (i32.add (global.get 1) (i32.add (local.get $size) (i32.const 4)))) ;; 1 == $heap_end
          (i32.store (local.get $current_ptr) (local.get $size))
          (i32.store (i32.add (local.get $current_ptr) (i32.const 4)) (i32.const 0))
          (return (local.get $allocated_ptr))
        )
      )

      (local.set $prev_ptr (local.get $current_ptr))
      (local.set $current_ptr (i32.add (i32.load (local.get $current_ptr)) (i32.const 8)))
    )
    (local.get $allocated_ptr)
  )

  (func $free (param $ptr i32)
    (local $current_ptr i32)
    (local $prev_ptr i32)
    (local $block_size i32)
    (local.set $prev_ptr (i32.const 0))
    (local.set $current_ptr (global.get 0)) ;; 0 == $heap_start

    (loop
      (if (i32.eq (local.get $current_ptr) (local.get $ptr))
        (then
          (local.set $block_size (i32.load (local.get $current_ptr)))
          (if (i32.eq (local.get $prev_ptr) (i32.const 0))
            (then
              (i32.store (local.get $current_ptr) (i32.const 0))
              (i32.store (i32.add (local.get $current_ptr) (i32.const 4)) (global.get 0)) ;; 0 == $heap_start
              (global.set 0 (local.get $current_ptr)) ;; 0 == $heap_start
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
)

