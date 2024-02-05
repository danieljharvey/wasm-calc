;; taken entirely from https://gist.github.com/bryanburgers/2b0f08fd583cf0401a958d7e8edc7552#file-figure-06-wat
(module
    ;; Create memory with at least 1 page of 64k of memory
    (memory $mem 1)

    ;; the pointer of the next allocation
    ;; we add this in the compiler now
    ;; so that we can offset it to allow manual access
    ;; earlier in the memory
    ;; (global $alloc.offset (mut i32) (i32.const 32))

    ;; allocator function
    (func $alloc (param $size i32) (result (;pointer;) i32)
        (local $this_alloc_ptr i32)
        (local $next_alloc_ptr i32)
        (local $current_capacity i32)

        ;; If the requested size is more than a 64k page, fail.
        local.get $size
        i32.const 65536
        i32.gt_u
        (if
            (then
                i32.const 0x01
                unreachable
            )
        )

        ;; calculate the current ptr and the next ptr
        global.get 0 ;; global 0 == $alloc.offset
        local.tee $this_alloc_ptr
        local.get $size
        i32.add
        local.set $next_alloc_ptr

        ;; If this allocation extends into a page of memory we haven't reserved
        ;; we need to reserve more memory
        memory.size
        i32.const 65536
        i32.mul
        local.set $current_capacity

        local.get $next_alloc_ptr
        local.get $current_capacity
        i32.gt_u
        (if
            (then
                i32.const 1
                memory.grow

                ;; if memory couldn't grow, panic
                i32.const -1
                i32.eq
                (if
                    (then
                        i32.const 0x02
                        unreachable
                    )
                )
            )
        )

        ;; store the ptr to the next allocation
        local.get $next_alloc_ptr
        global.set 0 ;; global 0 == $alloc.offset

        ;; and return the current pointer
        local.get $this_alloc_ptr
    )
)
