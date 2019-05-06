(define boot-asm
  `((label boot)
    (label shutdown)
    (di)
    (ld a 6)
    (out (4) a)
    (ld a #x81)
    (out (7) a)
    (ld sp 0)
    (call sleep)

    (label restart)
    (label reboot)
    (di)
    (ld sp 0)
    (ld a 6)
    (out (4) a)
    (ld a #x81)
    (out (7) a)
    (ld a 3)
    (out (#xe) a)
    (xor a)
    (out (#xf) a)
    (call unlock-flash)
    (xor a)
    (out (#x25) a)
    (dec a)
    (out (#x26) a)
    (out (#x23) a)
    (out (#x22) a)
    (call lock-flash)
    (ld a 1)
    (out (#x20) a)


    (ld a #b0001011)
    (out (3) a)
    (ld hl #x8000)
    (ld (hl) 0)
    (ld de #x8001)
    (ld bc #x7fff)
    (ldir)

    
    ;; Arbitrarily complicated macros!
    ,@(concat-map (lambda (x)
                    `((ld a ,x)
                      ;; (call #x50f)
                      (call lcd-delay)
                      (out (#x10) a)))
                  '(5 1 3 #x17 #xb #xef))

    ;; "main", after everything has been set up.
    ;; Just go straight to the Forth portion!
    ,@forth-asm
    (jp shutdown)))
