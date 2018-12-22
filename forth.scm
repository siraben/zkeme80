;; Let's see if we can make a simple Forth system

(define next
  `((jp next-sub)))

(define push-bc-rs
  `((dec ix)
    (ld (+ ix 0) b)
    (dec ix)
    (ld (+ ix 0) c)))

(define pop-bc-rs
  `((ld c (+ ix 0))
    (inc ix)
    (ld b (+ ix 0))
    (inc ix)))

(define push-hl-rs
  `((dec ix)
    (ld (+ ix 0) h)
    (dec ix)
    (ld (+ ix 0) l)))

(define pop-hl-rs
  `((ld l (+ ix 0))
    (inc ix)
    (ld h (+ ix 0))
    (inc ix)))

(define push-de-rs
  `((dec ix)
    (ld (+ ix 0) d)
    (dec ix)
    (ld (+ ix 0) e)))

(define pop-de-rs
  `((ld e (+ ix 0))
    (inc ix)
    (ld d (+ ix 0))
    (inc ix)))


(define reset-link
  (lambda ()
    (set! prev-pointer 0)
    '()))

(define make-link
  (lambda ()
    ;; We need to compute and return the instruction record for the
    ;; previous byte, but perform the side effect of changing the link
    ;; pointer as well.
    (let ((out (assemble-expr `(dw (,prev-pointer)))))
      (set! prev-pointer *pc*)
      out)))

(define (string->bytes x)
  `(,@(bytevector->u8-list (string->utf8 x)) 0))

(define (defcode name flags label)
  (let ((len (string-length name)))
    `(,make-link
      (db (,(+ len flags)))
      (db ,(string->bytes name))
      (label ,label))))


(define prev-pointer 0)
(define linked-list-prog
  `((db ,(string->bytes "This is a Forth interpreter!"))
    ,@(defcode "EXIT" 0 'exit)
    (db ,(string->bytes "hi I'm code"))
    ,@(defcode "DUP" 0 'dup)
    (db ,(string->bytes "hi I'm more code"))
    ,@(defcode "DROP" 0 'drop)
    (db ,(string->bytes "zkeme80"))
    ))

(define hl-to-bc
  `((ld b h)
    (ld c l)))

(define next-sub
  `((label next-sub)
    (ld a (de))
    (ld l a)
    (inc de)
    (ld a (de))
    (ld h a)
    (inc de)
    (jp (hl))))

(define docol-sub
  `((label docol)
    ,@push-de-rs
    (pop de)
    ,@next))

(define forth-prog
  `(,reset-link
    (ld bc #x1234)
    (ld de main)
    (ld ix #x8200)
    (ld sp 0)
    ,@next
    ,@next-sub

    ,@docol-sub


    ,@(defcode "EXIT" 0 'exit)
    ,@pop-de-rs
    ,@next

    ,@(defcode "DUP" 0 'dup)
    (push bc)
    ,@next

    ,@(defcode "+" 0 'add)
    (pop hl)
    (add hl bc)
    ,@hl-to-bc
    ,@next

    ,@(defcode "-" 0 'sub)
    (xor a)
    (pop hl)
    (sbc hl bc)
    ,@hl-to-bc
    ,@next

    ,@(defcode "LIT" 0 'lit)
    (ld a (de))
    (ld l a)
    (inc de)
    (ld a (de))
    (ld h a)
    (inc de)
    (push bc)
    ,@hl-to-bc
    ,@next

    ,@(defcode "OMEGA" 0 'omega)
    (label foo)
    (jp foo)
    
    (label main)
    (dw (lit 3 lit 5 add omega))
    ;; ,fill-until-end
    ))

(define fill-until-end
  (lambda () (assemble-expr `(db ,(make-list
                                   (- #x100000 *pc*)
                                   #xff)))))
