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

;; (define *word-count* 0)
;; (define make-link-pointer-label
;;   (string->symbol (format #f "forth-word~a" (number->string *word-count*))))

;; (define (defcode name flags label)

;;   ;; need to use lambda somehow
;;   (let ((prev *link-pointer*)
;;         (len (string-length name)))
;;     `((label ,make-link-pointer-label)
;;       (dw (,prev))
;;       (db (,(+ len flags)))
;;       (db ,(string name)))))

(define hl-to-bc
  `((ld b h)
    (ld c l)))
;; (define forth-prog
;;   `((label next-sub)
;;     (ld a (de))
;;     (ld l a)
;;     (inc de)
;;     (ld a (de))
;;     (ld h a)
;;     (inc de)
;;     (jp (hl))
;;     (label docol)
;;     ,@push-de-rs
;;     (pop de)
;;     ,@next

;;     ,@(defcode "EXIT" 0 'exit)
;;     ,@pop-de-rs
;;     ,@next

;;     ,@(defcode "DUP" 0 'dup)
;;     (push bc)
;;     ,@next

;;     ,@(defcode "+" 0 'add)
;;     (pop hl)
;;     (add hl bc)
;;     ,@hl-to-bc
;;     ,@next

;;     ,@(defcode "-" 0 'sub)
;;     (xor a)
;;     (pop hl)
;;     (sbc hl bc)
;;     ,@hl-to-bc
;;     ,@next



;;     ))

(define make-link
  (lambda ()
    ;; We need to compute and return the instruction record for the
    ;; previous byte, but perform the side effect of changing the link
    ;; pointer as well.
    (let ((out (assemble-expr `(db (,prev-pointer)))))
      (set! prev-pointer *pc*)
      out)))

(define (string->bytes x)
  `(,@(bytevector->u8-list (string->utf8 x)) 0))

(define (defcode name flags label)
  (let ((len (string-length name)))
    `((label ,label)
      ,make-link
      (db (,(+ len flags)))
      (db ,(string->bytes name)))))


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
