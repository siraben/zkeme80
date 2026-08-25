;; Forth portion of the operating system.

(define (include-file-as-bytes filename)
  (let* ((port (open-file filename "r"))
         (res (get-string-all port))
         (expr `((db ,(string res)))))
    (close-port port)
    expr))

;; Immediate flag
(define immediate 128)
(define hidden 64)

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

(define hl-to-bc
  `((ld b h)
    (ld c l)))

(define bc-to-hl
  `((ld h b)
    (ld l c)))

(define hl-to-de
  `((ld d h)
    (ld e l)))

(define reset-link
  (lambda ()
    (set! *link-pointer* 0)
    '()))

(define make-link
  (lambda ()
    ;; We need to compute and return the instruction record for the
    ;; previous byte, but perform the side effect of changing the link
    ;; pointer as well.
    (let ((out (assemble-expr `(dw (,*link-pointer*)))))
      (set! *link-pointer* *pc*)
      out)))

(define (string->bytes x)
  `(,@(bytevector->u8-list (string->utf8 x)) 0))

(define (defcode name flags label)
  (let ((len (string-length name)))
    ;; Record dictionary metadata for the labelmap/debug output.
    (set! *forth-words* (cons (list name flags label) *forth-words*))
    `(,make-link
      (db (,(+ len flags)))
      (db ,(string->bytes name))
      (label ,label))))

(define (defword name flags label)
  `(,@(defcode name flags label)
    (call docol)))

(define *var-list* '())
(define *var-count* 0)
(define (next-var-addr!)
  (set! *var-count* (1+ *var-count*))
  (+ #x8400 (* 2 *var-count*)))

(define reset-var
  (lambda ()
    (set! *var-list* '())
    (set! *var-count* 0)
    '()))


;; We must relocate these variables elsewhere, where RAM is writable.
(define (defvar name label default . flags-arg)
  ;; Store the list of variable default values.

  (let ((var-label (string->symbol (format #f "var-~a" label)))
        (var-addr (next-var-addr!))
        (flags (if (null? flags-arg) 0 (car flags-arg))))
    (set! *var-list* `((,var-label . ,default) .  ,*var-list*))

    `(,@(defcode name flags label)
      (push bc)
      (ld bc ,var-addr)
      ,@next)))

;; (lit* '(1 2 3 4 5)) => (lit 1 lit 2 ...)
(define (lit* l)
  (if (null? l)
      '()
      `(lit ,(car l) . ,(lit* (cdr l)))))

(define *link-pointer* 0)

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

;; Runtime for definitions made by CREATE.  CALL leaves the data-field
;; address on the machine stack; make it the new Forth data-stack top.
(define dovar-sub
  `((label dovar)
    (pop hl)
    (push bc)
    ,@hl-to-bc
    ,@next))


(define forth-stack-words
  `(,@(defcode "DUP" 0 'dup)
    (push bc)
    ,@next

    ,@(defcode "?DUP" 0 '?dup)
    (ld hl 0)
    (call cp-hl-bc)
    (jp nz dup)
    ,@next

    ,@(defcode "DROP" 0 'drop)
    (pop bc)
    ,@next

    ,@(defcode "SWAP" 0 'swap)
    (pop hl)
    (push bc)
    ,@hl-to-bc
    ,@next

    ,@(defcode "NIP" 0 'nip)
    (pop hl)
    ,@next

    ,@(defcode "TUCK" 0 'tuck)
    (pop hl)
    (push bc)
    (push hl)
    ,@next

    ,@(defcode ">R" 0 '>r)
    ,@push-bc-rs
    (pop bc)
    ,@next

    ,@(defcode "R>" 0 'r>)
    (push bc)
    ,@pop-bc-rs
    ,@next

    ,@(defcode "R@" 0 'r@)
    (push bc)
    (ld c (+ ix 0))
    (ld b (+ ix 1))
    ,@next

    ,@(defcode "2>R" 0 '2>r)
    (pop hl)
    ,@push-hl-rs
    ,@push-bc-rs
    (pop bc)
    ,@next

    ,@(defcode "2R>" 0 '2r>)
    (push bc)
    ,@pop-bc-rs
    ,@pop-hl-rs
    (push hl)
    ,@next

    ,@(defcode "RP!" 0 'rp!)
    (push bc)
    (pop ix)
    (pop bc)
    ,@next

    ,@(defcode "RP@" 0 'rp@)
    (push bc)
    (push ix)
    (pop bc)
    ,@next

    ,@(defcode "RDROP" 0 'rdrop)
    (inc ix)
    (inc ix)
    ,@next

    ,@(defcode "2RDROP" 0 '2rdrop)
    (inc ix)
    (inc ix)
    (inc ix)
    (inc ix)
    ,@next

    ,@(defcode "SP@" 0 'sp@)
    (push bc)
    (ld hl 0)
    (add hl sp)
    ,@hl-to-bc
    ,@next

    ,@(defcode "SP!" 0 'sp!)
    ,@bc-to-hl
    (ld sp hl)
    (pop bc)
    ,@next

    ,@(defcode "OVER" 0 'over)
    (pop hl)
    (push hl)
    (push bc)
    ,@hl-to-bc
    ,@next

    ,@(defcode "ROT" 0 'rot)
    (pop hl)
    (ex (sp) hl)
    (push bc)
    ,@hl-to-bc
    ,@next

    ,@(defcode "-ROT" 0 '-rot)
    (ld h b)
    (ld l c)
    (pop bc)
    (ex (sp) hl)
    (push hl)
    ,@next

    ,@(defcode "2DROP" 0 '2drop)
    (pop bc)
    (pop bc)
    ,@next

    ,@(defcode "2DUP" 0 '2dup)
    (pop hl)
    (push hl)
    (push bc)
    (push hl)
    ,@next

    ,@(defcode "2SWAP" 0 '2swap)
    (ld (var-temp-cell) de)
    (pop hl)
    (ld d b)
    (ld e c)
    (pop bc)
    (ex (sp) hl)
    (push de)
    (push hl)
    (ld de (var-temp-cell))
    ,@next

    ;; T{ 1 2 3 4 2OVER -> 1 2 3 4 1 2 }T
    ,@(defcode "2OVER" 0 '2over)
    (push bc)
    (pop bc)
    (pop hl)
    (pop bc)
    (pop hl)

    (push hl)
    (push bc)
    (dec sp)
    (dec sp)
    (dec sp)
    (dec sp)
    (push hl)
    ,@next


    ))

(define forth-math-words
  `(,@(defcode "+" 0 '+)
    (pop hl)
    (add hl bc)
    ,@hl-to-bc
    ,@next

    ,@(defcode "-" 0 '-)
    (xor a)
    (pop hl)
    (sbc hl bc)
    ,@hl-to-bc
    ,@next

    ,@(defcode "AND" 0 'and)
    (pop hl)
    (ld a c)
    (and l)
    (ld c a)
    (ld a b)
    (and h)
    (ld b a)
    ,@next

    ,@(defcode "OR" 0 'or)
    (pop hl)
    (ld a c)
    (or l)
    (ld c a)
    (ld a b)
    (or h)
    (ld b a)
    ,@next

    ,@(defcode "XOR" 0 'xor)
    (pop hl)
    (ld a c)
    (xor l)
    (ld c a)
    (ld a b)
    (xor h)
    (ld b a)
    ,@next

    ,@(defcode "2*" 0 '2*)
    (xor a)
    (rl c)
    (rl b)
    ,@next

    ;; Aliased for speed.
    ,@(defcode "CELLS" 0 'cells)
    (xor a)
    (rl c)
    (rl b)
    ,@next

    ;; It's a no-op.
    ,@(defcode "CHARS" 0 'chars)
    ,@next

    ,@(defcode "2/" 0 '2/)
    ;; Sign-extend the high byte while preserving its low bit in carry for C.
    (bit 7 b)
    (jr z two-divide-positive)
    (srl b)
    (set 7 b)
    (jr two-divide-high-done)
    (label two-divide-positive)
    (srl b)
    (label two-divide-high-done)
    (rr c)
    ,@next

    ;; Logical right shift.  Counts at or above the 16-bit cell width yield
    ;; zero, while 2/ above remains the required arithmetic operation.
    ,@(defcode "RSHIFT" 0 'rshift)
    (ld a b)
    (or a)
    (jr nz rshift-zero)
    (ld a c)
    (cp 16)
    (jr nc rshift-zero)
    (pop hl)
    (or a)
    (jr z rshift-done)
    (label rshift-loop)
    (srl h)
    (rr l)
    (dec a)
    (jr nz rshift-loop)
    (label rshift-done)
    ,@hl-to-bc
    ,@next
    (label rshift-zero)
    (pop hl)
    (ld bc 0)
    ,@next

    ,@(defcode "INVERT" 0 'invert)
    (ld a c)
    (cpl)
    (ld c a)
    (ld a b)
    (cpl)
    (ld b a)
    ,@next

    ,@(defcode "*" 0 '*)
    (ld (var-temp-cell) de)
    (pop de)
    (call mul-16-by-16)
    ,@hl-to-bc
    (ld de (var-temp-cell))
    ,@next

    ;; Unsigned double-cell by single-cell division.  The high cell must be
    ;; smaller than the divisor; ANS makes other cases ambiguous.
    ,@(defcode "UM/MOD" 0 'um/mod)
    (ld (var-temp-cell) de)
    (ld d b)
    (ld e c)
    (pop hl)
    (pop bc)
    (ld a b)
    (ld	b 16)

    (label um-div-loop)
    ;; SLL C is equivalent to the supported SLA C followed by INC C; INC
    ;; preserves the carry containing the shifted-out quotient bit.
    (sla c)
    (inc c)
    (rla)
    (adc hl hl)
    ;; A carry here is the seventeenth remainder bit.  Such a trial is
    ;; necessarily at least the divisor, but SBC must not consume that carry
    ;; as an extra borrow-in.
    (jr c um-div-overflow)
    (or a)
    (sbc hl de)
    (jr	nc um-div-accepted)
    (add hl de)
    (dec c)
    (jr um-div-accepted)
    (label um-div-overflow)
    (or a)
    (sbc hl de)
    (label um-div-accepted)

    (djnz um-div-loop)
    (ld b a)
    (push hl)
    (ld de (var-temp-cell))
    ,@next

    ;; Internal single-cell unsigned division.
    ,@(defword "U/MOD16" hidden 'u-divmod)
    (dw (lit 0 swap um/mod exit))

    ,@(defcode "UM*" 0 'um*)
    (ld (var-temp-cell) de)
    (pop de)
    (call mul-16-by-16)
    (push hl)
    (ld b d)
    (ld c e)
    (ld de (var-temp-cell))
    ,@next

    ,@(defcode "NEGATE" 0 'negate)
    (ld hl 0)
    (or a)
    (sbc hl bc)
    ,@hl-to-bc
    ,@next

    ,@(defword "ABS" 0 'abs)
    (dw (dup 0< 0jump abs-done negate))
    (label abs-done)
    (dw (exit))

    ,@(defword "S>D" 0 's>d)
    (dw (dup 0< exit))

    ,@(defword "DNEGATE" hidden 'dnegate)
    (dw (invert swap negate tuck 0= - exit))

    ,@(defword "DABS" hidden 'dabs)
    (dw (dup 0< 0jump dabs-done dnegate))
    (label dabs-done)
    (dw (exit))

    ,@(defword "M*" 0 'm*)
    (dw (2dup xor >r abs swap abs swap um* r> 0<))
    (dw (0jump m-star-done dnegate))
    (label m-star-done)
    (dw (exit))

    ;; Symmetric signed division.  The unsigned primitive yields remainder
    ;; then quotient; restore the double dividend sign on the remainder and
    ;; the XOR of operand signs on the quotient.
    ,@(defword "SM/REM" 0 'sm/rem)
    (dw (dup 0< swap abs 2>r dup 0< >r dabs))
    (dw (r> r> swap >r um/mod))
    ;; Preserve the dividend sign for the remainder before combining it with
    ;; the divisor sign for the quotient.
    (dw (r> r> over >r xor 0jump sm-rem-quotient-positive negate))
    (label sm-rem-quotient-positive)
    (dw (r> 0jump sm-rem-done swap negate swap))
    (label sm-rem-done)
    (dw (exit))

    ,@(defword "FM/MOD" 0 'fm/mod)
    (dw (dup >r sm/rem over ?dup 0jump fm-mod-no-adjust))
    (dw (r@ xor 0< 0jump fm-mod-no-adjust 1- swap r> + swap exit))
    (label fm-mod-no-adjust)
    (dw (rdrop exit))

    ,@(defword "/MOD" 0 '/mod)
    (dw (>r s>d r> sm/rem exit))

    ,@(defword "MOD" 0 'mod)
    (dw (/mod drop exit))

    ,@(defword "/" 0 '/)
    (dw (/mod nip exit))

    ,@(defword "*/MOD" 0 '*/mod)
    (dw (>r m* r> sm/rem exit))

    ,@(defword "*/" 0 '*/)
    (dw (*/mod nip exit))

    ,@(defcode "1+" 0 '1+)
    (inc bc)
    ,@next

    ,@(defcode "CHAR+" 0 'char+)
    (inc bc)
    ,@next

    ,@(defcode "1-" 0 '1-)
    (dec bc)
    ,@next

    ,@(defcode "2-" 0 '2-)
    (dec bc)
    (dec bc)
    ,@next

    ,@(defcode "2+" 0 '2+)
    (inc bc)
    (inc bc)
    ,@next

    ,@(defcode "CELL+" 0 'cell+)
    (inc bc)
    (inc bc)
    ,@next

    ,@(defword "TRUE" 0 'true)
    (dw (lit 65535 exit))

    ,@(defword "FALSE" 0 'false)
    (dw (lit 0 exit))

    ,@(defcode "'0'" 0 'zeroc)
    (push bc)
    (ld bc 48)
    ,@next

    ,@(defcode "'9'" 0 'ninec)
    (push bc)
    (ld bc 57)
    ,@next

    ,@(defword "WITHIN" 0 'within)
    (dw (over - >r - r> u< exit))

    ;; Convert one ASCII digit to its value.  Uppercase digits through Z are
    ;; accepted so the interpreter follows BASE for the usual range 2..36.
    ;; ( char -- digit flag )
    ,@(defword "DIGIT?" 0 'digit?)
    (dw (dup lit 48 lit 58 within 0jump digit-check-alpha))
    (dw (lit 48 - true exit))
    (label digit-check-alpha)
    (dw (dup lit 65 lit 91 within 0jump digit-fail))
    (dw (lit 65 - lit 10 + true exit))
    (label digit-fail)
    (dw (drop lit 0 false exit))

    ,@(defword "NUM?" 0 'num?)
    (dw (digit? nip exit))

    ;; Parse a number starting at an address.
    ;; ( addr -- num | nothing)
    ;; The caller of PARSE-NUMBER should check the variable NUM-STATUS
    ;; to see if the parsing suceeded.

    ,@(defword "PARSE-NUMBER" 0 'parse-number)
    (dw (lit 0 num-status !))
    ;; Remember a leading minus on the return stack.
    (dw (dup c@ lit 45 = 0jump parse-num-positive))
    (dw (1+ true jump parse-num-sign-ready))
    (label parse-num-positive)
    (dw (false))
    (label parse-num-sign-ready)
    (dw (>r dup c@ digit? 0jump parse-num-fail-two))
    (dw (dup base @ u< 0jump parse-num-fail-two))
    (label parse-num-continue)
    (dw (swap 1+ swap)) ;; ( addr+1 n -- )
    (label parse-num-loop)
    (dw (over c@ 0jump parse-num-done))
    (dw (over c@ digit? 0jump parse-num-fail-three))
    (dw (dup base @ u< 0jump parse-num-fail-three))
    (dw (swap base @ * +))
    (dw (jump parse-num-continue))

    (label parse-num-done)
    (dw (swap drop r> 0jump parse-num-store negate))
    (label parse-num-store)
    (dw (true num-status ! exit))
    (label parse-num-fail-three)
    (dw (drop))
    (label parse-num-fail-two)
    (dw (2drop r> drop false num-status ! exit))

    ,@(defword "MAX" 0 'max)
    (dw (2dup > 0branch 8 drop branch 4 nip exit))

    ,@(defword "MIN" 0 'min)
    (dw (2dup < 0branch 8 drop branch 4 nip exit))

    ,@(defword "2@" 0 '2@)
    (dw (dup cell+ @ swap @ exit))

    ,@(defword "2!" 0 '2!)
    (dw (swap over ! cell+ ! exit))

    ))

(define forth-memory-words
  `(;; The Z80 permits cell access at every byte address, so every address is
    ;; already aligned in this implementation.
    ,@(defcode "ALIGNED" 0 'forth-aligned)
    ,@next

    ,@(defcode "ALIGN" 0 'align)
    ,@next

    ,@(defcode "!" 0 '!)
    (pop hl)
    (ld a l)
    (ld (bc) a)
    (inc bc)
    (ld a h)
    (ld (bc) a)
    (pop bc)
    ,@next

    ,@(defcode "@" 0 '@)
    (ld a (bc))
    (ld l a)
    (inc bc)
    (ld a (bc))
    (ld h a)
    ,@hl-to-bc
    ,@next

    ,@(defword "?" 0 '?)
    (dw (@ u. exit))

    ,@(defcode "+!" 0 '+!)
    (pop hl)
    (ld a (bc))
    (add a l)
    (ld (bc) a)
    (inc bc)
    (ld a (bc))
    (adc a h)
    (ld (bc) a)
    (pop bc)
    ,@next

    ,@(defcode "-!" 0 '-!)
    (pop hl)
    (push de)
    (ld a (bc))
    (ld e a)
    (inc bc)
    (ld a (bc))
    (ld d a)
    (dec bc)
    (xor a)
    (ex de hl)
    (sbc hl de)

    (ld a l)
    (ld (bc) a)
    (inc bc)
    (ld a h)
    (ld (bc) a)
    (pop de)
    (pop bc)
    ,@next

    ,@(defcode "C!" 0 'c!)
    (pop hl)
    (ld a l)
    (ld (bc) a)
    (pop bc)
    ,@next

    ,@(defcode "C@" 0 'c@)
    (ld a (bc))
    (ld c a)
    (ld b 0)
    ,@next

    ,@(defcode "C@C!" 0 'c@c!)
    (pop hl)
    (ld a (bc))
    (ld (hl) a)
    (inc hl)
    (inc bc)
    (push hl)
    ,@next

    ,@(defcode "CMOVE" 0 'cmove)
    (ld (var-temp-cell) de)
    (pop de)
    (pop hl)
    (ld a b)
    (or c)
    (jr z cmove-done)
    (ldir)
    (label cmove-done)
    (ld de (var-temp-cell))
    (pop bc)
    ,@next

    ,@(defcode "CMOVE>" 0 'cmove>)
    (ld (var-temp-cell) de)
    (pop de)
    (pop hl)
    (ld a b)
    (or c)
    (jr z cmove-backward-done)
    (add hl bc)
    (dec hl)
    (ex de hl)
    (add hl bc)
    (dec hl)
    (ex de hl)
    (lddr)
    (label cmove-backward-done)
    (ld de (var-temp-cell))
    (pop bc)
    ,@next

    ;; ( c-addr u char -- )
    ,@(defcode "FILL" 0 'fill)
    (ld (var-temp-cell) de)
    (ld a c)
    (pop bc)
    (pop hl)
    (ld d a)
    (ld a b)
    (or c)
    (jr z fill-done)
    (ld (hl) d)
    (dec bc)
    (ld a b)
    (or c)
    (jr z fill-done)
    (ld d h)
    (ld e l)
    (inc de)
    (ldir)
    (label fill-done)
    (ld de (var-temp-cell))
    (pop bc)
    ,@next

    ;; Copy safely for either overlap direction.
    ;; ( addr1 addr2 u -- )
    ,@(defword "MOVE" 0 'move)
    ;; Addresses are unsigned on the Z80; signed < chooses the wrong direction
    ;; whenever the two buffers straddle $8000.
    (dw (>r 2dup u< r> swap 0jump move-forward cmove> exit))
    (label move-forward)
    (dw (cmove exit))

    ,@(defword "USED" 0 'used)
    (dw (here h0 - exit))

    ,@(defword "UNUSED" 0 'unused)
    (dw (lit #xc000 here - exit))
    ))

(define forth-graphics-words
  `( ;; Draw a rectangle using OR
    ;; ( x y width height -- )
    ,@(defcode "RECT-OR" 0 'rect-or-forth)
    (ld b c)
    (pop hl)
    (ld c l)
    (pop hl)
    (ld (var-temp-cell) de)
    (pop de)
    (ld iy screen-buffer)
    (call rect-or)
    (call fast-copy)
    (ld de (var-temp-cell))
    (pop bc)
    ,@next

    ;; Draw a rectangle using XOR
    ;; ( x y width height -- )
    ,@(defcode "RECT-XOR" 0 'rect-xor-forth)
    (ld b c)
    (pop hl)
    (ld c l)
    (pop hl)
    (ld (var-temp-cell) de)
    (pop de)
    (ld iy screen-buffer)
    (call rect-xor)
    (call fast-copy)
    (ld de (var-temp-cell))
    (pop bc)
    ,@next

    ;; Draw a rectangle using AND
    ;; ( x y width height -- )
    ,@(defcode "RECT-AND" 0 'rect-and-forth)
    (ld b c)
    (pop hl)
    (ld c l)
    (pop hl)
    (ld (var-temp-cell) de)
    (pop de)
    (ld iy screen-buffer)
    (call rect-and)
    (call fast-copy)
    (ld de (var-temp-cell))
    (pop bc)
    ,@next

    ;; ( x y -- )
    ,@(defcode "SET-PIXEL" 0 'set-pixel-forth)
    (ld iy screen-buffer)
    (ld l c)
    (pop bc)
    (ld a c)
    (call set-pixel)
    (pop bc)
    ,@next

    ,@(defcode "CLEAR-SCREEN" 0 'clear-screen)
    (ld iy screen-buffer)
    (call clear-buffer)
    (call fast-copy)
    ,@next

    ;; Scroll the framebuffer upward by one six-pixel text row.
    ,@(defcode "SCROLL" 0 'scroll)
    (push bc)
    (push de)
    (push hl)
    (push iy)
    (ld hl screen-buffer-scroll-source)
    (ld de screen-buffer)
    (ld bc ,(- 768 72))
    (ldir)
    ;; Clear the six rows exposed at the bottom.
    (xor a)
    (ld (de) a)
    (ld h d)
    (ld l e)
    (inc de)
    (ld bc 71)
    (ldir)
    (ld a (var-cur-row))
    (sub 6)
    (jr nc scroll-save-row)
    (xor a)
    (label scroll-save-row)
    (ld (var-cur-row) a)
    (ld iy screen-buffer)
    (call fast-copy)
    (pop iy)
    (pop hl)
    (pop de)
    (pop bc)
    ,@next

    ,@(defword "PAGE" 0 'page)
    (dw (clear-screen origin exit))

    ;; Draw a sprite to the screen.
    ;; ( sprite_addr height x y -- )
    ,@(defcode "PUT-SPRITE-OR" 0 'put-sprite-or-forth)
    (ld iy screen-buffer)
    (ld (var-temp-cell) de)
    (ld e c)
    (pop bc)
    (ld d c)
    (pop bc)
    (ld b c)
    (pop hl)
    (call put-sprite-or)
    (call fast-copy)
    (pop bc)
    (ld de (var-temp-cell))
    ,@next

    ,@(defcode "PUT-SPRITE-AND" 0 'put-sprite-and-forth)
    (ld iy screen-buffer)
    (ld (var-temp-cell) de)
    (ld e c)
    (pop bc)
    (ld d c)
    (pop bc)
    (ld b c)
    (pop hl)
    (call put-sprite-and)
    (call fast-copy)
    (pop bc)
    (ld de (var-temp-cell))
    ,@next


    ,@(defcode "PUT-SPRITE-XOR" 0 'put-sprite-xor-forth)
    (ld iy screen-buffer)
    (ld (var-temp-cell) de)
    (ld e c)
    (pop bc)
    (ld d c)
    (pop bc)
    (ld b c)
    (pop hl)
    (call put-sprite-xor)
    (call fast-copy)
    (pop bc)
    (ld de (var-temp-cell))
    ,@next

    ;; Draw a region of memory to the screen.
    ;; ( addr --  )
    ,@(defcode "DRAW" 0 'draw)
    (push bc)
    (pop iy)
    (call fast-copy)
    (pop bc)
    ,@next

    ;; Plot the default memory screen (starting at address #x8100)
    ,@(defcode "PLOT" 0 'plot)
    (ld iy screen-buffer)
    (call fast-copy)
    ,@next


    (label all-black)
    (db (#b11110000))
    (db (#b11110000))
    (db (#b11110000))
    (db (#b11110000))
    (db (#b11110000))

    ;; Plot a character to the screen.
    ;; ( char -- )
    ,@(defcode "EMIT" 0 'emit)
    (push de)
    (push ix)

    ;; Margin of 0.
    ;; (ld ixh 0)
    (db (#xdd))
    (ld h 0)

    ;; Draw with OR
    (db (#xdd))
    (ld l 0)

    ;; Drawing coordinates.
    (ld a (var-cur-col))
    (ld d a)
    (ld a (var-cur-row))
    (ld e a)

    (ld iy screen-buffer)
    (ld hl all-black)
    (ld b 5)
    (call put-sprite-and)

    ;; Character to print.
    (ld a c)
    ;; Bounding box limits.
    (ld bc 25152)

    (call wrap-char-shared)
    (call fast-copy)
    (ld a d)
    (ld (var-cur-col) a)
    (ld a e)
    (ld (var-cur-row) a)

    (pop ix)
    (pop de)
    (pop bc)

    ,@next

    ;; Carriage return
    ,@(defcode "CR" 0 'cr)
    (push de)
    (push bc)
    (ld a (var-cur-col))
    (ld d a)
    (ld a (var-cur-row))
    (ld e a)
    (ld b 0)
    (call newline)
    (ld a d)
    (ld (var-cur-col) a)
    (ld a e)
    (ld (var-cur-row) a)
    (pop bc)
    (pop de)
    ,@next

    ;; ( x y --)
    ,@(defcode "AT-XY" 0 'at-xy)
    (ld a c)
    (ld (var-cur-row) a)
    (pop bc)
    (ld a c)
    (ld (var-cur-col) a)
    (pop bc)
    ,@next

    ,@(defword "CHAR-AT-XY" 0 'char-at-xy)
    (dw (lit 6 * swap lit 4 * swap at-xy))
    (dw (exit))

    ;; Draw a string to the screen
    ;; ( str_addr -- )
    ,@(defcode "PLOT-STRING" 0 'plot-string)
    (ld (var-temp-cell) de)
    (ld a (var-cur-col))
    (ld d a)
    (ld a (var-cur-row))
    (ld e a)
    ,@bc-to-hl
    (ld iy screen-buffer)
    (ld bc 25152)
    (xor a)
    (call wrap-str)
    (call fast-copy)
    (ld a d)
    (ld (var-cur-col) a)
    (ld a e)
    (ld (var-cur-row) a)
    (ld de (var-temp-cell))
    (pop bc)
    ,@next

    ;; Type exactly n characters starting at addr.  Rendering the complete
    ;; span in the framebuffer before one fast-copy keeps wrapped editing
    ;; responsive; the old threaded EMIT loop copied all 768 bytes per glyph.
    ;; ( addr n -- )
    ,@(defcode "TYPE" 0 'type)
    (ld (var-temp-cell) de)
    (pop hl)
    (ld (type-count) bc)
    (ld a b)
    (or c)
    (jr z type-native-done)
    (push ix)
    (db (#xdd))
    (ld h 0)
    (db (#xdd))
    (ld l 0)
    (ld a (var-cur-col))
    (ld d a)
    (ld a (var-cur-row))
    (ld e a)
    (ld iy screen-buffer)
    ;; TYPE is the exact-length renderer used by the line editor.  Its right
    ;; edge is the physical 96-pixel display boundary, so a following cursor
    ;; can never be positioned off-screen.
    (ld bc 24640)
    (label type-native-loop)
    (ld a (hl))
    (call wrap-char-shared)
    (inc hl)
    (push hl)
    (ld hl (type-count))
    (dec hl)
    (ld (type-count) hl)
    (ld a h)
    (or l)
    (pop hl)
    (jr nz type-native-loop)
    (call fast-copy)
    (ld a d)
    (ld (var-cur-col) a)
    (ld a e)
    (ld (var-cur-row) a)
    (pop ix)
    (label type-native-done)
    (ld de (var-temp-cell))
    (pop bc)
    ,@next

    ,@(defword "TELL" 0 'tell)
    (dw (drop plot-string exit))

    ))

(define forth-logic-words
  `(;; Absolute jumps!  Use with caution.

    ;; Absolute jumps are NOT portable because they depend on the
    ;; context in which the code was written.  JUMP primitives should
    ;; only be used in the bootstrapping stage (i.e. writing Forth
    ;; words by hand).  After that it is recommended to write in Forth
    ;; and decompile back into 0BRANCH and BRANCH primitives.
    ,@(defcode "JUMP" 0 'jump)
    (ld a (de))
    (ld l a)
    (inc de)
    (ld a (de))
    (ld h a)
    ,@hl-to-de
    ,@next

    ,@(defcode "0JUMP" 0 '0jump)
    (xor a)
    (cp c)
    (jp z zjump-maybe)
    (jp nz zjump-fail)

    (label zjump-maybe)
    (xor a)
    (cp b)
    (jp nz zjump-fail)
    (pop bc)
    (jp jump)

    (label zjump-fail)
    (inc de)
    (inc de)
    (pop bc)
    ,@next

    ,@(defcode "BRANCH" 0 'branch)
    (ex de hl)
    (ld e (hl))
    (inc hl)
    (ld d (hl))
    (dec hl)

    (add hl de)
    (ex de hl)
    ,@next

    ,@(defcode "0BRANCH" 0 '0branch)
    (xor a)
    (cp c)
    (jp z zbranch-maybe)
    (jp nz zbranch-fail)

    (label zbranch-maybe)
    (xor a)
    (cp b)
    (jp nz zbranch-fail)
    (pop bc)
    (jp branch)

    (label zbranch-fail)
    (inc de)
    (inc de)
    (pop bc)
    ,@next

    ,@(defcode "=" 0 '=)
    (pop hl)
    (call cp-hl-bc)
    (jp z tru)
    (jp fal)

    ,@(defcode "<>" 0 '<>)
    (pop hl)
    (call cp-hl-bc)
    (jp z fal)
    (jp tru)

    ,@(defcode "<" 0 '<)
    (pop hl)
    ;; Bias both signed cells into unsigned order, then compare normally.
    (ld a h)
    (xor #x80)
    (ld h a)
    (ld a b)
    (xor #x80)
    (ld b a)
    (call cp-hl-bc)
    (jp c tru)
    (jp fal)

    ,@(defword ">" 0 '>)
    (dw (swap < exit))

    ,@(defword "<=" 0 '<=)
    (dw (> 0= exit))

    ,@(defword ">=" 0 '>=)
    (dw (< 0= exit))

    ,@(defcode "U<" 0 'u<)
    (pop hl)
    (call cp-hl-bc)
    (jp c tru)
    (jp fal)

    ,@(defcode "0<" 0 '0<)
    (bit 7 b)
    (jp nz tru)
    (jp fal)

    ,@(defcode "0=" 0 '0=)
    (ld hl 0)
    (call cp-hl-bc)
    (jp c fal)
    (jp tru)

    ,@(defword "NOT" 0 'not)
    (dw (0= exit))))

(define forth-text-words
  `(,@(defcode "KEYC" 0 'keyc)
    (call get-key)
    (push bc)
    (ld b 0)
    (ld c a)
    ,@next

    ;; Blocking raw keypad scan.  ANS KEY is the character-oriented word
    ;; below; calculator UI code that needs scan codes uses RAW-KEY/KEYC.
    ,@(defcode "RAW-KEY" 0 'raw-key)
    (call flush-keys)
    (call wait-key)
    (push bc)
    (ld b 0)
    (ld c a)
    ,@next

    ;; Return one raw key event in A.  If a different key appears before
    ;; the current key is fully released, preserve it for the next call.
    ;; This handles normal key rollover without repeating a held key.
    (label akey-read-event)
    (ld a (akey-pending))
    (or a)
    (jr z akey-wait-event)
    (push af)
    (xor a)
    (ld (akey-pending) a)
    (pop af)
    (jr akey-wait-release)
    (label akey-wait-event)
    (call wait-key)
    (label akey-wait-release)
    (ld e a)
    (label akey-release-loop)
    (call scan-key)
    (or a)
    (jr z akey-event-ready)
    (cp e)
    (jr z akey-release-loop)
    (ld (akey-pending) a)
    (label akey-event-ready)
    (ld a e)
    (ret)

    ;; Read a key as an ASCII character.
    ,@(defcode "AKEY" 0 'akey)
    (ld (var-temp-cell) de)
    (push bc)
    (label akey-read)
    (call akey-read-event)
    ;; Alphabetic input is the default.  2ND selects the numeric/symbol
    ;; table for the following key.
    (cp 54)
    (jr nz akey-alpha)
    (call akey-read-event)
    (ld de numeric-char-lookup-table)
    (jr akey-lookup)
    (label akey-alpha)
    (ld de char-lookup-table)
    (label akey-lookup)
    (ld h 0)
    (ld l a)
    (ld b h)
    (add hl de)
    (ld c (hl))
    (ld a c)
    (or a)
    (jr z akey-read)
    (ld de (var-temp-cell))
    ,@next

    ,@(defword "KEY" 0 'key)
    (dw (akey exit))

    ,@(defcode "TO-ASCII" 0 'to-ascii)
    (push de)
    (ld a b)
    (or a)
    (jr nz to-ascii-invalid)
    (ld a c)
    (cp 128)
    (jr nc to-ascii-invalid)
    (ld h 0)
    (ld l c)
    (ld b h)
    (ld de char-lookup-table)
    (add hl de)
    (ld c (hl))
    (pop de)
    ,@next
    (label to-ascii-invalid)
    (ld bc 0)
    (pop de)
    ,@next

    ;; Convert a raw key code using the numeric/symbol table.
    ,@(defcode "TO-NUMERIC" 0 'to-numeric)
    (push de)
    (ld a b)
    (or a)
    (jr nz to-numeric-invalid)
    (ld a c)
    (cp 128)
    (jr nc to-numeric-invalid)
    (ld h 0)
    (ld l c)
    (ld b h)
    (ld de numeric-char-lookup-table)
    (add hl de)
    (ld c (hl))
    (pop de)
    ,@next
    (label to-numeric-invalid)
    (ld bc 0)
    (pop de)
    ,@next

    ,@(defword "ORIGIN" 0 'origin)
    (dw (lit 0 dup cur-col ! cur-row ! exit))


    (label cursor)
    (db (#b11110000))
    (db (#b11110000))
    (db (#b11110000))
    (db (#b11110000))
    (db (#b11110000))

    (label blank)
    (db (0 0 0 0 0))

    ;; ( addr u -- )
    ;; Expect at most u characters (or a newline, whichever comes first),
    ;; store them at addr, and record the received count in SPAN.  EXPECT
    ;; never writes beyond the caller's u-byte region.
    ;; Written in Forth because it's easier.
    ;; Still somewhat buggy.
    ,@(defword "(EXPECT)" hidden 'expect-internal)
    (dw (lit expect-full-edit c!))
    ;; Store the address and count so we can do various checks.
    (dw (dup lit expect-capacity ! lit expect-count ! dup lit expect-ptr !))
    ;; Rollover belongs to one input session; never inherit a stale key from
    ;; the menu, a prior prompt, or a transient shell teardown.
    (dw (lit 0 lit akey-pending c!))
    ;; And the initial/edit pointers.
    (dw (dup lit expect-edit-ptr ! lit expect-ptr-initial !))
    ;; Remember where this field begins on screen.
    (dw (lit var-cur-col @ lit expect-col-save !))
    (dw (lit var-cur-row @ lit expect-row-save !))
    (label expect-loop)
    ;; Clear the complete wrapped input region, preserving output above it.
    (dw (lit expect-col-save @ lit expect-row-save @))
    (dw (lit 96 lit expect-col-save @ - lit 5 rect-and-forth))
    (dw (lit 0 lit expect-row-save @ lit 6 + lit 96))
    (dw (lit 58 lit expect-row-save @ - rect-and-forth))
    (dw (lit expect-col-save @ lit expect-row-save @ at-xy))
    (dw (lit expect-ptr-initial @ lit expect-ptr @))
    (dw (lit expect-ptr-initial @ - type))
    ;; If wrapping reached the bottom, scroll one text row, move the saved
    ;; field origin with the framebuffer, and redraw.  A 128-byte field fits
    ;; once its origin reaches row zero even with the widest font glyphs.
    ;; Keep one complete text row below the field for evaluation output.
    (dw (lit var-cur-row @ lit 49 < 0jump expect-scroll-field))
    ;; Position the cursor by rendering only the exact-length prefix.
    (dw (lit expect-col-save @ lit expect-row-save @ at-xy))
    (dw (lit expect-ptr-initial @ lit expect-edit-ptr @))
    (dw (lit expect-ptr-initial @ - type))
    (dw (lit cursor lit 5 cur-col @ cur-row @ put-sprite-xor-forth))
    ;; Standard EXPECT completes as soon as u characters have been received.
    ;; The shell editor extension stays active so a full field can still be
    ;; navigated, overwritten, shortened, and appended again.
    (dw (lit expect-count @ 0jump expect-at-capacity jump expect-got-blank))
    (label expect-at-capacity)
    (dw (lit expect-full-edit c@ 0jump expect-end jump expect-got-blank))

    (label expect-got-blank)
    (dw (akey))
    (dw (?dup 0jump expect-got-blank))
    (dw (dup lit ,(char->integer #\newline) <> 0jump expect-got-newline))
    (dw (dup lit ,(char->integer #\backspace) <> 0jump expect-got-backspace))
    (dw (dup lit 2 <> 0jump expect-got-left))
    (dw (dup lit 6 <> 0jump expect-got-right))
    (dw (dup lit 16 <> 0jump expect-got-up))
    (dw (dup lit 14 <> 0jump expect-got-down))
    ;; General case
    (dw (lit expect-edit-ptr @ lit expect-ptr @ <> 0jump expect-append))
    (dw (lit expect-edit-ptr @ c!))
    (dw (lit 1 lit expect-edit-ptr +! jump expect-loop))
    (label expect-append)
    (dw (lit expect-count @ 0jump expect-drop-full))
    (dw (lit expect-edit-ptr @ c!))
    (dw (lit 1 lit expect-edit-ptr +!))
    (dw (lit 1 lit expect-ptr +!))
    (dw (lit 1 lit expect-count -!))
    (dw (jump expect-loop))
    (label expect-drop-full)
    (dw (drop jump expect-got-blank))

    (label expect-got-newline)
    (dw (drop))
    (label expect-end)
    ;; Remove the cursor, advance from the end of the wrapped field (even if
    ;; editing ended in its middle), and leave output below the full input.
    (dw (lit cursor lit 5 cur-col @ cur-row @ put-sprite-xor-forth))
    (dw (lit expect-col-save @ lit expect-row-save @ at-xy))
    (dw (lit expect-ptr-initial @ lit expect-ptr @))
    (dw (lit expect-ptr-initial @ - type cr))
    (dw (lit expect-ptr @ lit expect-ptr-initial @ - dup span ! drop exit))

    (label expect-got-backspace)
    (dw (drop))
    (dw (lit expect-ptr-initial @ lit expect-edit-ptr @))
    (dw (<> 0jump expect-got-blank))
    (dw (lit 1 lit expect-edit-ptr -!))
    ;; Shift the exact-length suffix one byte left without touching addr+u.
    (dw (lit expect-edit-ptr @ 1+ lit expect-edit-ptr @))
    (dw (lit expect-ptr @ lit expect-edit-ptr @ - 1- cmove))
    (dw (lit 1 lit expect-ptr -!))
    (dw (lit 1 lit expect-count +!))
    (dw (jump expect-loop))

    (label expect-got-left)
    (dw (drop lit expect-ptr-initial @ lit expect-edit-ptr @))
    (dw (<> 0jump expect-got-blank))
    (dw (lit 1 lit expect-edit-ptr -! jump expect-loop))

    (label expect-got-right)
    (dw (drop lit expect-ptr @ lit expect-edit-ptr @))
    (dw (<> 0jump expect-got-blank))
    (dw (lit 1 lit expect-edit-ptr +! jump expect-loop))

    ;; UP/DOWN are editor controls.  A shell may install EDIT-HISTORY with
    ;; stack effect ( addr capacity used direction -- used' ), where -1 is
    ;; older and +1 is newer.  Plain EXPECT/ACCEPT leave the vector zero.
    (label expect-got-up)
    (dw (drop lit 65535 jump expect-history))
    (label expect-got-down)
    (dw (drop lit 1))
    (label expect-history)
    (dw (>r lit var-edit-history @ dup 0jump expect-history-disabled >r))
    (dw (lit expect-ptr-initial @ lit expect-capacity @))
    (dw (lit expect-ptr @ lit expect-ptr-initial @ -))
    (dw (r> r> swap execute))
    ;; Refuse a buggy callback result rather than letting it move editor
    ;; pointers outside the caller-provided field.
    ;; Treat the callback count as unsigned: values above capacity, including
    ;; negative cells such as $FFFF, are invalid.
    (dw (dup lit expect-capacity @ swap u< 0jump expect-history-valid))
    (dw (drop jump expect-got-blank))
    (label expect-history-valid)
    (dw (dup lit expect-ptr-initial @ + dup lit expect-ptr !))
    (dw (lit expect-edit-ptr !))
    (dw (lit expect-capacity @ swap - lit expect-count ! jump expect-loop))
    (label expect-history-disabled)
    (dw (drop rdrop jump expect-got-blank))

    (label expect-scroll-field)
    (dw (lit expect-row-save @ 0jump expect-field-at-top))
    (dw (scroll lit 6 lit expect-row-save -! jump expect-loop))
    (label expect-field-at-top)
    (dw (lit expect-col-save @ lit expect-row-save @ at-xy))
    (dw (lit expect-ptr-initial @ lit expect-edit-ptr @))
    (dw (lit expect-ptr-initial @ - type))
    (dw (lit cursor lit 5 cur-col @ cur-row @ put-sprite-xor-forth))
    (dw (jump expect-got-blank))

    ,@(defword "EXPECT" 0 'expect)
    (dw (false expect-internal exit))

    ;; Calculator line-editor extension: retain cursor editing at capacity.
    ,@(defword "EDIT-LINE" 0 'edit-line)
    (dw (true expect-internal exit))

    ,@(defword "ACCEPT" 0 'accept)
    (dw (expect span @ exit))

    ,@(defword "REFILL" 0 'refill)
    ;; An evaluated string is already a complete input source and cannot be
    ;; replenished.  In particular, REFILL must not replace it with the
    ;; calculator's current interactive/bootstrap device.
    (dw (lit var-evaluation-depth @ 0jump refill-device false exit))
    (label refill-device)
    (dw (lit var-current-input-device @ execute))
    (dw (0jump refill-fail))
    ;; (dw (lit input-buffer lit var-input-ptr ! ))
    (dw (true exit))
    (label refill-fail)
    (dw (false exit))

    ;; Install an input source and reset its parse offset.  SOURCE! is a
    ;; system extension used by the built-in devices; SOURCE and >IN retain
    ;; their standard interfaces.
    ;; ( c-addr u -- )
    ,@(defword "SOURCE!" 0 'source-store)
    (dw (lit var-source-length ! dup lit var-source-address !))
    (dw (lit var-input-ptr ! lit 0 lit var-to-in ! exit))

    ;; Install a private NUL-terminated bootstrap source.  This is kept
    ;; separate from SOURCE! so EVALUATE strings may use their exact length.
    ;; ( c-addr -- )
    ,@(defword "CSTRING-SOURCE" 0 'cstring-source)
    (dw (dup))
    (label cstring-source-scan)
    (dw (dup c@ 0jump cstring-source-ready 1+ jump cstring-source-scan))
    (label cstring-source-ready)
    (dw (over - source-store exit))

    ;; Return the current input buffer without exposing its private cursor.
    ;; ( -- c-addr u )
    ,@(defword "SOURCE" 0 'source)
    (dw (lit var-source-address @ lit var-source-length @ exit))

    ;; Read one byte at SOURCE + >IN.  BC is scratch, while HL and DE are
    ;; preserved so the native WORD parser can share this routine.  A zero
    ;; result is the kernel's private end-of-source sentinel.
    (label source-getc-sub)
    (push hl)
    (ld hl (var-source-length))
    (ld bc (var-to-in))
    (xor a)
    (sbc hl bc)
    (jr z source-getc-eof)
    (jr c source-getc-eof)
    (ld hl (var-source-address))
    (add hl bc)
    (ld a (hl))
    (inc hl)
    (ld (var-input-ptr) hl)
    (ld hl (var-to-in))
    (inc hl)
    (ld (var-to-in) hl)
    (pop hl)
    (ret)
    (label source-getc-eof)
    (pop hl)
    (xor a)
    (ret)

    ;; Get the next character from the input source.
    ,@(defcode "GETC" 0 'getc)
    (push bc)
    (call source-getc-sub)
    (ld c a)
    (ld b 0)
    ,@next

    ,@(defword "UNGETC" 0 'ungetc)
    (dw (lit var-to-in @ ?dup 0jump ungetc-done 1- dup lit var-to-in !))
    (dw (lit var-source-address @ + lit var-input-ptr !))
    (label ungetc-done)
    (dw (exit))

    ;; Parse a delimiter-separated word into the standard counted-string
    ;; transient region.  The private trailing NUL lets internal clients use
    ;; the same buffer without imposing a NUL requirement on FIND callers.
    ;; ( char -- c-addr )
    ,@(defcode "WORD" 0 'word)
    (ld (var-temp-cell) de)
    (ld d c)                         ; delimiter
    (ld e 0)                         ; stored character count
    (ld hl word-buffer-data)
    (label word-skip-delimiters)
    (call source-getc-sub)
    (or a)
    (jr z word-finish)
    (cp d)
    (jr z word-skip-delimiters)
    (label word-character)
    (cp d)
    (jr z word-finish)
    (or a)
    (jr z word-finish)
    (ld c a)
    (ld a e)
    (cp 255)
    (jr z word-discard-overflow)
    (ld a c)
    (ld (hl) a)
    (inc hl)
    (inc e)
    (label word-read-next)
    (call source-getc-sub)
    (jr word-character)
    ;; More than 255 characters is an ambiguous condition, but it must never
    ;; write past the transient region.  Consume the rest of the field and
    ;; return the safely truncated counted string.
    (label word-discard-overflow)
    (call source-getc-sub)
    (or a)
    (jr z word-finish)
    (cp d)
    (jr nz word-discard-overflow)
    (label word-finish)
    (ld (hl) 0)
    (ld a e)
    (ld (word-buffer) a)
    (ld bc word-buffer)
    (ld de (var-temp-cell))
    ,@next

    ;; Internal source token parser.  Unlike standard WORD it treats all
    ;; input whitespace as a delimiter and skips backslash comments.  It
    ;; returns the address of the characters plus their length, retaining the
    ;; old kernel contract while keeping WORD available to applications.
    ;; ( -- addr len | 0 )
    ,@(defword "TOKEN" hidden 'token)
    (dw (lit token-buffer-data lit token-ptr !))
    (label token-skip-space)
    (dw (getc))
    (dw (?dup 0jump empty-token))
    (dw (dup lit ,(char->integer #\space) <>))
    (dw (0jump token-skip-space-consume))
    (dw (dup lit ,(char->integer #\newline) <>))
    (dw (0jump token-skip-space-consume))
    (dw (dup lit ,(char->integer #\tab) <>))
    (dw (0jump token-skip-space-consume))
    (dw (dup lit ,(char->integer #\\) <>))
    (dw (0jump token-skip-comment-start))

    (dw (jump token-store-character))

    (label token-skip-space-consume)
    (dw (drop))
    (dw (jump token-skip-space))

    (label token-skip-comment-start)
    (dw (drop))
    (label token-skip-comment)
    (dw (getc ?dup 0jump empty-token))
    (dw (dup lit ,(char->integer #\newline) <> 0jump token-skip-space-consume))
    (dw (drop))
    (dw (jump token-skip-comment))

    (label token-store-character)
    ;; Keep accepting source text through the full counted-string capacity.
    ;; Dictionary creation separately rejects names longer than 31 bytes;
    ;; lookup cannot mistake this 255-byte result for a dictionary name.
    (dw (lit token-ptr @ lit token-buffer-terminator =
             0jump token-store-character-safe))
    (dw (drop jump token-discard-overflow))
    (label token-store-character-safe)
    (dw (lit token-ptr @ c!))
    (dw (lit 1 lit token-ptr +!))

    (label token-character-loop)
    (dw (getc))
    (dw (dup 0jump token-done))
    (dw (dup lit ,(char->integer #\space)   <> 0jump token-done))
    (dw (dup lit ,(char->integer #\newline) <> 0jump token-done))
    (dw (dup lit ,(char->integer #\tab) <> 0jump token-done))

    (dw (jump token-store-character))

    (label token-discard-overflow)
    (dw (getc dup 0jump token-overflow-done))
    (dw (dup lit ,(char->integer #\space)   <> 0jump token-overflow-done))
    (dw (dup lit ,(char->integer #\newline) <> 0jump token-overflow-done))
    (dw (dup lit ,(char->integer #\tab) <> 0jump token-overflow-done))
    (dw (drop jump token-discard-overflow))
    (label token-overflow-done)
    (dw (jump token-done))

    (label token-done)
    (dw (drop))
    (dw (lit 0 lit token-ptr @ c!))
    (dw (lit token-ptr @ lit token-buffer-data -))
    (dw (lit token-buffer-data dup lit token-ptr !))
    (dw (swap exit))
    ;; Couldn't get a token, return 0.
    (label empty-token)
    (dw (lit 0 exit))

    ,@(defword "CHAR" 0 'char)
    (dw (token ?dup 0jump char-no-name drop c@ exit))
    (label char-no-name)
    (dw (lit #xfff0 throw))

    ,@(defword "[CHAR]" immediate 'char-brac)
    (dw (tick lit comma char comma exit))
    ))

(define forth-semantics-words
  `(,@(defcode "LIT" 0 'lit)
    (push bc)
    (ld a (de))
    (ld c a)
    (inc de)
    (ld a (de))
    (ld b a)
    (inc de)
    ,@next

    ,@(defword "LITERAL" immediate 'literal)
    (dw (tick lit comma comma exit))

    ;; Parse a dictionary name or raise the standard undefined-word exception.
    ,@(defword "(PARSE-HEADER)" hidden 'parse-header)
    (dw (token ?dup 0jump parse-header-undefined))
    (dw (find-header ?dup 0jump parse-header-undefined exit))
    (label parse-header-undefined)
    (dw (lit #xfff3 throw))

    ,@(defword "POSTPONE" immediate 'postpone)
    (dw (parse-header dup ?immediate 0jump postpone-non-immediate))
    ;; An immediate word's compilation semantics are performed by compiling
    ;; its execution token directly into the current definition.
    (dw (>cfa comma exit))
    (label postpone-non-immediate)
    ;; A non-immediate word's compilation semantics append its xt later,
    ;; when the definition containing POSTPONE executes.
    (dw (>cfa tick lit comma comma tick comma comma exit))

    ,@(defword "[']" immediate 'tick-brac)
    (dw (run-tick tick tick comma comma exit))

    ,@(defcode "LITSTRING" 0 'litstring)
    (ld a (de))
    (ld l a)
    (inc de)
    (ld a (de))
    (ld h a)
    (inc de)
    (push bc)
    (push de)
    ,@hl-to-bc
    (add hl de)
    (inc hl)
    (ex de hl)
    ,@next

    ,@(defword "S\"" immediate 's-quote)
    (dw (state @ 0jump s-quote-interpret))
    (dw (tick litstring comma here lit 0 comma))
    (label s-quote-compile-loop)
    (dw (getc dup 0jump s-quote-compile-eof))
    (dw (dup lit 34 <> 0jump s-quote-compile-done))
    (dw (c-comma jump s-quote-compile-loop))
    (label s-quote-compile-done)
    (dw (drop lit 0 c-comma dup here swap - lit 3 - swap ! exit))
    (label s-quote-compile-eof)
    (dw (drop lit #xffee throw))

    (label s-quote-interpret)
    (dw (here))
    (label s-quote-interpret-loop)
    (dw (getc dup 0jump s-quote-interpret-eof))
    (dw (dup lit 34 <> 0jump s-quote-interpret-done))
    (dw (over c! 1+ jump s-quote-interpret-loop))
    (label s-quote-interpret-done)
    (dw (drop here - here swap exit))
    (label s-quote-interpret-eof)
    (dw (2drop lit #xffee throw))

    ,@(defword ".\"" immediate 'dot-quote)
    (dw (state @ 0jump dot-quote-interpret))
    (dw (s-quote tick type comma exit))
    (label dot-quote-interpret)
    (dw (getc dup 0jump dot-quote-eof))
    (dw (dup lit 34 <> 0jump dot-quote-done emit jump dot-quote-interpret))
    (label dot-quote-done)
    (dw (drop exit))
    (label dot-quote-eof)
    (dw (drop lit #xffee throw))

    ;; Run-time and compilation semantics for the standard ABORT" word.
    ,@(defword "(ABORT\")" hidden 'abort-quote-runtime)
    (dw (rot 0jump abort-quote-false type lit #xfffe throw))
    (label abort-quote-false)
    (dw (2drop exit))

    ,@(defword "ABORT\"" immediate 'abort-quote)
    (dw (s-quote tick abort-quote-runtime comma exit))

    ;; ( -- )
    ;; Exit from the current word.
    ,@(defcode "EXIT" 0 'exit)
    ,@pop-de-rs
    ,@next

    ;; ( addr -- )
    ;; Execute code at address ADDR.
    ,@(defcode "EXECUTE" 0 'execute)
    ,@bc-to-hl
    (pop bc)
    (jp (hl))
    ,@next

    ,@(defword "RECURSE" immediate 'recurse)
    (dw (latest @ >cfa comma exit))

    ,@(defword "CATCH" 0 'catch)
    (dw (sp@ >r handler @ >r rp@ handler ! execute r> handler ! r> drop))
    (dw (lit 0 exit))

    ,@(defword "THROW" 0 'throw)
    (dw (?dup 0jump throw-zero))
    (dw (handler @ ?dup 0jump throw-uncaught))
    (dw (rp! r> handler ! r> swap >r sp! drop r> exit))
    (label throw-uncaught)
    ;; ANS leaves an uncaught exception's implementation-defined reporting to
    ;; the system, but it must not restore a return stack through address zero.
    (dw (sp0 @ sp! quit))
    (label throw-zero)
    (dw (exit))

    ;; Internal dictionary lookup for a private NUL-terminated token.
    ;; Return its header address rather than its execution token so compiler
    ;; internals can inspect the header flags.
    ;; ( c-addr -- header | 0 )
    ,@(defcode "FIND-HEADER" hidden 'find-header)
    (pop bc)
    (push de)
    ,@bc-to-hl
    (ld de (var-latest))
    (inc de)
    (inc de)
    (inc de)
    (label find-loop)
    (call strcmp)
    (jp z find-succeed)
    (jp nz find-retry)

    (label find-succeed)
    (dec de)
    (ld a (de))
    (bit 6 a)
    (jp nz find-succ-hidden)
    (dec de)
    (dec de)
    (pop hl)
    (ex de hl)
    ,@hl-to-bc
    ,@next

    (label find-retry)
    (dec de)

    (label find-succ-hidden)
    (dec de)
    (dec de)
    (push hl)
    (ld a (de))
    (ld l a)
    (inc de)
    (ld a (de))
    (ld h a)
    (dec de)
    (ld a l)
    (or a)
    (jp z find-maybe-fail)

    (label find-retry-cont)
    (inc hl)
    (inc hl)
    (inc hl)
    (ex de hl)
    (pop hl)
    (jp find-loop)

    (label find-maybe-fail)
    (ld a h)
    (cp 0)
    (jp z find-fail)
    (jp nz find-retry-cont)

    (label find-fail)
    (pop hl)
    (pop de)
    (jp fal)

    (label strcmp)
    (push hl)
    (push de)
    (label strcmp-loop)
    (ld a (de))
    (or a)
    (jr z strcmp-end)
    (cp (hl))
    (jr nz strcmp-exit)
    (inc hl)
    (inc de)
    (jr strcmp-loop)

    (label strcmp-end)
    (ld a (hl))
    (or a)
    (label strcmp-exit)
    (ccf)
    (pop de)
    (pop hl)
    (ret)

    ;; Standard counted-string dictionary lookup.
    ;; ( c-addr -- c-addr 0 | xt 1 | xt -1 )
    ,@(defcode "FIND" 0 'find)
    (ld (var-temp-cell) de)
    ;; Retain the original counted-string address for the failure result.
    (push bc)
    (ld de (var-latest))
    (label find-counted-loop)
    (ld a d)
    (or e)
    (jr z find-counted-fail)
    (push de)                        ; candidate header
    (inc de)
    (inc de)
    (ld a (de))                     ; length and flags
    (bit 6 a)
    (jr nz find-counted-retry)
    (and 31)
    ;; Fetch the original input address without removing it from the data
    ;; stack, then retain the candidate header for retry/success handling.
    (pop de)
    (pop hl)
    (push hl)
    (push de)
    (cp (hl))
    (jr nz find-counted-retry)
    (ld b a)
    (inc hl)                         ; input characters
    (inc de)
    (inc de)
    (inc de)                         ; dictionary characters
    (ld a b)
    (or a)
    (jr z find-counted-succeed)
    (label find-counted-compare)
    (ld a (de))
    (cp (hl))
    (jr nz find-counted-retry)
    (inc de)
    (inc hl)
    (djnz find-counted-compare)
    (label find-counted-succeed)
    (pop hl)                         ; header
    (pop bc)                         ; discard original c-addr
    (inc hl)
    (inc hl)
    (ld a (hl))                     ; retain flags in C
    (ld c a)
    (and 31)
    (ld e a)
    (ld d 0)
    (inc hl)                         ; first name character
    (add hl de)
    (inc hl)                         ; skip private name terminator
    (push hl)                        ; xt is below the flag result
    (bit 7 c)
    (jr nz find-counted-immediate)
    (ld bc 65535)
    (jr find-counted-done)
    (label find-counted-immediate)
    (ld bc 1)
    (label find-counted-done)
    (ld de (var-temp-cell))
    ,@next

    (label find-counted-retry)
    (pop de)                         ; candidate header
    (ld a (de))
    (ld l a)
    (inc de)
    (ld a (de))
    (ld h a)
    (ex de hl)                       ; previous header link
    (jr find-counted-loop)

    (label find-counted-fail)
    ;; The original c-addr is already below the zero result.
    (ld bc 0)
    (ld de (var-temp-cell))
    ,@next

    ;; Not standard compilant.  Doesn't conform to run-time behavior.
    ;; Exactly the same as LIT
    ,@(defcode "(')" 0 'tick)
    (push bc)
    (ld a (de))
    (ld c a)
    (inc de)
    (ld a (de))
    (ld b a)
    (inc de)
    ,@next

    ;; Correct implementation of tick.
    ,@(defword "'" 0 'run-tick)
    (dw (parse-header >cfa exit))

    ,@(defcode "," 0 'comma)
    (call _comma)
    (pop bc)
    ,@next

    (label _comma)
    (push de)
    (ld hl (var-dp))
    (ld (hl) c)
    (inc hl)
    (ld (hl) b)
    (inc hl)
    (ld de var-dp)
    (ex de hl)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (pop de)
    (ret)

    ,@(defcode "C," 0 'c-comma)
    (call _c-comma)
    (pop bc)
    ,@next

    (label _c-comma)
    (push de)
    (ld hl (var-dp))
    (ld (hl) c)
    (inc hl)
    (ld de var-dp)
    (ex de hl)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (pop de)
    (ret)

    ,@(defcode "?IMMEDIATE" 0 '?immediate)
    (inc bc)
    (inc bc)
    (ld a (bc))
    (bit 7 a)
    (jp z fal)
    (jp tru)

    ,@(defcode "IMMEDIATE" 0 'immed)
    (ld hl (var-latest))
    (inc hl)
    (inc hl)
    (ld a 128)
    (or (hl))
    (ld (hl) a)
    ,@next

    ,@(defcode ">CFA" 0 '>cfa)
    ;; DE is the threaded instruction pointer.  Preserve it while using a
    ;; temporary DE pair for the name-length offset.
    (push de)
    (inc bc)
    (inc bc)
    (ld a (bc))
    ;; len-mask
    (and 31)
    (ld h 0)
    (ld l a)
    (inc bc)
    (add hl bc)
    (inc hl)
    ,@hl-to-bc
    (pop de)
    ,@next

    ,@(defword ">DFA" 0 '>dfa)
    (dw (>cfa lit 3 + exit))

    ;; Standard >BODY consumes an execution token, not an internal header.
    ;; Created definitions use a three-byte CALL code field.
    ,@(defword ">BODY" 0 '>body)
    (dw (lit 3 + exit))

    ,@(defword "CFA>" 0 'cfa>)
    (dw (latest @ ?dup 0branch 22 2dup swap))
    (dw (< 0branch 6 nip exit @ branch ,(- 65536 24) drop))
    (dw (lit 0 exit))

    ,@(defword "PICK" 0 'pick)
    (dw (1+ 2* sp@ + @ exit))

    ;; ( name length -- )
    ;; Parse a name and create a definition header for it.

    ,@(defcode "CREATE_" hidden 'create_)
    ;; Header flags reserve only five bits for the name length.  TOKEN already
    ;; enforces this, but keep the primitive safe for internal direct callers.
    (ld a b)
    (or a)
    (jr nz create-name-too-long)
    (ld a c)
    (cp 32)
    (jr nc create-name-too-long)
    (ld hl (var-dp))
    (ld (var-temp-cell) de)
    (ld de (var-latest))
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (dec hl)

    (ld de var-latest)
    (ld a l)
    (ld (de) a)
    (inc de)
    (ld a h)
    (ld (de) a)

    (inc hl)
    (inc hl)
    (ld a c)
    (ld (hl) a)
    (inc hl)

    (ex de hl)
    (pop hl)
    (ld b 0)
    (ldir)

    (xor a)
    (ld (de) a)
    (inc de)

    (ld hl var-dp)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (ld de (var-temp-cell))
    (pop bc)
    (push de)
    ;; Write the CALL DOCOL instruction.
    (ld de (var-dp))
    (ld a #xcd)
    (ld (de) a)
    (inc de)
    (ld hl docol)

    (ld a l)
    (ld (de) a)
    (inc de)
    (ld a h)
    (ld (de) a)
    (inc de)
    (ld hl var-dp)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (pop de)
    ,@next

    (label create-name-too-long)
    (pop hl)                         ; name address
    (pop bc)                         ; previous data-stack item
    ,@next

    ;; Replace the CALL DOCOL code field made by CREATE_ with CALL DOVAR.
    ;; ( header -- )
    ,@(defcode "MAKE-DOVAR" hidden 'make-dovar)
    (ld (var-temp-cell) de)
    ,@bc-to-hl
    (inc hl)
    (inc hl)
    (ld a (hl))
    (and 31)
    (ld e a)
    (ld d 0)
    (inc hl)
    (add hl de)
    (inc hl)
    (ld (hl) #xcd)
    (inc hl)
    (ld de dovar)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (ld de (var-temp-cell))
    (pop bc)
    ,@next

    ,@(defword "CREATE" 0 'create)
    (dw (token ?dup 0jump create-no-name))
    (dw (dup lit 32 u< 0jump create-name-too-long-forth))
    (dw (create_ latest @ make-dovar exit))
    (label create-name-too-long-forth)
    (dw (2drop exit))
    (label create-no-name)
    (dw (exit))

    ,@(defcode "HIDDEN" 0 'hidden)
    ,@bc-to-hl
    (inc hl)
    (inc hl)
    (ld a 64)
    (xor (hl))
    (ld (hl) a)
    (pop bc)
    ,@next

    ,@(defcode "?HIDDEN" 0 '?hidden)
    ,@bc-to-hl
    (inc hl)
    (inc hl)
    (bit 6 (hl))
    (jp nz tru)
    (jp fal)

    ;; STATE is 0 while interpreting.
    ,@(defcode "[" immediate 'lbrac)
    (ld hl var-state)
    (ld (hl) 0)
    (inc hl)
    (ld (hl) 0)
    ,@next

    ;; STATE is 1 while compiling.
    ,@(defcode "]" 0 'rbrac)
    (ld hl var-state)
    (ld (hl) 1)
    (inc hl)
    (ld (hl) 0)
    ,@next

    ,@(defword ":" 0 'colon)
    ;; A fresh definition cannot inherit loop fixups from an abandoned
    ;; compilation (for example, one terminated by ABORT).
    (dw (lit 0 lit loop-compile-depth ! token ?dup 0jump colon-no-name))
    (dw (dup lit 32 u< 0jump colon-name-too-long))
    (dw (dp @ lit compile-start-dp ! latest @ lit compile-start-latest !))
    (dw (create_ latest @))
    (dw (hidden rbrac exit))
    (label colon-name-too-long)
    (dw (2drop lbrac exit))
    (label colon-no-name)
    (dw (lbrac exit))

    ,@(defword ";" immediate 'semicolon)
    (dw (lit exit comma))
    (dw (latest @ hidden))
    (dw (lit 0 lit compile-start-dp ! lbrac exit))

    ,@(defword "CONSTANT" 0 'constant)
    (dw (token ?dup 0jump constant-no-name))
    (dw (dup lit 32 u< 0jump constant-name-too-long))
    (dw (create_))
    (dw (tick lit comma comma tick exit comma exit))
    (label constant-name-too-long)
    (dw (2drop drop exit))
    (label constant-no-name)
    (dw (drop exit))

    ,@(defword "VALUE" 0 'value)
    (dw (token ?dup 0jump value-no-name))
    (dw (dup lit 32 u< 0jump value-name-too-long))
    (dw (create_))
    (dw (tick lit comma comma tick exit comma exit))
    (label value-name-too-long)
    (dw (2drop drop exit))
    (label value-no-name)
    (dw (drop exit))

    ,@(defword "TO" immediate 'to)
    (dw (parse-header >dfa cell+ state @ 0branch 20 tick lit comma comma))
    (dw (tick ! comma branch 4 ! exit))

    ,@(defword "+TO" immediate '+to)
    (dw (parse-header >dfa cell+ state @ 0branch 20 tick lit comma comma))
    (dw (tick +! comma branch 4 +! exit))

    ,@(defcode "(DOES>)" 0 'does-brac)
    (push bc)
    (ld bc (var-latest))
    (inc bc)
    (inc bc)
    (ld a (bc))
    (and 31)
    (ld h 0)
    (ld l a)
    (inc bc)
    (add hl bc)
    (inc hl)
    (inc hl)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (pop bc)
    ,@pop-de-rs
    ,@next

    ,@(defcode "DOES>" immediate 'does>)
    (push de)
    (ld de (var-dp))
    (ld hl does-brac)
    (ld a l)
    (ld (de) a)
    (inc de)
    (ld a h)
    (ld (de) a)
    (inc de)
    (ld a #xcd)
    (ld (de) a)
    (inc de)

    (ld hl dodoes)
    (ld a l)
    (ld (de) a)
    (inc de)
    (ld a h)
    (ld (de) a)
    (inc de)

    (ld hl var-dp)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (pop de)
    ,@next

    (label dodoes)
    ,@push-de-rs
    (pop de)
    ,@bc-to-hl
    (pop bc)
    (push hl)
    ,@next

    ,@(defword "DEFAULT-EOF" 0 'default-eof)
    (dw (page lit quit-eof-msg plot-string pause poweroff))

    ;; Reset dictionary/compiler state shared by successful input, premature
    ;; source end, and interpreter errors.  Return-stack reset stays in QUIT
    ;; itself so this helper can return normally.
    ,@(defword "(QUIT-CLEANUP)" hidden 'quit-cleanup)
    ;; Roll back an incomplete colon definition before accepting new input.
    ;; A normal semicolon clears the marker; errors and premature source end
    ;; leave it set to the dictionary state captured by colon.
    (dw (lit compile-start-dp @ ?dup 0jump quit-no-rollback))
    (dw (lit compile-start-latest @ latest ! dp !))
    (dw (lit 0 lit compile-start-dp !))
    (label quit-no-rollback)
    (dw (lit 0 state ! lit 0 handler ! lit 0 lit loop-compile-depth ! exit))

    ,@(defword "QUIT" 0 'quit)
    (label try-more)
    ;; QUIT establishes the outer interpreter's clean control state.  In
    ;; particular, no CATCH frame or compiler context may survive a return to
    ;; the command loop.  The cleanup belongs at the shared loop head so it is
    ;; also applied after each completed input source.
    (dw (quit-cleanup r0 @ rp!))
    (dw (lit ok-msg plot-string cr))
    (label refill-more)
    (dw (refill 0jump quit-eof))
    (dw (interpret))
    (dw (?dup 0= 0jump not-ok))
    ;; An unfinished colon definition spans input sources.  Preserve STATE,
    ;; its transaction marker, and compiler context until semicolon or error.
    ;; The marker, rather than STATE, also covers a line ending inside [ ... ].
    (label interpret-succeeded)
    (dw (lit compile-start-dp @ 0jump try-more jump refill-more))


    (label quit-eof)
    ;; A transient input device may use EOF to select a new source.  Its
    ;; handler returns after doing so, and the original interpreter loop
    ;; consumes that source without nesting QUIT.
    (dw (quit-cleanup r0 @ rp!))
    (dw (lit var-current-eof-handler @ execute interpret))
    (dw (?dup 0= 0jump not-ok))
    (dw (jump interpret-succeeded))

    (label not-ok)
    ;; Preserve the status across the ABORT-like data-stack reset, report it,
    ;; then use the common cleanup/rollback path before reading another line.
    (dw (lit var-temp-cell ! sp0 @ sp! lit var-temp-cell @))
    (dw (lit var-current-error-handler @ execute quit-cleanup r0 @ rp!))
    (dw (jump refill-more))

    (label ok-msg)
    (db ,(string " ok"))
    (label not-ok-msg)
    (db ,(string "?"))
    (label quit-eof-msg)
    (db ,(string "Received EOF from input device."))

    ,@(defword "ABORT" 0 'abort)
    ;; ABORT is the standard -1 exception when caught; an uncaught THROW
    ;; below performs the required data-stack reset and outer QUIT.
    (dw (lit #xffff throw exit))

    ,@(defword "INTERPRET" 0 'interpret)

    (label interpret-loop)
    (dw (token ?dup 0jump interpret-done))
    (dw (find-header ?dup 0jump maybe-number))
    (dw (state @ 0jump interpret-word))

    (label compiling-word)
    (dw (dup ?immediate 0jump compile-word))

    (label interpret-word)
    (dw (>cfa execute jump interpret-loop))

    (label compile-word)
    (dw (>cfa comma jump interpret-loop))


    (label maybe-number)
    (dw (lit token-buffer-data parse-number))
    (dw (num-status @ 0jump num-fail))
    ;; Read a number.
    ;; If we're interpreting, just keep the number on the satck.
    (dw (state @ 0jump interpret-loop))
    ;; Otherwise we compile LIT and the number.
    (label compile-num)
    (dw (lit lit comma comma jump interpret-loop))

    ;; Failed to read a number.
    (label num-fail)
    (dw (jump undefined-word))

    (label interpret-done)
    (dw (lit 0 exit))

    (label undefined-word)
    (dw (lit 1 exit))

    ;; Interpret a bounded string while preserving and restoring the complete
    ;; caller input source, even if the evaluated text throws.
    ,@(defword "(EVALUATE)" hidden 'evaluate-inner)
    (dw (source-store interpret ?dup 0jump evaluate-inner-done))
    (dw (drop lit #xfff3 throw))
    (label evaluate-inner-done)
    (dw (exit))

    ,@(defword "EVALUATE" 0 'evaluate)
    (dw (lit 1 lit var-evaluation-depth +!))
    (dw (source lit var-to-in @ >r 2>r tick evaluate-inner catch))
    (dw (lit #xffff lit var-evaluation-depth +!))
    ;; CATCH left the exception code on the data stack.  Recover the old
    ;; { address, length, >IN }, reinstall it, then rethrow (zero is a no-op).
    (dw (2r> r> >r rot >r source-store r> r> lit var-to-in ! throw exit))

    ,@(defword "ID." 0 'id.)
    (dw (lit 3 + plot-string exit))



    ))

(define forth-control-words
  `(;; Runtime setup for DO.  The return stack layout remains
    ;; { limit, index }, so I and J keep their existing offsets.
    ,@(defcode "(DO)" hidden 'do-runtime)
    (pop hl)
    ,@push-bc-rs
    ,@push-hl-rs
    (pop bc)
    ,@next

    ;; Add the signed increment to the loop index and report whether it
    ;; crossed the limit.  For a positive step, (new-limit) < step; for a
    ;; negative step, (limit-new) < -step.  These unsigned modular tests also
    ;; preserve DO's equal-bounds wrap behavior.
    ,@(defcode "(+LOOP)" hidden 'plus-loop-runtime)
    (push de)
    (ld l (+ ix 2))
    (ld h (+ ix 3))
    (add hl bc)
    (ld (+ ix 2) l)
    (ld (+ ix 3) h)
    (bit 7 b)
    (jr nz plus-loop-negative)

    (ld e (+ ix 0))
    (ld d (+ ix 1))
    (or a)
    (sbc hl de)
    (call cp-hl-bc)
    (jr c plus-loop-crossed)
    (jr plus-loop-continue)

    (label plus-loop-negative)
    (ld e (+ ix 0))
    (ld d (+ ix 1))
    (ex de hl)
    (or a)
    (sbc hl de)
    (ld d b)
    (ld e c)
    (ld a e)
    (cpl)
    (ld e a)
    (ld a d)
    (cpl)
    (ld d a)
    (inc de)
    (call cp-hl-de)
    (jr nc plus-loop-continue)

    (label plus-loop-crossed)
    (inc ix)
    (inc ix)
    (inc ix)
    (inc ix)
    (pop de)
    (jp tru)

    (label plus-loop-continue)
    (pop de)
    (jp fal)

    ,@(defcode "(UNLOOP)" hidden 'loop-cleanup)
    (inc ix)
    (inc ix)
    (inc ix)
    (inc ix)
    ,@next

    ,@(defword "IF" immediate 'if)
    (dw (tick 0branch comma here lit 0 comma exit))

    ,@(defword "THEN" immediate 'then)
    (dw (dup here swap - swap ! exit))

    ,@(defword "ELSE" immediate 'else)
    (dw (tick branch comma here lit 0 comma swap dup))
    (dw (here swap - swap ! exit))

    ,@(defword "BEGIN" immediate 'begin)
    (dw (here exit))

    ,@(defword "UNTIL" immediate 'until)
    (dw (tick 0branch comma here - comma exit))

    ,@(defword "AGAIN" immediate 'again)
    (dw (tick branch comma here - comma exit))

    ,@(defword "WHILE" immediate 'while)
    (dw (tick 0branch comma here lit 0 comma exit))

    ,@(defword "ALLOT" 0 'allot)
    (dw (dp +! exit))

    ,@(defword "VARIABLE" 0 'variable)
    (dw (token ?dup 0jump variable-no-name))
    (dw (dup lit 32 u< 0jump variable-name-too-long))
    (dw (create_ latest @ make-dovar))
    (dw (lit 0 comma exit))
    (label variable-name-too-long)
    (dw (2drop exit))
    (label variable-no-name)
    (dw (exit))

    ,@(defword "REPEAT" immediate 'repeat)
    (dw (tick branch comma swap here - comma))
    (dw (dup here swap - swap ! exit))

    ;; Loop compiler contexts live outside the data-stack control-flow
    ;; entries used by IF/THEN.  That lets LEAVE appear inside conditionals
    ;; and supports nested loops.  A context is
    ;; { loop-start, leave-head }.
    ,@(defword "DO" immediate 'do)
    (dw (lit loop-compile-depth @ lit 16 u< 0jump do-too-deep))
    (dw (lit loop-compile-depth @ lit 4 * lit loop-compile-contexts +))
    (dw (tick do-runtime comma))
    (dw (here over ! lit 0 over lit 2 + ! drop))
    (dw (lit 1 lit loop-compile-depth +! exit))
    (label do-too-deep)
    (dw (abort))

    ;; Complete the current loop and patch its linked list of LEAVEs.
    ,@(defword "(END-LOOP)" hidden 'end-loop)
    (dw (lit loop-compile-depth @ ?dup 0jump end-loop-no-loop))
    (dw (1- lit 4 * lit loop-compile-contexts +))
    (dw (tick plus-loop-runtime comma tick 0branch comma dup @ here - comma))
    (dw (dup lit 2 + @))
    (label end-loop-patch-leaves)
    (dw (?dup 0jump end-loop-patches-done))
    (dw (dup @ swap dup here swap - swap ! jump end-loop-patch-leaves))
    (label end-loop-patches-done)
    (dw (drop lit 1 lit loop-compile-depth -! exit))
    (label end-loop-no-loop)
    (dw (abort))

    ,@(defword "LOOP" immediate 'loop)
    (dw (tick lit comma lit 1 comma end-loop exit))

    ,@(defword "+LOOP" immediate '+loop)
    (dw (end-loop exit))

    ,@(defword "LEAVE" immediate 'leave)
    (dw (lit loop-compile-depth @ ?dup 0jump leave-no-loop))
    (dw (1- lit 4 * lit loop-compile-contexts +))
    (dw (tick loop-cleanup comma tick branch comma here over lit 2 + @ comma))
    (dw (swap lit 2 + ! exit))
    (label leave-no-loop)
    (dw (abort))

    ,@(defword "CASE" immediate 'case)
    (dw (lit 0 exit))

    ,@(defword "OF" immediate 'of)
    (dw (tick over comma tick = comma if tick drop comma exit))

    ,@(defword "ENDOF" immediate 'endof)
    (dw (else exit))

    ,@(defword "ENDCASE" immediate 'endcase)
    (dw (tick drop comma ?dup 0branch 8 then branch ,(- 65536 10) exit))

    ,@(defword "FORGET" 0 'forget)
    (dw (parse-header dup @ latest ! dp ! exit))

    ,@(defcode "I" 0 'curr-loop-index)
    (push bc)
    (ld c (+ ix 2))
    (ld b (+ ix 3))
    ,@next

    ,@(defcode "J" 0 'curr-loop-index2)
    (push bc)
    (ld c (+ ix 6))
    (ld b (+ ix 7))
    ,@next))

(define forth-shared-header
  `(
    ,reset-link
    ,reset-var
    (ld de main)
    (ld ix #xc000)
    (ld sp 65532)
    ,@next
    ,@next-sub

    ,@docol-sub
    ,@dovar-sub

    (label tru)
    (ld bc 65535)
    ,@next

    (label fal)
    (ld bc 0)
    ,@next

    ))

(define forth-meta-words
  `(,@(defword "PAUSE" 0 'pause)
    (dw (key drop exit))

    ;; Pictured numeric output uses a private descending buffer at the end of
    ;; PROMPT-SPACE.  HLD is separate from TEMP-CELL because UM/MOD uses the
    ;; latter while # is formatting a digit.
    ,@(defword "<#" 0 'less-hash)
    (dw (lit prompt-space lit 128 + hld ! exit))

    ,@(defword "HOLD" 0 'hold)
    (dw (lit prompt-space hld @ u< 0jump hold-overflow))
    (dw (lit 1 hld -! hld @ c! exit))
    (label hold-overflow)
    (dw (lit #xffef throw))

    ,@(defword "SIGN" 0 'sign)
    (dw (0< 0jump sign-done lit 45 hold))
    (label sign-done)
    (dw (exit))

    ,@(defword "DIGIT>CHAR" hidden 'digit>char)
    (dw (dup lit 10 u< 0jump digit-to-letter lit 48 + exit))
    (label digit-to-letter)
    (dw (lit 10 - lit 65 + exit))

    ,@(defword "#" 0 'hash)
    ;; Divide the high cell first, then use its remainder as the high half of
    ;; the low-cell division.  This produces one base digit and a new double.
    (dw (lit 0 swap base @ um/mod -rot base @ um/mod))
    (dw (swap digit>char hold swap exit))

    ,@(defword "#S" 0 'hashes)
    (label hashes-loop)
    (dw (hash 2dup or 0jump hashes-done jump hashes-loop))
    (label hashes-done)
    (dw (exit))

    ,@(defword "#>" 0 'hash-greater)
    (dw (2drop hld @ lit prompt-space lit 128 + over - exit))

    ;; Add one digit to an unsigned double after multiplying it by BASE.
    ;; ( ud digit -- ud' )
    ,@(defword "D*BASE+" hidden 'd*base+)
    (dw (>r swap base @ um* rot base @ * + r> >r swap r@ +))
    (dw (dup r@ u< negate rot + rdrop exit))

    ,@(defword ">NUMBER" 0 '>number)
    (label to-number-loop)
    (dw (dup 0jump to-number-done over c@ digit?))
    (dw (0jump to-number-invalid dup base @ u< 0jump to-number-invalid))
    (dw (-rot 2>r d*base+ 2r> 1- swap 1+ swap jump to-number-loop))
    (label to-number-invalid)
    (dw (drop))
    (label to-number-done)
    (dw (exit))

    ;; ( num -- )
    ,@(defword "(U.)" hidden 'u._)
    (dw (lit 0 less-hash hashes hash-greater type exit))

    ,@(defword "U." 0 'u.)
    (dw (u._ space exit))

    ,@(defword "." 0 '.)
    (dw (dup 0< 0jump dot-positive lit 45 emit negate))
    (label dot-positive)
    (dw (u._ space exit))

    ,@(defword "UWIDTH" 0 'uwidth)
    (dw (base @ u-divmod nip ?dup 0branch 10 uwidth 1+ branch 6))
    (dw (lit 1 exit))

    ,@(defword "SPACE" 0 'space)
    (dw (lit ,(char->integer #\space) emit exit))

    ,@(defword "SPACES" 0 'spaces)
    ;; A negative count and zero both emit no characters.  Testing the sign
    ;; bit directly avoids depending on the current signed-comparison words.
    (dw (dup 0= 0jump spaces-check-sign drop exit))
    (label spaces-check-sign)
    (dw (dup lit #x8000 and 0jump spaces-loop drop exit))
    (label spaces-loop)
    (dw (space 1- ?dup 0jump spaces-done jump spaces-loop))
    (label spaces-done)
    (dw (exit))

    ,@(defword "U.R" 0 'u.r)
    (dw (swap dup uwidth rot swap - spaces u._ exit))

    ,@(defword "DEPTH" 0 'depth)
    (dw (sp0 @ sp@ - 2- 2/ exit))

    ,@(defword ".S" 0 '.s)
    (dw (lit ,(char->integer #\<) emit depth u._))
    (dw (lit ,(char->integer #\>) emit space))
    (dw (sp@ dup sp0 @ < 0branch 18 dup @))
    (dw (u. lit 2 + branch 65510 drop exit))

    ,@(defword "HEX" 0 'hex)
    (dw (lit 16 base ! exit))

    ,@(defword "DECIMAL" 0 'dec)
    (dw (lit 10 base ! exit))


    ))

;; ANS permits a system to report every environmental attribute as
;; unknown.  Keep this minimal implementation independent of the normal
;; dictionary search order; individual stable queries can be added later.
(define forth-environment-words
  `(,@(defword "ENVIRONMENT?" 0 'environment-query)
    (dw (2drop false exit))))

(define forth-misc-words
  `(;; Shut down the calculator.
    ,@(defcode "POWEROFF" 0 'poweroff)
    (jp shutdown)

    ,@(defcode "ERASE-SECTOR" 0 'erase-sector-forth)
    (ld a c)
    (di)
    (call unlock-flash)
    (call erase-flash-sector)
    (call lock-flash)
    (ei)
    ,@next

    ;; Enable interrupts
    ,@(defcode "ENABLE-INTERRUPTS" 0 'enable-interrupts)
    (ei)
    ,@next

    ;; Disable interrupts
    ,@(defcode "DISABLE-INTERRUPTS" 0 'disable-interrupts)
    (di)
    ,@next

    ;; Interrupt mode 1
    ,@(defcode "IM1" 0 'im1)
    (im 1)
    ,@next

    ;; Interrupt mode 2
    ,@(defcode "IM2" 0 'im2)
    (im 2)
    ,@next

    ;; Dummy
    ,@(defword "FOO" 0 'foo)
    (dw (lit ddd exit))

    (label ddd)
    (ex af afs)
    (exx)

    (ld iy #x8100)
    (ld de ddd-data)
    (ld a (de))
    (cp 10)
    (jp nc ddd-too-long)
    (jp ddd-cont)
    (label ddd-too-long)
    (ld a 0)
    (label ddd-cont)
    (ld b 2)
    (ld c a)
    (inc a)
    (ld (de) a)

    (ld e 0)
    (ld l 10)
    (call rect-xor)
    (call fast-copy)

    (ex af afs)
    (exx)


    (ret)


    ;; ( addr byte -- )
    ;; Set the current interrupt register to BYTE with routine at ADDR.
    ,@(defcode "SET-INTERRUPT" 0 'set-interrupt)
    (di)
    ;; Write TOS into the interrupt register.
    (ld a c)
    (ld i a)
    (pop hl)
    ;; HL now contains the address of the interrupt service routine
    ;; (ISR).  Save DE and use it as a pointer to write the ISR.
    (push de)

    ;; 0x??3F
    (ld d a)
    (ld e #x3f)
    (ld a l)
    (ld (de) a)
    (inc de)
    (ld a h)
    (ld (de) a)

    ;; 0x??7F
    (ld e #x7f)
    (ld a l)
    (ld (de) a)
    (inc de)
    (ld a h)
    (ld (de) a)

    ;; 0x??BF
    (ld e #xbf)
    (ld a l)
    (ld (de) a)
    (inc de)
    (ld a h)
    (ld (de) a)

    ;; 0x??FF
    (ld e #xff)
    (ld a l)
    (ld (de) a)
    (inc de)
    (ld a h)
    (ld (de) a)

    (pop de)
    (pop bc)
    (im 2)
    (ei)
    ,@next

    ;; ( src dest amount -- )
    ,@(defcode "CMOVE-FLASH" 0 'cmove-flash)
    (ld (var-temp-cell) de)

    (pop de)
    (pop hl)
    (di)
    (call unlock-flash)
    (call write-flash-buffer)
    (call lock-flash)
    (ei)
    (ld de (var-temp-cell))
    (pop bc)

    ,@next

    ;; Set page number loaded at memory bank A.
    ;; Addresses 16#4000 to 16#7fff
    ;; ( n -- flag )
    ,@(defcode "SET-RAM-MEMA" 0 'set-ram-mema)
    (ld a c)
    (cp 8)
    (jp nc invalid-bank-selected)
    ;; We can try loading a RAM page now.
    (di)
    (ld a 64)
    (add a c)
    (out (6) a)
    (ei)
    (ld bc 65535)
    ,@next

    (label invalid-bank-selected)
    (ld bc 0)
    ,@next

    ;; Advance past a counted-string length byte and return its count.
    ;; ( c-addr1 -- c-addr2 u )
    ,@(defcode "COUNT" 0 'count)
    ,@bc-to-hl
    (ld c (hl))
    (ld b 0)
    (inc hl)
    (push hl)
    ,@next

    ,@(defword "SHUTDOWN" 0 'shutdown-forth)
    (dw (pause poweroff exit))

    ,@(defword "(" immediate 'comment)
    (label comment-loop)
    (dw (getc dup 0jump comment-eof lit 41 <> 0jump comment-done jump comment-loop))
    (label comment-eof)
    (dw (drop exit))
    (label comment-done)
    (dw (exit))
    ))

(define (defconst name label val)
  `(,@(defcode name 0 label)
    (push bc)
    (ld bc ,val)
    ,@next)
  )



(define forth-vars
  `(;; The current state (compiling (1) or interpreting (0)).
    ,@(defvar "STATE" 'state 0)
    ;; A pointer to the latest word defined.
    ,@(defvar "LATEST" 'latest 0)
    ;; The data pointer.
    ,@(defvar "DP" 'dp 'dp-start)
    ;; The current column (in pixels) for the cursor.
    ,@(defvar "CUR-COL" 'cur-col 0)
    ;; The current row (in pixels) for the cursor.
    ,@(defvar "CUR-ROW" 'cur-row 0)
    ;; The current numerical base
    ,@(defvar "BASE" 'base 10)
    ;; A temporary cell for making things faster.
    ,@(defvar "TEMP-CELL" 'temp-cell 0)
    ;; Pictured numeric output pointer.
    ,@(defvar "HLD" 'hld 0 hidden)
    ;; Canonical flag reporting whether PARSE-NUMBER succeeded.
    ,@(defvar "NUM-STATUS" 'num-status 0)
    ;; Start of the data stack.
    ,@(defvar "SP0" 'sp0 0)
    ;; Start of the return stack.
    ,@(defvar "R0" 'r0 0)
    ;; Input pointer (used by GETC and UNGETC).
    ,@(defvar "INPUT-PTR" 'input-ptr 0)
    ;; Standard input source state.  >IN is deliberately writable; every
    ;; parser read is derived from SOURCE-ADDRESS + >IN.
    ,@(defvar ">IN" 'to-in 0)
    ,@(defvar "SOURCE-ADDRESS" 'source-address 0 hidden)
    ,@(defvar "SOURCE-LENGTH" 'source-length 0 hidden)
    ;; Nesting depth of EVALUATE, used to give REFILL its required false result
    ;; for string input sources.
    ,@(defvar "EVALUATION-DEPTH" 'evaluation-depth 0 hidden)
    ;; Exception handler.
    ,@(defvar "HANDLER" 'handler 0)
    ;; Number of characters received by EXPECT.
    ,@(defvar "SPAN" 'span 0)
    ;; Optional calculator line-editor history callback.  EDIT-LINE passes
    ;; (addr capacity used direction), where direction is -1/+1 for UP/DOWN.
    ,@(defvar "EDIT-HISTORY" 'edit-history 0)
    ,@(defvar "CURRENT-INPUT-DEVICE" 'current-input-device 0)
    ;; Consumes the nonzero status returned by INTERPRET.
    ,@(defvar "CURRENT-ERROR-HANDLER" 'current-error-handler 0)
    ;; Handles input-device EOF and may arrange the next input source.
    ,@(defvar "CURRENT-EOF-HANDLER" 'current-eof-handler 0)
    ,@(defconst "H0" 'h0 'dp-start)
    ,@(defconst "OS-END" 'os-end-forth 'os-end)
    ,@(defconst "SCREEN-BUF" 'screen-buf 'screen-buffer)
    ,@(defconst "WORD-BUF" 'word-buf 'word-buffer)
    ,@(defconst "TOKEN-BUF" 'token-buf 'token-buffer-data)
    ,@(defconst "PBUF" 'prompt-buf 'prompt-space)
    ,@(defconst "MEMA" 'mema #x4000)
    ,@(defconst "HERE" 'here '(var-dp))
    ,@(defconst "BL" 'bl 32)

    ;; Maximum x and y coordinates that can be drawn on the screen.
    ,@(defconst "MAX-COL" 'max-col 95)
    ,@(defconst "MAX-ROW" 'max-row 63)

    ;; Maximum x and y coordinates that can be drawn on the screen for
    ;; characters.
    ,@(defconst "CHAR-MAX-COL" 'char-max-col 23)
    ,@(defconst "CHAR-MAX-ROW" 'char-max-row 11)

    ))

(define (make-char-lookup-table)
  (define res (make-list 128 0))
  (define (put-char! id char)
    (list-set! res id (char->integer char))
    res)

  (put-char! 47 #\A)
  (put-char! 39 #\B)
  (put-char! 31 #\C)
  (put-char! 46 #\D)
  (put-char! 38 #\E)
  (put-char! 30 #\F)
  (put-char! 22 #\G)
  (put-char! 14 #\H)
  (put-char! 45 #\I)
  (put-char! 37 #\J)
  (put-char! 29 #\K)
  (put-char! 21 #\L)
  (put-char! 13 #\M)
  (put-char! 44 #\N)
  (put-char! 36 #\O)
  (put-char! 28 #\P)
  (put-char! 20 #\Q)
  (put-char! 12 #\R)
  (put-char! 43 #\S)
  (put-char! 35 #\T)
  (put-char! 27 #\U)
  (put-char! 19 #\V)
  (put-char! 11 #\W)
  (put-char! 42 #\X)
  (put-char! 34 #\Y)
  (put-char! 26 #\Z)
  (put-char! 9 #\newline)
  (put-char! 2 #\backspace)
  (put-char! 56 #\backspace)
  (put-char! 33 #\space)
  (put-char! 18 #\@)
  (put-char! 25 #\.)
  (put-char! 53 #\:)
  (put-char! 52 #\;)
  (put-char! 10 #\")
  (put-char! 17 #\?)

  ;; Non-printing editor controls, matching the conventional Emacs bytes.
  (list-set! res 2 2)                 ; LEFT / control-B
  ;; zkeme80's arrow table remaps physical RIGHT to 1 (keyboard.scm).
  (list-set! res 1 6)                 ; RIGHT / control-F
  (list-set! res 4 16)                ; UP / control-P
  (list-set! res 3 14)                ; DOWN / control-N

  ;; Add more characters as you need them.
  res
  )

(define (make-numeric-char-lookup-table)
  (define res (make-list 128 0))
  (define (put-char! id char)
    (list-set! res id (char->integer char))
    res)

  ;; Keys whose unshifted legends are useful in Forth source.
  (put-char! 9 #\newline)
  (put-char! 2 #\backspace)
  (put-char! 56 #\backspace)
  (put-char! 33 #\0)
  (put-char! 34 #\1)
  (put-char! 26 #\2)
  (put-char! 18 #\3)
  (put-char! 35 #\4)
  (put-char! 27 #\5)
  (put-char! 19 #\6)
  (put-char! 36 #\7)
  (put-char! 28 #\8)
  (put-char! 20 #\9)
  (put-char! 10 #\+)
  (put-char! 11 #\-)
  (put-char! 12 #\*)
  (put-char! 13 #\/)
  (put-char! 17 #\-)
  (put-char! 21 #\))
  (put-char! 25 #\.)
  (put-char! 29 #\()
  (put-char! 37 #\,)

  ;; Remaining printable ASCII punctuation.  Together with the direct
  ;; alphabetic table, 2ND exposes every character from space through '~'.
  (put-char! 47 #\!)               ; MATH
  (put-char! 39 #\#)               ; APPS
  (put-char! 31 #\$)               ; PRGM
  (put-char! 46 #\%)               ; reciprocal
  (put-char! 38 #\&)               ; SIN
  (put-char! 30 #\')               ; COS
  (put-char! 23 #\<)               ; VARS
  (put-char! 32 #\=)               ; STAT
  (put-char! 49 #\>)               ; GRAPH
  (put-char! 45 #\[)               ; square
  (put-char! 44 #\\)               ; LOG
  (put-char! 43 #\])               ; LN
  (put-char! 14 #\^)               ; power
  (put-char! 42 #\_)               ; STO
  (put-char! 40 #\`)               ; X,T,theta,n
  (put-char! 50 #\{)               ; TRACE
  (put-char! 52 #\|)               ; WINDOW
  (put-char! 51 #\})               ; ZOOM
  (put-char! 53 #\~)               ; Y=

  ;; Navigation remains navigation after 2ND; begin/end bindings are future
  ;; work, but shifted arrows must never become deletion or text insertion.
  (list-set! res 2 2)                 ; LEFT / control-B
  (list-set! res 1 6)                 ; RIGHT / control-F
  (list-set! res 4 16)                ; UP / control-P
  (list-set! res 3 14)                ; DOWN / control-N

  res)

(define forth-char-lookup-table
  `((label char-lookup-table)
    (db ,(make-char-lookup-table))
    (label numeric-char-lookup-table)
    (db ,(make-numeric-char-lookup-table))))

(define forth-input-devices
  `(;; Example of an input device.
    (label string-input-device)
    (call docol)
    (dw (lit bootstrap-load-bool @ 0jump string-input-eof))
    (dw (lit bootstrap-fs cstring-source))
    (dw (lit 0 lit bootstrap-load-bool ! true exit))
    (label string-input-eof)
    (dw (false exit))

    ;; An input device that should be a prompt.
    ,@(defword "PROMPT" 0 'prompt)
    ;; Reserve one complete output row below the input field.
    (label prompt-make-room)
    (dw (lit var-cur-row @ lit 48 > 0jump prompt-has-room))
    (dw (scroll jump prompt-make-room))
    (label prompt-has-room)
    (dw (lit prompt-prefix plot-string))
    ;; Keep the editor to one display line, then install an exact-length
    ;; source with a private terminator for diagnostics.
    (dw (lit prompt-space lit 20 edit-line))
    (dw (lit prompt-space lit input-buffer span @ cmove))
    (dw (lit 0 lit input-buffer span @ + c!))
    (dw (lit input-buffer span @ source-store))
    (dw (true))
    (dw (exit))
    (label prompt-prefix)
    (db ,(string "> "))))

(define forth-main
  `((label main)
    (dw (origin))
    (dw (lit last-forth-word latest !))
    (dw (lit dp-start dp !))
    (dw (lit 10 base !))
    (dw (lit 0 state !))
    ;; We set the stack pointer two lower because it's changed
    ;; slightly since when we did (ld sp 65532)
    ;; TODO: Fix pre-assigned variable values.
    (dw (lit 65530 sp0 !))
    (dw (lit return-stack-start r0 !))
    (dw (lit 1 lit bootstrap-load-bool !))
    (dw (lit string-input-device lit var-current-input-device !))
    (dw (lit abort lit var-current-error-handler !))
    (dw (lit default-eof lit var-current-eof-handler !))
    (dw (quit))

    (dw (poweroff))

    ))


(define forth-asm
  `(,@forth-shared-header
    ,@forth-semantics-words
    ,@forth-control-words
    ,@forth-text-words
    ,@forth-logic-words
    ,@forth-stack-words
    ,@forth-math-words
    ,@forth-memory-words
    ,@forth-graphics-words
    ,@forth-char-lookup-table
    ,@forth-misc-words
    ,@forth-vars
    ,@forth-meta-words
    ,@forth-environment-words
    ,@forth-input-devices

    ,@forth-main

    (label last-forth-word)
    ;; This needs to be the last word to be defined!
    ,@(defword "STAR" 0 'star)
    (dw (lit 42 emit exit))

    ))
