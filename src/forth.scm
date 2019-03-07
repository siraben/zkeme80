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
(define (defvar name label default)
  ;; Store the list of variable default values.

  (let ((var-label (string->symbol (format #f "var-~a" label)))
        (var-addr (next-var-addr!)))
    (set! *var-list* `((,var-label . ,default) .  ,*var-list*))

    `(,@(defcode name 0 label)
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
    ((ex (sp) hl))
    (push bc)
    ,@hl-to-bc
    ,@next

    ,@(defcode "-ROT" 0 '-rot)
    (ld h b)
    (ld l c)
    (pop bc)
    ((ex (sp) hl))
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
    ((ex (sp) hl))
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
    (srl b)
    (rr c)
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

    ,@(defcode "/MOD" 0 '/mod)
    (ld (var-temp-cell) de)
    (ld d b)
    (ld e c)
    (pop bc)
    (ld a b)
    ;;  ac by de and places the quotient in ac and the remainder in hl
    (label div-ac-by-de)
    (ld	hl 0)
    (ld	b 16)

    (label div-ac-de-loop)
    (db (#xcb #x31)) ;; sll c
    (rla)
    (adc hl hl)
    (sbc hl de)
    (jr	nc div-ac-de-local)
    (add hl de)
    (dec c)
    (label div-ac-de-local)

    (djnz div-ac-de-loop)
    (ld b a)
    (push hl)
    (ld de (var-temp-cell))
    ,@next

    ,@(defword "MOD" 0 'mod)
    (dw (/mod drop exit))

    ,@(defword "/" 0 '/)
    (dw (/mod nip exit))

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
    (dw (lit 1 exit))

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
    (dw (over - >r - r> <= exit))

    ,@(defword "NUM?" 0 'num?)
    (dw (zeroc))
    (dw (ninec))
    (dw (within exit))

    ;; Parse a number starting at an address.
    ;; ( addr -- num | nothing)
    ;; The caller of PARSE-NUMBER should check the variable NUM-STATUS
    ;; to see if the parsing suceeded.

    ;; Currently only parses base 10.
    ,@(defword "PARSE-NUMBER" 0 'parse-number)
    ;; Check if the first character is numeric.
    (dw (dup c@ dup num? 0jump parse-num-fail))
    ;; Convert the first digit.
    (dw (zeroc -))
    ;; It is. Continue until end of string.
    ;; Stack right now: ( addr n -- )
    (label parse-num-continue)
    (dw (swap 1+ swap)) ;; ( addr+1 n -- )
    (label parse-num-loop)
    ;; Check if we reached the end of string already.
    (dw (over c@ 0jump parse-num-done))
    ;; Nope. ( addr+1 n -- )
    ;; Still check if it's numeric.  Since the string shouldn't
    ;; contain spaces, we can just check for the null byte.
    (dw (over c@ num? 0jump parse-num-fail))
    ;; Continue.
    (dw (over c@ zeroc - swap lit 10 * +))
    ;; ( addr+1 nm -- )
    (dw (jump parse-num-continue))

    (label parse-num-done)
    (dw (swap drop))
    (dw (lit 1 num-status ! exit))
    (label parse-num-fail)
    (dw (2drop lit 0 num-status ! exit))

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
  `(,@(defcode "!" 0 '!)
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
    ((ex de hl))
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
    (ldir)
    (ld de (var-temp-cell))
    (pop bc)
    ,@next

    ,@(defcode "CMOVE>" 0 'cmove>)
    (ld (var-temp-cell) de)
    (pop de)
    (pop hl)
    (lddr)
    (ld de (var-temp-cell))
    (pop bc)
    ,@next

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

    ;; Type n characters starting at addr.
    ;; ( addr n -- )
    ,@(defword "TYPE" 0 'type)
    (label type-loop)
    (dw (?dup 0jump type-done))
    (dw (1- swap dup c@ emit 1+ swap jump type-loop))
    (label type-done)
    (dw (drop exit))

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
    ((ex de hl))
    (ld e (hl))
    (inc hl)
    (ld d (hl))
    (dec hl)

    (add hl de)
    ((ex de hl))
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
    (call cp-hl-bc)
    (jp c tru)
    (jp fal)

    ,@(defcode ">" 0 '>)
    (pop hl)
    (push hl)
    (push bc)
    (call cp-hl-bc)
    (pop bc)
    (pop hl)
    (jp nc gt-check-neq)
    (jp fal)
    (label gt-check-neq)
    (call cp-hl-bc)
    (jp z fal)
    (jp tru)


    ,@(defcode "<=" 0 '<=)
    ,@bc-to-hl
    (pop bc)
    (call cp-hl-bc)
    (jp nc tru)
    (jp fal)

    ,@(defcode ">=" 0 '>=)
    (pop hl)
    (call cp-hl-bc)
    (jp nc tru)
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

    ,@(defcode "KEY" 0 'key)
    (call flush-keys)
    (call wait-key)
    (push bc)
    (ld b 0)
    (ld c a)
    ,@next

    ;; Read a key as an ASCII character.
    ,@(defcode "AKEY" 0 'akey)
    (ld (var-temp-cell) de)
    (call flush-keys)
    (call wait-key)
    (ld h 0)
    (ld l a)
    (ld de char-lookup-table)
    (push bc)
    (ld b h)
    (add hl de)
    (ld c (hl))
    (ld de (var-temp-cell))
    ,@next

    ,@(defcode "TO-ASCII" 0 'to-ascii)
    (push de)
    (ld h 0)
    (ld l c)
    (ld b h)
    (ld de char-lookup-table)
    (add hl de)
    (ld c (hl))
    (pop de)
    ,@next

    ,@(defword "ORIGIN" 0 'origin)
    (dw (lit 0 dup cur-col ! cur-row ! exit))


    (label cursor)
    (db (#b11100000))
    (db (#b11100000))
    (db (#b11100000))
    (db (#b11100000))
    (db (#b11100000))

    (label blank)
    (db (0 0 0 0 0))

    ;; ( addr u -- )
    ;; Expect u characters (or a newline, whichever comes first) and
    ;; store them at address addr.
    ;; Written in Forth because it's easier.
    ;; Still somewhat buggy.
    ,@(defword "EXPECT" 0 'expect)
    ;; Store the address and count so we can do various checks.
    (dw (lit expect-count ! dup lit expect-ptr !))
    ;; And the initial pointer.
    (dw (lit expect-ptr-initial !))
    (label expect-loop)
    ;; Check if we have no characters left.
    (dw (lit expect-count @ not 0jump expect-more))
    (dw (exit))
    (label expect-more)
    (dw (page lit expect-ptr-initial @ plot-string))
    (dw (lit 8 cur-row +! lit ,(char->integer #\^) emit))
    (dw (lit 8 cur-row -!))

    ;; Draw a cursor
    ;; (dw (lit cursor lit 5 cur-col @ cur-row @ put-sprite-xor-forth plot))

    (label expect-got-blank)
    (dw (akey))
    (dw (?dup 0jump expect-got-blank))
    (dw (dup lit ,(char->integer #\newline) <> 0jump expect-got-newline))
    (dw (dup lit ,(char->integer #\backspace) <> 0jump expect-got-backspace))
    ;; General case
    (dw (lit expect-ptr @ c!))
    (dw (lit 1 lit expect-ptr +!))
    (dw (lit 1 lit expect-count -!))
    (dw (drop))
    (dw (jump expect-loop))

    (label expect-got-newline)
    (dw (lit expect-ptr-initial @ lit expect-ptr @))
    (dw (<> 0jump expect-got-blank))
    (label expect-end)
    (dw (origin lit expect-ptr-initial @ plot-string))
    (dw (drop lit 0 lit expect-ptr @ c! exit))

    (label expect-got-backspace)
    (dw (drop))
    (dw (lit expect-ptr-initial @ lit expect-ptr @))
    (dw (<> 0jump expect-backspace-to-loop))
    (dw (lit 1 lit expect-ptr -!))
    (dw (lit 0 lit expect-ptr @ c!))
    (dw (lit 1 lit expect-count +!))
    (label expect-backspace-to-loop)
    (dw (drop jump expect-loop))

    ,@(defword "REFILL" 0 'refill)
    (dw (lit var-current-input-device @ execute))
    (dw (0jump refill-fail))
    ;; (dw (lit input-buffer lit var-input-ptr ! ))
    (dw (true exit))
    (label refill-fail)
    (dw (false exit))

    ;; Get the next character from the input source.
    ,@(defword "GETC" 0 'getc)
    (dw (lit var-input-ptr @ c@))
    (dw (lit 1 lit var-input-ptr +!))
    (dw (exit))

    ,@(defword "UNGETC" 0 'ungetc)
    (dw (lit 1 lit var-input-ptr -!))
    (dw (exit))

    ;; Parse the next word.

    ;; We're going off the standard here and will skip whitespace
    ;; (instead of what the stack passed to it), and comments.
    ;; Whitespace includes #\space #\newline
    ;; Comments are #\\ until #\newline
    ;; ( -- addr len )
    ,@(defword "WORD" 0 'word)
    (dw (lit word-buffer lit word-ptr !))
    (dw (lit 0))
    (label skip-space)
    (dw (drop))
    (dw (getc))
    (dw (?dup 0jump empty-word))
    (dw (dup lit ,(char->integer #\space) <>))
    (dw (0jump skip-space))
    (dw (dup lit ,(char->integer #\newline) <>))
    (dw (0jump skip-space))
    (dw (dup lit ,(char->integer #\tab) <>))
    (dw (0jump skip-space))
    (dw (dup lit ,(char->integer #\\) <>))
    (dw (0jump skip-comment))

    (dw (jump actual-word))

    (label skip-comment)
    (dw (drop))
    (dw (getc ?dup 0jump empty-word))
    (dw (dup lit ,(char->integer #\newline) <> 0jump skip-space))
    (dw (jump skip-comment))

    (label actual-word)
    (dw (lit word-ptr @ c!))
    (dw (lit 1 lit word-ptr +!))

    (label actual-word-loop)
    (dw (getc))
    (dw (dup 0jump word-done))
    (dw (dup lit ,(char->integer #\space)   <> 0jump word-done))
    (dw (dup lit ,(char->integer #\newline) <> 0jump word-done))
    (dw (dup lit ,(char->integer #\tab) <> 0jump word-done))

    (dw (jump actual-word))

    (label word-done)
    (dw (drop))
    ;; Write a 0 and reset the word buffer pointer.
    (dw (lit 0 lit word-ptr @ c!))
    ;; Push the word length on the stack.
    (dw (lit word-ptr @ lit word-buffer -))
    (dw (lit word-buffer dup lit word-ptr !))
    (dw (swap exit))
    ;; Couldn't get a word, return 0.
    (label empty-word)
    (dw (lit 0 exit))

    ,@(defword "CHAR" 0 'char)
    (dw (word drop c@ exit))
    
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

    ,@(defword "POSTPONE" immediate 'postpone)
    (dw (word find >cfa comma exit))

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
    ((ex de hl))
    ,@next

    ,@(defword "S\"" immediate 's-quote)
    (dw (state @ 0branch 66 tick litstring comma here lit 0 comma))
    (dw (getc dup lit 34 <> 0branch 8 c-comma branch 65518 drop))
    (dw (lit 0 c-comma dup here swap - lit 3 - swap ! branch 38))
    (dw (here getc dup lit 34 <> 0branch 12 over c! 1+ branch 65514))
    (dw (drop here - here swap exit))

    ,@(defword ".\"" immediate 'dot-quote)
    (dw (state @ 0branch 14 s-quote tick tell comma branch 26))
    (dw (getc dup lit 34 = 0branch 6 drop exit emit branch 65514))
    (dw (exit))

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
    (dw (?dup 0branch 26 handler @ rp! r> handler ! r> swap >r sp! drop))
    (dw (r> exit))

    ;; Find a word.
    ;; ( addr -- xt | 0 )
    ;; Going off standard.  We're returning 0 for a word that is not
    ;; found, and let other words check if it's immediate or not.
    ,@(defcode "FIND" 0 'find)
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
    ((ex de hl))
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
    ((ex de hl))
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
    (dw (word find >cfa exit))

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
    ((ex de hl))
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
    ((ex de hl))
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
    (xor (hl))
    (ld (hl) a)
    ,@next

    ,@(defcode ">CFA" 0 '>cfa)
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
    ,@next

    ,@(defword ">DFA" 0 '>dfa)
    (dw (>cfa lit 3 + exit))

    ,@(defword "CFA>" 0 'cfa>)
    (dw (latest @ ?dup 0branch 22 2dup swap))
    (dw (< 0branch 6 nip exit @ branch ,(- 65536 24) drop))
    (dw (lit 0 exit))

    ,@(defword "PICK" 0 'pick)
    (dw (1+ 2* sp@ + @ exit))

    ;; ( name length -- )
    ;; Parse a name and create a definition header for it.

    ,@(defcode "CREATE_" hidden 'create_)
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

    ((ex de hl))
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

    ,@(defword "CREATE" 0 'create)
    (dw (word create_ exit))

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
    (ld a 64)
    (and (hl))
    (ld b 0)
    (ld c a)
    ,@next

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
    (dw (create latest @))
    (dw (hidden rbrac exit))

    ,@(defword ";" immediate 'semicolon)
    (dw (lit exit comma))
    (dw (latest @ hidden))
    (dw (lbrac exit))

    ,@(defword "CONSTANT" 0 'constant)
    (dw (create tick lit comma comma tick exit comma exit))

    ,@(defword "VALUE" 0 'value)
    (dw (create tick lit comma comma tick exit comma exit))

    ,@(defword "TO" immediate 'to)
    (dw (word find >dfa cell+ state @ 0branch 20 tick lit comma comma))
    (dw (tick ! comma branch 4 ! exit))

    ,@(defword "+TO" immediate '+to)
    (dw (word find >dfa cell+ state @ 0branch 20 tick lit comma comma))
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

    ,@(defword "QUIT" 0 'quit)
    (label try-more)
    (dw (lit ok-msg plot-string cr))
    (dw (lbrac refill 0jump quit-eof))
    (dw (interpret))
    (dw (?dup 0= 0jump not-ok))
    (dw (lit ok-msg plot-string cr jump try-more))


    (label quit-eof)
    (dw (page lit quit-eof-msg plot-string pause poweroff))

    (label not-ok)
    (dw (lit not-ok-msg plot-string abort))
    (dw (exit))

    (label ok-msg)
    (db ,(string " ok"))
    (label not-ok-msg)
    (db ,(string "?"))
    (label quit-eof-msg)
    (db ,(string "Received EOF from input device."))

    (label abort-msg1)
    (db ,(string "Error "))
    (label abort-msg2)
    (db ,(string "occured at "))
    (label abort-msg3)
    (db ,(string ">>>"))
    (label abort-msg4)
    (db ,(string "<<<"))
    (label abort-msg5)
    (db ,(string "Unconsumed input: "))

    ,@(defword "ABORT" 0 'abort)
    (dw (page lit abort-msg1 plot-string))
    ;; Print error number and rest of error message.
    (dw (u. lit abort-msg2 plot-string cr lit abort-msg3 plot-string))
    (dw (lit word-buffer plot-string lit abort-msg4 plot-string cr))
    (dw (lit abort-msg5 plot-string cr))
    (dw (lit var-input-ptr @ lit 24 type))
    (dw (pause poweroff))

    ,@(defword "INTERPRET" 0 'interpret)

    (label interpret-loop)
    (dw (word ?dup 0jump try-more))
    (dw (find ?dup 0jump maybe-number))
    (dw (state @ 0jump interpret-word))

    (label compiling-word)
    (dw (dup ?immediate 0jump compile-word))

    (label interpret-word)
    (dw (>cfa execute jump interpret-loop))

    (label compile-word)
    (dw (>cfa comma jump interpret-loop))


    (label maybe-number)
    (dw (lit word-buffer parse-number))
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

    (label undefined-word)
    (dw (lit 1 exit))

    ,@(defword "ID." 0 'id.)
    (dw (lit 3 + plot-string exit))



    ))

(define forth-control-words
  `(,@(defword "IF" immediate 'if)
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
    (dw (here lit 2 allot create tick lit comma comma))
    (dw (tick exit comma exit))

    ,@(defword "REPEAT" immediate 'repeat)
    (dw (tick branch comma swap here - comma))
    (dw (dup here swap - swap ! exit))

    ,@(defword "DO" immediate 'do)
    (dw (here tick >r comma tick >r comma exit))

    ,@(defword "LOOP" immediate 'loop)
    (dw (tick r> comma tick r> comma tick 1+ comma tick 2dup comma))
    (dw (tick = comma tick 0branch comma here - comma tick 2drop comma exit))

    ,@(defword "+LOOP" immediate '+loop)
    (dw (tick r> comma tick r> comma tick rot comma tick + comma))
    (dw (tick 2dup comma tick = comma tick 0branch comma here))
    (dw (- comma tick 2drop comma exit))

    ,@(defword "CASE" immediate 'case)
    (dw (lit 0 exit))

    ,@(defword "OF" immediate 'of)
    (dw (tick over comma tick = comma if tick drop comma exit))

    ,@(defword "ENDOF" immediate 'endof)
    (dw (else exit))

    ,@(defword "ENDCASE" immediate 'endcase)
    (dw (tick drop comma ?dup 0branch 8 then branch ,(- 65536 10) exit))

    ,@(defword "FORGET" 0 'forget)
    (dw (word find dup @ latest ! dp ! exit))

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

    (label tru)
    (ld bc 1)
    ,@next

    (label fal)
    (ld bc 0)
    ,@next

    ))

(define forth-meta-words
  `(,@(defword "PAUSE" 0 'pause)
    (dw (key drop exit))

    ;; ( num -- )
    ,@(defword "U." 0 'u._)
    (dw (base @ /mod ?dup 0branch 4 u._ dup lit 10))
    (dw (< 0branch 10 lit 48 branch 12 lit 10))
    (dw (- lit 65 + emit exit))

    ,@(defword "U." 0 'u.)
    (dw (u._ space exit))

    ,@(defword "." 0 '.)
    (dw (u._ space exit))

    ,@(defword "UWIDTH" 0 'uwidth)
    (dw (base @ / ?dup 0branch 10 uwidth 1+ branch 6))
    (dw (lit 1 exit))

    ,@(defword "SPACE" 0 'space)
    (dw (lit ,(char->integer #\space) emit exit))

    ,@(defword "SPACES" 0 'spaces)
    (dw (lit 0 >r >r space r> r> 1+ 2dup = 0branch 65518))
    (dw (2drop exit))

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
    ((ex af afs))
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

    ((ex af afs))
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
    (ld bc 1)
    ,@next

    (label invalid-bank-selected)
    (ld bc 0)
    ,@next

    ;; Find the length of a string.
    ,@(defcode "COUNT" 0 'count)
    (push bc)
    ,@bc-to-hl
    (push af)
    (push hl)
    (xor a)
    (ld b a)
    (ld c a)
    (cpir)
    (ld a c)
    (cpl)
    (ld c a)
    (ld a b)
    (cpl)
    (ld b a)
    (pop hl)
    (pop af)
    ,@next

    ,@(defword "SHUTDOWN" 0 'shutdown-forth)
    (dw (pause poweroff exit))

    ,@(defword "(" immediate 'comment)
    (dw (getc lit 41 = 0branch 4 exit branch 65520))
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
    ;; The current column (in pixels) for the cursor.
    ,@(defvar "CUR-ROW" 'cur-row 0)
    ;; The current numerical base
    ,@(defvar "BASE" 'base 10)
    ;; A temporary cell for making things faster.
    ,@(defvar "TEMP-CELL" 'temp-cell 0)
    ;; Check if reading a number has failed, since we can't return -1.
    ,@(defvar "NUM-STATUS" 'num-status 0)
    ;; Start of the data stack.
    ,@(defvar "SP0" 'sp0 0)
    ;; Start of the return stack.
    ,@(defvar "R0" 'r0 0)
    ;; Input pointer (used by GETC and UNGETC).
    ,@(defvar "INPUT-PTR" 'input-ptr 0)
    ;; Exception handler.
    ,@(defvar "HANDLER" 'handler 0)
    ,@(defvar "CURRENT-INPUT-DEVICE" 'current-input-device 0)
    ,@(defconst "H0" 'h0 'dp-start)
    ,@(defconst "OS-END" 'os-end-forth 'os-end)
    ,@(defconst "SCREEN-BUF" 'screen-buf 'screen-buffer)
    ,@(defconst "WORD-BUF" 'word-buf 'word-buffer)
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
  (put-char! 3 #\space)
  (put-char! 33 #\space)
  (put-char! 18 #\@)
  (put-char! 25 #\.)
  (put-char! 53 #\:)
  (put-char! 52 #\;)

  ;; Add more characters as you need them.
  )

(define forth-char-lookup-table
  `((label char-lookup-table)
    (db ,(make-char-lookup-table))))

(define forth-input-devices
  `(;; Example of an input device.
    (label string-input-device)
    (call docol)
    (dw (lit bootstrap-fs lit var-input-ptr !))
    (dw (lit 1 lit bootstrap-load-bool +! lit bootstrap-load-bool @))
    (dw (exit))

    ;; An input device that should be a prompt.
    ,@(defword "PROMPT" 0 'prompt)
    (dw (lit prompt-space lit 128 expect))
    (dw (lit prompt-space lit input-buffer lit 128 cmove))
    (dw (true))
    (dw (exit))))

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
    (dw (lit string-input-device lit var-current-input-device !))
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
    ,@forth-input-devices

    ,@forth-main

    (label last-forth-word)
    ;; This needs to be the last word to be defined!
    ,@(defword "STAR" 0 'star)
    (dw (lit 42 emit exit))

    ))
