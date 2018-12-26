;; Forth portion of the operating system.

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
  (+ #x83fe (* 2 *var-count*)))

(define reset-var
  (lambda ()
    (set! *var-list* '())
    (set! *var-count* 0)
    '()))


;; We must relocate these variables elsewhere, where RAM is writable.
(define (defvar name label default)
  ;; Store the list of variable default values.
  (set! *var-list* `(,default . ,*var-list*))
  (let ((var-label (string->symbol (format #f "var-~a" label)))
        (var-addr (next-var-addr!)))
    
    `(,(lambda () (add-label! var-label var-addr) '())
      ,@(defcode name 0 label)
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
    
    ,@(defcode "DROP" 0 'drop)
    (pop bc)
    ,@next

    ,@(defcode "SWAP" 0 'swap)
    (pop hl)
    (push bc)
    ,@hl-to-bc
    ,@next

    ,@(defcode ">R" 0 '>r)
    ,@push-bc-rs
    (pop bc)
    ,@next
    
    ,@(defcode "R>" 0 'r>)    
    (push bc)
    ,@pop-bc-rs
    ,@next

    ,@(defcode "OVER" 0 'over)
    (pop hl)
    (push hl)
    (push bc)
    ,@hl-to-bc
    ,@next

    ,@(defcode "ROT" 0 'rot)
    ,@push-de-rs
    (pop hl)
    (pop de)
    (push hl)
    (push bc)
    ((ex de hl))
    ,@hl-to-bc
    ,@pop-de-rs
    ,@next

    ,@(defcode "-ROT" 0 '-rot)
    ,@push-de-rs
    (pop hl)
    (pop de)
    (push bc)
    (push de)
    ,@hl-to-bc
    ,@pop-de-rs
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
    ,@next))

(define forth-math-words
  `(,@(defcode ">>" 0 '>>)
    (push de)
    (ld d b)
    (ld e c)
    (srl d)
    (rr e)
    (ld b d)
    (ld c e)
    (pop de)
    ,@next

    ,@(defcode "+" 0 '+)
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

    ,@(defcode "*" 0 '*)
    ,@push-de-rs
    (pop de)
    (call mul-16-by-16)
    ,@hl-to-bc
    ,@pop-de-rs
    ,@next
    
    ,@(defcode "/MOD" 0 '/mod)
    ,@push-de-rs
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
    ,@pop-de-rs
    ,@next
    
    ,@(defword "/" 0 '/)
    (dw (/mod swap drop exit))
    
    ,@(defcode "1+" 0 '1+)
    (inc bc)
    ,@next

    ,@(defcode "1-" 0 '1-)
    (dec bc)
    ,@next

    ,@(defcode "2-" 0 '2-)
    (dec bc)
    (dec bc)
    ,@next))

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
    
    ,@(defcode "+!" 2 '+!)
    (pop hl)
    (push de)
    (ld a (bc))
    (ld e a)
    (inc bc)
    (ld a (bc))
    (ld d a)
    (dec bc)
    (add hl de)
    (ld a l)
    (ld (bc) a)
    (inc bc)
    (ld a h)
    (ld (bc) a)
    (inc bc)

    (pop de)
    (pop bc)
    ,@next

    ,@(defcode "-!" 2 '-!)
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
    ,@push-de-rs
    (pop de)
    (pop hl)
    (ldir)
    ,@pop-de-rs
    (pop bc)
    ,@next))

(define forth-graphics-words
  `(;; Draw a rectangle using OR
    ;; ( x y width height -- )
    ,@(defcode "RECT-OR" 0 'rect-or-forth)
    (ld b c)
    (pop hl)
    (ld c l)
    (pop hl)
    ,@push-de-rs
    (pop de)
    (ld iy screen-buffer)
    (call rect-or)
    ,@pop-de-rs
    (pop bc)
    ,@next

    ;; Draw a rectangle using XOR
    ;; ( x y width height -- )
    ,@(defcode "RECT-XOR" 0 'rect-xor-forth)
    (ld b c)
    (pop hl)
    (ld c l)
    (pop hl)
    ,@push-de-rs
    (pop de)
    (ld iy screen-buffer)
    (call rect-xor)
    (call fast-copy)
    ,@pop-de-rs
    (pop bc)
    ,@next

    ,@(defcode "CLEAR-SCREEN" 0 'clear-screen)
    (ld iy screen-buffer)
    (call clear-buffer)
    (call fast-copy)
    ,@next

    ;; Draw a sprite to the screen.
    ;; ( sprite_addr height x y -- )
    ,@(defcode "PUT-SPRITE-OR" 0 'put-sprite-or-forth)
    (ld iy screen-buffer)
    ,@push-de-rs
    (ld e c)
    (pop bc)
    (ld d c)
    (pop bc)
    (ld b c)
    (pop hl)
    (call put-sprite-or)
    ,@pop-de-rs
    ,@next

        ;; Draw a region of memory to the screen.
    ;; ( addr --  )
    ,@(defcode "DRAW" 0 'draw)
    (push bc)
    (pop iy)
    (pop bc)
    (call fast-copy)
    ,@next

    ;; Plot the default memory screen (starting at address #x8100)
    ,@(defcode "PLOT" 0 'plot)
    (ld iy screen-buffer)
    (call fast-copy)
    ,@next    

))

(define forth-vars
  `(,@(defvar "STATE" 'state 1)
    ,@(defvar "LATEST" 'latest 0)
    ,@(defvar "SP0" 'sp0 0)
    ,@(defvar "HERE" 'here 'os-end)
    ,@(defvar "CUR-COL" 'cur-col 0)
    ,@(defvar "CUR-ROW" 'cur-row 0)
    ,@(defvar "BASE" 'base 10)
    ,@(defvar "S0" 's0 0)))

(define (make-char-lookup-table)
  (define res (make-list 128 0))
  (define (put-char! id char)
    (list-set! res id (char->integer char)))

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
  (put-char! 33 #\space)

  res)

(define forth-char-lookup-table
  `((label char-lookup-table)
    (db ,(make-char-lookup-table))))



(define forth-asm
  `(,reset-link
    ,reset-var
    ;; Reasonable settings for Forth's stacks.
    (ld de main)
    (ld ix #xc000)
    (ld sp 65534)
    ,@next
    ,@next-sub

    ,@docol-sub

    (label tru)
    (ld bc 1)
    ,@next
    
    (label fal)
    (ld bc 0)
    ,@next

    ,@(defcode "EXIT" 0 'exit)
    ,@pop-de-rs
    ,@next

    ,@(defcode "?DUP" 0 '?dup)
    (ld hl 0)
    (call cp-hl-bc)
    (jp nz dup)
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

    ;; Sometimes absolute jumps are easier!
    ,@(defcode "JUMP" 0 'jump)
    (ld a (de))
    (ld l a)
    (inc de)
    (ld a (de))
    (ld h a)
    ,@hl-to-de
    ,@next

    ,@(defcode "0JUMP" 0 '0jump)
    (ld a c)
    (cp 0)
    (jp z zjump-maybe)
    (jp nz zjump-fail)
    
    (label zjump-maybe)
    (ld a b)
    (cp 0)
    (jp nz zjump-fail)
    (pop bc)
    (jp jump)
    
    (label zjump-fail)
    (inc de)
    (inc de)
    (pop bc)
    ,@next

    ,@(defcode "BRANCH" 0 'branch)
    (ld a (de))
    (ld l a)
    (inc de)
    (ld a (de))
    (ld h a)
    (dec de)
    (add hl de)
    ,@hl-to-de
    ,@next

    ,@(defcode "0BRANCH" 0 '0branch)
    (ld a c)
    (cp 0)
    (jp z zbranch-maybe)
    (jp nz zbranch-fail)
    
    (label zbranch-maybe)
    (ld a b)
    (cp 0)
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

    ,@(defcode "0=" 0 '0=)
    (ld hl 0)
    (call cp-hl-bc)
    (jp c fal)
    (jp tru)

    ,@(defword "NOT" 0 'not)
    (dw (0= exit))

    ,@(defcode "KEYC" 0 'keyc)
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
    ,@push-de-rs
    (call flush-keys)
    (call wait-key)
    (ld h 0)
    (ld l a)
    (ld de char-lookup-table)
    (add hl de)
    (ld a (hl))
    (push bc)
    (ld b 0)
    (ld c a)
    ,@pop-de-rs
    ,@next

    ,@(defcode "TO-ASCII" 0 'to-ascii)
    (push de)
    (ld h 0)
    (ld l c)
    (ld de char-lookup-table)
    (add hl de)
    (ld a (hl))
    (ld c a)
    (ld b 0)
    (pop de)
    ,@next

    ,@(defword "ORIGIN" 0 'origin)
    (dw (lit 0 dup cur-col ! cur-row ! exit))

    ;; ( addr u -- )
    ;; Expect u characters (or a newline, whichever comes first) and
    ;; store them at address addr.
    ;; Written in Forth because it's easier.
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
    (dw (clear-screen origin lit expect-ptr-initial @ plot-str))
    (dw (lit 8 cur-row +! lit ,(char->integer #\^) emit))
    (dw (lit 8 cur-row -!))
    (label expect-got-blank)
    (dw (akey))
    (dw (dup 0jump expect-more))
    (dw (dup lit ,(char->integer #\newline) <> 0jump expect-got-newline))
    (dw (dup lit ,(char->integer #\backspace) <> 0jump expect-got-backspace))
    ;; General case
    (dw (lit expect-ptr @ !))
    (dw (lit 1 lit expect-ptr +!))
    (dw (lit 1 lit expect-count -!))
    (dw (jump expect-loop))
    
    (label expect-got-newline)
    (dw (drop lit 0 lit expect-ptr @ ! exit))
    
    (label expect-got-backspace)
    (dw (lit 0 lit expect-ptr @ !))
    (dw (lit expect-ptr-initial @ lit expect-ptr @))
    (dw (<> 0jump expect-loop))
    (dw (lit 1 lit expect-ptr -!))
    (dw (lit 1 lit expect-count +!))
    (dw (jump expect-loop))
    
    
    
    ,@forth-stack-words
    ,@forth-math-words
    ,@forth-memory-words
    ,@forth-graphics-words
    ,@forth-char-lookup-table
    
    ;; Shut down the calculator.
    ,@(defcode "POWEROFF" 0 'poweroff)
    (jp shutdown)



    ;; Plot a character to the screen.
    ;; ( char -- )
    ,@(defcode "EMIT" 0 'emit)
    ,@push-de-rs
    (ld a (var-cur-col))
    (ld d a)
    (ld a (var-cur-row))
    (ld e a)
    ;; Character to print
    (ld a c)
    (ld iy screen-buffer)
    (call draw-char)
    (call fast-copy)
    (ld a d)
    (ld (var-cur-col) a)
    (ld a e)
    (ld (var-cur-row) a)    
    ,@pop-de-rs
    (pop bc)
    ,@next

    ;; Carriage return
    ,@(defcode "CR" 0 'cr)
    ,@push-de-rs
    (ld a (var-cur-col))
    (ld d a)
    (ld a (var-cur-row))
    (ld e a)
    (call newline)
    (ld a d)
    (ld (var-cur-col) a)
    (ld a e)
    (ld (var-cur-row) a)    
    ,@pop-de-rs
    ,@next

    ;; Draw a string to the screen
    ;; ( str_addr -- )
    ,@(defcode "PLOT-STR" 0 'plot-str)
    ,@push-de-rs
    (ld a (var-cur-col))
    (ld d a)
    (ld a (var-cur-row))
    (ld e a)
    ,@bc-to-hl
    (ld iy screen-buffer)
    (ld b 98)
    (ld c 64)
    (ld a 0)
    (call wrap-str)
    (call fast-copy)
    (ld a d)
    (ld (var-cur-col) a)
    (ld a e)
    (ld (var-cur-row) a)
    ,@pop-de-rs
    (pop bc)
    ,@next

    ,@forth-vars



    ,@(defcode "SP@" 0 'sp@)
    (push bc)
    (ld (var-sp0) sp)
    (ld hl (var-sp0))
    ,@hl-to-bc
    ,@next


    ,@(defword "PAUSE" 0 'pause)
    (dw (key drop exit))


    ;; ( num -- )
    ,@(defword "U." 0 'u._)
    (dw (base @ /mod ?dup 0branch 4 u._ dup lit 10))
    (dw (< 0branch 10 lit 48 branch 12 lit 10))
    (dw (- lit 65 + emit exit))

    ,@(defword "U." 0 'u.)
    (dw (u._ space exit))

    ,@(defword "UWIDTH" 0 'uwidth)
    (dw (base @ / ?dup 0branch 10 uwidth 1+ branch 6))
    (dw (lit 1 exit))

    ,@(defword "SPACE" 0 'space)
    (dw (lit ,(char->integer #\space) emit exit))

    ,@(defword "SPACES" 0 'spaces)
    (dw (lit 0 >r >r space r> r> 1+ 2dup = 0branch 65520))
    (dw (2drop exit))

    ,@(defword "U.R" 0 'u.r)
    (dw (swap dup uwidth rot swap - spaces u._ exit))

    ,@(defword "DEPTH" 0 'depth)
    (dw (s0 @ sp@ - 2- >> exit))

    ,@(defword ".S" 0 '.s)
    (dw (lit ,(char->integer #\<) emit depth u._))
    (dw (lit ,(char->integer #\>) emit space))
    (dw (sp@ dup s0 @ < 0branch 18 dup @))
    (dw (u. lit 2 + branch 65510 drop exit))


    (label title1)
    (db ,(string "Welcome to Ben's"))
    (label title2)
    (db ,(string "Forth-based OS!"))
    (label title3)
    (db ,(string "Press any key to continue to key demo..."))
    (label title4)
    (db ,(string "You pressed: "))
    (label title5)
    (db ,(string "As an ASCII code: "))
    (label title6)
    (db ,(string "As a character: "))
    (label title7)
    (db ,(string "Enter a message: "))
    (label title8)
    (db ,(string "You typed: "))
    
    (label main)
    (dw (lit 10 base !))
    (dw (lit 65534 s0 !))
    (dw (lit title1 plot-str cr))
    (dw (lit title2 plot-str cr pause))
    (dw (base @ u. cr))
    (dw (lit 1 lit 2 lit 3 lit 4))
    (dw (.s drop drop drop drop cr))
    (dw (lit title3 plot-str cr pause))

    (dw (lit title7 plot-str cr))
    (dw (lit input-buffer lit 24 expect cr))
    (dw (lit title8 plot-str))
    (dw (lit input-buffer plot-str pause))
    (label demo-loop)
    (dw (key clear-screen dup dup))
    (dw (lit 0 cur-row !))    
    (dw (lit 0 cur-col ! lit title4 plot-str u. cr))
    (dw (lit title5 plot-str to-ascii u. cr))
    (dw (lit title6 plot-str to-ascii emit cr))
    (dw (.s))
    (dw (jump demo-loop))
    
    (dw (poweroff))
    ;; Who said we couldn't mix Forth and Scheme code?
    ;; (dw (,@(lit* '(30 30 20 20)) rect-xor-forth
    ;;      ,@(lit* '(40 40 20 20)) rect-xor-forth key
    ;;      clear-screen poweroff))
    ,(lambda ()
       (format #t "End of forth.asm: 0")
       (PRINT-PC))
    ))

