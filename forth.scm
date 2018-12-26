;; Forth portion of the operating system.

;; Immediate flag
(define immediate 128)

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
    ,@pop-hl-rs
    ,@push-hl-rs
    (ld c (hl))
    (inc hl)
    (ld b (hl))
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
    ,@next

    ,@(defcode "RDROP" 0 'rdrop)
    ,@pop-hl-rs
    ,@next

    ,@(defcode "2RDROP" 0 '2rdrop)
    ,@pop-hl-rs
    ,@pop-hl-rs
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
    ,@next

    ,@(defcode "2SWAP" 0 '2swap)
    ,@push-de-rs
    (pop de)
    (pop hl)
    ,@push-hl-rs
    (pop hl)
    ,@(push* '(de bc hl))
    ,@pop-bc-rs
    ,@pop-de-rs
    ,@next
    

    ))

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
    ,@next

    ,@(defcode "2+" 0 '2+)
    (inc bc)
    (inc bc)
    ,@next

    ,@(defword "TRUE" 0 'true)
    (dw (lit 1 exit))

    ,@(defword "FALSE" 0 'false)
    (dw (lit 0 exit))
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
    (pop bc)
    ,@pop-de-rs
    ,@next

    ,@(defcode "PUT-SPRITE-AND" 0 'put-sprite-and-forth)
    (ld iy screen-buffer)
    ,@push-de-rs
    (ld e c)
    (pop bc)
    (ld d c)
    (pop bc)
    (ld b c)
    (pop hl)
    (call put-sprite-and)
    (pop bc)
    ,@pop-de-rs
    ,@next


    ,@(defcode "PUT-SPRITE-XOR" 0 'put-sprite-xor-forth)
    (ld iy screen-buffer)
    ,@push-de-rs
    (ld e c)
    (pop bc)
    (ld d c)
    (pop bc)
    (ld b c)
    (pop hl)
    (call put-sprite-and)
    (pop bc)
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
    ,@(defword "AT-XY" 0 'at-xy)
    (dw (cur-row ! cur-col !))
    (dw (exit))
    
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
    (dw (origin lit expect-ptr-initial @ plot-str))

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
    (dw (origin lit expect-ptr-initial @ plot-str))
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
    (dw (lit current-input-device @ execute))
    (dw (0jump refill-fail))
    (dw (lit input-buffer lit input-ptr ! true exit))
    (label refill-fail)
    (dw (false exit))

    ;; Get the next character from the input source.
    ,@(defword "GETC" 0 'getc)
    (dw (lit input-ptr @ c@))
    (dw (lit 1 lit input-ptr +!))
    (dw (exit))

    ,@(defword "UNGETC" 0 'ungetc)
    (dw (lit 1 lit input-ptr -!))
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
    (dw (dup lit ,(char->integer #\\) <>))
    (dw (0jump skip-comment))

    (dw (jump actual-word))
    (dw (exit))

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
    (dw (lit 0 exit))))

(define forth-semantics-words
  `(,@(defcode "LIT" 0 'lit)
    (ld a (de))
    (ld l a)
    (inc de)
    (ld a (de))
    (ld h a)
    (inc de)
    (push bc)
    ,@hl-to-bc
    ,@next
    
    ,@(defcode "EXIT" 0 'exit)
    ,@pop-de-rs
    ,@next

    ,@(defcode "EXECUTE" 0 'execute)
    ,@bc-to-hl
    (pop bc)
    (jp (hl))
    ,@next

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
    
    ;; Not standard compilant.
    ,@(defword "'" 0 'tick)
    (ld a (de))
    (ld l a)
    (inc de)
    (ld a (de))
    (ld h a)
    (inc de)
    (push bc)
    ,@hl-to-bc
    ,@next
    
    ,@(defcode "," 0 'comma)
    (call _comma)
    (pop bc)
    ,@next

    (label _comma)
    (push de)
    (ld hl (var-here))
    (ld (hl) c)
    (inc hl)
    (ld (hl) b)
    (inc hl)
    (ld de var-here)
    ((ex de hl))
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (pop de)
    (ret)

    ,@(defcode "?IMMED" 0 '?immed)
    (inc bc)
    (inc bc)
    (ld a (bc))
    (bit 7 a)
    (jp z fal)
    (jp tru)

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

    ;; ( name length -- )
    ;; Parse a name and create a definition header for it.
    ;; When name executes, 
    
    ,@(defcode "CREATE" 0 'create)
    (ld hl (var-here))
    ,@push-de-rs
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

    (ld hl var-here)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    ,@pop-de-rs
    (pop bc)
    ,@next

    ,@(defcode "DOCOL_H" 0 'docol-header)
    (push de)
    (ld de (var-here))
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
    (ld hl var-here)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (pop de)
    ,@next

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
    (dw (word create docol-header))
    (dw (latest @ hidden))
    (dw (rbrac exit))

    ,@(defword ";" immediate 'semicolon)
    (dw (lit exit comma))
    (dw (latest @ hidden))
    (dw (lbrac exit))
    
    ))

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
    (dw (s0 @ sp@ - 2- >> exit))

    ,@(defword ".S" 0 '.s)
    (dw (lit ,(char->integer #\<) emit depth u._))
    (dw (lit ,(char->integer #\>) emit space))
    (dw (sp@ dup s0 @ < 0branch 18 dup @))
    (dw (u. lit 2 + branch 65510 drop exit))

    ,@(defword "HEX" 0 'hex)
    (dw (lit 16 base ! exit))
    
    ,@(defword "DEC" 0 'dec)
    (dw (lit 10 base ! exit))

    ,@(defcode "LATEST" 0 'latest)
    (push bc)
    (ld bc var-latest)
    ,@next
    
    ))
(define forth-misc-words
  `(;; Shut down the calculator.
    ,@(defcode "POWEROFF" 0 'poweroff)
    (jp shutdown)))

(define forth-vars
  `(,@(defvar "STATE" 'state 0)
    
    ,@(defvar "SP0" 'sp0 0)
    ,@(defvar "HERE" 'here 'here-start)
    ,@(defvar "CUR-COL" 'cur-col 0)
    ,@(defvar "CUR-ROW" 'cur-row 0)
    ,@(defvar "BASE" 'base 10)
    ,@(defvar "S0" 's0 0)
    ,@(defvar "R0" 'r0 0)))

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
  (put-char! 3 #\space)
  (put-char! 33 #\space)
  (put-char! 40 #\@)
  ;; Add more characters as you need them.
  )

(define forth-char-lookup-table
  `((label char-lookup-table)
    (db ,(make-char-lookup-table))))

(define forth-main
  `(
    ,@(defword "5" 0 '5)
    (dw (lit 5 exit))
    ;; Example of an input device.
    (label string-input-device)
    (call docol)
    (dw (lit default-expr lit input-buffer lit 128 cmove))
    (dw (lit 1 lit spare +! lit spare @))
    (dw (exit))


    ;; The default input device.
    ,@(defword "PROMPT" 0 'prompt)
    (dw (lit prompt-space lit 128 expect))
    (dw (lit prompt-space lit input-buffer lit 128 cmove))
    (dw (true))
    (dw (exit))

    ;; (label not-ok-msg)
    ;; (db ,(string "ERROR"))
    
    ,@(defword "QUIT" 0 'quit)
    (label try-more)
    (dw (lit ok-msg plot-str cr))    
    (dw (lbrac refill 0jump quit-eof))
    (dw (interpret))
    (dw (?dup 0= 0jump not-ok))
    (dw (lit ok-msg plot-str cr jump try-omore))
    
    (dw (lbrac refill 0jump quit-eof))    
    (dw (interpret))

    (label quit-eof)
    (dw (lit quit-eof-msg plot-str pause poweroff))
    (label not-ok)
    ;; Handle exception number?  Drop for now.
    (dw (u. lit not-ok-msg plot-str abort))

    (label quit-eof-msg)
    (db ,(string "EOF from input device."))
    (label abort-msg)
    (db ,(string "Error occurred, aborting."))
    ,@(defword "ABORT" 0 'abort)
    (dw (lit abort-msg plot-str pause poweroff))

    ,@(defword "INTERPRET" 0 'interpret)

    (label interpret-loop)
    (dw (word ?dup 0jump try-more))
    (dw (find ?dup 0jump undefined-word))    
    (dw (state @ 0jump interpret-word))
    
    (label compiling-word)
    (dw (dup ?immed 0jump compile-word))
    
    (label run-immediate-word)
    (dw (>cfa execute jump interpret-loop))
    
    (label compile-word)
    (dw (>cfa comma jump interpret-loop))
    
    (label interpret-word)
    (dw (>cfa execute jump interpret-loop))
    
    (label undefined-word)
    (dw (lit 666 exit))

    (label interpret-eof)
    (dw (lit 0 exit))

    (label post-exec)
    (db ,(string "Stack after word is run:"))
    (label main)
    (dw (origin))
    (dw (lit last-forth-word latest !))
    (dw (lit here-start here !))
    (dw (lit 10 base !))
    (dw (lit 0 state !))
    ;; We set the stack pointer two lower because it's changed
    ;; slightly since when we did (ld sp 65532)
    (dw (lit 65530 s0 !))
    (dw (lit #xc000 r0 !))
    (dw (lit string-input-device lit current-input-device !))
    (dw (quit))

    (label default-expr)
    (db ,(string "HEX : DOUBLE DUP + ; 5 DOUBLE U. PAUSE POWEROFF"))
    (label ok-msg)
    (db ,(string " ok"))
    (label not-ok-msg)
    (db ,(string "?"))
    ;; (db (0 0 0))
    ;; (dw (lit title7 plot-str cr))
    ;; (dw (lit input-buffer lit 24 expect cr))
    ;; (dw (lit title8 plot-str))
    ;; (dw (lit input-buffer plot-str pause))
    ;; (label demo-loop)
    ;; (dw (key clear-screen dup dup))
    ;; (dw (origin .s cr))    
    ;; (dw (lit title4 plot-str u. cr))
    ;; (dw (lit title5 plot-str to-ascii u. cr))
    ;; (dw (lit title6 plot-str to-ascii emit cr))
    ;; (dw (.s ))
    ;; (dw (jump demo-loop))
    
    (dw (poweroff))
    ))

(define forth-asm
  `(,@forth-shared-header
    ,@forth-semantics-words
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

    ,@(defcode "SP@" 0 'sp@)
    (push bc)
    (ld (var-sp0) sp)
    (ld hl (var-sp0))
    ,@hl-to-bc
    ,@next


    ,@forth-main

    (label last-forth-word)
    ;; This needs to be the last word to be defined!
    ,@(defword "STAR" 0 'star)
    (dw (lit 42 emit exit))
    
    ,(lambda ()
       (format #t "End of forth.asm: 0")
       (PRINT-PC))
    ))

