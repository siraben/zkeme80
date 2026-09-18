(load "assembler.scm")
(load "macros.scm")

(define swap-sector #x38)
;; The flash trampoline is at 8000; reserve F000..FFFF for the data stack.
(define dictionary-limit #xf000)
(load "modules.scm")
(load "forth.scm")
(load "header.scm")
(load "boot.scm")
(load "interrupt.scm")
(load "flash.scm")
(load "util.scm")
(load "display.scm")
(load "keyboard.scm")
(load "math.scm")
(load "font.scm")
(load "text.scm")
;; Essential code that modifies sets an interrupt mode of 1 and writes
;; to port #x14.
(define wtf-prog
  `((rst 0)
    (ld a i)
    (jp pe #x4008)
    (ld a i)
    (push af)
    (di)
    (ld a 1)
    (nop)
    (nop)
    (im 1)
    (di)
    (out (#x14) a)
    (pop af)
    (ret po)
    (ei)
    (ret)
    (ld a i)
    (jp pe #x401e)
    (ld a i)
    (push af)
    (di)
    (xor a)
    (nop)
    (nop)
    (im 1)
    (di)
    (out (#x14) a)
    (pop af)
    (ret po)
    (ei)
    (ret)
    (nop)
    (rst #x38)))

;; These helpers execute only after boot installs their page-2 image in fixed
;; RAM. Keep dictionary headers in page 0; moving cold rendering code and data
;; leaves room there for the native Forth kernel and shell primitives.
(define resident-ui-asm
  `(,@font-asm
    ,@text-asm
    ,@forth-char-lookup-table
    (label bootstrap-fs)
    ,@(include-file-as-bytes "boot.fs")))

(define zkeme80
  `((ram-range #x8000 #xc000)
    ,(equ 'flash-executable-ram #x8000)
    ,(equ 'flash-executable-ram-size 100)
    ,(equ 'flash-operation-status #x80ff)
    ,(equ 'screen-buffer #x8100)
    ,(equ 'screen-buffer-scroll-source (+ #x8100 72))
    ,(equ 'swap-sector #x38)

    ,@header-asm
    ,@boot-asm
    ,@interrupt-asm
    ,@flash-asm
    ,@util-asm
    ,@display-asm
    ,@keyboard-asm
    ,@math-asm

    (label os-end)
    ,(lambda ()
       (format #t "End of zkeme80 kernel: 0x")
       (PRINT-PC)
       (format #t "~a bytes left for page 0.\n" (- #x4000 *pc*))
       '())
    ;; Must be less than 0x4000.

    ,(fill-up-to #xff #x4000)

    ,@(module-source (car module-layout))
    ,(fill-up-to #xff #x8000)
    ,(fill-up-to #xff #x8402)

    ;; We start the Forth data here.
    ,@(concat-map (lambda (x)
                    `((label ,(car x))
                      (dw (,(cdr x)))))
                  (reverse *var-list*))

    ;; Forth system variables.  Put here because it's writable when
    ;; loaded into RAM.

    ;; Edge-filter state must not alias the display/flash trampoline at
    ;; 0x8000, which is routinely overwritten between keypad scans.
    (label keyboard-last-key)
    (db (0))

    ;; Remaining byte count for the batched native TYPE renderer.
    (label type-count)
    (dw (0))

    ;; Transient input buffer.
    (label input-buffer)
    ;; PROMPT may receive 128 characters; keep one extra byte for the
    ;; interpreter's private zero terminator.
    (db ,(make-list 129 0))

    ;; Transient counted-string buffer used by WORD.  ANS Forth requires
    ;; counted strings to support at least 255 characters; the extra byte is
    ;; a private trailing NUL used by the kernel's internal token lookup.
    (label word-buffer)
    (db (0))
    (label word-buffer-data)
    (db ,(make-list 255 0))
    (label word-buffer-terminator)
    (db (0))

    ;; Interpreter tokens cannot share WORD's transient region: executing the
    ;; next token must not invalidate a counted string returned by WORD.
    (label token-buffer-data)
    (db ,(make-list 255 0))
    (label token-buffer-terminator)
    (db (0))
    (label token-ptr)
    (dw (0))

    ;; Example input device; the Forth word "EXPECT".
    ;; See "EXPECT" in forth.scm for the source.
    (label expect-ptr-initial)
    (dw (0))
    (label expect-ptr)
    (dw (0))
    (label expect-edit-ptr)
    (dw (0))
    (label expect-count)
    (dw (0))
    (label expect-capacity)
    (dw (0))
    ;; Nonzero only for the shell editor extension, which keeps accepting
    ;; navigation/deletion after the input field reaches capacity.
    (label expect-full-edit)
    (db (0))
    (label expect-col-save)
    (dw (0))
    (label expect-row-save)
    (dw (0))

    ;; Compiler bookkeeping for DO/+LOOP/LEAVE.  Each of the sixteen
    ;; contexts is { loop-start, linked-LEAVE-head }.
    (label loop-compile-depth)
    (dw (0))
    (label loop-compile-contexts)
    (db ,(make-list 64 0))

    ;; Transaction boundary for a colon definition.  QUIT restores these
    ;; values if compilation ends through an error or premature source end.
    (label compile-start-dp)
    (dw (0))
    (label compile-start-latest)
    (dw (0))

    ;; Raw key observed while the previous AKEY was being released.
    (label akey-pending)
    (db (0))

    (label ddd-data)
    (db (0))

    (label prompt-space)
    (db ,(make-list 128 0))
    ;; Dedicated guard byte for the public 128-byte PBUF/editor boundary.
    (label prompt-space-canary)
    (db (0))

    ;; One pending bootstrap source.  main initializes this to 1 and the
    ;; string device clears it after installing the source; later REFILLs
    ;; therefore report end of input.
    (label bootstrap-load-bool)
    (dw (65535))

    ;; Direct-mapped FIND cache.  Each of the 64 slots holds the NFA of a
    ;; previously resolved name.  The whole RAM area is cleared at boot.
    (label find-cache)
    (db (0))
    (label find-cache-second)
    (db ,(make-list 127 0))
    (label find-cache-end)

    ;; State for the peephole compiler.  CREATE_ records the first byte of
    ;; the current definition body; COMPILE-XT uses it to avoid examining
    ;; bytes in the header when looking behind the current DP.
    (label current-definition-body)
    (dw (0))
    (label compile-xt-saved-ip)
    (dw (0))
    (label compile-xt-current)
    (dw (0))
    (label display-dirty)
    (db (0))

    (label resident-ui-start)
    ,@resident-ui-asm
    (label resident-ui-end)
    ,(lambda ()
       (add-label! 'resident-ui-source
                   (- (resolve-label 'resident-ui-start) #x4000))
       (add-label! 'resident-ui-size
                   (- *pc* (resolve-label 'resident-ui-start)))
       '())

    (dw ,(make-list 128 0))
    (label return-stack-start)

    ;; Free space until #xc000
    (label dp-start)
    ,(lambda ()
       (format #t "~a bytes left for HERE.\n" (- dictionary-limit *pc*))
       '())

    ,(fill-up-to #x0 #xc000)

    ;; Emit resident/tool pages and reserve page 6 for verified images.
    ,@(concat-map
       (lambda (page)
         (let ((entry (find (lambda (e) (= (rom-allocation-page e) page)) module-layout)))
           (append
            (cond (entry (module-source entry))
                  ((= page 6)
                   (let ((image (getenv "ZKEME80_BOOTSTRAP_IMAGE")))
                     (if image (include-binary-as-bytes image) '())))
                  (else '()))
            (list (fill-up-to #xff (* (+ page 1) #x4000))))))
       (iota (- (max 6 (apply max (map rom-allocation-page module-layout))) 2) 3))

    ,(fill-up-to #xff #xf0000)

    ,@wtf-prog

    ,fill-until-end

    ,(lambda ()
       (format #t "End of binary: 0x")
       (PRINT-PC))
    ))

(define (make-rom filename)
  (assemble-to-file zkeme80 filename))

(define (make-rom+map filename map-filename)
  (assemble-to-file+debug zkeme80 filename map-filename))

(define (remake filename)
  (load "zkeme80.scm")
  (make-rom filename))
