(load "assembler.scm")
(load "macros.scm")

(define swap-sector #x38)
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

(define zkeme80
  `((ram-range #x8000 #xc000)
    ,(equ 'flash-executable-ram #x8000)
    ,(equ 'flash-executable-ram-size 100)
    ,(equ 'flash-operation-status #x80ff)
    ,(equ 'screen-buffer #x8100)
    ,(equ 'swap-sector #x38)

    ,@header-asm
    ,@boot-asm
    ,@interrupt-asm
    ,@flash-asm
    ,@util-asm
    ,@display-asm
    ,@keyboard-asm
    ,@math-asm
    ,@font-asm
    ,@text-asm

    (label bootstrap-fs)
    ,@(include-file-as-bytes "boot.fs")

    (label os-end)
    ,(lambda ()
       (format #t "End of zkeme80 kernel: 0x")
       (PRINT-PC)
       (format #t "~a bytes left for page 0.\n" (- #x4000 *pc*))
       '())
    ;; Must be less than 0x4000.

    ,(fill-up-to #xff #x4000)

    (label bootstrap-flash1)
    ,@(include-file-as-bytes "bootstrap-flash1.fs")

    ,(fill-up-to #xff #x8000)

    (label bootstrap-flash2)
    ,@(include-file-as-bytes "bootstrap-flash2.fs")


    ,(lambda ()
       (format #t "Start of Forth data: 0x")
       (PRINT-PC)
       (format #t "~a bytes left for page 2.\n" (- #x8400 *pc*))
       '())



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
    (label expect-count)
    (dw (0))
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

    (label ddd-data)
    (db (0))

    (label prompt-space)
    (db ,(make-list 128 0))

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

    (dw ,(make-list 128 0))
    (label return-stack-start)

    ;; Free space until #xc000
    (label dp-start)
    ,(lambda ()
       (format #t "~a bytes left for HERE.\n" (- #xc000 *pc*))
       '())

    ,(fill-up-to #x0 #xc000)

    ,@(include-file-as-bytes "bootstrap-flash3.fs")
    ,(fill-up-to #xff #x10000)

    ,@(include-file-as-bytes "bootstrap-flash4.fs")
    ,(fill-up-to #xff #x14000)

    ,@(include-file-as-bytes "bootstrap-flash5.fs")

    ,(lambda ()
       (format #t "End of Forth data: 0x")
       (PRINT-PC)
       (format #t "~a bytes left for page 4.\n" (- #x18000 *pc*))
       '())

    ,(fill-up-to #xff #x18000)

    ;; Optional, verified post-bootstrap dictionary image.  Ordinary builds
    ;; leave page 6 erased and take the text-bootstrap fallback.
    (label bootstrap-image-page)
    ,@(let ((image (getenv "ZKEME80_BOOTSTRAP_IMAGE")))
        (if image
            (include-binary-as-bytes image)
            '()))

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
