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
  `((db (#xc7 #xed #x57 #xea #x08 #x40 #xed #x57 #xf5 #xf3 #x3e #x01 #x00 #x00 #xed #x56))
    (db (#xf3 #xd3 #x14 #xf1 #xe0 #xfb #xc9 #xed #x57 #xea #x1e #x40 #xed #x57 #xf5 #xf3))
    (db (#xaf #x00 #x00 #xed #x56 #xf3 #xd3 #x14 #xf1 #xe0 #xfb #xc9 #x00 #xff #xff #xff))))


(define smiley-os
  `(,(equ 'flash-executable-ram #x8000)
    ,(equ 'flash-executable-ram-size 100)
    ,(equ 'screen-buffer #x8100)
    
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
    ,@(include-file-as-bytes "bootstrap.fs")

    (label os-end)
    ,(lambda ()
       (format #t "End of smiley-os: 0x")
       (PRINT-PC)
       (format #t "There are ~a bytes left.\n" (- #x4000 *pc*))
       '())
    ;; Must be less than 0x4000.


    ,(lambda ()
       (assemble-expr `(db ,(make-list
                             (- #x8402 *pc*)
                             #xff))))

    
    
    ,@(apply append (map (lambda (x)
                           `((label ,(car x))
                             (dw (,(cdr x)))))
                         (reverse *var-list*)))
    ;; ,@(if (> *var-count* 0)
    ;;       `((db ,(make-list (* 2 *var-count*) 0)))
    ;;       '())

    ;; Forth system variables.  Put here because it's writable when
    ;; loaded into RAM.

    ;; The current input "device", it's really a pointer to a
    ;; arbitrary assembly code.  That code must conform to the
    ;; following:
    ;; Inputs: number of characters to read in BC
    ;; Output: BC is either 0 or 1 representing failure or success
    ;;         the input is written starting at input-buffer
    ;; Returning: must use the ,@next macro instead of (ret).
    ;; i.e. or make it a Forth word!
    
    (label current-input-device)
    (dw (0))

    ;; Transient input buffer.
    (label input-buffer)
    (db ,(make-list 128 0))
    (label input-ptr)
    (dw (0))

    ;; Transient word buffer.
    (label word-buffer)
    (db ,(make-list 32 0))
    (label word-ptr)
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

    (label prompt-space)
    (db ,(make-list 128 0))

    ;; This value, when incremented, becomes 0.  This causes REFILL to
    ;; detect that this "device" no longer has input, and thus will stop.
    (label bootstrap-load-bool)
    (dw (65535))
    

    ;; 2K bytes of free space.
    (label here-start)
    (db ,(make-list 2048 0))


    
    ,(lambda ()
       (assemble-expr `(db ,(make-list
                             (- #xf0000 *pc*)
                             #xff))))
    

    ,@wtf-prog

    ,fill-until-end
    
    ,(lambda ()
       (format #t "End of binary: 0x")
       (PRINT-PC))
    ))

(define (make-rom filename)
  (assemble-to-file smiley-os filename))

(define (remake filename)
  (load "smiley-os.scm")
  (make-rom filename))
