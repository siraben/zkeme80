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
    
    (label os-end)
    ,(lambda ()
       (format #t "End of smiley-os: 0x")
       (PRINT-PC))
    
    ,(lambda ()
       (assemble-expr `(db ,(make-list
                             (- #x8400 *pc*)
                             #xff))))

    (dw ,(reverse *var-list*))
    ;; ,@(if (> *var-count* 0)
    ;;       `((db ,(make-list (* 2 *var-count*) 0)))
    ;;       '())

    ;; This is used by Forth.
    (label input-buffer)
    (db ,(make-list 64 0))
    (label word-buffer)
    (db ,(make-list 32 0))
    
    (label expect-ptr-initial)
    (dw (0))
    (label expect-ptr)
    (dw (0))
    (label expect-count)
    (dw (0))
    
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
