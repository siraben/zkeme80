(load "assembler3.scm")
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
    
    ,(lambda ()
       (assemble-expr `(db ,(make-list
                             (- #xf0000 *pc*)
                             #xff))))
    
    ,@wtf-prog

    ,PRINT-PC
    
    ,fill-until-end
    
    ,PRINT-PC))

(define (make-rom filename)
  (assemble-to-file smiley-os filename))
