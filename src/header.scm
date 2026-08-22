(include "macros.scm")
(define header-asm
  `((jp boot)
    (db ,(map char->integer (string->list "SK")))
    (db (0 0))

    (dec sp)
    (ret)
    ,@(repeat 5 `(,@ (repeat 7 '((nop)))
                     (ret)))
    ,@(repeat 7 '((nop)))
    ,(lambda ()
       (format #t "System interrupt at 0x")
       (PRINT-PC))
    (jp sys-interrupt)
    ,@(repeat 24 '((nop)))
    ;; Entry point used by the retail boot page: after a soft reset
    ;; TI's boot sector checks that (0x0038) != 0xFF and that the word
    ;; at 0x0056 equals 0xA55A; if both hold it jumps here to hand off
    ;; to the installed OS.  Keep these three bytes consistent!
    (jp boot)
    (db (#x5a #xa5 #xff))))
