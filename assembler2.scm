(include "assembler.scm")
(define reg-direct
  (oneof-sym '("bc" "de" "hl" "af" "ix" "iy" "sp"
               "a" "b" "c" "d" "e" "f" "e" "l" "h"
               "ixl")))

(define reg-indirect
  (doMP* (symb "(")
         (x <- reg)
         (symb ")")
         (return `(,x indirect))))

(define reg
  (<:> reg-direct
       reg-indirect))

(define op
  (oneof-sym '("push" "pop" "ex" "add" "djnz" "adc" "inc")))

(define (read-file-string filename)
  (let* ((port (open-input-file filename))
         (data (get-string-all port)))
    (close-port port)
    data))

(define asm-comment
  (doMP* (token (char #\;))
         (up-to #\newline)
         (return '())))

(define asm-label
  (doMP* (name <- (many (<:> alpha-num (oneof "_"))))
         (char #\:)
         (return `(label ,(string->symbol name)))))


(define asm-op
  (doMP* (op <- op)
         (args <- (<:> (doMP* (a <- reg)
                              (symb ",")
                              (b <- reg)
                              (return `(,a ,b)))
                       reg
                       (return '())))
         (return `(,op ,args))))
(define asm-instr
  (<:> asm-label
       asm-op))

(define asm-prog2
  (doMP* (prog <- (sepby-n asm-instr (many (<:>  (char #\space) (char #\newline) asm-comment))))
         (char #\newline)
         (return prog)))


