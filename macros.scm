(define (string s)
  `(,@(map char->integer (string->list s)) 0))

;; At assembly time, print the value of the program counter.
(define PRINT-PC
  (lambda ()
    (format #t "~a\n" (num->hex *pc*))
    ;; Macros need to return () or an instruction record..
    '()))

;; Multiple pushes.
(define (push* l)
  (map (lambda (x) `(push ,x))
       l))

;; Multple pops.
(define (pop* l)
  (map (lambda (x) `(pop ,x))
       l))

;; Multiple calls.
(define (call* l)
  (map (lambda (x) `(call ,x))
       l))

;; Relative jumps like JR $+3
;; Write ,(jr-rel 3) instead in the quasi-quoted program.
(define (jr-rel amount)
  (lambda () (assemble-expr `(jr `,(+ *pc* ,amount))))
  )

;; Constant symbols. VAL must be an integer
(define (equ sym val)
  (lambda ()
    (if (not (16-bit-imm? val))
        (error (format #f "Error in equ: Cannot set ~a to ~a." sym val))
        (add-label! sym val))
    '()))

(define-syntax with-regs-preserve
  (syntax-rules ()
    ((_ (reg reg* ...) body body* ...)
     `(,@(push* '(reg reg* ...))
       body body* ...
       ,@(pop* (reverse '(reg reg* ...)))))))

(define fill-until-end
  (lambda () (assemble-expr `(db ,(make-list
                                   (- #x100000 *pc*)
                                   #xff)))))
