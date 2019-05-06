;; Convenience functions
(define (string s)
  `(,@(map char->integer (string->list s)) 0))

;; At assembly time, print the value of the program counter.
(define PRINT-PC
  (lambda ()
    (format #t "~a\n" (num->hex *pc*))
    ;; Macros need to return () or an instruction record..
    '()))

(define (make-multi-op op)
  (lambda (l)
    (map (lambda (x) `(,op ,x))
         l)))

;; Multiple operations.
(define push* (make-multi-op 'push))
(define pop* (make-multi-op 'pop))
(define call* (make-multi-op 'call))

;; Relative jumps like JR $+3
;; Write ,(jr-rel 3) instead in the quasi-quoted program.
(define (jr-rel amount)
  (lambda () (assemble-expr `(jr ,(+ *pc* amount))))
  )

;; With a flag
(define (jr-rel-f flag amount)
  (lambda () (assemble-expr `(jr ,flag ,(+ *pc* amount))))
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

(define (fill-up-to byte addr)
  (lambda ()
    (assemble-expr `(db ,(make-list
                          (- addr *pc*)
                          byte)))))

(define fill-until-end
  (lambda ()
    (assemble-expr
     `(db ,(make-list (- #x100000 *pc*) #xff)))))

(define (concat l) (apply append l))

(define (repeat n expr)
  (concat (make-list n expr)))

(define (concat-map f l)
  (concat (map f l)))
