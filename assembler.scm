(include "parse-utils.scm")
(include "state-utils.scm")

(define (read-file-string filename)
  (let* ((port (open-input-file filename))
         (data (get-string-all port)))
    (close-port port)
    data))

;; Parse a list of symbols.
(define (symb* symb-list)
  (if (null? symb-list)
      (return "")
      (>>=P (symb (car symb-list))
            (lambda (_) (symb* (cdr symb-list))))))

(define asm-ex-de-hl
  (doMP* (symb* '("ex" "de" "," "hl"))
         (return #b11101011)))

(define asm-ex-af-afs
  (doMP* (symb* '("ex" "af" "," "af'"))
         (return #b00001000)))

(define asm-ex
  (<:> asm-ex-de-hl
       asm-ex-af-afs))

;; Parse a string and convert it to a symbol.
(define (symb-conv s)
  (doMP* (string <- (symb s))
         (return (string->symbol string))))

(define (oneof-sym sym-list)
  (if (null? sym-list)
      fail
      (<:> (symb-conv (car sym-list))
           (oneof-sym (cdr sym-list)))))

(define reg-direct
  (oneof-sym '("bc" "de" "hl" "af" "ix" "iy" "sp")))

(define reg-indirect
  (doMP* (symb "(")
         (x <- reg)
         (symbol ")")
         (return `(,x indirect))))

(define reg
  (<:> reg-direct
       reg-indirect))

(define op
  (oneof-sym '("push" "pop" "ex" "add" "djnz" "adc")))

(define opcode-data
  '((ldir . ((args . 0)
             (opcode . (#b11101101 #b10110000))))
    (push . ((args . 1)
             (opcode . #b11000101)
             (bc . #b00)
             (de . #b01)
             (hl . #b10)
             (af . #b11)))
    (pop . ((args . 1)
            (offset . 4)
            (opcode . #b11000001)
            (bc . #b00)
            (de . #b01)
            (hl . #b10)
            (af . #b11)))
    (ex . ((args . 2)
           (arg-data . ((#(0) .
                         ((de hl) . #b11101011))))))))


(define (lookup key alist)
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
        #f)))

(define (data-for-op op)
  (or (lookup op opcode-data)
      (error (format #f "No data for operation ~a." op))))

(define (opcode-for-simple-op op)
  (lookup 'opcode (data-for-op op)))

(define (arglen-for-op op)
  (lookup 'args (data-for-op op)))

(define (make-opcode op . args)
  (let ((op-data (lookup op opcode-data)))
    (if (null? op-data)
        (error "Invalid opcode!")
        (let ((opcode (lookup 'opcode op-data)))
          (if (null? args)
              opcode
              (let ((reg-code (lookup (car args) op-data))
                    (offset (lookup 'offset op-data)))
                (cond ((null? opcode)  (error (format #f "No opcode for operation ~a." op)))
                      ((null? reg-code) (error (format #f "Invalid register for operation ~a: ~a" op (car reg))))
                      ((null? offset) (error (format #f "No offset for operation ~a." op)))
                      (else (logior (ash reg-code
                                         offset)
                                    opcode)))))))))

(define asm-ldir
  (doMP* (symb "ldir")
         (return (make-opcode 'ldir))))
(define asm-push
  (doMP* (symb "push")
         (m <- reg)
         (return (make-opcode 'push m))))

(define asm-pop
  (doMP* (symb "pop")
         (m <- reg)
         (return (make-opcode 'pop m))))

(define asm-label
  (doMP* (name <- (many alpha-num))
         (char #\:)
         (return `(label ,(string->symbol name)))))

(define asm-statement
  (<:> asm-label
       asm-ex
       asm-ldir
       asm-push
       asm-pop))

(define asm-prog
  (doMP* (prog <- (sepby-n asm-statement (many (<:> (char #\space) (char #\newline)))))
         (char #\newline)
         (return prog)))

(define (parse-asm-file filename)
  (parse asm-prog (read-file-string filename)))

