(load "../src/assembler.scm")

(define (check-encoding instruction expected)
  (let ((actual (flatten (assemble-prog (list instruction)))))
    (if (not (equal? actual expected))
        (error (format #f "~s assembled as ~s; expected ~s"
                       instruction actual expected)))))

(for-each
 (lambda (case)
   (let ((register (car case))
         (in-opcode (cadr case))
         (out-opcode (caddr case)))
     (check-encoding `(in ,register (c)) (list #xed in-opcode))
     (check-encoding `(out (c) ,register) (list #xed out-opcode))))
 '((b #x40 #x41)
   (c #x48 #x49)
   (d #x50 #x51)
   (e #x58 #x59)
   (h #x60 #x61)
   (l #x68 #x69)
   (a #x78 #x79)))

(define (check-rejected instruction)
  (let ((rejected
         (catch #t
           (lambda () (flatten (assemble-prog (list instruction))) #f)
           (lambda args #t))))
    (if (not rejected)
        (error (format #f "invalid instruction was accepted: ~s" instruction)))))

(for-each check-rejected
          '((push sp)
            (pop sp)
            (ld af #x1234)
            (ld a (sp))
            (add hl af)
            (xor i)))

(display "assembler I/O encoding tests passed\n")
