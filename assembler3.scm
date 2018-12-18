(use-modules (ice-9 match)
             (ice-9 binary-ports)
             (rnrs bytevectors)
             (srfi srfi-9))

;; Turns out these things care called "register groups", bit-fields
;; for each register in various instructions.  Will be revised as I
;; implement more of the Z80 instruction set.
(define 16-bit-regs
  '((af . #b11)
    (bc . #b00)
    (de . #b01)
    (hl . #b10)))

(define push-pop-index-regs
  '((ix . #b11011101)
    (iy . #b11111101)))

(define ld-regs
  '((a . #b111)
    (b . #b000)
    (c . #b001)
    (d . #b010)
    (e . #b011)
    (h . #b100)
    (l . #b101)
    ((hl) . #b110)))

(define condition-codes
  '((nz . #b000)
    (z  . #b001)
    (nc . #b010)
    (c  . #b011)
    (po . #b100)
    (pe . #b101)
    (p . #b110)
    (m . #b111)))

(define (lookup key alist)
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
        #f)))

(define (index-reg? reg)
  (memq reg '(ix iy)))

;; We often see in the data sheet opcodes like this:
;; PUSH reg16 -> 11[reg16]0101
;; ((bc . #b00) (de . #b01) (hl . #b10) (af . #b11))

;; This means we can generate the opcode for that instruction by
;; offsetting a "register code" by some number of bits and performing
;; logical or on the result with the "template" opcode.
(define (make-opcode reg-code offset opcode)
  (logior (ash reg-code
               offset)
          opcode))

(define-record-type <inst>
  (make-inst cycles length generator)
  inst?
  (cycles inst-cycles)
  (length inst-length)
  (generator inst-generator))

(define (gen-inst inst)
  (force (inst-generator inst)))

(define (assemble-push reg)
  (if (index-reg? reg)
      (make-inst 15 2 (delay `(,(lookup reg push-pop-index-regs) #b11100101)))
      (make-inst 11 1 (delay `(,(make-opcode (lookup reg 16-bit-regs) 4 #b11000101))))))

(define (assemble-pop reg)
  (if (index-reg? reg)
      (make-inst 14 2 (delay `(,(lookup reg push-pop-index-regs) #b11100001)))
      (make-inst 10 1 (delay `(,(make-opcode (lookup reg 16-bit-regs) 4 #b11000001))))))

(define (unsigned-nat? x)
  (and (integer? x) (>= x 0)))
(define (8-bit-imm? x)
  (and (unsigned-nat? x)
       (> (ash 1 8) x)))
(define (16-bit-imm-or-label? x)
  (or (symbol? x)
      (and (unsigned-nat? x)
           (> (ash 1 16) x))))

(define (16-bit-imm? x)
  (and (unsigned-nat? x)
       (> (ash 1 16) x)))

(define (num->binary n)
  (format #f "~8,'0b" n))

(define (16-bit-reg? x)
  (lookup x 16-bit-regs))

;; Unsure of register group, so this solution for now.
(define (8-bit-reg? x)
  (member x '(a b c d e f h l ixl ixh i r (hl))))

(define (reg? x)
  (or (16-bit-reg? x)  (8-bit-reg? x)))

(define (assemble-ld-reg8-reg8 dest src)
  (make-inst (if (eqv? dest '(hl)) 7 4)
             1
             (delay
               `(,(make-opcode (lookup src ld-regs)
                               0
                               (make-opcode (lookup dest ld-regs)
                                            3
                                            #b01000000))))))
(define (assemble-ld-reg8-imm8 reg8 imm8)
  (make-inst (if (eqv? reg8 '(hl)) 10 7)
             2
             (delay
               `(,(make-opcode (lookup reg8 ld-regs) 3 #b00000110) ,imm8))))

(define (assemble-ld-hl-iimm16 iimm16)
  (make-inst 16
             3
             (delay `(#b00101010
                      ,(lsb iimm16)
                      ,(msb iimm16)))))

(define ld-iregs
  '((bc . #b0) (de . #b1)))

(define (assemble-ld-a-ireg16 reg)
  (make-inst 14
             3
             (delay
               `(,(make-opcode (lookup reg ld-iregs) 4 #b00001010)))))

;; Least significant byte.
(define (lsb n)
  (logand n 255))

;; Most significant byte.
(define (msb n)
  (ash n -8))

(define (resolve-label label-or-imm16)
  (if (16-bit-imm? label-or-imm16)
      label-or-imm16
      (or (lookup label-or-imm16 *labels*)
          (error (format #f "Label not found: ~a" label-or-imm16)))))

(define (assemble-ld-reg16-imm16 reg16 imm16)
  (make-inst 10
             3
             (delay (let ((imm16 (resolve-label imm16)))
                      `(,(make-opcode (lookup reg16 16-bit-regs) 4 #b00000001)
                        ,(lsb imm16)
                        ,(msb imm16))))))

(define (assemble-ld-ireg16-a reg16)
  (make-inst 7
             1
             (delay `(,(make-opcode (lookup reg16 16-bit-regs) 4 #b00000010)))))

(define (assemble-ld args)
  (match args
    (((? 8-bit-reg? a) (? 8-bit-reg? b))
     (assemble-ld-reg8-reg8 a b))
    (('a ((? 16-bit-reg? b)))
     (assemble-ld-a-ireg16 b))
    ((((? 16-bit-reg? b)) 'a)
     (assemble-ld-ireg16-a b))
    (((? 8-bit-reg? a) (? 8-bit-imm? b))
     (assemble-ld-reg8-imm8 a b))
    (((? 16-bit-reg? a) (? 16-bit-imm-or-label? b))
     (assemble-ld-reg16-imm16 a b))
    (('hl ((? 16-bit-imm-or-label? b)))
     (assemble-ld-hl-iimm16 b))
    )
  )

(define (simple-op? op) (lookup op simple-ops))

;; Operations that don't receive arguments or have specific ones.
(define simple-ops
  '((otdr         .  (0 2 (#b11101101 #b10111011)))
    (lddr         .  (0 2 (#b11101101 #b10111000)))
    (otir         .  (0 2 (#b11101101 #b10110011)))
    (indr         .  (0 2 (#b11101101 #b10110010)))
    (ldir         .  (0 2 (#b11101101 #b10110000)))
    (outd         .  (0 2 (#b11101101 #b10101011)))
    (ind          .  (0 2 (#b11101101 #b10101010)))
    (outi         .  (0 2 (#b11101101 #b10100011)))
    (ldi          .  (0 2 (#b11101101 #b10100000)))
    (rld          .  (0 2 (#b11101101 #b01101111)))
    (rrd          .  (0 2 (#b11101101 #b01100111)))
    (reti         .  (0 2 (#b11101101 #b01001101)))
    (retn         .  (0 2 (#b11101101 #b01000101)))
    (ei           .  (0 1 (#b11111011)))
    (di           .  (0 1 (#b11110011)))
    ((ex de hl)   .  (0 1 (#b11101011)))
    ((ex (sp) hl) .  (0 1 (#b11100011)))
    (exx          .  (0 1 (#b11011001)))
    (ret          .  (0 1 (#b11001001)))
    (ccf          .  (0 1 (#b00111111)))
    (scf          .  (0 1 (#b00110111)))
    (rra          .  (0 1 (#b00011111)))
    (rla          .  (0 1 (#b00010111)))
    (rrca         .  (0 1 (#b00001111)))
    ((ex af afp)  .  (0 1 (#b00001000)))
    (rlca         .  (0 1 (#b00000111)))
    (nop          .  (0 1 (#b00000000)))
    ))

(define (assemble-simple a)
  (let ((res (lookup a simple-ops)))
    (if res
        (make-inst (car res) (cadr res) (delay (caddr res)))
        (error (format #f "Operation not found: ~a" a)))))

(define (add-label! name)
  (if (assv name *labels*)
      (error (format #f "Cannot add another label of ~a" name))
      (set! *labels* `((,name . ,*pc*) . ,*labels*))))

(define (assemble-label name)
  (add-label! name)
  '())

(define (assemble-org new-pc)
  (set! *pc* new-pc)
  '())

(define (condition? s) (lookup s condition-codes))

(define (assemble-cond-jp cond imm16)
  (make-inst 10
             3
             (delay (let ((imm16 (resolve-label imm16)))
                      `(,(make-opcode (lookup cond condition-codes) 3 #b11000010)
                        ,(lsb imm16)
                        ,(msb imm16))))))

(define (assemble-uncond-jp imm16)
  (make-inst 10
             3
             (delay (let ((imm16 (resolve-label imm16)))
                      `(#b11000011
                        ,(lsb imm16)
                        ,(msb imm16))))))

(define (assemble-jp args)
  (match args
    (((? condition? a) (? 16-bit-imm-or-label? b))
     (assemble-cond-jp a b))
    ((? 16-bit-imm-or-label? a)
     (assemble-uncond-jp a))))


(define (assemble-cond-call cond imm16)
  (make-inst 13.5
             3
             (delay (let ((imm16 (resolve-label imm16)))
                      `(,(make-opcode (lookup cond condition-codes) 3 #b11000100)
                        ,(lsb imm16)
                        ,(msb imm16))))))

(define (assemble-uncond-call imm16)
  (make-inst 17
             3
             (delay (let ((imm16 (resolve-label imm16)))
                      `(#b11001101
                        ,(lsb imm16)
                        ,(msb imm16))))))

(define (assemble-call args)
  (match args
    (((? condition? a) (? 16-bit-imm-or-label? b))
     (assemble-cond-call a b))
    ((? 16-bit-imm-or-label? a)
     (assemble-uncond-call a))))

(define (assemble-dw word-list)
  (make-inst 0
             (ash (length word-list) 1)
             (delay (flatten (map
                              (lambda (x)
                                (list (lsb x) (msb x)))
                              word-list)))))

(define (assemble-db byte-list)
  (make-inst 0
             (length byte-list)
             (delay byte-list)))

(define (assemble-expr expr)
  (match expr
    (((? simple-op? a))
     (assemble-simple a))
    (`(ld ,dest ,src)
     (assemble-ld `(,dest ,src)))
    (`(push ,(? 16-bit-reg? a))
     (assemble-push a))
    (`(pop ,(? 16-bit-reg? a))
     (assemble-pop a))
    (`(label ,name)
     (assemble-label name))
    (`(org ,(? 16-bit-imm? a))
     (assemble-org a))
    (`(jp ,args)
     (assemble-jp args))
    (`(call ,args)
     (assemble-call args))
    (`(db ,arg)
     (assemble-db arg))
    (`(dw ,arg)
     (assemble-dw arg))
    (_
     (error (format #f "Unknown expression: ~s\n" expr))))
  )

(define *pc* 0)
(define *labels* 0)
(define (reset-labels-and-pc!)
  (set! *labels* '())
  (set! *pc* 0))

(define (assemble-prog exprs)
  (reset-labels-and-pc!)
  
  (map-in-order (lambda (x)
                  (let* ((res (assemble-expr x)))
                    (set! *pc* (+ *pc* (length res)))
                    res))
                exprs))

(define (write-bytevector-to-file bv fn)
  (let ((port (open-output-file fn)))
    (put-bytevector port bv)
    (close-port port)))

(define sample-prog
  '((org #x0000)
    (label foo)
    (ld hl #x1234)
    (push hl)
    (ret)

    (call foo)
    (jp end) ;; test look-ahead labels
    (ld a (hl))
    (ldir)
    (pop bc)
    (ld (bc) a)
    (label end)))

(define (flatten l)
  (if (null? l)
      '()
      (append (car l) (flatten (cdr l)))))

(define (assemble-to-binary prog)
  (map num->binary (flatten (assemble-prog prog))))

(define (assemble-to-binary2 prog)
  (map num->binary (flatten (assemble-prog2 prog))))

(define (assemble-to-file prog filename)
  (write-bytevector-to-file (u8-list->bytevector (flatten (assemble-prog prog))) filename))

(define (assemble-to-file2 prog filename)
  (write-bytevector-to-file (u8-list->bytevector (flatten (assemble-prog2 prog))) filename))

;; (assemble-to-file sample-prog "out.bin")

;; Use a Z80 disassembler to verify the output.



(define (pass1 exprs)
  ;; Check each instruction for correct syntax and produce code
  ;; generating thunks.  Meanwhile, increment PC accordingly and build
  ;; up labels.
  
  (reset-labels-and-pc!)
  
  (delq '() (map-in-order (lambda (x)
                            (let ((res (assemble-expr x)))
                              (if (inst? res)
                                  (set! *pc* (+ *pc* (inst-length res))))
                              res))
                          exprs)))

(define (pass2 insts)
  ;; Force the code generating thunks.  All labels should be resolved by now.
  (map-in-order gen-inst insts))

(define (assemble-prog2 prog)
  (pass2 (pass1 prog)))
