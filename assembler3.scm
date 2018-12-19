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
    (hl . #b10)
    (sp . #b11)))

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

(define ir-regs
  '((i . 0)
    (r . 1)))

(define condition-codes
  '((nz . #b000)
    (z  . #b001)
    (nc . #b010)
    (c  . #b011)
    (po . #b100)
    (pe . #b101)
    (p . #b110)
    (m . #b111)))

(define jr-condition-codes
  '((nz . #b00)
    (z  . #b01)
    (nc . #b10)
    (c  . #b11)))

(define (lookup key alist)
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
        (begin
          (format #t "Failed to lookup: ~a\n" key)
          #f))))

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
  (make-inst-rec cycles length generator)
  inst?
  (cycles inst-cycles)
  (length inst-length)
  (generator inst-generator))

(define-syntax make-inst
  (syntax-rules ()
    ((_ cycles length generator)
     (make-inst-rec cycles length (delay generator)))))

(define (gen-inst inst)
  (force (inst-generator inst)))

(define (assemble-push reg)
  (if (index-reg? reg)
      (make-inst 15 2 `(,(lookup reg push-pop-index-regs) #b11100101))
      (make-inst 11 1 `(,(make-opcode (lookup reg 16-bit-regs) 4 #b11000101)))))

(define (assemble-pop reg)
  (if (index-reg? reg)
      (make-inst 14 2  `(,(lookup reg push-pop-index-regs) #b11100001))
      (make-inst 10 1 `(,(make-opcode (lookup reg 16-bit-regs) 4 #b11000001)))))

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

(define (num->hex n)
  (format #f "~2,'0x" n))

(define (16-bit-reg? x)
  (lookup x 16-bit-regs))

;; Unsure of register group, so this solution for now.
(define (8-bit-reg? x)
  (member x '(a b c d e f h l ixl ixh i r (hl))))

(define (ir-reg? x)
  (lookup x ir-regs))

(define (reg? x)
  (or (16-bit-reg? x)  (8-bit-reg? x)))

(define (assemble-ld-reg8-reg8 dest src)
  (make-inst (if (eqv? dest '(hl)) 7 4)
             1
             `(,(make-opcode (lookup src ld-regs)
                             0
                             (make-opcode (lookup dest ld-regs)
                                          3
                                          #b01000000)))))
(define (assemble-ld-reg8-imm8 reg8 imm8)
  (make-inst (if (eqv? reg8 '(hl)) 10 7)
             2
             `(,(make-opcode (lookup reg8 ld-regs) 3 #b00000110) ,imm8)))

(define (assemble-ld-hl-iimm16 iimm16)
  (make-inst 16
             3
             `(#b00101010
               ,(lsb iimm16)
               ,(msb iimm16))))

(define ld-iregs
  '((bc . #b0) (de . #b1)))

(define (assemble-ld-a-ireg16 reg)
  (make-inst 14
             3
             `(,(make-opcode (lookup reg ld-iregs) 4 #b00001010))))

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
             (let ((imm16 (resolve-label imm16)))
               `(,(make-opcode (lookup reg16 16-bit-regs) 4 #b00000001)
                 ,(lsb imm16)
                 ,(msb imm16)))))

(define (assemble-ld-ireg16-a reg16)
  (make-inst 7
             1
             `(,(make-opcode (lookup reg16 16-bit-regs) 4 #b00000010))))

(define ld-index-imm16-regs
  '((ix . #b11011101)
    (iy . #b11111101)))

(define (assemble-ld-index-imm16 ireg imm16)
  (make-inst 14
             4
             (let ((imm16 (resolve-label imm16)))
               `(,(lookup ireg ld-index-imm16-regs)
                 #b00100001
                 ,(lsb imm16)
                 ,(msb imm16)))))

(define (assemble-ld-ir-reg ir)
  (make-inst 9
             2
             `(#b11101101
               ,(make-opcode (lookup ir ir-regs) 3 #b01010111))))

(define (assemble-ld-iimm16-a addr)
  (make-inst 13
             3
             `(#b00110010
               ,(lsb addr)
               ,(msb addr))))

(define (assemble-ld-a-imm16 addr)
  (make-inst 13
             3
             `(#b00111010
               ,(lsb addr)
               ,(msb addr))))

(define (assemble-ld args)
  (match args
    (('a (? ir-reg? b))
     (assemble-ld-ir-reg b))
    (((? 8-bit-reg? a) (? 8-bit-reg? b))
     (assemble-ld-reg8-reg8 a b))
    (('a ((? 16-bit-reg? b)))
     (assemble-ld-a-ireg16 b))
    (('a ((? 16-bit-imm-or-label? b)))
     (assemble-ld-a-imm16 b))
    ((((? 16-bit-reg? b)) 'a)
     (assemble-ld-ireg16-a b))
    ((((? 16-bit-imm-or-label? a)) 'a)
     (assemble-ld-iimm16-a a))
    (((? 8-bit-reg? a) (? 8-bit-imm? b))
     (assemble-ld-reg8-imm8 a b))
    (((? 16-bit-reg? a) (? 16-bit-imm-or-label? b))
     (assemble-ld-reg16-imm16 a b))
    (('hl ((? 16-bit-imm-or-label? b)))
     (assemble-ld-hl-iimm16 b))
    (((? index-reg? a) (? 16-bit-imm-or-label? b))
     (assemble-ld-index-imm16 a b))
    
    (_
     (error (format #f "Invalid operands to ld: ~a" args))))
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
    ((ex af afs)  .  (0 1 (#b00001000)))
    (rlca         .  (0 1 (#b00000111)))
    (nop          .  (0 1 (#b00000000)))
    ))

(define (assemble-simple a)
  (let ((res (lookup a simple-ops)))
    (if res
        (make-inst (car res) (cadr res) (caddr res))
        (error (format #f "Operation not found: ~a" a)))))

(define (add-label! name val)
  (if (assv name *labels*)
      (error (format #f "Cannot add another label of ~a" name))
      (begin
        (format #t "Adding label ~a with value 0x~4,'0x\n" name val)
        (set! *labels* `((,name . ,val) . ,*labels*)))))


(define (advance-pc! count)
  (set! *pc* (+ *pc* count)))
(define (assemble-label name)
  (add-label! name *pc*)
  '())

(define (assemble-org new-pc)
  (set! *pc* new-pc)
  '())

(define (condition? s) (lookup s condition-codes))

(define (assemble-cond-jp cond imm16)
  (make-inst 10
             3
             (let ((imm16 (resolve-label imm16)))
               `(,(make-opcode (lookup cond condition-codes) 3 #b11000010)
                 ,(lsb imm16)
                 ,(msb imm16)))))

(define (assemble-uncond-jp imm16)
  (make-inst 10
             3
             (let ((imm16 (resolve-label imm16)))
               `(#b11000011
                 ,(lsb imm16)
                 ,(msb imm16)))))

(define (assemble-jp args)
  (match args
    (((? condition? a) b)
     (assemble-cond-jp a b))
    (((? 16-bit-imm-or-label? a))
     (assemble-uncond-jp a))))


(define (signed-8-bit-imm? x)
  (and (integer? x)
       (>= 127 (abs x))))

(define (jr-simm8-convert x)
  (if (negative? x)
      (+ 256 x)
      x)
  )

(define (resolve-jr-label-or-simm x)
  (if (symbol? x)
      (let* ((dest (resolve-label x))
             (offset (- dest *pc*)))
        ;; (format #t "~a\n" *pc*)
        ;; Compute the offset from the current program counter
        (if (not (signed-8-bit-imm? offset))
            (error (format #f "Offset ~a too far for 8-bit signed.\n" offset))
            (jr-simm8-convert offset)))
      (and (signed-8-bit-imm? x)
           (jr-simm8-convert (- x *pc*)))))

(define (assemble-cond-jr cond simm8)
  (make-inst 9.5
             2
             (let ((simm8 (resolve-jr-label-or-simm simm8)))
               `(,(make-opcode (lookup cond condition-codes) 3 #b00100000)
                 ,simm8))))

(define (assemble-uncond-jr simm8)
  (make-inst 12
             2
             (let ((simm8 (resolve-jr-label-or-simm simm8)))
               `(#b00011000
                 ;; Follwed by a signed byte, -127 to +127
                 ,simm8))))

(define (assemble-jr args)
  (match args
    (((? condition? a) b)
     (assemble-cond-jr a b))
    (((? 16-bit-imm-or-label? a))
     (assemble-uncond-jr a))
    ))


(define (assemble-cond-call cond imm16)
  (make-inst 13.5
             3
             (let ((imm16 (resolve-label imm16)))
               `(,(make-opcode (lookup cond condition-codes) 3 #b11000100)
                 ,(lsb imm16)
                 ,(msb imm16)))))

(define (assemble-uncond-call imm16)
  (make-inst 17
             3
             (let ((imm16 (resolve-label imm16)))
               `(#b11001101
                 ,(lsb imm16)
                 ,(msb imm16)))))

(define (assemble-call args)
  (match args
    (((? condition? a) (? 16-bit-imm-or-label? b))
     (assemble-cond-call a b))
    (((? 16-bit-imm-or-label? a))
     (assemble-uncond-call a))
    (_
     (error (format #f "Invalid operands to call: ~a" args)))))

(define (assemble-dw word-list)
  (make-inst 0
             (ash (length word-list) 1)
             (flatten (map
                       (lambda (x)
                         (list (lsb x) (msb x)))
                       word-list))))

(define (assemble-db byte-list)
  (make-inst 0
             (length byte-list)
             byte-list))

(define (assemble-out-iimm8-a port)
  (make-inst 11
             2
             `(#b11010011
               ,port)))

(define (assemble-out-c-reg reg)
  (make-inst 12
             2
             `(#b11101011
               ,(make-opcode (lookup reg ld-regs) 3 #b01000001))))

(define (assemble-out arg)
  (match arg
    ((((? 8-bit-imm? p)) 'a)
     (assemble-out-iimm8-a p))
    (`((c) ,(? 8-bit-reg? r))
     (assemble-out-c-reg r))
    (_
     (error (format #f "Invalid operands to out: ~a" arg)))))

(define (assemble-in-a-iimm8 imm8)
  (make-inst 11
             2
             `(#b11011011
               ,imm8)))

(define (assemble-in-reg8-ic reg)
  (make-inst 12
             2
             `(#b11101011
               ,(make-opcode (lookup reg ld-regs) 3 #b01000000))))

(define (assemble-in arg)
  (match arg
    (('a ((? 8-bit-imm? p)))
     (assemble-in-a-iimm8 p))
    (`(,(? 8-bit-reg? r) (c))
     (assemble-in-reg8-ic r))
    (_
     (error (format #f "Invalid operands to out: ~a" arg)))))

(define (assemble-xor-8-bit-reg a)
  (make-inst (if (eqv? a '(hl)) 7 4)
             1
             `(,(make-opcode (lookup a ld-regs) 0 #b10101000))))

(define (assemble-xor-8-bit-imm a)
  (make-inst 7
             2
             `(#b11101110
               ,a)))

(define (assemble-xor arg)
  (match arg
    ((? 8-bit-reg? a)
     (assemble-xor-8-bit-reg a))
    ((? 8-bit-imm? a)
     (assemble-xor-8-bit-imm a))
    (_
     (error (format #f "Invalid operands to xor: ~a" arg)))))

(define (assemble-dec-8-bit-reg a)
  (make-inst (if (eqv? a '(hl)) 11 4)
             1
             `(,(make-opcode (lookup a ld-regs) 3 #b00000101))))

(define (assemble-dec-16-bit-reg a)
  (make-inst 6
             1
             `(,(make-opcode (lookup a 16-bit-regs) 4 #b00001011))))

(define (assemble-dec arg)
  (match arg
    ((? 8-bit-reg? a)
     (assemble-dec-8-bit-reg a))
    ((? 16-bit-reg? a)
     (assemble-dec-16-bit-reg a))
    (_
     (error (format #f "Invalid operands to dec: ~a" arg)))))

(define (assemble-inc-8-bit-reg arg)
  (make-inst (if (eqv? arg '(hl)) 11 4)
             1
             `(,(make-opcode (lookup arg ld-regs) 3 #b00000100))))

(define (assemble-inc-16-bit-reg arg)
  (make-inst 6
             1
             `(,(make-opcode (lookup arg 16-bit-regs) 4 #b00000011))))

(define (assemble-inc arg)
  (match arg
    ((? 8-bit-reg? a)
     (assemble-inc-8-bit-reg a))
    ((? 16-bit-reg? a)
     (assemble-inc-16-bit-reg a))))

(define (assemble-bit imm3 reg8)
  (make-inst (if (eqv? reg8 '(hl)) 12 8)
             2
             `(#b11001011
               ,(make-opcode imm3
                             3
                             (make-opcode (lookup reg8 ld-regs) 0 #b01000000)))))

(define (assemble-res imm3 reg8)
  (make-inst (if (eqv? reg8 '(hl)) 15 8)
             2
             `(#b11001011
               ,(make-opcode imm3
                             3
                             (make-opcode (lookup reg8 ld-regs) 0 #b10000000)))))

(define (assemble-set imm3 reg8)
  (make-inst (if (eqv? reg8 '(hl)) 15 8)
             2
             `(#b11001011
               ,(make-opcode imm3
                             3
                             (make-opcode (lookup reg8 ld-regs) 0 #b11000000)))))

(define (assemble-adc-8-bit-reg reg)
  (make-inst (if (eqv? reg '(hl)) 7 4)
             1
             `(,(make-opcode (lookup reg ld-regs) 0 #b10001000))))

(define (assemble-adc arg)
  (match arg
    (`(a ,(? 8-bit-reg? a))
     (assemble-adc-8-bit-reg a))
    (_
     (error (format #f "Invalid operands to adc: ~a" arg)))))

(define (assemble-and-8-bit-reg a)
  (make-inst (if (eqv? a '(hl)) 7 4)
             1
             `(,(make-opcode (lookup a ld-regs) 0 #b10100000))))

(define (assemble-and-8-bit-imm a)
  (make-inst 7
             2
             `(#b11100110
               ,a)))

(define (assemble-and arg)
  (match arg
    ((? 8-bit-reg? a)
     (assemble-and-8-bit-reg a))
    ((? 8-bit-imm? a)
     (assemble-and-8-bit-imm a))
    (_
     (error (format #f "Invalid operands to and: ~a" arg)))))

(define (assemble-or-8-bit-reg a)
  (make-inst (if (eqv? a '(hl)) 7 4)
             1
             `(,(make-opcode (lookup a ld-regs) 0 #b10110000))))

(define (assemble-or-8-bit-imm a)
  (make-inst 7
             2
             `(#b11110110
               ,a)))

(define (assemble-or arg)
  (match arg
    ((? 8-bit-reg? a)
     (assemble-or-8-bit-reg a))
    ((? 8-bit-imm? a)
     (assemble-or-8-bit-imm a))
    (_
     (error (format #f "Invalid operands to or: ~a" arg)))))

(define (assemble-ret-cond c)
  (make-inst 8
             1
             `(,(make-opcode (lookup c condition-codes) 3 #b11000000))))

(define (assemble-ret arg)
  (match arg
    ((? condition? a)
     (assemble-ret-cond a))
    (_
     (error (format #f "Invalid operands to ret: ~a" arg)))))

(define (assemble-expr expr)
  ;; Pattern match EXPR against the valid instructions and dispatch to
  ;; the corresponding sub-assembler.
  (match expr
    (((? simple-op? a))
     (assemble-simple a))
    (`(ld ,dest ,src)
     (assemble-ld `(,dest ,src)))
    (`(push ,a)
     (assemble-push a))
    (`(pop ,a)
     (assemble-pop a))
    (`(label ,name)
     (assemble-label name))
    (`(org ,(? 16-bit-imm? a))
     (assemble-org a))
    ;; Some instructions have multiple possible arguments, they should
    ;; be handled by the sub-assembler.
    (`(jp . ,args)
     (assemble-jp args))
    (`(jr . ,args)
     (assemble-jr args))
    (`(call . ,args)
     (assemble-call args))
    (`(bit ,imm3 ,arg)
     (assemble-bit imm3 arg))
    (`(res ,imm3 ,arg)
     (assemble-res imm3 arg))
    (`(set ,imm3 ,arg)
     (assemble-set imm3 arg))
    (`(ret ,arg)
     (assemble-ret arg))
    (`(db ,arg)
     (assemble-db arg))
    (`(dw ,arg)
     (assemble-dw arg))
    (`(out ,dest ,src)
     (assemble-out `(,dest ,src)))
    (`(in ,dest ,src)
     (assemble-in `(,dest ,src)))
    (`(xor ,arg)
     (assemble-xor arg))
    (`(or ,arg)
     (assemble-or arg))
    (`(dec ,arg)
     (assemble-dec arg))
    (`(inc ,arg)
     (assemble-inc arg))
    (`(adc . ,args)
     (assemble-adc args))
    (`(and ,arg)
     (assemble-and arg))
    (_
     (error (format #f "Unknown expression: ~s\n" expr))))
  )

(define *pc* 0)
(define *labels* 0)
(define (reset-pc!)
  (set! *pc* 0))
(define (reset-labels!)
  (set! *labels* '()))

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

(define (assemble-to-hex prog)
  (map num->hex (flatten (assemble-prog prog))))

(define (assemble-to-file prog filename)
  (write-bytevector-to-file (u8-list->bytevector (flatten (assemble-prog prog))) filename))

;; (assemble-to-file sample-prog "out.bin")

(define (pass1 exprs)
  ;; Check each instruction for correct syntax and produce code
  ;; generating thunks.  Meanwhile, increment PC accordingly and build
  ;; up labels.
  
  (reset-labels!)
  (reset-pc!)
  (format #t "Pass one...\n")

  ;; Every assembled instruction, or inlined procedure should return a
  ;; value.  A value of () indicates that it will not be included in
  ;; pass 2.
  (filter
   (lambda (x) (not (null? x)))
   (map-in-order
    (lambda (x)
      (if (procedure? x)
          ;; Evaluate an inlined procedure (could do anything(!)).
          (let ((macro-val (x)))
            ;; But that procedure has to return () or an instruction
            ;; record.
            (if (not (or (null? macro-val)
                         (inst? macro-val)))
                (error (format #f
                               "Error during pass one: macro did not return an instruction record: instead got ~a.  PC: ~a\n"
                               macro-val
                               *pc*))
                macro-val))

          ;; Assemble a normal instruction.
          (let ((res (assemble-expr x)))
            (if (inst? res)
                (advance-pc! (inst-length res)))
            res)))
    exprs)))

(define (pass2 insts)
  (reset-pc!)
  (format #t "Pass two...\n")
  ;; Force the code generating thunks.  All labels should be resolved by now.
  (map-in-order
   (lambda (x)
     (if (not (inst? x))
         (error (format #f "Error during pass two: not an instruction record: ~a. PC: ~a." x (num->hex *pc*))))
     (advance-pc! (inst-length x))
     (let ((res (gen-inst x)))
       (format #t "PC: ~a ~a\n" (num->hex *pc*) res)
       res))
   insts)
  )

(define (assemble-prog prog)
  (pass2 (pass1 prog)))

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

;; Assembling this program should yield a bit-for-bit identical binary
;; to that of the SmileyOS kernel (a minimal OS that draws a smiley to
;; the screen).  I'm using it because it's the "minimum viable" OS, as
;; it includes everything from unlocking flash to unlocking the screen
;; and other stuff I'm not currently interested in.

(define swap-sector #x38)
(define smiley-os
  `(,(equ 'flash-executable-ram #x8000)
    ,(equ 'flash-executable-ram-size 100)
    (jp boot)
    (ld d e)
    (ld c e)
    (nop)
    (nop)
    (dec sp)
    (ret)
    ,@(apply append (make-list 5 `(,@ (make-list 7 '(nop))
                                      (ret))))
    ,@(make-list 7 '(nop))
    (jp #xe9)
    ,@(make-list 24 '(nop))
    (jp boot)
    (db (#xff #xa5 #xff))
    
    (label boot)
    (di)
    (ld a #x6)
    (out (4) a)
    (ld a #x81)
    (out (7) a)
    (ld sp 0)
    (call #x3e7)
    (di)
    (ld sp 0)
    (ld a 6)
    (out (4) a)
    (ld a #x81)
    (out (7) a)
    (ld a #x3)
    (out (#xe) a)
    (xor a)
    (out (#xf) a)
    (call #x414)
    (xor a)
    (out (#x25) a)
    (dec a)
    (out (#x26) a)
    (out (#x23) a)
    (out (#x22) a)
    (call #x42a)
    (ld a 1)
    (out (#x20) a)
    (ld a #xb)
    (out (3) a)
    (ld hl #x8000)
    (ld (hl) 0)
    (ld de #x8001)
    (ld bc #x7fff)
    (ldir)

    ;; Arbitrarily complicated macros!
    ,@(apply append (map (lambda (x)
                           `((ld a ,x)
                             (call #x50f)
                             (out (#x10) a)))
                         '(5 1 3 #x17 #xb #xef)))
    
    (ld iy #x8100)
    (ld hl #xe5)
    (ld b 4)
    (ld de 0)
    ,@(call* '(#x866 #x4ca #xbb4 #xbad))
    (jp boot)
    (ld d b)
    (nop)
    (adc a b)
    (ld (hl) b)
    (label sys-interrupt)
    (di)
    ,@(push* '(af bc de hl ix iy))
    (exx)
    ((ex af afs))
    ,@(push* '(af bc de hl))
    (jp usb-interrupt)
    (label interrupt-resume)
    (in a (4))
    (bit 0 a)
    (jr nz int-handle-on)
    (bit 1 a)
    (jr nz int-handle-timer1)
    (bit 2 a)
    (jr nz int-handle-timer2)
    (bit 4 a)
    (jr nz int-handle-link)
    (jr sys-interrupt-done)
    
    (label int-handle-on)
    (in a (3))
    (res 0 a)
    (out (3) a)
    (set 0 a)
    (out (3) a)
    (jr sys-interrupt-done)
    
    (label int-handle-timer1)
    (in a (3))
    (res 1 a)
    (out (3) a)
    (set 1 a)
    (out (3) a)
    (jr sys-interrupt-done)

    (label int-handle-timer2)
    (in a (3))
    (res 2 a)
    (out (3) a)
    (set 2 a)
    (out (3) a)
    (jr sys-interrupt-done)
    
    (label int-handle-link)
    (in a (3))
    (res 4 a)
    (out (3) a)
    (set 4 a)
    (out (3) a)
    
    (label sys-interrupt-done)
    
    ,@(pop* '(hl de bc af))
    (exx)
    ((ex af afs))
    ,@(pop* '(iy ix hl de bc af))
    (ei)
    (ret)
    (label usb-interrupt)
    (in a (#x55))
    (bit 0 a)
    (jr z usb-unknown-event)
    (bit 2 a)
    (jr z usb-line-event)
    (bit 4 a)
    (jr z usb-protocol-event)
    (jp interrupt-resume)
    (label usb-unknown-event)
    (jp interrupt-resume)
    (label usb-line-event)

    (in a (#x56))
    (xor #xff)
    (out (#x57) a)
    (jp #xfb)

    (label usb-protocol-event)
    ,@(map (lambda (x) `(in a (,x)))
           '(#x82 #x83 #x84 #x85 #x86))

    (jp interrupt-resume)
    
    (label write-flash-byte)
    (push bc)
    (ld b a)
    (push af)
    (ld a i)
    (push af)
    (di)
    (ld a b)
    ,@(push* '(hl de bc hl de bc))

    (ld hl write-flash-byte-ram)
    (ld de flash-executable-ram)
    (ld bc #x1f)
    (ldir)
    ,@(pop* '(bc de hl))
    (call flash-executable-ram)
    ,@(pop* '(bc de hl af))
    (jp po local-label1)
    (ei)
    (label local-label1)
    ,@(pop* '(af bc))
    (ret)
    (label write-flash-byte-ram)
    (and (hl))
    (ld b a)
    (ld a #xaa)
    (ld (#xaaa) a)
    (ld a #x55)
    (ld (#x555) a)
    (ld a #xa0)
    (ld (#xaaa) a)
    (ld (hl) b)
    (label local-label2)
    (ld a b)
    (xor (hl))
    (bit 7 a)
    (jr z write-flash-byte-done)
    (bit 5 (hl))
    (jr z local-label2)
    (label write-flash-byte-done)
    (ld (hl) #xf0)
    (ret)
    
    (label write-flash-byte-ram-end)
    
    (label write-flash-buffer)
    (push af)
    (ld a i)
    (push af)
    (di)
    ,@(push* '(hl de bc hl de bc))
    (ld hl write-flash-buffer-ram)
    (ld de flash-executable-ram)
    (ld bc #x2c)
    (ldir)
    ,@(pop* '(bc de hl))
    (call flash-executable-ram)
    ,@(pop* '(bc de hl af))
    (jp po local-label3)
    (ei)
    (label local-label3)
    (pop af)
    (ret)

    (label write-flash-buffer-ram)
    (label write-flash-buffer-loop)
    (ld a #xaa)
    (ld (#xaaa) a)
    (ld a #x55)
    (ld (#x555) a)
    (ld a #xa0)
    (ld (#xaaa) a)
    (ld a (hl))
    (ld (de) a)
    (inc de)
    (dec bc)
    
    (label local-label4)
    (xor (hl))
    (bit 7 a)
    (jr z local-label5)
    (bit 5 a)
    (jr z local-label4)
    (ld a #xf0)
    (ld (0) a)
    (ret)
    (label local-label5)
    (inc hl)
    (ld a b)
    (or a)
    (jr nz write-flash-buffer-loop)
    (ld a c)
    (or a)
    (jr nz write-flash-buffer-loop)
    (ret)
    
    (label write-flash-buffer-ram-end)
    
    (label erase-flash-sector)
    (push bc)
    (ld b a)
    (push af)
    (ld a i)
    (ld a i)
    (push af)
    (di)
    (ld a b)
    ,@(push* '(hl de bc hl de bc))
    (ld hl erase-flash-sector-ram)
    (ld de flash-executable-ram)
    (ld bc #x30)
    (ldir)
    ,@(pop* '(bc de hl))
    (call flash-executable-ram)
    ,@(pop* '(bc de hl af))
    (jp po local-label6)
    (ei)
    (label local-label6)
    (pop af)
    (pop bc)
    (ret)
    
    (label erase-flash-sector-ram)
    (out (6) a)
    (ld a #xaa)
    (ld (#x0aaa) a)
    (ld a #x55)
    (ld (#x0555) a)
    (ld a #x80)
    (ld (#x0aaa) a)
    (ld a #xaa)
    (ld (#x0aaa) a)
    (ld a #x55)
    (ld (#x0555) a)
    (ld a #x30)
    (ld (#x4000) a)
    (label local-label7)
    (ld a (#x4000))
    (bit 7 a)
    (ret nz)
    (bit 5 a)
    (jr z local-label7)
    (ld a #xf0)
    (ld (#x4000) a)
    (ret)
    (label erase-flash-sector-ram-end)

    (label erase-flash-page)
    ,@(push* '(af bc af))
    (call copy-sector-to-swap)
    (pop af)
    (push af)
    (call erase-flash-sector)
    (pop af)
    (ld c a)
    (and #b11111100)
    (ld b ,swap-sector)
    (label local-label8)
    (cp c)
    (jr z local-label9)
    (call copy-flash-page)
    (label local-label9)
    (inc b)
    (inc a)
    (push af)
    (ld a b)
    (and #b11111100)
    (or a)
    (jr z local-label10)
    (pop af)
    (jr local-label8)
    (label local-label10)
    ,@(pop* '(af bc af))
    (ret)
    (label erase-flash-page-ram)
    (label copy-sector-to-swap)
    (push af)
    (ld a ,swap-sector)
    (call erase-flash-sector)
    (pop af)
    (push bc)
    (ld b a)
    (push af)
    (ld a i)
    (ld a i)
    (push af)
    (di)
    (ld a b)
    (and #b11111100)
    (push hl)
    (push de)
    (ld hl copy-sector-to-swap-ram)
    
    ,PRINT-PC
    ))

(define (reload-and-assemble-to-file prog file)
  (load "assembler3.scm")
  (assemble-to-file prog file))
