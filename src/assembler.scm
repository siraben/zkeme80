(use-modules (ice-9 match) (rnrs io ports) (rnrs bytevectors) (srfi srfi-9))

;; set! this to #t to see debugging information.  Note that `lookup`
;; will complain a lot but generally it's fine.
(define verbose? #f)

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
          ;; Verbose
          (if verbose? (format #t "Failed to lookup: ~a\n" key))
          #f))))

(define (index-reg? reg)
  (member reg '(ix iy)))

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
  (make-inst-rec length generator)
  inst?
  (length inst-length)
  (generator inst-generator))

(define-syntax make-inst
  (syntax-rules ()
    ((_ length generator)
     (make-inst-rec length (delay generator)))))

(define (gen-inst inst)
  (force (inst-generator inst)))

(define (assemble-push reg)
  (if (index-reg? reg)
      (make-inst 2 `(,(lookup reg push-pop-index-regs) #b11100101))
      (make-inst 1 `(,(make-opcode (lookup reg 16-bit-regs) 4 #b11000101)))))

(define (assemble-pop reg)
  (if (index-reg? reg)
      (make-inst 2  `(,(lookup reg push-pop-index-regs) #b11100001))
      (make-inst 1 `(,(make-opcode (lookup reg 16-bit-regs) 4 #b11000001)))))

(define (unsigned-nat? x) (and (integer? x) (>= x 0)))
(define (num->binary n) (format #f "~8,'0b" n))
(define (num->hex n) (format #f "~2,'0x" n))
(define (16-bit-reg? x) (lookup x 16-bit-regs))
(define (8-bit-reg? x) (member x '(a b c d e f h l i r (hl))))
(define (ir-reg? x) (lookup x ir-regs))
(define (reg? x) (or (16-bit-reg? x)  (8-bit-reg? x)))

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


(define (assemble-ld-reg8-reg8 dest src)
  (make-inst 1
             `(,(make-opcode (lookup src ld-regs)
                             0
                             (make-opcode (lookup dest ld-regs)
                                          3
                                          #b01000000)))))

(define (assemble-ld-reg8-imm8 reg8 imm8)
  (make-inst 2
             `(,(make-opcode (lookup reg8 ld-regs) 3 #b00000110) ,imm8)))

(define (assemble-ld-hl-iimm16 iimm16)
  (make-inst 3
             (let ((iimm16 (resolve-label iimm16)))
               `(#b00101010
                 ,(lsb iimm16)
                 ,(msb iimm16)))))

(define ld-iregs '((bc . #b0) (de . #b1)))

(define (assemble-ld-a-ireg16 reg)
  (make-inst 1 `(,(make-opcode (lookup reg ld-iregs) 4 #b00001010))))

;; Least significant byte.
(define (lsb n) (logand n 255))

;; Most significant byte.
(define (msb n) (ash n -8))

(define (resolve-label label-or-imm16)
  (if (16-bit-imm? label-or-imm16)
      label-or-imm16
      (or (lookup label-or-imm16 *labels*)
          (error (format #f "Label not found: ~a" label-or-imm16)))))

(define (assemble-ld-reg16-imm16 reg16 imm16)
  (make-inst 3 (let ((imm16 (resolve-label imm16)))
                 `(,(make-opcode (lookup reg16 16-bit-regs) 4 #b00000001)
                   ,(lsb imm16)
                   ,(msb imm16)))))


(define (assemble-ld-reg16-iimm16 reg16 imm16)
  (make-inst 4 (let ((imm16 (resolve-label imm16)))
                 `(#b11101101
                   ,(make-opcode (lookup reg16 16-bit-regs) 4 #b01001011)
                   ,(lsb imm16)
                   ,(msb imm16)))))

(define (assemble-ld-ireg16-a reg16)
  (make-inst 1 `(,(make-opcode (lookup reg16 16-bit-regs) 4 #b00000010))))

(define ld-index-imm16-regs
  '((ix . #b11011101)
    (iy . #b11111101)))

(define (assemble-ld-index-imm16 ireg imm16)
  (make-inst 4 (let ((imm16 (resolve-label imm16)))
                 `(,(lookup ireg ld-index-imm16-regs)
                   #b00100001
                   ,(lsb imm16)
                   ,(msb imm16)))))

(define (assemble-ld-ir-reg ir)
  (make-inst 2 `(#b11101101
                 ,(make-opcode (lookup ir ir-regs) 3 #b01010111))))

(define (assemble-ld-a-ir ir)
  (make-inst 2 `(#b11101101
                 ,(make-opcode (lookup ir ir-regs) 3 #b01000111))))

(define (assemble-ld-iimm16-a addr)
  (make-inst 3 (let ((addr (resolve-label addr)))
                 `(#b00110010
                   ,(lsb addr)
                   ,(msb addr)))))

(define (assemble-ld-a-imm16 addr)
  (make-inst 3 (let ((addr (resolve-label addr)))
                 `(#b00111010
                   ,(lsb addr)
                   ,(msb addr)))))

(define (assemble-ld-sp-hl)
  (make-inst 1 `(#b11111001)))

(define (assemble-ld-reg8-index-offset a b c)
  (make-inst 3 `(,(if (eq? b 'ix) #b11011101 #b11111101)
                 ,(make-opcode (lookup a ld-regs) 3 #b01000110)
                 ,c)))

(define (assemble-ld-index-reg8 a b c)
  (make-inst 3 `(,(lookup a ld-index-imm16-regs)
                 ,(make-opcode (lookup c ld-regs) 0 #b01110000)
                 ,b)))

(define (assemble-ld-iimm-reg16 a b)
  (make-inst 4 (let ((a (resolve-label a)))
                 `(#b11101101
                   ,(make-opcode (lookup b 16-bit-regs) 4 #b01000011)
                   ,(lsb a)
                   ,(msb a)))))

(define (assemble-ld args)
  (match args
    ('(sp hl)                                                  (assemble-ld-sp-hl))
    (('a (? ir-reg? b))                                        (assemble-ld-ir-reg b))
    (((? ir-reg? b) 'a)                                        (assemble-ld-a-ir b))
    (((? 8-bit-reg? a) (? 8-bit-reg? b))                       (assemble-ld-reg8-reg8 a b))
    (((? 8-bit-reg? a) ('+ (? index-reg? b) (? 8-bit-imm? c))) (assemble-ld-reg8-index-offset a b c))
    (((? 8-bit-reg? a) ('+ (? 8-bit-imm? c) (? index-reg? b))) (assemble-ld-reg8-index-offset a b c))
    (('a ((? 16-bit-reg? b)))                                  (assemble-ld-a-ireg16 b))
    (('a ((? 16-bit-imm-or-label? b)))                         (assemble-ld-a-imm16 b))
    ((((? 16-bit-reg? b)) 'a)                                  (assemble-ld-ireg16-a b))
    ((((? 16-bit-imm-or-label? a)) 'a)                         (assemble-ld-iimm16-a a))
    (((? 8-bit-reg? a) (? 8-bit-imm? b))                       (assemble-ld-reg8-imm8 a b))
    (((? 16-bit-reg? a) (? 16-bit-imm-or-label? b))            (assemble-ld-reg16-imm16 a b))
    (((? 16-bit-reg? a) ((? 16-bit-imm-or-label? b)))          (assemble-ld-reg16-iimm16 a b))
    (('hl ((? 16-bit-imm-or-label? b)))                        (assemble-ld-hl-iimm16 b))
    (((? index-reg? a) (? 16-bit-imm-or-label? b))             (assemble-ld-index-imm16 a b))
    ((('+ (? index-reg? a) (? 8-bit-imm? b)) (? 8-bit-reg? c)) (assemble-ld-index-reg8 a b c))
    ((('+ (? 8-bit-imm? b) (? index-reg? a)) (? 8-bit-reg? c)) (assemble-ld-index-reg8 a b c))
    ((((? 16-bit-imm-or-label? a)) (? 16-bit-reg? b))          (assemble-ld-iimm-reg16 a b))
    (_
     (error (format #f "Invalid operands to ld: ~a" args))))
  )

(define (simple-op? op) (lookup op simple-ops))

;; Operations that don't receive arguments or have specific ones.
(define simple-ops
  '((otdr         .  (#b11101101 #b10111011))
    (lddr         .  (#b11101101 #b10111000))
    (otir         .  (#b11101101 #b10110011))
    (indr         .  (#b11101101 #b10110010))
    (cpir         .  (#b11101101 #b10110001))    
    (ldir         .  (#b11101101 #b10110000))
    (outd         .  (#b11101101 #b10101011))
    (ind          .  (#b11101101 #b10101010))
    (outi         .  (#b11101101 #b10100011))
    (ldi          .  (#b11101101 #b10100000))
    (rld          .  (#b11101101 #b01101111))
    (rrd          .  (#b11101101 #b01100111))
    (reti         .  (#b11101101 #b01001101))
    (retn         .  (#b11101101 #b01000101))
    (neg          .  (#b11101101 #b01000100))
    (ei           .  (#b11111011))
    (di           .  (#b11110011))
    ((ex de hl)   .  (#b11101011))
    ((ex (sp) hl) .  (#b11100011))
    (exx          .  (#b11011001))
    (ret          .  (#b11001001))
    (halt         .  (#b01110110))
    (ccf          .  (#b00111111))
    (scf          .  (#b00110111))
    (cpl          .  (#b00101111))
    (rra          .  (#b00011111))
    (rla          .  (#b00010111))
    (rrca         .  (#b00001111))
    ((ex af afs)  .  (#b00001000))
    (rlca         .  (#b00000111))
    (nop          .  (#b00000000))
    ))

(define (assemble-simple a)
  (let ((res (lookup a simple-ops)))
    (if res
        (make-inst (length res) res)
        (error (format #f "Operation not found: ~a" a)))))

(define (add-label! name val)
  (if (assv name *labels*)
      (error (format #f "Cannot add another label of ~a" name))
      (begin
        (if verbose?
            (format #t "Adding label ~a with value 0x~4,'0x\n" name val))
        (set! *labels* `((,name . ,val) . ,*labels*)))))

(define (advance-pc! count) (set! *pc* (+ *pc* count)))

(define (assemble-label name)
  (add-label! name *pc*)
  '())

(define (assemble-org new-pc)
  (set! *pc* new-pc)
  '())

(define (condition? s) (lookup s condition-codes))

(define (assemble-cond-jp cond imm16)
  (make-inst 3 (let ((imm16 (resolve-label imm16)))
                 `(,(make-opcode (lookup cond condition-codes) 3 #b11000010)
                   ,(lsb imm16)
                   ,(msb imm16)))))

(define (assemble-uncond-jp imm16)
  (make-inst 3 (let ((imm16 (resolve-label imm16)))
                 `(#b11000011
                   ,(lsb imm16)
                   ,(msb imm16)))))

(define (assemble-jp args)
  (match args
    ((('hl))                      (make-inst 1 `(#b11101001)))
    (((? condition? a) b)         (assemble-cond-jp a b))
    (((? 16-bit-imm-or-label? a)) (assemble-uncond-jp a))
    (_
     (error (format #f "Invalid operands to jp: ~a" args)))))

(define (signed-8-bit-imm? x)
  (and (integer? x) (>= 127 (abs x))))

(define (jr-simm8-convert x)
  (if (negative? x) (+ 256 x) x))

(define (resolve-jr-label-or-simm x)
  (if (symbol? x)
      (let* ((dest (resolve-label x))
             (offset (- dest *pc*)))
        ;; (format #t "~a\n" *pc*)
        ;; Compute the offset from the current program counter
        (if (not (signed-8-bit-imm? offset))
            (error (format #f "Operand to jr ~a not an 8-bit signed integer." offset))
            (jr-simm8-convert offset)))
      (and (signed-8-bit-imm? x)
           (jr-simm8-convert (- x *pc*)))))

(define (assemble-cond-jr cond simm8)
  (make-inst 2 (let ((simm8 (resolve-jr-label-or-simm simm8)))
                 `(,(make-opcode (lookup cond condition-codes) 3 #b00100000)
                   ,simm8))))

(define (assemble-uncond-jr simm8)
  (make-inst 2 (let ((simm8 (resolve-jr-label-or-simm simm8)))
                 `(#b00011000
                   ;; Follwed by a signed byte, -127 to +127
                   ,simm8))))

(define (assemble-jr args)
  (match args
    (((? condition? a) b)         (assemble-cond-jr a b))
    (((? 16-bit-imm-or-label? a)) (assemble-uncond-jr a))
    (_
     (error (format #f "Invalid operands to jr: ~a" args)))))

(define (assemble-cond-call cond imm16)
  (make-inst 3 (let ((imm16 (resolve-label imm16)))
                 `(,(make-opcode (lookup cond condition-codes) 3 #b11000100)
                   ,(lsb imm16)
                   ,(msb imm16)))))

(define (assemble-uncond-call imm16)
  (make-inst 3 (let ((imm16 (resolve-label imm16)))
                 `(#b11001101
                   ,(lsb imm16)
                   ,(msb imm16)))))

(define (assemble-call args)
  (match args
    (((? condition? a) (? 16-bit-imm-or-label? b)) (assemble-cond-call a b))
    (((? 16-bit-imm-or-label? a))                  (assemble-uncond-call a))
    (_
     (error (format #f "Invalid operands to call: ~a" args)))))

(define (assemble-dw word-list)
  (make-inst (ash (length word-list) 1)
                          (flatten (map
                                    (lambda (x)
                                      (let ((x (if (symbol? x) (resolve-label x) x)))
                                        (if x
                                            (list (lsb x) (msb x))
                                            (error (format #f "Invalid word in dw: ~a" x)))))
                                    word-list))))

(define (assemble-db byte-list)
  (make-inst (length byte-list)
                          (if (all-sat? 8-bit-imm? byte-list)
                              byte-list
                              (error (format #f "Invalid byte in db: ~a" byte-list)))))

(define (assemble-out-iimm8-a port)
  (make-inst 2 `(#b11010011
                 ,port)))

(define (assemble-out-c-reg reg)
  (make-inst 2 `(#b11101011
                 ,(make-opcode (lookup reg ld-regs) 3 #b01000001))))

(define (assemble-out arg)
  (match arg
    ((((? 8-bit-imm? p)) 'a)     (assemble-out-iimm8-a p))
    (`((c) ,(? 8-bit-reg? r))    (assemble-out-c-reg r))
    (_ (error (format #f "Invalid operands to out: ~a" arg)))))

(define (assemble-in-a-iimm8 imm8)
  (make-inst 2 `(#b11011011
                 ,imm8)))

(define (assemble-in-reg8-ic reg)
  (make-inst 2 `(#b11101011
                 ,(make-opcode (lookup reg ld-regs) 3 #b01000000))))

(define (assemble-in arg)
  (match arg
    (('a ((? 8-bit-imm? p))) (assemble-in-a-iimm8 p))
    (((? 8-bit-reg? r) '(c)) (assemble-in-reg8-ic r))
    (_ (error (format #f "Invalid operands to out: ~a" arg)))))

(define (assemble-xor-8-bit-reg a)
  (make-inst 1 `(,(make-opcode (lookup a ld-regs) 0 #b10101000))))

(define (assemble-xor-8-bit-imm a)
  (make-inst 2 `(#b11101110
                 ,a)))

(define (assemble-xor arg)
  (match arg
    ((? 8-bit-reg? a) (assemble-xor-8-bit-reg a))
    ((? 8-bit-imm? a) (assemble-xor-8-bit-imm a))
    (_
     (error (format #f "Invalid operands to xor: ~a" arg)))))

(define (assemble-dec-8-bit-reg a)
  (make-inst 1 `(,(make-opcode (lookup a ld-regs) 3 #b00000101))))

(define (assemble-dec-16-bit-reg a)
  (make-inst 1 `(,(make-opcode (lookup a 16-bit-regs) 4 #b00001011))))

(define (assemble-dec-index-reg a)
  (make-inst 2 `(,(lookup a ld-index-imm16-regs)
                 #b00101011)))

(define (assemble-dec arg)
  (match arg
    ((? 8-bit-reg?  a) (assemble-dec-8-bit-reg a))
    ((? 16-bit-reg? a) (assemble-dec-16-bit-reg a))
    ((? index-reg?  a) (assemble-dec-index-reg a))
    (_
     (error (format #f "Invalid operands to dec: ~a" arg)))))

(define (assemble-inc-8-bit-reg arg)
  (make-inst 1 `(,(make-opcode (lookup arg ld-regs) 3 #b00000100))))

(define (assemble-inc-16-bit-reg arg)
  (make-inst 1 `(,(make-opcode (lookup arg 16-bit-regs) 4 #b00000011))))


(define (assemble-inc-index-reg arg)
  (make-inst 2 `(,(lookup arg ld-index-imm16-regs)
                 #b00100011)))

(define (assemble-inc arg)
  (match arg
    ((? 8-bit-reg? a)  (assemble-inc-8-bit-reg a))
    ((? 16-bit-reg? a) (assemble-inc-16-bit-reg a))
    ((? index-reg? a)  (assemble-inc-index-reg a))
    (_
     (error #f "Invalid operands to inc: ~a" arg))))

(define (assemble-bit imm3 reg8)
  (make-inst 2 `(#b11001011
                 ,(make-opcode imm3
                               3
                               (make-opcode (lookup reg8 ld-regs) 0 #b01000000)))))

(define (assemble-res imm3 reg8)
  (make-inst 2 `(#b11001011
                 ,(make-opcode imm3
                               3
                               (make-opcode (lookup reg8 ld-regs) 0 #b10000000)))))

(define (assemble-set imm3 reg)
  (cond ((8-bit-reg? reg)
         (make-inst 2 `(#b11001011
                        ,(make-opcode imm3
                                      3
                                      (make-opcode (lookup reg ld-regs) 0 #b11000000)))))
        ((index-reg? (car reg))
         (make-inst 4 `(,(lookup (car reg) ld-index-imm16-regs)
                        #b11001011
                        ;; No offset for now.
                        #b00000000
                        ,(make-opcode imm3 3 #b11000110))))
        (else
         (error (format #f "Invalid operands to set: ~a" `(,imm3 ,reg))))))

(define (assemble-adc-8-bit-reg reg)
  (make-inst 1 `(,(make-opcode (lookup reg ld-regs) 0 #b10001000))))

(define (assemble-adc-16-bit-reg reg)
  (make-inst 2 `(#b11101101
                 ,(make-opcode (lookup reg 16-bit-regs) 4 #b01001010))))

(define (assemble-adc arg)
  (match arg
    (`(a ,(? 8-bit-reg? a))
     (assemble-adc-8-bit-reg a))
    (`(hl ,(? 16-bit-reg? a))
     (assemble-adc-16-bit-reg a))
    (_
     (error (format #f "Invalid operands to adc: ~a" arg)))))

(define (assemble-and-8-bit-reg a)
  (make-inst 1 `(,(make-opcode (lookup a ld-regs) 0 #b10100000))))

(define (assemble-and-8-bit-imm a)
  (make-inst 2 `(#b11100110 ,a)))

(define (assemble-and-index-reg a)
  (make-inst 3 `(,(lookup a ld-index-imm16-regs)
                 #b10100110
                 ;; No offset for now.
                 #b00000000)))

(define (assemble-and arg)
  (match arg
    ((? 8-bit-reg? a)   (assemble-and-8-bit-reg a))
    ((? 8-bit-imm? a)   (assemble-and-8-bit-imm a))
    (((? index-reg? a)) (assemble-and-index-reg a))
    (_
     (error (format #f "Invalid operands to and: ~a" arg)))))

(define (assemble-or-8-bit-reg a)
  (make-inst 1 `(,(make-opcode (lookup a ld-regs) 0 #b10110000))))

(define (assemble-or-8-bit-imm a)
  (make-inst 2 `(#b11110110 ,a)))

(define (assemble-or arg)
  (match arg
    ((? 8-bit-reg? a) (assemble-or-8-bit-reg a))
    ((? 8-bit-imm? a) (assemble-or-8-bit-imm a))
    (_
     (error (format #f "Invalid operands to or: ~a" arg)))))

(define (assemble-ret-cond c)
  (make-inst 1 `(,(make-opcode (lookup c condition-codes) 3 #b11000000))))

(define (assemble-add-hl-reg16 a)
  (make-inst 1 `(,(make-opcode (lookup a 16-bit-regs) 4 #b00001001))))

(define (assemble-add-reg8 a)
  (make-inst 1 `(,(make-opcode (lookup a ld-regs) 0 #b10000000))))

(define (assemble-add-index-reg16 a b)
  (make-inst 2 `(,(if (eq? a 'ix) #b11011101 #b11111101)
                 ,(make-opcode (lookup b 16-bit-regs) 4 #b00001001))))

(define (assemble-add-imm8 a)
  (make-inst 2 `(#b11000110 ,a)))

(define (assemble-add arg)
  (match arg
    (('hl (? 16-bit-reg? a))              (assemble-add-hl-reg16 a))
    (((? index-reg? a) (? 16-bit-reg? b)) (assemble-add-index-reg16 a b))
    (('a (? 8-bit-reg? a))                (assemble-add-reg8 a))
    (('a (? 8-bit-imm? a))                (assemble-add-imm8 a))
    (_
     (error (format #f "Invalid operands to add: ~a" arg)))))

(define (assemble-sub-reg8 a)
  (make-inst 1 `(,(make-opcode (lookup a ld-regs) 0 #b10010000))))

(define (assemble-sub-imm8 a)
  (make-inst 2 `(#b11010110 ,a)))

(define (assemble-sub arg)
  (match arg
    (((? 8-bit-reg? a)) (assemble-sub-reg8 a))
    (((? 8-bit-imm? a)) (assemble-sub-imm8 a))
    (_
     (error (format #f "Invalid operands to sub: ~a" arg)))))

(define (assemble-ret arg)
  (match arg
    ((? condition? a) (assemble-ret-cond a))
    (_
     (error (format #f "Invalid operands to ret: ~a" arg)))))

(define (assemble-cp-reg8 arg)
  (make-inst 1 `(,(make-opcode (lookup arg ld-regs) 0 #b10111000))))

(define (assemble-cp-imm8 arg)
  (make-inst 2 `(#b11111110 ,arg)))


(define (assemble-cp arg)
  (match arg
    ((? 8-bit-reg? arg) (assemble-cp-reg8 arg))
    ((? 8-bit-imm? arg) (assemble-cp-imm8 arg))
    (_
     (error (format #f "Invalid operands to cp: ~a" arg)))))

(define (assemble-sbc-hl-reg16 a)
  (make-inst 2 `(#b11101101
                 ,(make-opcode (lookup a 16-bit-regs) 4 #b01000010))))

(define (assemble-sbc arg)
  (match arg
    (('hl (? 16-bit-reg? a)) (assemble-sbc-hl-reg16 a))
    (_
     (error (format #f "Invalid operands to sbc: ~a" arg)))))

(define (assemble-im arg)
  (match arg
    (0 (make-inst 2 '(#b11101101 #b01000110)))
    (1 (make-inst 2 '(#b11101101 #b01010110)))
    (2 (make-inst 2 '(#b11101101 #b01011110)))))

(define (assemble-sla-reg8 a)
  (make-inst 2 `(#b11001011
                 ,(make-opcode (lookup a ld-regs) 0 #b00100000))))

(define (assemble-sla arg)
  (match arg
    ((? 8-bit-reg? a) (assemble-sla-reg8 a))
    (_
     (error (format #f "Invalid operands to sla: ~a" arg)))))

(define (assemble-rl-reg8 a)
  (make-inst 2 `(#b11001011
                 ,(make-opcode (lookup a ld-regs) 0 #b00010000))))

(define (assemble-rl arg)
  (match arg
    ((? 8-bit-reg? a) (assemble-rl-reg8 a))
    (_
     (error (format #f "Invalid operands to rl: ~a" arg)))))

(define (assemble-rr-reg8 a)
  (make-inst 2 `(#b11001011 ,(make-opcode (lookup a ld-regs) 0 #b00011000))))

(define (assemble-rr arg)
  (match arg
    ((? 8-bit-reg? a) (assemble-rr-reg8 a))
    (_
     (error (format #f "Invalid operands to rr: ~a" arg)))))

(define (assemble-djnz simm8)
  (make-inst 2
             (let ((simm8 (resolve-jr-label-or-simm simm8)))
               `(#b00010000
                 ;; Follwed by a signed byte, -127 to +127
                 ,simm8))))

(define (assemble-srl-reg8 a)
  (make-inst 2
             `(#b11001011
               ,(make-opcode (lookup a ld-regs) 0 #b00111000))))

(define (assemble-srl arg)
  (match arg
    ((? 8-bit-reg? a) (assemble-srl-reg8 a))
    (_
     (error (format #f "Invalid operands to srl: ~a" arg)))))

(define rst-numbers
  '((#x00 . #b000)
    (#x08 . #b001)
    (#x10 . #b010)
    (#x18 . #b011)
    (#x20 . #b100)
    (#x28 . #b101)
    (#x30 . #b110)
    (#x38 . #b111)))

(define (rst-number? a) (lookup a rst-numbers))

(define (assemble-rst arg)
  (match arg
    ((? rst-number? a)
     (make-inst 1
                `(,(make-opcode (lookup a rst-numbers)
                                3
                                #b11000111))))
    (_
     (error (format #f "Invalid operands to rst: ~a" arg)))))

(define (assemble-expr expr)
  ;; Pattern match EXPR against the valid instructions and dispatch to
  ;; the corresponding sub-assembler.
  (match expr
    (((? simple-op? a))          (assemble-simple a))
    (`(ld    ,dest ,src)         (assemble-ld `(,dest ,src)))
    (`(push  ,arg)               (assemble-push arg))
    (`(pop   ,arg)               (assemble-pop arg))
    (`(label ,name)              (assemble-label name))
    (`(org   ,(? 16-bit-imm? a)) (assemble-org a))
    (`(jp    .     ,args)        (assemble-jp args))
    (`(jr    .     ,args)        (assemble-jr args))
    (`(call  .     ,args)        (assemble-call args))
    (`(add   .     ,args)        (assemble-add args))
    (`(sub   .     ,args)        (assemble-sub args))
    (`(sbc   .     ,args)        (assemble-sbc args))
    (`(adc   .     ,args)        (assemble-adc args))
    (`(bit   ,imm3 ,arg)         (assemble-bit imm3 arg))
    (`(res   ,imm3 ,arg)         (assemble-res imm3 arg))
    (`(set   ,imm3 ,arg)         (assemble-set imm3 arg))
    (`(ret         ,arg)         (assemble-ret arg))
    (`(db          ,arg)         (assemble-db arg))
    (`(dw          ,arg)         (assemble-dw arg))
    (`(out   ,dest ,src)         (assemble-out `(,dest ,src)))
    (`(in    ,dest ,src)         (assemble-in `(,dest ,src)))
    (`(xor         ,arg)         (assemble-xor arg))
    (`(cp          ,arg)         (assemble-cp arg))
    (`(or          ,arg)         (assemble-or arg))
    (`(dec         ,arg)         (assemble-dec arg))
    (`(inc         ,arg)         (assemble-inc arg))
    (`(and         ,arg)         (assemble-and arg))
    (`(im          ,arg)         (assemble-im arg))
    (`(sla         ,arg)         (assemble-sla arg))
    (`(rl          ,arg)         (assemble-rl arg))
    (`(rr          ,arg)         (assemble-rr arg))
    (`(djnz        ,arg)         (assemble-djnz arg))
    (`(srl         ,arg)         (assemble-srl arg))
    (`(rst         ,arg)         (assemble-rst arg))
    (_ (error (format #f "Unknown expression: ~a" expr))))
  )

(define *pc*            0)
(define *labels*        0)
(define (reset-pc!)     (set! *pc* 0))
(define (reset-labels!) (set! *labels* '()))

(define (write-bytevector-to-file bv fn)
  (let ((port (open-output-file fn)))
    (put-bytevector port bv)
    (close-port port)))

(define (flatten l)
  (if (null? l)
      '()
      (append (car l) (flatten (cdr l)))))

(define (all-sat? p l)
  (cond ((null? l) #t)
        ((p (car l)) (all-sat? p (cdr l)))
        (else #f)))

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
   (lambda (x) (not (null? (car x))))
   ;; Order of SRFI1 map is unspecified, but Guile's map-in-order goes from
   ;; left to right.
   (map-in-order
    (lambda (expr)
      (if (procedure? expr)
          ;; Evaluate an inlined procedure (could do anything(!)).
          (let ((macro-val (expr)))
            ;; But that procedure has to return () or an instruction
            ;; record.
            (if (not (or (null? macro-val)
                         (inst? macro-val)))
                (error (format #f
                               "Error during pass one: macro did not return an instruction record: instead got ~a.  PC: ~a"
                               macro-val
                               *pc*))
                (begin (if (inst? macro-val)
                           ;; This macro generated an instruction
                           ;; record, so advance the program counter.
                           (advance-pc! (inst-length macro-val)))
                       ;; Return a "tagged" result, where the original
                       ;; expression is preserved for debugging.
                       (cons macro-val expr))))

          ;; Assemble a normal instruction.
          (let ((res (assemble-expr expr)))
            (if (inst? res)
                (advance-pc! (inst-length res)))
            ;; Return a "tagged" result, where the original expression
            ;; is preserved, for debugging..
            (cons res expr))))
    exprs)))

(define (pass2 insts)
  (reset-pc!)
  (format #t "Pass two...\n")
  ;; Force the code generating thunks.  All labels should be resolved by now.
  (map-in-order
   (lambda (x)
     (if (not (inst? (car x)))
         (error (format #f "Pass 2: not an instruction record: ~a. PC: ~a." (car x) (num->hex *pc*))))
     (advance-pc! (inst-length (car x)))
     (let ((res (gen-inst (car x))))
       (if verbose? (format #t "PC: ~a ~a\n" (num->hex *pc*) (cdr x)))
       (cond
        ;; Check consistency of declared instruction length and actual
        ;; length.
        ((not (= (inst-length (car x)) (length res)))
         (error (format #f
                        "Pass 2: Instruction length declared does not match actual: Expected length ~a, got length ~a of expression ~a\n PC: ~a"
                        (inst-length (car x))
                        (length res)
                        res
                        *pc*)))
        ;; Check that everything is an 8-bit unsigned number.
        ((not (all-sat? 8-bit-imm? res))
         (error (format #f "Invalid byte at ~4'0x: ~a" *pc* res)))
        (else
         ;; We're ok.
         res))))
   insts))

(define (assemble-prog prog)
  (pass2 (pass1 prog)))

(define (assemble-to-binary prog)
  (map num->binary (flatten (assemble-prog prog))))

(define (assemble-to-hex prog)
  (map num->hex (flatten (assemble-prog prog))))

(define (assemble-to-file prog filename)
  (write-bytevector-to-file
   (u8-list->bytevector (flatten (assemble-prog prog)))
   filename))

;; Take n elements from a list.
(define (take n list)
  (if (or (zero? n) (null? list))
      '()
      (cons (car list)
            (take (1- n) (cdr list)))))

;; For debugging purposes.  Assemble the program and find the
;; instruction that is at the specified byte address.
(define (assemble-find-instr-byte byte prog context)
  (let ((partial-asm (pass1 prog)))
    (let loop ((pc 0) (rest-insts partial-asm))
      (cond ((null? rest-insts) (error (format #f "Reached end of program before specified address ~a" byte)))
            ((>= pc byte)
             (map cdr (take context rest-insts)))
            (else
             (loop (+ pc (inst-length (caar rest-insts)))
                   (cdr rest-insts)))))))
