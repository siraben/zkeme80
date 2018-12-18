        ;; ( -- )
_:
        ;; 16-bit AND operator, yay!
;; 19 + 10 + 19 + 10 = 58
                                            ; (2 bytes) and for size bytes (2 bytes)
        ;; ( 32bit_high 32bit_low 8bit -- 32*8high 32*8 low )
;; 4 + 19 + 4 + 19 = 46
;; 4 + 4 = 8
        ;; 46 + 10 + 10 + 14 + 14 + 4 + 8 + 58 = 164
        ;; 46 + 14 = 60
        ;; a b c d => c d a b
        ;; ( a b -- remainder quotient )
        ;; a certain length).
;;  ACIX: ACIX / DE
        ;; a contains the byte received.
        ;; A contains the character.
;; A contains the next character from the buffer.
actual_word:
actual_word_loop:
actual_word_write:
        adc a, b
        adc hl,de
        adc hl, de
        adc hl, hl
add16to32:
add16to32_done:
;; add16To32 [Maths]
        add a, 1
        add a, h
        add hl,bc
        add hl, bc
        add hl,de
        add hl, de
        add    hl, de        ;hl now points to data location in original program.
        add hl, hl
        add ix, de
        add ix, ix
        ;; address of DOCOL right now, but we shouldn't hardcode it so
        ;; Address of the most recently defined word.
        ;; ( address_to_write_to -- )
        ;; ( addr len -- hash_addr )
        ;; Add the offset
        ;; Ad-hoc solution to read a number.
        ;; A is another non-space, printable character.
akey_asm:
akey_return_space:
        ;; Allocate 255 bytes (default block size, change later if needed)
        and (hl)
        ;; AND lowest byte first.
        ;; Are two strings equal?
        ;; Are we compiling or are we interpreting?
;; asking for input until a character that is in the table is input.
;;  B: 0
;; ( -- base_addr len )
        ;; Base of the parameter stack.
        ;; b_call _CpHLDE
        ; bc = -bc
        ;; BC contains the high quotient
;     BC is the frequency
;;  BC: Multiplicand
        ;; BC points to a codeword, execute it!
;; BC = TOS   IX = RSP
        bit 6, a
        bit 7, a
bit_cache: .dw 0
        ;; Bitwise NOT.
blk_name_buffer: .fill 9, 0
        call akey_asm
        call _c_comma
        call _comma
        call cpHLBC
        call div32By16
        ;; call dodoes.  We want this on the top of the stack.
        call get_char_asm
        call get_str
        call key_asm
        call mul16by16
        call p_FreqOut
        call printhl_safe
        call setup_data_segment
        call strcmp
        call unget_char
        call zero_blk_name_buffer
        ccf
_c_comma
        ;; c d a, BC = b
        ;; changing the value of rbrac's string.
        ;; Check if 0th column.
clear_loop:
;; Coming later:  A interpreter defined in Forth and meta-compiled.
_comma:
;;  Compares DE to BC.
;;  Compares HL to BC.
;;  Compares HL to DE.
        ;; Compile a call in the new word where call DOCOL used to be.
        ;; Convention for this Forth:  ( high low -- )
        ;; Convert a key code into an ASCII character by way of a
        ;; Convert a pointer returned by FIND to the start of the code
        ;; Convert a pointer returned by FIND to the start of the name
        ;; Convert to ASCII.
;; Convience macros to transfer 16-bit register contents.
        ;; Copy the 8-character name.
        cp ' '
        cp '\\'
        cp 0
        cp 0 ;; or a
        cp b
cpBCDE:
;; cpBCDE [Maths]
        cpd
        cp (hl)
cpHLBC:
;; cpHLBC [Maths]
cpHLDE:
;; cpHLDE [Maths]
        cpir
        cp kClear
        cp kDel
        cp kEnter
        cp kLeft
        cp kRight ;; right arrow, alpha mode
        cp kSpace
        cpl
        cp '\n'
        cp '\n' ;; if we're reading from a converted text file.
        cp STRING_BUFFER_SIZE
        cp '\t'
        ;; Create link pointer and update var_latest to point to it.
;; Creating an editor.  Rough idea: We want to have a block-editing
;;  C: Set if string HL is alphabetically earlier than string DE
data_end:
data_start:
        ;; .db ") "
.db "     ",$00,"  " ;; 0   - 7
.db "]  , .01"       ;; 136 - 143
        ;; .db "0BRANCH ( "
.db "        "       ;; 104 - 111
.db "        "       ;; 112 - 119
.db "        "       ;; 120 - 127
.db "        "       ;; 16  - 23
.db "        "       ;; 184 - 191
.db "      : "       ;; 192 - 199
.db "  ?\"    "      ;; 200 - 207
.db "        "       ;; 208 - 215
.db "        "       ;; 216 - 223
.db "        "       ;; 224 - 231
.db "    {};\\"      ;; 232 - 239
.db "23456789"       ;; 144 - 151
.db "        "       ;; 240 - 247
.db "        "       ;; 24  - 31
.db "        "       ;; 248 - 255
.db "        "       ;; 32  - 39
.db "       !"       ;; 40  - 47
.db "   =  ' "       ;; 48  - 55
.db "_@>     "       ;; 56  - 63
.db " <      "       ;; 64  - 71
.db "        "       ;; 72  - 79
.db "        "       ;; 80  - 87
.db "        "       ;; 8   - 15
.db "        "       ;; 88  - 95
.db "        "       ;; 96  - 103
.db "  ABCDEF"       ;; 152 - 159
        .db $BB,$6D
        ;; .db "BRANCH ( "
.db "+-*/^()",$C1    ;; 128 - 135
        .db $CB, $31 ; sll c
        ;; .db "EXIT "
.db "GHIJKLMN"       ;; 160 - 167
        ;; .db "IMMEDIATE "
.db "OPQRSTUV"       ;; 168 - 175
.db "WXYZ    "       ;; 176 - 183
dd_loop:
dd_overflow:
dd_setBit:
        dec b
        dec bc
        ;; DE = C, BC = D, HL = A (old-DE B)
        dec c
        dec de
        dec hl
        dec (hl)
        dec ixl
        ;; DE contains the address of the next instruction to go to
        ;; DE contains the start of the memory location.
        ;; decrements BC, loops until BC = 0.
;; Defined as a jump to reduce code size.
;;  DEHL: Product of DE and BC.
;;  DEHL: product of DEHL and A
;; DE = IP    IY = UP
;;  DE: Multiplier
;;  DE: String pointer
;;  Determines if two strings are equal, and checks alphabetical sort order.
        ;; Display a null-terminated string starting at the address
div32By16:
;; div32By16 [Maths]
divACbyDE:
        ;; Divide C by 16.
        djnz clear_loop
        djnz dd_loop
        djnz sqrt_loop
        ex de,hl
        ex de, hl
        ex    de, hl         ;write back.
        ex (sp), hl
        ;; How many bytes have we used?
        inc b
        inc bc
        inc bc ;; Skip the link pointer.
        inc c
        inc d
        inc de
        inc de          ;; 6
        inc de  ;; Skip link pointer and length
        inc hl
        inc ixl
        ;; Indicate that this variable is a program.
        ;; Initialize word_buffer_ptr to point at the actual start.
        ;; Input: LA
;; Input: none
;; Inputs:
;Inputs:
        ;; interpreter to read the entered text
is_del:
        ;; Is this word immediate? (assuming it's a pointer returned by FIND)
;; it on the screen.
        jp actual_word
        jp actual_word_write
        jp branch
        jp c, fal
        jp c, fblk_fail
        jp c, tru
        jp done
        jp done_cont
        jp fal
        jp find_loop
        jp (hl)
        jp (hl)         ;; 13
        jp key_loop
        jp nc, add16to32_done
        jp nc, gt_check_neq
        jp nc, tru
        jp nz, dup
        jp nz, find_retry
        jp nz, find_retry_cont
        jp nz, find_succ_hidden
        jp nz, not_del
        jp nz, not_enter
        jp nz, print_stack_error
        jp nz, zbranch_fail
        jp nz,zbranch_fail
        jp    pe,FreqOutLoop2
        jp skip_comment
        jp skip_space
        jp tru
        jp word_retry
        jp write_char
        jp z, add16to32_done
        jp z, akey_asm
        jp z, akey_return_space
        jp z, empty_word
        jp z, empty_word  ;; get_char_asm returned nothing, so we need to retry
        jp z, fal
        jp z, find_fail
        jp z, find_maybe_fail
        jp z, find_succeed
        jp z, get_char_end
        jp z, is_del
        jp z, key_loop
        jp z, no_chars
        jp z, skip_comment
        jp z, skip_space
        jp z, strchr_succ
        jp z, tru
        jp z, word_done
        jp z, write_space
        jp z, zbranch_maybe
        jr  c, dd_overflow
        jr    c,FreqOutLoop1
        jr nc, $ + 3
        jr nc, $+3
        jr nc, $+4
        jr  nc, dd_setBit
        jr nc, mul32by8_noAdd
        jr nz, mul32by8_loop
        jr nz, not_backup_line
        jr nz, not_clear
        jr nz, strcmp_exit
        jr strchr_loop
        jr strcmp_loop
        jr    z,FreqOutDone
        jr z, key_loop
        jr z, strchr_fail
        jr z, strcmp_end
        ;; jr z, unget_char_done
key_asm:
key_loop:
key_table:
        ld a, ' '
        ld a, 0
        ld a, 128
        ld a, 64
        ld a,b
        ld a, b
        ld a, (bc)
        ld a, (bc) ;; Get the length and flags of the word.
        ld a, c
        ld a,c \ cpl \ ld c, a \ ld a,b \ cpl \ ld b, a
        ld a, $CD
        ld a, (de)
        ld a, (de)      ;; 7
        ld    a,e
        ld a, e
        ld    a,h
        ld a, h
        ld a, (hl)
        ld a, (IY + AppFlags)
        ld a, (IY + TextFlags)
        ld a, l
        ld a, ProgObj
        ld a, (var_precision)
        ld b, 0
        ld b, 0  ;; c should contain the number of characters.
        ld b, 0 ;; sanitize input, maybe?
        ld b, 32
        ld b, 7
        ld b, a
        ld b, a ;; B will store how many characters we have read.
        ld b, c
        ld bc, 0
        ld bc, 1
        ld bc, 48
        ld bc, 57
        ld bc, 7
        ld bc, $8292
        ld bc, 9999
        ld (bc), a
        ld    bc, data_end - data_start
        ld bc, scr_name
        ld bc, var_base
        ld bc, var_here
        ld bc, (var_latest)
        ld bc, var_latest
        ld bc, var_precision
        ld bc, var_state
        ld bc, var_sz
        ld b, d
        ld b, h
        ld b, (hl)
        ld (bit_cache), hl
        ld (bit_cache), ix
        ld b, (ix + 3)
        ld b, (ix + 7)
        ld b, ixh
        ld c, 1
        ld c, a
        ld c, b
        ld c, d
        ld c, e
        ld c, h
        ld c, (hl)
        ld c, (ix + 2)
        ld c, (ix + 6)
        ld c, ixl
        ld c, l
        ld (curcol), a
        ld (curCol), a
        ld (CurCol), a
        ld (currow), a
        ld (curRow), a
        ld d, a
        ld d, b
        ld de, 0040h	; 40h appends "01" to D
        ld (de), a
        ld de, blk_name_buffer
        ld de, blk_name_buffer ;; skip the tag byte.
        ld de, (gets_ptr)
        ld de, here_start
        ld de, key_table
        ld de, prog
        ld de, string_buffer
        ld de, (var_here)
        ld de, var_here
        ld de, (var_latest)
        ld de, var_latest
        ld de, word_buffer
        ld d, h
        ld    e,a
        ld e, a
        ld e, c
        ld e, l
        ld (gets_ptr), hl
        ld h, 0
        ld h, a
        ld h, a         ;; 4
        ld h, b
        ld h, d
        ld (hl), 0
        ld hl, 0
        ld (hl), 1
        ld hl, 1024
        ld (hl), 15
        ld (hl), 193
        ld (hl), 255
        ld hl, 255
        ld (hl), 34
        ld (hl), 4
        ld hl, $848e
        ld hl, 9999
        ld (hl), a
        ld (hl), b
        ld hl, (bit_cache)
        ld hl, bit_cache
        ld hl, blk_name_buffer
        ld (hl), c
        ld hl, CurCol
        ld hl, CurRow
        ld (hl), d
        ld    hl, data_start
        ld    hl, data_start - $9D95 + 4    ; have to add 4 because of tasmcmp token
        ld hl, docol
        ld hl, dodoes
        ld hl, does_brac
        ld (hl), e
        ld hl, (gets_ptr)
        ld hl, gets_ptr
        ld hl, name_dot_quote
        ld hl, name_lbrac
        ld hl, name_s_quote
        ld hl, possible_error_msg
        ld hl, (prog_exit)
        ld hl, prog_exit
        ld hl, (var_here)
        ld hl, var_here
        ld hl, (var_latest)
        ld hl, (var_sp)
        ld hl, (var_stack_empty)
        ld hl, var_state
        ld hl, word_buffer
        ld hl, (word_buffer_ptr)
        ld hl, word_buffer_ptr
        ldir
        ;; LDIR loads the value at (HL) to (DE), increments both,
        ld ixl, 8
        ld ix, return_stack_top
        ld ix, (save_ix)
        ld ix,(save_ix)
        ld (IY + AppFlags), a
        ld (IY + TextFlags), a
        ld l, %00000100
        ld l, %00001000
        ld l , a
        ld l, a
        ld l, a         ;; 4
        ld l, b
        ld l , c
        ld l, c
        ld (PenCol), a
        ld (PenRow), a
        ld (save_ix), ix
        ld (save_sp), sp
        ld sp, hl
        ld sp, (save_sp)
        ld (var_sp), sp
        ld (var_sz), sp
load_long_name_msg: .db "Name must be 8 characters or shorter. ", 0
load_not_found_msg1: .db "File ",0
load_not_found_msg2: .db " not found. ",0
        ;; Load the scratch buffer back into the current state.
        ;; location $9824, below AppBackupScreen at $9872.
        ;; lookup table.
;;; Macros to make life easier.
;; Make some variables!
        ;; MAKE SURE THIS IS THE LAST WORD TO BE DEFINED!
        ;; Make the last word immediate
        ;; Maybe it's the delete key?
        ;; Mimic exit
mul16By16:
;; mul16By16 [Maths]
mul32By8:
mul32by8_loop:
;; mul32By8 [Maths]
mul32by8_noAdd:
        ;; ( n1 n2 -- high_mult low_mult )
        ;; ( n^2 -- n )
        ;; ( n addr -- )
        ;; ( name_string name_len -- block_start )
        ;; ( name_string name_len -- data_start )
        ; need to clear the carry beforehand
        ;; Newline found.  Then go to start.
        ;; New version in progress, doesn't seem to work well.
next_sub:               ;; Cycle count (total 47)
;; NEXT, the basis of many Forth CODE words.
        ;; No characters left.
no_chars:
        nop
not_backup_line:
not_clear:
not_del:
not_enter:
;; not forth the effort trying to debug a macro (which took up a lot
        ;; Now DE = C, BC = D, (old-DE)
        ;; Now it contains the low quotient
        ;; Now we have to write the length of the new word.
        ;; Now we need to overwrite the destination of call docol with DE
        ;; Now we're at the length/flags pointer.
;; of time for defword and defcode!)
ok_msg: .db "ok",0
        ;; Old version of SEE
        ;; Opcode of CALL
        ;; opcode of call is CD <LOW> <HIGH> B6 9D seems to be the
        or a
        .org 9D93h
        or (hl)
        ;; Original column was 0, so we need to back up to the
        or    l
;; Our register allocations
        out    (0),a
        out (0),a       ;reset the port, else the user will be really annoyed.
;; Output:
        ;; Output: D
;; Output: none
;; Outputs:
        ;; Parameter stack :     ( addr len -- )
;;  Performs `ACIX = ACIX / DE`
;;  Performs `ACIX = ACIX + DE`
;;  Performs an unsigned multiplication of DE and BC.
;;  Performs an unsigned multiplication of DEHL and A.
p_FreqOut:
;; Place a false value on the top of the stack.
;; Place a truth value on the top of the stack.
        pop af
        pop bc
        pop    bc
        pop bc ;; FIXME: Check length.
        pop bc ;; get the high part
        pop bc ;; new top of stack
        pop bc ;; New top of stack
        pop bc ;; Save the place where this program needs to go.
        pop de
        pop de ;; get the second element from the stack
        pop hl
        pop ix
        pop ix ;; get the low part
        pop ix \ pop bc
;; Pop the top entry of the return stack to BC.
;; Pop the top entry of the return stack to DE.
possible_error_msg: .db "Warning: Stack not empty or underflowed.",0
        ;; previous line.
        ;; printable.
printhl_safe:
print_stack_error:
        ;; Print the top of the stack.
prog:
prog_exit: .dw 0
        ;; ( prog_start_ptr --  )
        push af
        push bc
        push    bc
        push bc ;; old stack top
        push bc \ push ix
;; Push BC to the return stack.
        push de
        push de ;; push address of string
        push de ;; push high part onto stack.
;; Push DE to the return stack.
        push hl
;; Push HL to the return stack.
        push ix
        ;; Push remainder
        ;; Ran out of input.
        ;; Read a floating point number.
        ;; Recall that Forth words start with a call to docol.  The
;; Refer to https://github.com/KnightOS/kernel/blob/830f4ac87c50fa42faf634ab3ee476f0ab85b741/src/00/strings.asm
        ;; Register contents (return stack contents)
        ;; Reinitialize the gets_ptr to point to the buffered input.
        ;; Remainder, then quotient.
        ;; Remember that var_here is a pointer, so you need to do
        ret
        ;; Return 0 if not found.
        ;; Return a pointer to the first occurrence of a character in a string
return_stack_top  .EQU    $91DC + 294
        rla
        rla \ jr nc, $+5 \ ld h, d \ ld l, e
        rl c
        rl d
        rl e
        rr e
        ;; ( s1 s2 -- bool )
;;  Same as z80 CP instruction.
        ;; Same goes for S" and ."
save_here:   .dw scratch
        ;; Save IP and TOS.
save_ix:   .dw 0
save_latest: .dw star
save_sp:   .dw 0
        sbc hl, bc
        sbc hl, de
        sbc hl, de		; optimised last iteration
        scf
        ;; scf
scratch:
scr_name: .db "SCRATCH",0
setup_data_segment:
;; Side effect:
        ;; Since we can't do ld hl, sp
skip_comment:
        ;; Skip null pointer.  (Even though we have the length, because
;; Skipping spaces, get the next word from the user.  Remember that
skip_space:
        ;; Skip the string.
        ;; Some other character.
        ;; ( source destination amount -- )
        ;; Special keys that actually write a space.
        ;; specific key that doesn't correspond to something
sqrt_la:
sqrt_loop:
        sra c
        srl d
start:
strchr_fail:
strchr_loop:
strchr_succ:
strcmp:
strcmp_end:
strcmp_exit:
strcmp_loop:
;; strcmp [Strings]
;; string_buffer contains the user input.
string_buffer: .fill 128,0
        ;; String length
        ;; ( string-ptr length-- pointer to word )
str_print:
str_println:
        sub c
        ;; Switch the input stream to the pointer on the stack.
;; system to be able to save and read programs.  We use the small
;; Taken from http://z80-heaven.wikidot.com/sound
        ;; Taken from the KnightOS kernel.
;;  The current gets_ptr may be at a different string buffer, so we can't compare it.
        ;; The second get_str_forth is necessary as we don't want the
        ;; The top of the stack contains the address of the data folloinwg
        ;; The top of the stack wasn't zero.  Skip the offset and resume execution.
        ;; The "x" gets replaced with "[" at program start, see "start:"
        ;; This b_call pushes to the floating point stack, at memory
  ;; This is the first word to be defined, so set the link.
;; this needs to be flexible enough to be called from Forth as well.
        ;; This word was bootstrapped from an interpreted definition.
        ;; TI-84 can't store the string "[", so we have to fix it by
;; TODO: 2NIP, 2TUCK, 2ROT, 2OVER
;; Trashes hl, de, returns the ASCII character in register A If the
tru:
undef_msg: .db " ?",0
unget_char:
unget_char_done:
;; user pressed enter, this is recorded as $00.  This routine keeps
;; variable width font instead of the large one, so that we may place
var_latest:
var_sp:
;; We also want immediate feedback to the user.
        ;; We could be reading from a text file.
        ;; we don't have a bcall Linux that can print out a string with
;; We expect it to return a pointer to the next word following
        ;; We found the word.  But is it hidden?
        ;; We got a space back as per the table, so it's not printable.  Try again.
        ;; We have to clear everything!
        ;; We have to do a very hacky thing.
        ;; we'll let the assembler do its job.
;; Well since there's only like 6 variables defined in Jonesforth it's
        ;; We need both because sometimes we might want to check for a
        ;; We reach here at the end of the program.
;; We really need a word.  Ask again!
        ;; We're still at 0 characters!  Restart.
;; We should be able to define an interpreter in Forth that supports compiled code if we try.
        ;; We should echo enter.
        ;; When WORD runs out of input it calls GETS, which resets gets_ptr
        ;; Which is the "action" part of the word being defined with DOES>.
                          ;; with get_str
        ;; without entering anything, we need to check for that too.
word_buffer:     .fill 32, 0
word_buffer_ptr: .dw 0
word_done:
word_restart:
word_retry:
        ;; works as a DEL.
write_char:
write_space:
        xor     3    ;this will toggle the lower two bits (the data being sent to the link port)
        xor a
        xor b
        xor (hl)
        xor l
        ;; Yep.  Let's give some feedback.
        ;; Yes, this is correct.  We are writing a call docol instruction manually.
zbranch_fail:
zbranch_maybe:
zero_blk_name_buffer:
;;  Z: Set if equal, reset if not equal
