actual_word:
actual_word_loop:
actual_word_write:
        add hl, hl
        add ix, de
        add ix, ix
        ;; djnz clear_loop
        ;; djnz dd_loop
        ;; djnz sqrt_loop
        ex de,hl
        ex de, hl
        ex    de, hl            ;lhello
