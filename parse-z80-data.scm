(load "parse-utils.scm")
(load "assembler.scm")

(define comment
  (doMP* (char #\#)
         (up-to #\newline)
         (return '())))

(define operand
  (doMP* (symb "OPERAND")
         (gname <- (token (many alpha-num)))
         (gentry <- (<:> reg (token (many alpha-num))))
         (gopcode <- (token (many1 digit)))
         (return `(operand ,gname ,gentry ,gopcode))))

(define arch
  (doMP* (symb "ARCH")
         (arch-name <- (token (many alpha-num)))
         (return `(arch ,(string->symbol arch-name)))))

(define data-statements
  (<:> operand arch comment))

(define parse-data
  (doMP* (dat <- (sepby-nm data-statements (many (<:> comment (char #\space) (char #\newline)))))
         (char #\newline)
         (return dat)))
