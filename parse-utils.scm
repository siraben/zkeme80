(use-modules (ice-9 textual-ports))

(include "monad.scm")
;; Monadic parsing in Scheme

;; Helper procedures `compile-port' and `emit' for output.
(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format #t "Not an output port ~s." p)))
     p)))

(define (emit . args)
  (apply format (compile-port) args)
  (format (compile-port) "")
  (newline (compile-port)))

;; Scheme doesn't treat strings like lists, but we can!
(define (string-car s) (string-ref s 0))
(define (string-cdr s) (substring s 1))

;; Scheme doesn't have a `show' procedure like in Haskell, so make our
;; own.
(define (to-string o)
  (if (char? o) (string o) (object->string o)))

;; Strings in Scheme aren't lists like in Haskell.  Unfortunately this
;; causes problems later, so we fix it with cons-string.

;; (cons-string #\h "ello") => "hello".
(define (cons-string c s)
  (if (null? s)
      (to-string c)
      (string-concatenate/shared `(,(to-string c) ,s))))

;; Parse a single character. 
;; Parser Char
(define item
  (lambda (s)
    (if (string-null? s)
        '()
        (cons (string-car s) (string-cdr s)))))


;; Given two parsers p and q, try p then if that fails try q.
(define +++
  (lambda (p q)
    (lambda (string)
      (let ((res (p string)))
        (if (null? res)
            (q string)
            res)))))

;; Choice operator
(define-syntax <:>
  (syntax-rules ()
    ((_ a)
     a)
    ((_ a b ...)
     (+++ a (<:> b ...)))))

;; Lift a predicate into a parser.
;; (Char -> Bool) -> Parser Char
(define sat
  (lambda (p)
    (doMP* (c <- item)
           (if (p c)
               (return c)
               fail))))

;; Make a parser that only accepts a certain character.
;; Char -> Parser Char
(define (char c)
  (sat (lambda (t)
         (eq? t c))))

;; Allows a parser p to be repeated fail or more times.
(define (many p)
  (<:> (many1 p) (return '())))

;; Allows a parser p to be repeated one or more times.
;; many and many1 are mutually recursive.
(define (many1 p)
  (doMP* (a <- p)
         (as <- (many p))
         ;; We use cons-string here because we want to possibly
         ;; collect individual characters into strings.
         (return (cons-string a as))))

;; Allows a parser p to be repeated fail or more times.
(define (many-n p)
  (<:> (many1-n p) (return '())))

;; Allows a parser p to be repeated one or more times.
;; many-n and many1-n are mutually recursive.
(define (many1-n p)
  (doMP* (a <- p)
         (as <- (many-n p))
         ;; We use cons here because we want collect whatever the
         ;; parser returned into a list.  This is a limitation of
         ;; using Scheme, as strings aren't lists of characters.
         (return (cons a as))))


;; Eat whitespace.
(define space
  (many (char #\space)))

;; Turn a parser p into a "token" parser, i.e. one that also eats up
;; whitespace following the parse.
(define (token p)
  (doMP* (a <- p)
         space
         (return a)))

;; Make a parser that only accepts a certain string s.
(define (str s)
  (if (string-null? s)
      (return '())
      (let ((c (string-car s))
            (cs (string-cdr s)))
        (doMP* (char c)
               (str cs)
               ;; Use string-concatenate/shared for possible speedup,
               ;; also because no mutation is performed.
               (return (string-concatenate/shared `(,(string c) ,cs)))))))

;; Tokenize a string.
(define (symb cs)
  (token (str cs)))

;; Before applying parser p, eat up leading whitespace.
(define (apply-p p)
  (doMP* space
         p))

;; Haven't found a good use for chainl and chainl1, not sure if it
;; works as expected.  Taken from Hutton's paper on monadic parsing.
(define (chainl p op a)
  (<:> (chainl1 p op)
       (return a)))

(define (chainl1 p op)
  (define (rest a)
    (<:> (letMP* ((f op)
                  (b p))
                 (rest (f a b)))
         (return a)))
  (letMP* ((a p))
          (rest a)))

;; Given a parser m and a predicate p, apply the parser and check the
;; result against the predicate, then succeed or fail based on that.
(define (:> m p)
  (letMP* ((a m))
          (if (p a)
              (return a)
              fail)))

;; Parse a single numeric character.
(define digit
  (doMP* (a <- (:> item char-numeric?))
         (return a)))

;; Parse a natural number.
(define nat
  (doMP* (xs <- (many1 digit))
         (return (string->number xs))))

;; A natural number, with whitespace following.
(define natural
  (token nat))

;; From a paper, forgot which one.
(define (sepby p sep)
  (<:> (sepby1 p sep)
       (return '())))

(define (sepby1 p sep)
  (doMP* (a <- p)
         (as <- (many (letMP* ((_ sep) (a p)) (return a))))
         (return (cons a as))))

(define (sepby-n p sep)
  (<:> (sepby1-n p sep)
       (return '())))

(define (sepby1-n p sep)
  (doMP* (a <- p)
         (as <- (many-n (letMP* ((_ sep) (a p)) (return a))))
         (return (cons a as))))

(define (sepby-nm p sep)
  (<:> (sepby1-nm p sep)
       (return '())))

(define (sepby1-nm p sep)
  (doMP* (a <- p)
         (as <- (many-n (letMP* ((_ sep) (a p)) (return a))))
         (return (append a as))))
;; Parse an alphabetic character.
(define alpha
  (:> item char-alphabetic?))

;; Parse an alphanumeric character.
(define alpha-num
  (:> item (lambda (x)
             (or (char-alphabetic? x)
                 (char-numeric? x)))))

;; Consume a string up to a given character.
(define (up-to c)
  (letMP* ((a (many (sat (lambda (x) (not (eq? x c)))))))
          (return a)))

;; Given a string, treat it like a character set and create a parser
;; that only accepts characters pertaining to that character set.
(define (oneof string)
  (sat (lambda (x) (char-set-contains? (string->char-set string) x))))

(define (noneof string)
  (sat (lambda (x) (not (char-set-contains? (string->char-set string) x)))))

(define (parse p s)
  (let ((a (p s)))
    (if (null? a)
        (emit "Parsing failed.")
        (if (not (string-null? (cdr a)))
            (begin (emit "Warning: Unconsumed input from position ~a, \"~a\""
                         (- (string-length s) (string-length (cdr a)))
                         (cdr a))
                   (car a))
            (car a)))))
(define (read-file-string filename)
  (let* ((port (open-input-file filename))
         (data (get-string-all port)))
    (close-port port)
    data))
