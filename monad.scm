;; Monadic return
(define (return a)
  (lambda (state)
    (cons a state)))

;; Parser a -> (a -> Parser b) -> Parser b
(define (>>=P m k)
  (lambda (x)
    (let* ((ay (m x)))
      (if (null? ay)
          '() ;; propagate failures
          (let* ((a (car ay))
                 (y (cdr ay)))
            ((k a) y))))))

;; State a -> (a -> State b) -> State b
(define (>>=S m k)
  (lambda (state)
    (let* ((m-result (m state))
           (result-tag (cdr m-result))
           (result-val (car m-result))
           (tagged-b (k result-val)))
      (tagged-b result-tag))))


;; Failure.
(define fail
  (lambda (s)
    '()))

;;; letM*
;; This allows for a similar notation to "do" in Haskell.
;; do a <- b
;;    c <- d
;;    return a : c

;; Is the same as

;; (letM* ((a b)
;;         (c d))
;;        (return (cons a c)))

(define-syntax letMP*
  (syntax-rules ()
    ((_ () expr) expr)
    ((_ ((name val) (name2 val2) ...) expr)
     (>>=P val
           (lambda (name)
             (letMP* ((name2 val2) ...)
                     expr))))))

(define-syntax letMS*
  (syntax-rules ()
    ((_ () expr) expr)
    ((_ ((name val) (name2 val2) ...) expr)
     (>>=S val
           (lambda (name)
             (letMS* ((name2 val2) ...)
                     expr))))))


(define-syntax doMP*
  (syntax-rules (let let* letrec letrec* <-)
    ((doMP* s)                         s)
    ((doMP* (x <- s) ss ...)           (>>=P s (lambda (x) (doMP* ss ...))))
    ((doMP* (let bs) ss ...)           (let bs (doMP* ss ...)))
    ((doMP* (let* bs) ss ...)          (let* bs (doMP* ss ...)))
    ((doMP* (letrec bs) ss ...)        (letrec bs (doMP* ss ...)))
    ((doMP* (letrec* bs) ss ...)       (letrec* bs (doMP* ss ...)))
    ((doMP* s ss ...)                  (>>=P s (lambda (_) (doMP* ss ...))))))




(define-syntax doMS*
  (syntax-rules (let let* letrec letrec* <-)
    ((doMS* s)                         s)
    ((doMS* (x <- s) ss ...)           (>>=S s (lambda (x) (doMS* ss ...))))
    ((doMS* (let bs) ss ...)           (let bs (doMS* ss ...)))
    ((doMS* (let* bs) ss ...)          (let* bs (doMS* ss ...)))
    ((doMS* (letrec bs) ss ...)        (letrec bs (doMS* ss ...)))
    ((doMS* (letrec* bs) ss ...)       (letrec* bs (doMS* ss ...)))
    ((doMS* s ss ...)                  (>>=S s (lambda (_) (doMS* ss ...))))))
