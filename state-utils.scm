;; State monads and related things

;; Increment the current state
(define incr 
  (lambda (n)
    (cons n (1+ n))))

(define (numbered-apply f x)
  (doMS* (count <- incr)
         (return (cons (f x) count))))


(define (mapM func list)
  (if (null? list)
      (return '())
      (letMS* ((mapped-car (numbered-apply func (car list)))
               (mapped-cdr (mapM func (cdr list))))
              (return (cons mapped-car mapped-cdr)))))
