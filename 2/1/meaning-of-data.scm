(define (cons x y)
  (lambda (f) (if f x y)))

(define (car pair) (pair #t))
(define (cdr pair) (pair #f))
