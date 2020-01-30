(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-sets s1 s2)
  (filter (lambda (x) (element-of-set? x s2)) s1))

; 59)
(define (union-sets s1 s2)
  (fold-right adjoin-set s2 s1))
