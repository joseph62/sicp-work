(eval '(begin
         (define (cons x y)
           (define is-lazy-pair true) 
           ; Put an identifier into the pair
           ; environment so it can be identified
           (lambda (m) (m x y))) 
         (define (car z)
           (z (lambda (x y) x))) 
         (define (cdr z)
           (z (lambda (x y) y)))
         (define p (cons 1 2)))
      the-global-environment)

(define (user-print object)
  (cond ((lazy-pair? object)
         (display-lazy-pair object))
        ((compound-procedure? object)
         (display-compound-procedure object))
        (else (display object))))

(define (display-compound-procedure procedure)
  (display (list 'compound-procedure
                 (procedure-parameters procedure)
                 (procedure-body procedure)
                 '<procedure-env>)))

(define (lazy-pair? object)
  (and (compound-procedure? object)
       (not (null? (filter
                     (lambda (var)
                       (eq? var 'is-lazy-pair))
                     (frame-variables
                       (first-frame 
                         (procedure-environment object))))))))

(define (display-lazy-pair lazy-pair)
  (display "(")
  (display 
    (force-it
      (cadr 
        (frame-values 
          (first-frame 
            (procedure-environment lazy-pair))))))
  (display " ...)"))

