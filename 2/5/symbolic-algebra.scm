(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (list variable term-list))

  (define (variable p)
    (car p))

  (define (term-list p)
    (cadr p))

  (define (same-variable? p1 p2)
    (eq? (variable p1) (variable p2)))

  (define (variable? p)
    (symbol? p)) 

  ;; adjoin-term coeff

  (define (the-empty-termlist) (list))
  (define (adjoin-term term term-list)
    (cons term term-list))
  (define (empty-termlist? term-list)
    #t)
  (define (first-term term-list)
    (car term-list))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (make-term order coefficient)
    (list order coefficient))
  (define (order term)
    (car term))
  (define (coefficient term)
    (cadr term))


  (define (add-poly p1 p2)
    (if (same-variable? p1 p2)
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? p1 p2)
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var term-list) (tag (make-poly var term-list))))
  'done)
