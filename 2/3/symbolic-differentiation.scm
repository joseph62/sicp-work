(define (operation expression)
  (cadr expression))

(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? e n)
  (and (number? e) (= e n)))

; 57)
(define (make-sum a . rest)
  (let ((b (if (null? rest) () (apply make-sum rest))))
    (cond ((null? b) a)
          ((=number? a 0) b)
          ((=number? b 0) a)
          ((and (number? a) (number? b)) (+ a b))
          ; 58)
          (else (list a '+ b)))))

(define (addend sum)
  (car sum))

(define (augend sum)
  (if (> (length sum) 3) 
    (apply make-sum (cddr sum))
    (caddr sum)))


(define (sum? e)
  (and (pair? e) 
	   (eq? (operation e) '+)))

; 57)
(define (make-product a . rest)
  (let ((b (if (null? rest) () (apply make-product rest))))
    (cond ((null? b) a)
          ((or (=number? a 0) (=number? b 0)) 0)
          ((=number? a 1) b)
          ((=number? b 1) a)
          ; 58)
          (else (list a '* b)))))

(define (multiplier product)
  (car product))

(define (multiplicand product)
  (if (> (length product) 3)
    (apply make-product (cddr product))
    (caddr product)))

(define (product? e)
  (and (pair? e)
	   (eq? (operation e) '*)))

; 56)

(define (make-exponent b p)
  (cond ((=number? b 0) 0)
		((=number? b 1) 1)
		((=number? p 0) 1)
		((=number? p 1) b)
		((and (number? b) (number? p)) (expt b p))
    ; 58)
		(else '(b ** p))))

(define (base e)
  (car e))

(define (power e)
  (caddr e))

(define (exponent? e)
  (and (pair? e) (eq? (operation e) '**)))

(define (make-difference a b)
  (cond ((=number? b 0) a)
        ((and (number? a) (number? b)) (- a b))
        ; 58
        (else '(a - b))))

(define (subtrahend difference)
  (car difference))

(define (minuend difference)
  (caddr difference))

(define (difference? e)
  (and (pair? e) (eq? (operation e) '-)))

(define (deriv e var)
  (cond ((number? e) 0)
		((variable? e)
		 (if (same-variable? e var) 1 0))
		((sum? e)
		 (make-sum (deriv (addend e) var)
               (deriv (augend e) var)))
		((product? e)
		 (make-sum (make-product (multiplier e)
                             (deriv (multiplicand e) var))
               (make-product (deriv (multiplier e) var)
                             (multiplicand e))))
		; 56) cont....
    ((difference? e)
     (make-difference (deriv (subtrahend e) var)
                      (deriv (minuend e) var)))
		((exponent? e)
     (make-product (power e)
                   (make-product (make-exponent (base e)
                                                (make-difference (power e) 1))
                                 (derive (base e) var))))
		(else
      (error "unknown expression type -- DERIV" e))))



