(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? e n)
  (and (number? e) (= e n)))

(define (make-sum a b)
  (cond ((=number? a 0) b)
		((=number? b 0) a)
		((and (number? a) (number? b)) (+ a b))
		(else (list '+ a b))))

(define (addend sum)
  (cadr sum))

(define (augend sum)
  (caddr sum))

(define (sum? e)
  (and (pair? e) 
	   (eq? (car e) '+)))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
		((=number? a 1) b)
		((=number? b 1) a)
		(else (list '* a b))))

(define (multiplier product)
  (cadr product))

(define (multiplicand product)
  (caddr product))

(define (product? e)
  (and (pair? e)
	   (eq? (car e) '*)))

; 56)

(define (make-exponent b p)
  (cond ((=number? b 0) 0)
		((=number? b 1) 1)
		((=number? p 0) 1)
		((=number? p 1) b)
		((and (number? b) (number? p)) (expt b p))
		(else (list '** b p))))

(define (base e)
  (cadr e))

(define (power e)
  (caddr e))

(define (exponent? e)
  (and (pair? e) (eq? (car e) '**)))

(define (make-sub a b)
  (list '- a b))

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
		((exponent? e)
		 (make-product (power e)
					   (make-product (make-exponent (base e)
													(make-sub (power e) 1))
									 (derive (base e) var))))
		(else
		  (error "unknown expression type -- DERIV" e))))



