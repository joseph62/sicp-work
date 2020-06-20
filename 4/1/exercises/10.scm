(define (make-if predicate consequent alternative)
  (list consequent 'if predicate 'else alternative))

(define (make-one-armed-if predicate consequent)
  (list consequent 'if predicate))

(define (if? exp)
  (eq? (cadr exp) 'if))

(define (if-consequent exp) (car exp))

(define (if-alternative exp)
  (if (= (length exp) 5)
	(car (cddddr exp))
	'false))

(define alternate-if-example
  '((display "even") if (even? 4) else (display "odd")))
