(define (average a b) (/ (+ a b) 2))
(define (average-damp f) (lambda (x) (average x (f x))))

(load "./general-methods.lisp")

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
			   1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
			   1.0))

(define (derive g)
  (define dx 0.00001)
  (lambda (x)
	(/ (- (g (+ x dx)) (g x))
	   dx)))

(define (newton-transform g)
  (lambda (x)
	(- x (/ (g x) ((derive g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
				  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
							average-damp
							1.0))
