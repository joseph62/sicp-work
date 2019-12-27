(define (square x) 
  (* x x))

(define (average x y) 
  (/ (+ x y) 2.0))

(define (square-root x) 
	(define (improve guess)
	  (average guess (/ x guess)))

	(define (good-enough? guess)
	  (< (abs (- (square guess) x)) 0.001))

	(define (square-root-iter guess) 
	  (if (good-enough? guess)
		guess
		(square-root-iter (improve guess))))

	(square-root-iter 1.0))
