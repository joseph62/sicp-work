(define (average a b)
  (/ (+ a b) 2))

(define (search f negative-point positive-point)
  (define (close-enough? x y)
	(< (abs (- x y)) 0.001))
  (let ((mid-point (average negative-point positive-point)))
	(if (close-enough? negative-point positive-point)
	  mid-point
	  (let ((test-value (f mid-point)))
		(cond ((positive? test-value) 
			   (search f negative-point mid-point))
			  ((negative? test-value) 
			   (search f mid-point positive-point))
			  (else mid-point))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
		(b-value (f b)))
	(cond ((and (negative? a-value) (positive? b-value))
		   (search f a b))
		  ((and (positive? b-value) (negative? a-value))
		   (search f b a))
		  (else
			error "Values are not of opposite signs" a b))))

; find sin(x) = 0 between 2 and 4
(half-interval-method sin 2.0 4.0)

; find x^3 - 2x - 3 = 0 between 1 and 2
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		next
		(try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))


(define (print-fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (display "guess ")
	  (display guess) 
	  (display " next ")
	  (display next)
	  (newline)
	  (if (close-enough? guess next)
		next
		(try next))))
  (try first-guess))
