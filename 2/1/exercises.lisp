; 1)

(define (negative-rat? n d)
  (or (and (positive? n) (negative? d))
	  (and (negative? n) (positive? d))))

(define (make-rat n d)
  (let ((g (gcd n d))
		(sign (if (negative-rat? n d) 
				-1 
				1)))
	(cons (* (abs (/ n g))
			 sign)
		  (abs (/ d g)))))

