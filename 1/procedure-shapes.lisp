(define (factorial x) ; Linear Recursive Process - Linear space complexity
  (if (= x 1)
	1
	(* x (factorial (- x 1)))))

(define (factorial2 x) ; Linear Iterative Process - Constant space complexity
  (define (iter product counter)
	(if (> counter x)
	  product
	  (iter (* product counter)
			(+ counter 1))))
  (iter 1 1))
