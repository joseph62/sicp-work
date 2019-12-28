(define (expt b n)
  (if (= n 0)
	1
	(* b (expt b (- n 1)))))

(define (expt-iterative b n)
  (define (iter counter product)
	(if (= counter 0)
	  product
	  (iter (* product b) (- counter 1))))
  (iter 0 1))

(define (even? n)
  (= (remainder n 2) 0))

(define (successive-squaring b n)
  (cond ((= n 0) 1)
		((even? n) (square (successive-squaring b (/ n 2))))
		(else (* b (successive-squaring b (- n 1))))))
