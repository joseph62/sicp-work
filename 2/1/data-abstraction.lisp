(define (make-rat n d)
  (let ((g (gcd n d)))
	(cons (/ n g) 
		  (abs (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer y) (denom x)) 
		(* (numer x) (denom y))) 
	 (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) 
			   (* (numer y) (denom x))) 
			(* (denom x) (denom y))))

(define (mult-rat x y)
  (make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
			(* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
	 (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline)
  x)
