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

; 2)

(define (average a b)
  (/ (+ a b) 2))

(define (make-point x y) (cons x y))

(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment start-point end-point) (cons start-point end-point))

(define (start-point segment) (car segment))
(define (end-point segment) (cdr segment))

(define (midpoint segment)
  (make-point (average (x-point (start-point segment))
                       (x-point (end-point segment))) 
              (average (y-point (start-point segment))
                       (y-point (end-point segment)))))

(define (print-point point)
  (newline)
  (display "(")
  (display (x-point point))
  (display ",")
  (display (y-point point))
  (display ")"))

; 3)

(define (make-rectangle point-a point-b)
  (let ((top-y (max (y-point point-a) (y-point point-b)))
        (bottom-y (min (y-point point-a) (y-point point-b)))
        (top-x (max (x-point point-a) (x-point point-b)))
        (bottom-x (min (x-point point-a) (x-point point-b))))
    (cons (cons (make-point bottom-y bottom-x) 
                (make-point top-y bottom-x))
          (cons (make-point bottom-y top-x)
                (make-point top-y top-x)))))


