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

(define (hypotenuse a b)
  (sqrt (+ (square a)
           (square b))))

(define (segment-length segment)
  (hypotenuse (- (x-point (start-point segment))
                 (x-point (end-point segment))))
              (- (y-point (start-point segment))
                 (y-point (end-point segment))))

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

(define (rectangle-area rectangle)
  (* (rectangle-width rectangle)
     (rectangle-height rectangle)))

(define (rectangle-perimeter rectangle)
  (+ (* 2 (rectangle-width rectangle))
     (* 2 (rectangle-height rectangle))))

(define (rectangle-width rectangle)
  (segment-length (make-segment (x-point (car (car rectangle)))
                                (x-point (cdr (car rectangle))))))

(define (rectangle-height rectangle)
  (segment-length (make-segment (y-point (car (car rectangle)))
                                (y-point (car (cdr rectangle))))))


(define (make-rectangle point-a point-b)
  (let ((top-y (max (y-point point-a) (y-point point-b)))
        (bottom-y (min (y-point point-a) (y-point point-b)))
        (top-x (max (x-point point-a) (x-point point-b)))
        (bottom-x (min (x-point point-a) (x-point point-b))))
    (cons (make-segment (make-point bottom-y bottom-x) 
                        (make-point top-y bottom-x))
          (make-segment (make-point bottom-y top-x)
                        (make-point top-y top-x)))))

(define (rectangle-area rectangle)
  (* (rectangle-width rectangle)
     (rectangle-height rectangle)))

(define (rectangle-perimeter rectangle)
  (+ (* 2 (rectangle-width rectangle))
     (* 2 (rectangle-height rectangle))))

(define (rectangle-width rectangle)
  (segment-length (car rectangle)))

(define (rectangle-height rectangle)
  (segment-length (cdr rectangle)))



(define (rectangle-area rectangle)
  (* (rectangle-width rectangle)
     (rectangle-height rectangle)))

(define (rectangle-perimeter rectangle)
  (+ (* 2 (rectangle-width rectangle))
     (* 2 (rectangle-height rectangle))))

; 4)

(define (cons x y)
  (lambda (m) (m x y)))

(define (car pair)
  (pair (lambda (x y) x)))

(define (cdr pair)
  (pair (lambda (x y) y)))

; 5)

(define (divisible? x y)
  (= (remainder x y) 0))

(define (inc x) (+ x 1))
;
;(define (cons x y)
;  (* (expt 2 x) (expt 3 y)))
;
;(define (car pair)
;  (define (iter total pair)
;	(if (divisible? pair 2)
;		(iter (inc total) (/ pair 2))
;		total))
;  (iter 0 pair))
;
;(define (cdr pair)
;  (define (iter total pair)
;	(if (divisible? pair 3)
;		(iter (inc total) (/ pair 3))
;		total))
;  (iter 0 pair))
;
; 6)

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add g h)
  (lambda (f) (lambda (x) (g (h x)))))

; 7)
(load "interval-arithmetic.scm")

(define (make-interval a b) (cons a b))


(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

; 8) 

; (1,2)   - (3,4)   = (1-4,2-3)   = (-3,-1) = (l1-h2,h1-l2)
; (1,2)   - (-4,-3) = (1+3,2+4)   = (4,6)   = (l1-h2,h1-l2)
; (1,2)   - (-3,4)  = (1-4,2+3)   = (-3,5)  = (l1-h2,h1-l2)
; (-2,-1) - (3,4)   = (-2-4,-1-3) = (-6,-4) = (l1-h2,h1-l2)
; (-2,-1) - (-4,-3) = (-2+3,-1+4) = (1,3)   = (l1-h2,h1-l2)
; (-2,-1) - (-3, 4) = (-2-4,-1+3) = (-6,2)  = (l1-h2,h1-l2)
; (-1,2)  - (3,4)   = (-1-4,2-3)  = (-5,-1) = (l1-h2,h1-l2)
; (-1,2)  - (-4,-3) = (-1+3,2+4)  = (2,6)   = (l1-h2,h1-l2)
; (-1,2)  - (-3,4)  = (-1-4,2+3)  = (-5,5)  = (l1-h2,h1-l2)

(define (sub-interval x y)
  (make-interval (- (lower-bound x) 
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

; 9)

(define (width interval)
  (- (upper-bound interval) (lower-bound interval)))

(define a (make-interval 8.55 9.45))
(define b (make-interval 3.6 4.4))
(define c (add-interval a b))
(define d (mul-interval a b))

(= (+ (width a) (width b)) (width c))
(not (= (* (width a) (width b)) (width d)))

; 10)

(define (interval-contains-zero? a)
  (or (and (negative? (lower-bound a)) 
           (positive? (upper-bound a)))
      (zero? (lower-bound a))
      (zero? (upper-bound a))))

(define (div-interval x y)
  (if (inteval-contains-zero? y)
    (error "The divisor interval cannot contain zero!")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

; 11)

(define (interval-negative? a) 
  (negative? (upper-bound a)))

(define (interval-positive? a)
  (positive? (lower-bound a)))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; ( 1, 2) * ( 3, 4) = (  1*3,  2*4) = (l1*l2,h1*h2)
; ( 1, 2) * (-4,-3) = ( 2*-4, 1*-3) = (h1*l2,l1*h2)
; ( 1, 2) * (-3, 4) = ( 2*-3,  2*4) = (h1*l2,h1*h2)
; (-2,-1) * ( 3, 4) = ( -2*4, -1*3) = (l1*h2,h1*l2)
; (-2,-1) * (-4,-3) = (-1*-3,-2*-4) = (h1*h2,l1*l2)
; (-2,-1) * (-3, 4) = ( -2*4,-2*-3) = (l1*h2,l1*l2)
; (-1, 2) * ( 3, 4) = ( -1*4,  2*4) = (l1*h2,h1*h2)
; (-1, 2) * (-4,-3) = ( 2*-4,-1*-4) = (h1*l2,l1*l2)
; (-1, 2) * (-3, 4) = ( -2*3,  2*4) = (h1*l2,h1*h2)

; <shrug shoulders>


; 12)

(define (percent-to-decimal p) (/ p 100.0))

(define (decimal-to-percent d) (* d 100.0))

(define (make-center-percent c p)
  (let ((w (percent-to-decimal p)))
    (make-interval (- c w) (+ c w))))

(define (percent i)
  (deciaml-to-percent (width i)))

; 13) 
; <huge shrug of shoulders>


