; 1)

; (10)
; 10

(+ 5 3 4)
; 12

(- 9 1)
; 8

(/ 6 2)
; 3

(+ (* 2 4) (- 4 6))
; 6

(define a 3)
; a

(define b (+ a 1))
; b

(+ a b (* a b))
; 19

(= a b)
; #f

(if (and (> b a) (< b (* a b)))
  b
  a)
; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; 16

(+ 2 (if (> b a) b a))
; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
; 16

; 2)
(/ (+ 5
      4
      (- 2 
         (- 3 
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

; 3)
(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (sum-of-squares-largest x y z)
  (cond ((= x (min x y z)) (sum-of-squares y z))
        ((= y (min x y z)) (sum-of-squares x z))
        ((= z (min x y z)) (sum-of-squares x y)))

; 4)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; Add b to a if b is positive, else subtract b from a

; 5)
(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))
(test 0 (p))
; Applicative Order: (p) is evaluated first and recurses without stopping
; Normal Order: The arguments to test are applied to the underlying procedure and (p) is never evaluated

; 6)
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
		(else else-clause)))
; If new-if is used to implement the Newton approximation it will result in an infinite recursion because
; the special form delays evaluation of expressions while this version does not.

; 7)

(define (new-good-enough? previous-guess current-guess)
  (< (abs (- previous-guess current-guess)) 0.0001))

; 8)
(define (cube-root x) 
  (cube-root-iter 0.0 1.0 x))

(define (cube-root-iter previous-guess guess x)
  (if (new-good-enough? previous-guess guess)
	guess
	(cube-root-iter guess (improve-cube guess x) x)))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess))
		(* 2 guess))
	 3))

; 9)

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (+ a b) ; Recursive Process
  (if (= a 0)
	b
	(inc (+ (dec a) b))))

(+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc (5)))))
; (inc (inc (inc (6))))
; (inc (inc (7)))
; (inc (8))
; (9)

(define (++ a b) ; Iterative Process
  (if (= a 0)
	b
	(++ (dec a) (inc b))))
(++ 4 5)
; (++ 3 6)
; (++ 2 7)
; (++ 1 8)
; (++ 0 9)
; (9)

; 10)
(define (A x y)
  (cond ((= y 0) 0)
		((= x 0) (* 2 y))
		((= y 1) 2)
		(else (A (- x 1)
				 (A x (- y 1))))))
(A 1 10)
; 1024

(A 2 4)
; 65535

(A 3 3)
; 65535

(define (f n) (A 0 n))
; f(n) = 2n

(define (g n) (A 1 n))
; g(n) = 2^n

(define (k n) (A 2 n))
; k(n) = ???

; 11)
; f(n) = n if n < 3
; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n>=3
(define (solution-11 n)
  (cond ((< n 3) n)
		(else (+ (solution-11 (- n 1))
				 (* 2 (solution-11 (- n 2)))
				 (* 3 (solution-11 (- n 3)))))))

(define (solution-11-2 n)
  (define (iter n-1 n-2 n-3 counter)
    (cond ((< n 3) n)
          ((> counter n) n-1)
          (else (iter (+ n-1
                         (* 2 n-2)
                         (* 3 n-3))
                      n-1
                      n-2))))
  (iter 2 1 0 3))

; 12)

(define (is-edge? row column)
  (or (= column 0) (= row column)))

(define (pascal-triangle-value row column) 
  (if (is-edge? row column)
    1
    (+ (pascal-triangle-value (- row 1) column)
       (pascal-triangle-value (- row 1) (- column 1)))))

; 13)
; This is so far over my head I will be skipping over it. Should revisit though...

; 14)
; (count-change 11)
; (cc 11 5)
; (+ (cc 11 4) (cc -39 5))
; (+ (+ (cc 11 3) (cc -14 4)) 0)
; (+ (+ (+ (cc 11 2) (cc 1 3))) 0)
; (+ (+ (+ (+ (cc 11 1) (cc 6 2)) (+ (cc 1 2) (cc -9 3)))) 0)
; (+ (+ (+ (+ (+ (cc 11 0) (cc 10 1)) (+ (cc 6 1) (cc 1 2)) (+ (+ (cc 1 1) (cc -4 2)) 0)))) 0)
; (+ (+ (+ (+ (+ 0 (+ (cc 10 0) (cc 9 1))) (+ (+ (cc 6 0) (cc 5 1)) (+ (cc 1 1) (cc -4 2))) (+ (+ (+ (cc 1 0) (cc 0 1)) 0) 0)))) 0)

; 15)

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))
; a) (sine 12.15) ~ 3 levels deep
; b) linear recursive procedure -- linear steps, linear space
