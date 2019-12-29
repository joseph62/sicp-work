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

; 16)
(define (expt b n)
  (define (iter a counter)
	(cond ((= counter 1) a)
		  ((even? counter) (iter (* a (square b)) (/ counter 2)))
		  (else (iter (* a b) (- counter 1)))))
  (iter 1 n))

; 17)

(define (double n) (+ n n))
(define (halve n) (/ n 2)) ; cheatsy

(define (mult a b) 
  (if (= b 0)
	0
	(+ a (mult a (- b 1)))))

(define (fast-mult a b)
  (cond ((= b 0) 0)
		((even? b) (double (fast-mult a (halve b))))
		(else (+ a (fast-mult a (- b 1))))))

; 18)

(define (fast-iterative-mult a b)
  (define (iter product counter)
	(cond ((= counter 0) 0)
		  ((even? counter) (iter (+ product (double a)) (halve counter)))
		  (else (iter (+ product a) (- counter 1)))))
  (iter 0 b))

; 19)

(define (fast-fib n)
  (define (fast-fib-iter a b p q counter)
	(cond ((= counter 0) b)
		  ((even? counter)
		   (fast-fib-iter a
						  b ; Not quite sure about this, chief
						  (
						  ; 'q
						  (/ counter 2)))
		  (else (fast-fib-iter (+ (* b q) (* a q) (* a p))
							   (+ (* b p) (* a q))
							   p
							   q
							   (- counter 1)))))
  (fast-fib-iter 1 0 0 1 n))

; 20)

(define (gcd a b)
  (if (= b 0) 
	a
	(gcd b (remainder a b))))
; Normal Order
(gcd 206 40)
(gcd 40 (remainder 206 40))
; (= (remainder 206 40) 0) <-- 6
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; (= (remainder 40 (remainder 206 40)) 0) <--  4
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) 
; (= (remainder 6 4) 0) <--- 2
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
; (= (remainder 4 2) 0) <--- 0 TRUE
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; (remainder 6 (remainder 40 6))
; (remainder 6 4)
; 2

; Applicative Order
(gcd 206 40)
; (= 40 0)
(gcd 40 (remainder 206 40))
(gcd 40 6)
; (= 6 0)
(gcd 6 (remainder 40 6))
(gcd 6 4)
; (= 4 0)
(gcd 4 (remainder 6 4))
(gcd 4 2)
; (= 2 0)
(gcd 2 (remainder 4 2))
; (= 0 0)
; 2

; 22)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
	(report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (current-odd n) (if (even? n) (+ n 1) n))

(define (search-for-primes start quantity)
  (define (iter n q)
	(cond ((= q quantity) q)
		  ((prime? n) (timed-prime-test n) (iter (+ n 2) (+ q 1)))
		  (else (iter (+ n 2) q))))
  (iter (current-odd start) 0))

; There does not seem to be a functioning runtime procedure

(search-for-primes 1000 3)
; 1009***0
; 1013***0
; 1019***0

(search-for-primes 10000 3)
; 10007***0
; 10009***0
; 10037***0

; 23)

(define (next n) (if (= n 2) 3 (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; runtime does not work as expected so no benchmarking...

; 24)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
		((even? exp)
		 (remainder (square (expmod base (/ exp 2) m))
					m))
		(else
		  (remainder (* base (expmod base (- exp 1) m))
					 m))))

(define (fermat-test n)
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fermat-test n)
	(report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

; 25)
; Using the brief version of the expmod procedure results in very large calculations

; 26)
; Using the general multiplication method rather than the doubling method results in 
; slightly worse performance because the doubling algorithm only needs to add the argument to itself
; where the general procedure needs to do more computation

; 27) - 28) skip for now

; 29)


(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (simpson-coefficient k)
    (cond ((= k 0) 1)
          ((even? k) 2)
          (else 4)))
  (define (simpson-iter k)
    (if (> k n)
      0
      (+ (* (simpson-coefficient k) (f (+ a (* k h)))) (simpson-iter (+ k 1)))))
  (simpson-iter 0))

; 30)

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
   (iter a 0))

; 31)

(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      1
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (define (next n) (+ n 1))
  (product identity 1 next n))

; 32)

(define (accumulate combiner null-value term a next b)
  (define (iter a total)
    (if (> a b)
      total
      (iter (next a) (combiner (term a) total))))
  (iter a null-value))


(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) 
              (accumulate-recursive combiner null-value term (next a) next b))))

; 33)

(define (inc n) (+ n 1))

(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (iter a total)
    (if (> a b)
      total
      (iter (next a)
            (combiner (if (predicate (term a)) 
                        (term a) 
                        null-value)
                      total))))
  (iter a null-value))

(define (sum-of-prime-squares a b)
   (accumulate prime? + 0 square a inc b))

(define (relative-prime-product n)
  (define (relative-prime? m)
    (= (gcd m n) 1))
  (accumulate relative-prime? * 1 identity a inc b))

; 34)

(define (f g)
  (g 2))

(f square)
; 2

(f (lambda (x) (* x (+ x 1))))
; 6

(f f)
; (f 2)
; (2 2)
; There is no procedure named 2. error

; 35)
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
; 1.61803278

; 36)
(print-fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)

; about 35 iterations before returning

; 37)
(define (cont-frac n d k)
  (define (recurse i)
	(if (= i k)
	  (/ (n i) (d i))
	  (/ (n i) (+ (d i) (recurse (+ i 1))))))
  (recurse 0))

(define (cont-frac-iter n d k)
  (define (iter i total)
	(if (= i 0)
	  total
	  (iter (- i 1) (/ (n i) (+ (d i) total)))))
  (iter k 0))


; 38)
(define e (cont-frac (lambda (i) 1.0)
					 (lambda (i)
					   (cond ((= i 1) 1)
							 ((= (remainder i 3) 2) (* 2 (+ (quotient i 3) 1)))
							 (else 1)))
					 1000))

; 39
(define (tan angle k)
  (cont-frac (lambda (i) 
			   (if (= i 1) 
				 x 
				 (square x)))
			 (lambda (i) (- (* i 2) 1))
			 k))

; 40)
(define (cubic a b c)
  (lambda (x) (+ (* x x x)
				 (* a x x)
				 (* b x)
				 c)))

; 41)

(define (double f)
  (lambda (x) (f (f x))))
 
; 42)
(define (compose f g)
  (lambda (x) (f (g x))))

(define (double f)
  (compose f f))

; 43)
(define (repeated f n)
  (if (= n 0)
	f
	(compose f (repeated f (- n 1)))))

(define (repeated-iter f n)
  (define (iter i g)
	(if (> i n)
	  g
	  (iter (+ i 1) (compose f g))))
  (iter 0 f))

; 44)
(define (average-3 a b c) (/ (+ a b c) 3))
(define (smooth f)
  (define dx 0.0001)
  (lambda (x) (average-3 (f (- x dx)) (f x) (f (+ x dx)))))

(define (smooth-n f n)
  ((repeated smooth n) f))

; 45)
; -- Yikes, I don't know...

; 46)
(define (iterative-improve good-guess? improve-guess)
  (define (iter guess)
	(if (good-guess? guess)
	  guess
	  (iter (improve-guess guess)))))

(define (sqrt x)
  ((iterative-improve (lambda (guess)
						(< (abs (- (square guess) x)) 0.001))
					  (lambda (guess)
						(average guess (/ x guess)))) 1.0))
