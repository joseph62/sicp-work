(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

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
  (if (prime? n)
	(report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))


(define (next-odd n) (if (even? n) (+ n 1) n))

(define (search-for-primes start quantity)
  (define (iter n q)
	(cond ((= q quantity) q)
		  ((prime? n) (timed-prime-test n) (iter (+ n 2) (+ q 1)))
		  (else (iter (+ n 2) q))))
  (iter (next-odd start) 0))
