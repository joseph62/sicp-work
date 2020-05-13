(load "streams.scm")

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(stream-ref no-sevens 100)

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)
; 233

(define one (cons-stream 1 one))

(define (repeat n)
  (cons-stream n (repeat n)))

(define twos (repeat 2))

(define (combine-streams s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (combine-streams (stream-cdr s1)
                                    s2))))
(define (combine-streams . streams)
  (cond ((null? streams) the-empty-stream)
        ((empty-stream? (car streams)) 
         (apply combine-streams (cdr streams)))
        (else (cons-stream (stream-car (car streams))
                                       (apply combine-streams
                                              (cons (stream-cdr (car streams)))
                                                    (cdr streams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams one integers)))

(define fibs (cons-stream 0
                          (cons-stream 1
                                       (add-streams (stream-cdr fibs)
                                                    fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define (primes? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define primes
  (cons-stream
    2
    (stream-filter primes? (integers-starting-from 3))))
