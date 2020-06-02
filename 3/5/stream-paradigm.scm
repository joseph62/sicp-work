(load "infinite-streams.scm")
(load "exercises/55.scm")

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car 
              (make-tableau transform s)))

(define (stream-chain stream-of-streams)
  (cond ((stream-null? stream-of-streams) the-empty-stream)
        ((stream-null? (stream-car stream-of-streams)) 
         (stream-chain (stream-cdr stream-of-streams)))
        (else 
          (let ((first-stream (stream-car stream-of-streams))
                (rest-streams (stream-cdr stream-of-streams)))
            (cons-stream (stream-car first-stream)
                         (stream-chain (cons-stream (stream-cdr first-stream)
                                                    rest-streams)))))))

(define (stream-while predicate? stream)
  (if (predicate? (stream-car stream))
      (cons-stream (stream-car stream)
                   (stream-while predicate?
                                 (stream-cdr stream)))
      the-empty-stream))

(define int-pairs
  (stream-chain 
    (stream-map (lambda (i)
                  (stream-map (lambda (j)
                                (list j i))
                              (stream-while (lambda (k)
                                              (<= k i))
                                            integers)))
                integers)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (pairs (stream-cdr s) (stream-cdr t)))))

  
(define int-pairs (pairs integers integers))

(define prime-sum-pairs 
  (stream-filter (lambda (pair)
                   (prime? (+ (car pair) (cadr pair))))
                 int-pairs))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-stream (scale-stream integrand dt)
                             int)))
  int)
