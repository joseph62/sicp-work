(load "../../1/primality.scm")
(load "../../2/2/sequences-as-interfaces.scm")

(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count (iter (+ count 1) (+ count accum))))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

(define (stream-map f s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (f (stream-car s))
                   (stream-map f (stream-cdr s)))))

(define (stream-for-each f s)
  (if (stream-null? s)
      'done
      (begin (f (stream-car s))
             (stream-for-each f (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter p? s)
  (cond ((stream-null? s) the-empty-stream)
        ((p? (stream-car s)) (cons-stream (stream-car s)
                                          (stream-filter p? (stream-cdr s))))
        (else (stream-filter p? (stream-cdr s)))))


; Get second prime eager
; Constructs list from 10000 -> 1000000
; Then again with filter
; Then gets the second in the result
;(car (cdr (filter prime? (enumerate-interval 10000 1000000))))

; Get second prime lazy
; Get the value of the second result
; coming through the stream-filter 
; and the enumerate-interval
; One result at a time
; Until two results come through the filter
(stream-car
  (stream-cdr
    (stream-filter prime? (stream-enumerate-interval 10000 1000000))))
