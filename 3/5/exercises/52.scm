(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; sum: 1
(define y (stream-filter even? seq))
; sum: 6
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                       seq))
; sum: 10

(stream-ref y 7)
; sum = 136

(display-stream z)
; sum = 210
; 10 15 45 55 105 120 190 210

; The memoization does not factor in for the above usages
; because the stream delay objects are not reused. rather
; the usages of y and z trade off advancing seq

