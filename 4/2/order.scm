
(define (try a b)
  (if (= a 0) 1 b))
; (try 0 (/ 1 0))
; Error with applicative order
; Completes with normal order

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (divide a b)
  (unless (= b 0)
          (/ a b)
          (begin (display "exception: returning 0")
                 0)))
; Only works with normal order execution 
